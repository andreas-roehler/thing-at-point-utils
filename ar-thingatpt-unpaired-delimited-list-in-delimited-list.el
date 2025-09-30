;;; thingatpt-unpaired-delimited-list-in-delimited-list.el --- thing-in-thing functions -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2025 Andreas Röhler, unless
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

(defun ar-backslashed-in-braced-atpt ()
  "Employ actions of  at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th))
 
(defun ar-greaterangle-backslashed-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-greaterangle))
 
(defun ar-lesserangle-backslashed-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-lesserangle))
 
(defun ar-backslash-backslashed-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-backslash))
 
(defun ar-beg-backslashed-in-braced-atpt ()
  "Employ actions of BEG at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-beg))
 
(defun ar-blok-backslashed-in-braced-atpt ()
  "Employ actions of BLOK at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-blok))
 
(defun ar-bounds-backslashed-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-bounds))
 
(defun ar-brace-backslashed-in-braced-atpt ()
  "Employ actions of BRACE at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-brace))
 
(defun ar-bracket-backslashed-in-braced-atpt ()
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-bracket))
 
(defun ar-commatize-backslashed-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-commatize))
 
(defun ar-comment-backslashed-in-braced-atpt ()
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-comment))
 
(defun ar-dollar-backslashed-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-dollar))
 
(defun ar-double-backslash-backslashed-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-double-backslash))
 
(defun ar-doublequote-backslashed-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-doublequote))
 
(defun ar-doubleslash-backslashed-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-backslashed-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-doubleslash-paren))
 
(defun ar-end-backslashed-in-braced-atpt ()
  "Employ actions of END at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-end))
 
(defun ar-escape-backslashed-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-escape))
 
(defun ar-hide-backslashed-in-braced-atpt ()
  "Employ actions of HIDE at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-hide))
 
(defun ar-hide-show-backslashed-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-hide-show))
 
(defun ar-hyphen-backslashed-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-hyphen))
 
(defun ar-kill-backslashed-in-braced-atpt ()
  "Employ actions of KILL at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-kill))
 
(defun ar-left-right-singlequote-backslashed-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-left-right-singlequote))
 
(defun ar-length-backslashed-in-braced-atpt ()
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-length))
 
(defun ar-parentize-backslashed-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-parentize))
 
(defun ar-quote-backslashed-in-braced-atpt ()
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-quote))
 
(defun ar-separate-backslashed-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-separate))
 
(defun ar-show-backslashed-in-braced-atpt ()
  "Employ actions of SHOW at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-show))
 
(defun ar-singlequote-backslashed-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-singlequote))
 
(defun ar-slash-backslashed-in-braced-atpt ()
  "Employ actions of SLASH at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-slash))
 
(defun ar-slashparen-backslashed-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-slashparen))
 
(defun ar-sort-backslashed-in-braced-atpt ()
  "Employ actions of SORT at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-sort))
 
(defun ar-trim-backslashed-in-braced-atpt ()
  "Employ actions of TRIM at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-trim))
 
(defun ar-trim-left-backslashed-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-trim-left))
 
(defun ar-trim-right-backslashed-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-trim-right))
 
(defun ar-underscore-backslashed-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-underscore))
 
(defun ar-whitespace-backslashed-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-whitespace))
 
(defun ar-backslashed-in-bracketed-atpt ()
  "Employ actions of  at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th))
 
(defun ar-greaterangle-backslashed-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-greaterangle))
 
(defun ar-lesserangle-backslashed-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-lesserangle))
 
(defun ar-backslash-backslashed-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-backslash))
 
(defun ar-beg-backslashed-in-bracketed-atpt ()
  "Employ actions of BEG at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-beg))
 
(defun ar-blok-backslashed-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-blok))
 
(defun ar-bounds-backslashed-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-bounds))
 
(defun ar-brace-backslashed-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-brace))
 
(defun ar-bracket-backslashed-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-bracket))
 
(defun ar-commatize-backslashed-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-commatize))
 
(defun ar-comment-backslashed-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-comment))
 
(defun ar-dollar-backslashed-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-dollar))
 
(defun ar-double-backslash-backslashed-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-double-backslash))
 
(defun ar-doublequote-backslashed-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-doublequote))
 
(defun ar-doubleslash-backslashed-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-backslashed-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-doubleslash-paren))
 
(defun ar-end-backslashed-in-bracketed-atpt ()
  "Employ actions of END at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-end))
 
(defun ar-escape-backslashed-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-escape))
 
(defun ar-hide-backslashed-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-hide))
 
(defun ar-hide-show-backslashed-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-hide-show))
 
(defun ar-hyphen-backslashed-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-hyphen))
 
(defun ar-kill-backslashed-in-bracketed-atpt ()
  "Employ actions of KILL at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-kill))
 
(defun ar-left-right-singlequote-backslashed-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-left-right-singlequote))
 
(defun ar-length-backslashed-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-length))
 
(defun ar-parentize-backslashed-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-parentize))
 
(defun ar-quote-backslashed-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-quote))
 
(defun ar-separate-backslashed-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-separate))
 
(defun ar-show-backslashed-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-show))
 
(defun ar-singlequote-backslashed-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-singlequote))
 
(defun ar-slash-backslashed-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-slash))
 
(defun ar-slashparen-backslashed-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-slashparen))
 
(defun ar-sort-backslashed-in-bracketed-atpt ()
  "Employ actions of SORT at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-sort))
 
(defun ar-trim-backslashed-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-trim))
 
(defun ar-trim-left-backslashed-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-trim-left))
 
(defun ar-trim-right-backslashed-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-trim-right))
 
(defun ar-underscore-backslashed-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-underscore))
 
(defun ar-whitespace-backslashed-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-whitespace))
 
(defun ar-backslashed-in-lesserangled-atpt ()
  "Employ actions of  at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th))
 
(defun ar-greaterangle-backslashed-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-backslashed-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-lesserangle))
 
(defun ar-backslash-backslashed-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-backslash))
 
(defun ar-beg-backslashed-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-beg))
 
(defun ar-blok-backslashed-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-blok))
 
(defun ar-bounds-backslashed-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-bounds))
 
(defun ar-brace-backslashed-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-brace))
 
(defun ar-bracket-backslashed-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-bracket))
 
(defun ar-commatize-backslashed-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-commatize))
 
(defun ar-comment-backslashed-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-comment))
 
(defun ar-dollar-backslashed-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-dollar))
 
(defun ar-double-backslash-backslashed-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-double-backslash))
 
(defun ar-doublequote-backslashed-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-doublequote))
 
(defun ar-doubleslash-backslashed-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-backslashed-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-doubleslash-paren))
 
(defun ar-end-backslashed-in-lesserangled-atpt ()
  "Employ actions of END at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-end))
 
(defun ar-escape-backslashed-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-escape))
 
(defun ar-hide-backslashed-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-hide))
 
(defun ar-hide-show-backslashed-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-hide-show))
 
(defun ar-hyphen-backslashed-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-hyphen))
 
(defun ar-kill-backslashed-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-backslashed-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-left-right-singlequote))
 
(defun ar-length-backslashed-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-length))
 
(defun ar-parentize-backslashed-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-parentize))
 
(defun ar-quote-backslashed-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-quote))
 
(defun ar-separate-backslashed-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-separate))
 
(defun ar-show-backslashed-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-show))
 
(defun ar-singlequote-backslashed-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-singlequote))
 
(defun ar-slash-backslashed-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-slash))
 
(defun ar-slashparen-backslashed-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-slashparen))
 
(defun ar-sort-backslashed-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-sort))
 
(defun ar-trim-backslashed-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-trim))
 
(defun ar-trim-left-backslashed-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-trim-left))
 
(defun ar-trim-right-backslashed-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-trim-right))
 
(defun ar-underscore-backslashed-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-underscore))
 
(defun ar-whitespace-backslashed-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-whitespace))
 
(defun ar-backslashed-in-greaterangled-atpt ()
  "Employ actions of  at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th))
 
(defun ar-greaterangle-backslashed-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-backslashed-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-lesserangle))
 
(defun ar-backslash-backslashed-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-backslash))
 
(defun ar-beg-backslashed-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-beg))
 
(defun ar-blok-backslashed-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-blok))
 
(defun ar-bounds-backslashed-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-bounds))
 
(defun ar-brace-backslashed-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-brace))
 
(defun ar-bracket-backslashed-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-bracket))
 
(defun ar-commatize-backslashed-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-commatize))
 
(defun ar-comment-backslashed-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-comment))
 
(defun ar-dollar-backslashed-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-dollar))
 
(defun ar-double-backslash-backslashed-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-double-backslash))
 
(defun ar-doublequote-backslashed-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-doublequote))
 
(defun ar-doubleslash-backslashed-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-backslashed-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-doubleslash-paren))
 
(defun ar-end-backslashed-in-greaterangled-atpt ()
  "Employ actions of END at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-end))
 
(defun ar-escape-backslashed-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-escape))
 
(defun ar-hide-backslashed-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-hide))
 
(defun ar-hide-show-backslashed-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-hide-show))
 
(defun ar-hyphen-backslashed-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-hyphen))
 
(defun ar-kill-backslashed-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-backslashed-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-left-right-singlequote))
 
(defun ar-length-backslashed-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-length))
 
(defun ar-parentize-backslashed-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-parentize))
 
(defun ar-quote-backslashed-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-quote))
 
(defun ar-separate-backslashed-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-separate))
 
(defun ar-show-backslashed-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-show))
 
(defun ar-singlequote-backslashed-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-singlequote))
 
(defun ar-slash-backslashed-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-slash))
 
(defun ar-slashparen-backslashed-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-slashparen))
 
(defun ar-sort-backslashed-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-sort))
 
(defun ar-trim-backslashed-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-trim))
 
(defun ar-trim-left-backslashed-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-trim-left))
 
(defun ar-trim-right-backslashed-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-trim-right))
 
(defun ar-underscore-backslashed-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-underscore))
 
(defun ar-whitespace-backslashed-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-whitespace))
 
(defun ar-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th))
 
(defun ar-greaterangle-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-greaterangle))
 
(defun ar-lesserangle-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-lesserangle))
 
(defun ar-backslash-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-backslash))
 
(defun ar-beg-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-beg))
 
(defun ar-blok-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-blok))
 
(defun ar-bounds-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-bounds))
 
(defun ar-brace-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-brace))
 
(defun ar-bracket-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-bracket))
 
(defun ar-commatize-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-commatize))
 
(defun ar-comment-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-comment))
 
(defun ar-dollar-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-dollar))
 
(defun ar-double-backslash-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-double-backslash))
 
(defun ar-doublequote-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-doublequote))
 
(defun ar-doubleslash-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-doubleslash-paren))
 
(defun ar-end-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-end))
 
(defun ar-escape-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-escape))
 
(defun ar-hide-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-hide))
 
(defun ar-hide-show-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-hide-show))
 
(defun ar-hyphen-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-hyphen))
 
(defun ar-kill-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-kill))
 
(defun ar-left-right-singlequote-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-left-right-singlequote))
 
(defun ar-length-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-length))
 
(defun ar-parentize-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-parentize))
 
(defun ar-quote-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-quote))
 
(defun ar-separate-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-separate))
 
(defun ar-show-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-show))
 
(defun ar-singlequote-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-singlequote))
 
(defun ar-slash-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-slash))
 
(defun ar-slashparen-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-slashparen))
 
(defun ar-sort-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-sort))
 
(defun ar-trim-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-trim))
 
(defun ar-trim-left-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-trim-left))
 
(defun ar-trim-right-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-trim-right))
 
(defun ar-underscore-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-underscore))
 
(defun ar-whitespace-backslashed-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-whitespace))
 
(defun ar-backslashed-in-parentized-atpt ()
  "Employ actions of  at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th))
 
(defun ar-greaterangle-backslashed-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-greaterangle))
 
(defun ar-lesserangle-backslashed-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-lesserangle))
 
(defun ar-backslash-backslashed-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-backslash))
 
(defun ar-beg-backslashed-in-parentized-atpt ()
  "Employ actions of BEG at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-beg))
 
(defun ar-blok-backslashed-in-parentized-atpt ()
  "Employ actions of BLOK at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-blok))
 
(defun ar-bounds-backslashed-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-bounds))
 
(defun ar-brace-backslashed-in-parentized-atpt ()
  "Employ actions of BRACE at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-brace))
 
(defun ar-bracket-backslashed-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-bracket))
 
(defun ar-commatize-backslashed-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-commatize))
 
(defun ar-comment-backslashed-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-comment))
 
(defun ar-dollar-backslashed-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-dollar))
 
(defun ar-double-backslash-backslashed-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-double-backslash))
 
(defun ar-doublequote-backslashed-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-doublequote))
 
(defun ar-doubleslash-backslashed-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-backslashed-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-doubleslash-paren))
 
(defun ar-end-backslashed-in-parentized-atpt ()
  "Employ actions of END at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-end))
 
(defun ar-escape-backslashed-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-escape))
 
(defun ar-hide-backslashed-in-parentized-atpt ()
  "Employ actions of HIDE at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-hide))
 
(defun ar-hide-show-backslashed-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-hide-show))
 
(defun ar-hyphen-backslashed-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-hyphen))
 
(defun ar-kill-backslashed-in-parentized-atpt ()
  "Employ actions of KILL at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-kill))
 
(defun ar-left-right-singlequote-backslashed-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-left-right-singlequote))
 
(defun ar-length-backslashed-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-length))
 
(defun ar-parentize-backslashed-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-parentize))
 
(defun ar-quote-backslashed-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-quote))
 
(defun ar-separate-backslashed-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-separate))
 
(defun ar-show-backslashed-in-parentized-atpt ()
  "Employ actions of SHOW at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-show))
 
(defun ar-singlequote-backslashed-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-singlequote))
 
(defun ar-slash-backslashed-in-parentized-atpt ()
  "Employ actions of SLASH at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-slash))
 
(defun ar-slashparen-backslashed-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-slashparen))
 
(defun ar-sort-backslashed-in-parentized-atpt ()
  "Employ actions of SORT at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-sort))
 
(defun ar-trim-backslashed-in-parentized-atpt ()
  "Employ actions of TRIM at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-trim))
 
(defun ar-trim-left-backslashed-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-trim-left))
 
(defun ar-trim-right-backslashed-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-trim-right))
 
(defun ar-underscore-backslashed-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-underscore))
 
(defun ar-whitespace-backslashed-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-whitespace))
 
(defun ar-dollared-in-braced-atpt ()
  "Employ actions of  at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th))
 
(defun ar-greaterangle-dollared-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-greaterangle))
 
(defun ar-lesserangle-dollared-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-lesserangle))
 
(defun ar-backslash-dollared-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-backslash))
 
(defun ar-beg-dollared-in-braced-atpt ()
  "Employ actions of BEG at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-beg))
 
(defun ar-blok-dollared-in-braced-atpt ()
  "Employ actions of BLOK at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-blok))
 
(defun ar-bounds-dollared-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-bounds))
 
(defun ar-brace-dollared-in-braced-atpt ()
  "Employ actions of BRACE at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-brace))
 
(defun ar-bracket-dollared-in-braced-atpt ()
  "Employ actions of BRACKET at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-bracket))
 
(defun ar-commatize-dollared-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-commatize))
 
(defun ar-comment-dollared-in-braced-atpt ()
  "Employ actions of COMMENT at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-comment))
 
(defun ar-dollar-dollared-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-dollar))
 
(defun ar-double-backslash-dollared-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-double-backslash))
 
(defun ar-doublequote-dollared-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-doublequote))
 
(defun ar-doubleslash-dollared-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-dollared-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-doubleslash-paren))
 
(defun ar-end-dollared-in-braced-atpt ()
  "Employ actions of END at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-end))
 
(defun ar-escape-dollared-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-escape))
 
(defun ar-hide-dollared-in-braced-atpt ()
  "Employ actions of HIDE at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-hide))
 
(defun ar-hide-show-dollared-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-hide-show))
 
(defun ar-hyphen-dollared-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-hyphen))
 
(defun ar-kill-dollared-in-braced-atpt ()
  "Employ actions of KILL at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-kill))
 
(defun ar-left-right-singlequote-dollared-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-left-right-singlequote))
 
(defun ar-length-dollared-in-braced-atpt ()
  "Employ actions of LENGTH at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-length))
 
(defun ar-parentize-dollared-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-parentize))
 
(defun ar-quote-dollared-in-braced-atpt ()
  "Employ actions of QUOTE at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-quote))
 
(defun ar-separate-dollared-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-separate))
 
(defun ar-show-dollared-in-braced-atpt ()
  "Employ actions of SHOW at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-show))
 
(defun ar-singlequote-dollared-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-singlequote))
 
(defun ar-slash-dollared-in-braced-atpt ()
  "Employ actions of SLASH at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-slash))
 
(defun ar-slashparen-dollared-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-slashparen))
 
(defun ar-sort-dollared-in-braced-atpt ()
  "Employ actions of SORT at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-sort))
 
(defun ar-trim-dollared-in-braced-atpt ()
  "Employ actions of TRIM at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-trim))
 
(defun ar-trim-left-dollared-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-trim-left))
 
(defun ar-trim-right-dollared-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-trim-right))
 
(defun ar-underscore-dollared-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-underscore))
 
(defun ar-whitespace-dollared-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-whitespace))
 
(defun ar-dollared-in-bracketed-atpt ()
  "Employ actions of  at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th))
 
(defun ar-greaterangle-dollared-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-greaterangle))
 
(defun ar-lesserangle-dollared-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-lesserangle))
 
(defun ar-backslash-dollared-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-backslash))
 
(defun ar-beg-dollared-in-bracketed-atpt ()
  "Employ actions of BEG at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-beg))
 
(defun ar-blok-dollared-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-blok))
 
(defun ar-bounds-dollared-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-bounds))
 
(defun ar-brace-dollared-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-brace))
 
(defun ar-bracket-dollared-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-bracket))
 
(defun ar-commatize-dollared-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-commatize))
 
(defun ar-comment-dollared-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-comment))
 
(defun ar-dollar-dollared-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-dollar))
 
(defun ar-double-backslash-dollared-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-double-backslash))
 
(defun ar-doublequote-dollared-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-doublequote))
 
(defun ar-doubleslash-dollared-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-dollared-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-doubleslash-paren))
 
(defun ar-end-dollared-in-bracketed-atpt ()
  "Employ actions of END at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-end))
 
(defun ar-escape-dollared-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-escape))
 
(defun ar-hide-dollared-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-hide))
 
(defun ar-hide-show-dollared-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-hide-show))
 
(defun ar-hyphen-dollared-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-hyphen))
 
(defun ar-kill-dollared-in-bracketed-atpt ()
  "Employ actions of KILL at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-kill))
 
(defun ar-left-right-singlequote-dollared-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-left-right-singlequote))
 
(defun ar-length-dollared-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-length))
 
(defun ar-parentize-dollared-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-parentize))
 
(defun ar-quote-dollared-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-quote))
 
(defun ar-separate-dollared-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-separate))
 
(defun ar-show-dollared-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-show))
 
(defun ar-singlequote-dollared-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-singlequote))
 
(defun ar-slash-dollared-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-slash))
 
(defun ar-slashparen-dollared-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-slashparen))
 
(defun ar-sort-dollared-in-bracketed-atpt ()
  "Employ actions of SORT at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-sort))
 
(defun ar-trim-dollared-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-trim))
 
(defun ar-trim-left-dollared-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-trim-left))
 
(defun ar-trim-right-dollared-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-trim-right))
 
(defun ar-underscore-dollared-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-underscore))
 
(defun ar-whitespace-dollared-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-whitespace))
 
(defun ar-dollared-in-lesserangled-atpt ()
  "Employ actions of  at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th))
 
(defun ar-greaterangle-dollared-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-dollared-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-lesserangle))
 
(defun ar-backslash-dollared-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-backslash))
 
(defun ar-beg-dollared-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-beg))
 
(defun ar-blok-dollared-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-blok))
 
(defun ar-bounds-dollared-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-bounds))
 
(defun ar-brace-dollared-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-brace))
 
(defun ar-bracket-dollared-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-bracket))
 
(defun ar-commatize-dollared-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-commatize))
 
(defun ar-comment-dollared-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-comment))
 
(defun ar-dollar-dollared-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-dollar))
 
(defun ar-double-backslash-dollared-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-double-backslash))
 
(defun ar-doublequote-dollared-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-doublequote))
 
(defun ar-doubleslash-dollared-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-dollared-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-doubleslash-paren))
 
(defun ar-end-dollared-in-lesserangled-atpt ()
  "Employ actions of END at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-end))
 
(defun ar-escape-dollared-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-escape))
 
(defun ar-hide-dollared-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-hide))
 
(defun ar-hide-show-dollared-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-hide-show))
 
(defun ar-hyphen-dollared-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-hyphen))
 
(defun ar-kill-dollared-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-dollared-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-left-right-singlequote))
 
(defun ar-length-dollared-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-length))
 
(defun ar-parentize-dollared-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-parentize))
 
(defun ar-quote-dollared-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-quote))
 
(defun ar-separate-dollared-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-separate))
 
(defun ar-show-dollared-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-show))
 
(defun ar-singlequote-dollared-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-singlequote))
 
(defun ar-slash-dollared-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-slash))
 
(defun ar-slashparen-dollared-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-slashparen))
 
(defun ar-sort-dollared-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-sort))
 
(defun ar-trim-dollared-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-trim))
 
(defun ar-trim-left-dollared-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-trim-left))
 
(defun ar-trim-right-dollared-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-trim-right))
 
(defun ar-underscore-dollared-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-underscore))
 
(defun ar-whitespace-dollared-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-whitespace))
 
(defun ar-dollared-in-greaterangled-atpt ()
  "Employ actions of  at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th))
 
(defun ar-greaterangle-dollared-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-dollared-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-lesserangle))
 
(defun ar-backslash-dollared-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-backslash))
 
(defun ar-beg-dollared-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-beg))
 
(defun ar-blok-dollared-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-blok))
 
(defun ar-bounds-dollared-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-bounds))
 
(defun ar-brace-dollared-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-brace))
 
(defun ar-bracket-dollared-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-bracket))
 
(defun ar-commatize-dollared-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-commatize))
 
(defun ar-comment-dollared-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-comment))
 
(defun ar-dollar-dollared-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-dollar))
 
(defun ar-double-backslash-dollared-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-double-backslash))
 
(defun ar-doublequote-dollared-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-doublequote))
 
(defun ar-doubleslash-dollared-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-dollared-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-doubleslash-paren))
 
(defun ar-end-dollared-in-greaterangled-atpt ()
  "Employ actions of END at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-end))
 
(defun ar-escape-dollared-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-escape))
 
(defun ar-hide-dollared-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-hide))
 
(defun ar-hide-show-dollared-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-hide-show))
 
(defun ar-hyphen-dollared-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-hyphen))
 
(defun ar-kill-dollared-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-dollared-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-left-right-singlequote))
 
(defun ar-length-dollared-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-length))
 
(defun ar-parentize-dollared-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-parentize))
 
(defun ar-quote-dollared-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-quote))
 
(defun ar-separate-dollared-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-separate))
 
(defun ar-show-dollared-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-show))
 
(defun ar-singlequote-dollared-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-singlequote))
 
(defun ar-slash-dollared-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-slash))
 
(defun ar-slashparen-dollared-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-slashparen))
 
(defun ar-sort-dollared-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-sort))
 
(defun ar-trim-dollared-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-trim))
 
(defun ar-trim-left-dollared-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-trim-left))
 
(defun ar-trim-right-dollared-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-trim-right))
 
(defun ar-underscore-dollared-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-underscore))
 
(defun ar-whitespace-dollared-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-whitespace))
 
(defun ar-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th))
 
(defun ar-greaterangle-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-greaterangle))
 
(defun ar-lesserangle-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-lesserangle))
 
(defun ar-backslash-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-backslash))
 
(defun ar-beg-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-beg))
 
(defun ar-blok-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-blok))
 
(defun ar-bounds-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-bounds))
 
(defun ar-brace-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-brace))
 
(defun ar-bracket-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-bracket))
 
(defun ar-commatize-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-commatize))
 
(defun ar-comment-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-comment))
 
(defun ar-dollar-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-dollar))
 
(defun ar-double-backslash-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-double-backslash))
 
(defun ar-doublequote-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-doublequote))
 
(defun ar-doubleslash-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-doubleslash-paren))
 
(defun ar-end-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-end))
 
(defun ar-escape-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-escape))
 
(defun ar-hide-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-hide))
 
(defun ar-hide-show-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-hide-show))
 
(defun ar-hyphen-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-hyphen))
 
(defun ar-kill-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-kill))
 
(defun ar-left-right-singlequote-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-left-right-singlequote))
 
(defun ar-length-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-length))
 
(defun ar-parentize-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-parentize))
 
(defun ar-quote-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-quote))
 
(defun ar-separate-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-separate))
 
(defun ar-show-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-show))
 
(defun ar-singlequote-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-singlequote))
 
(defun ar-slash-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-slash))
 
(defun ar-slashparen-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-slashparen))
 
(defun ar-sort-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-sort))
 
(defun ar-trim-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-trim))
 
(defun ar-trim-left-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-trim-left))
 
(defun ar-trim-right-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-trim-right))
 
(defun ar-underscore-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-underscore))
 
(defun ar-whitespace-dollared-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-whitespace))
 
(defun ar-dollared-in-parentized-atpt ()
  "Employ actions of  at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th))
 
(defun ar-greaterangle-dollared-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-greaterangle))
 
(defun ar-lesserangle-dollared-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-lesserangle))
 
(defun ar-backslash-dollared-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-backslash))
 
(defun ar-beg-dollared-in-parentized-atpt ()
  "Employ actions of BEG at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-beg))
 
(defun ar-blok-dollared-in-parentized-atpt ()
  "Employ actions of BLOK at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-blok))
 
(defun ar-bounds-dollared-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-bounds))
 
(defun ar-brace-dollared-in-parentized-atpt ()
  "Employ actions of BRACE at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-brace))
 
(defun ar-bracket-dollared-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-bracket))
 
(defun ar-commatize-dollared-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-commatize))
 
(defun ar-comment-dollared-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-comment))
 
(defun ar-dollar-dollared-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-dollar))
 
(defun ar-double-backslash-dollared-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-double-backslash))
 
(defun ar-doublequote-dollared-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-doublequote))
 
(defun ar-doubleslash-dollared-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-dollared-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-doubleslash-paren))
 
(defun ar-end-dollared-in-parentized-atpt ()
  "Employ actions of END at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-end))
 
(defun ar-escape-dollared-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-escape))
 
(defun ar-hide-dollared-in-parentized-atpt ()
  "Employ actions of HIDE at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-hide))
 
(defun ar-hide-show-dollared-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-hide-show))
 
(defun ar-hyphen-dollared-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-hyphen))
 
(defun ar-kill-dollared-in-parentized-atpt ()
  "Employ actions of KILL at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-kill))
 
(defun ar-left-right-singlequote-dollared-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-left-right-singlequote))
 
(defun ar-length-dollared-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-length))
 
(defun ar-parentize-dollared-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-parentize))
 
(defun ar-quote-dollared-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-quote))
 
(defun ar-separate-dollared-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-separate))
 
(defun ar-show-dollared-in-parentized-atpt ()
  "Employ actions of SHOW at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-show))
 
(defun ar-singlequote-dollared-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-singlequote))
 
(defun ar-slash-dollared-in-parentized-atpt ()
  "Employ actions of SLASH at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-slash))
 
(defun ar-slashparen-dollared-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-slashparen))
 
(defun ar-sort-dollared-in-parentized-atpt ()
  "Employ actions of SORT at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-sort))
 
(defun ar-trim-dollared-in-parentized-atpt ()
  "Employ actions of TRIM at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-trim))
 
(defun ar-trim-left-dollared-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-trim-left))
 
(defun ar-trim-right-dollared-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-trim-right))
 
(defun ar-underscore-dollared-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-underscore))
 
(defun ar-whitespace-dollared-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-whitespace))
 
(defun ar-doublequoted-in-braced-atpt ()
  "Employ actions of  at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th))
 
(defun ar-greaterangle-doublequoted-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-greaterangle))
 
(defun ar-lesserangle-doublequoted-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-lesserangle))
 
(defun ar-backslash-doublequoted-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-backslash))
 
(defun ar-beg-doublequoted-in-braced-atpt ()
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-beg))
 
(defun ar-blok-doublequoted-in-braced-atpt ()
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-blok))
 
(defun ar-bounds-doublequoted-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-bounds))
 
(defun ar-brace-doublequoted-in-braced-atpt ()
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-brace))
 
(defun ar-bracket-doublequoted-in-braced-atpt ()
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-bracket))
 
(defun ar-commatize-doublequoted-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-commatize))
 
(defun ar-comment-doublequoted-in-braced-atpt ()
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-comment))
 
(defun ar-dollar-doublequoted-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-dollar))
 
(defun ar-double-backslash-doublequoted-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-double-backslash))
 
(defun ar-doublequote-doublequoted-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-doublequote))
 
(defun ar-doubleslash-doublequoted-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-doublequoted-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-doubleslash-paren))
 
(defun ar-end-doublequoted-in-braced-atpt ()
  "Employ actions of END at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-end))
 
(defun ar-escape-doublequoted-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-escape))
 
(defun ar-hide-doublequoted-in-braced-atpt ()
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-hide))
 
(defun ar-hide-show-doublequoted-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-hide-show))
 
(defun ar-hyphen-doublequoted-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-hyphen))
 
(defun ar-kill-doublequoted-in-braced-atpt ()
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-kill))
 
(defun ar-left-right-singlequote-doublequoted-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-left-right-singlequote))
 
(defun ar-length-doublequoted-in-braced-atpt ()
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-length))
 
(defun ar-parentize-doublequoted-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-parentize))
 
(defun ar-quote-doublequoted-in-braced-atpt ()
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-quote))
 
(defun ar-separate-doublequoted-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-separate))
 
(defun ar-show-doublequoted-in-braced-atpt ()
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-show))
 
(defun ar-singlequote-doublequoted-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-singlequote))
 
(defun ar-slash-doublequoted-in-braced-atpt ()
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-slash))
 
(defun ar-slashparen-doublequoted-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-slashparen))
 
(defun ar-sort-doublequoted-in-braced-atpt ()
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-sort))
 
(defun ar-trim-doublequoted-in-braced-atpt ()
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-trim))
 
(defun ar-trim-left-doublequoted-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-trim-left))
 
(defun ar-trim-right-doublequoted-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-trim-right))
 
(defun ar-underscore-doublequoted-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-underscore))
 
(defun ar-whitespace-doublequoted-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-whitespace))
 
(defun ar-doublequoted-in-bracketed-atpt ()
  "Employ actions of  at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th))
 
(defun ar-greaterangle-doublequoted-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-greaterangle))
 
(defun ar-lesserangle-doublequoted-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-lesserangle))
 
(defun ar-backslash-doublequoted-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-backslash))
 
(defun ar-beg-doublequoted-in-bracketed-atpt ()
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-beg))
 
(defun ar-blok-doublequoted-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-blok))
 
(defun ar-bounds-doublequoted-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-bounds))
 
(defun ar-brace-doublequoted-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-brace))
 
(defun ar-bracket-doublequoted-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-bracket))
 
(defun ar-commatize-doublequoted-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-commatize))
 
(defun ar-comment-doublequoted-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-comment))
 
(defun ar-dollar-doublequoted-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-dollar))
 
(defun ar-double-backslash-doublequoted-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-double-backslash))
 
(defun ar-doublequote-doublequoted-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-doublequote))
 
(defun ar-doubleslash-doublequoted-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-doublequoted-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-doubleslash-paren))
 
(defun ar-end-doublequoted-in-bracketed-atpt ()
  "Employ actions of END at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-end))
 
(defun ar-escape-doublequoted-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-escape))
 
(defun ar-hide-doublequoted-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-hide))
 
(defun ar-hide-show-doublequoted-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-hide-show))
 
(defun ar-hyphen-doublequoted-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-hyphen))
 
(defun ar-kill-doublequoted-in-bracketed-atpt ()
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-kill))
 
(defun ar-left-right-singlequote-doublequoted-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-left-right-singlequote))
 
(defun ar-length-doublequoted-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-length))
 
(defun ar-parentize-doublequoted-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-parentize))
 
(defun ar-quote-doublequoted-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-quote))
 
(defun ar-separate-doublequoted-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-separate))
 
(defun ar-show-doublequoted-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-show))
 
(defun ar-singlequote-doublequoted-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-singlequote))
 
(defun ar-slash-doublequoted-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-slash))
 
(defun ar-slashparen-doublequoted-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-slashparen))
 
(defun ar-sort-doublequoted-in-bracketed-atpt ()
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-sort))
 
(defun ar-trim-doublequoted-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-trim))
 
(defun ar-trim-left-doublequoted-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-trim-left))
 
(defun ar-trim-right-doublequoted-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-trim-right))
 
(defun ar-underscore-doublequoted-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-underscore))
 
(defun ar-whitespace-doublequoted-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-whitespace))
 
(defun ar-doublequoted-in-lesserangled-atpt ()
  "Employ actions of  at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th))
 
(defun ar-greaterangle-doublequoted-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-doublequoted-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-lesserangle))
 
(defun ar-backslash-doublequoted-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-backslash))
 
(defun ar-beg-doublequoted-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-beg))
 
(defun ar-blok-doublequoted-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-blok))
 
(defun ar-bounds-doublequoted-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-bounds))
 
(defun ar-brace-doublequoted-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-brace))
 
(defun ar-bracket-doublequoted-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-bracket))
 
(defun ar-commatize-doublequoted-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-commatize))
 
(defun ar-comment-doublequoted-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-comment))
 
(defun ar-dollar-doublequoted-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-dollar))
 
(defun ar-double-backslash-doublequoted-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-double-backslash))
 
(defun ar-doublequote-doublequoted-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-doublequote))
 
(defun ar-doubleslash-doublequoted-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-doublequoted-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-doubleslash-paren))
 
(defun ar-end-doublequoted-in-lesserangled-atpt ()
  "Employ actions of END at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-end))
 
(defun ar-escape-doublequoted-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-escape))
 
(defun ar-hide-doublequoted-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-hide))
 
(defun ar-hide-show-doublequoted-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-hide-show))
 
(defun ar-hyphen-doublequoted-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-hyphen))
 
(defun ar-kill-doublequoted-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-doublequoted-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-left-right-singlequote))
 
(defun ar-length-doublequoted-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-length))
 
(defun ar-parentize-doublequoted-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-parentize))
 
(defun ar-quote-doublequoted-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-quote))
 
(defun ar-separate-doublequoted-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-separate))
 
(defun ar-show-doublequoted-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-show))
 
(defun ar-singlequote-doublequoted-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-singlequote))
 
(defun ar-slash-doublequoted-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-slash))
 
(defun ar-slashparen-doublequoted-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-slashparen))
 
(defun ar-sort-doublequoted-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-sort))
 
(defun ar-trim-doublequoted-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-trim))
 
(defun ar-trim-left-doublequoted-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-trim-left))
 
(defun ar-trim-right-doublequoted-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-trim-right))
 
(defun ar-underscore-doublequoted-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-underscore))
 
(defun ar-whitespace-doublequoted-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-whitespace))
 
(defun ar-doublequoted-in-greaterangled-atpt ()
  "Employ actions of  at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th))
 
(defun ar-greaterangle-doublequoted-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-doublequoted-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-lesserangle))
 
(defun ar-backslash-doublequoted-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-backslash))
 
(defun ar-beg-doublequoted-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-beg))
 
(defun ar-blok-doublequoted-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-blok))
 
(defun ar-bounds-doublequoted-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-bounds))
 
(defun ar-brace-doublequoted-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-brace))
 
(defun ar-bracket-doublequoted-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-bracket))
 
(defun ar-commatize-doublequoted-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-commatize))
 
(defun ar-comment-doublequoted-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-comment))
 
(defun ar-dollar-doublequoted-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-dollar))
 
(defun ar-double-backslash-doublequoted-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-double-backslash))
 
(defun ar-doublequote-doublequoted-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-doublequote))
 
(defun ar-doubleslash-doublequoted-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-doublequoted-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-doubleslash-paren))
 
(defun ar-end-doublequoted-in-greaterangled-atpt ()
  "Employ actions of END at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-end))
 
(defun ar-escape-doublequoted-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-escape))
 
(defun ar-hide-doublequoted-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-hide))
 
(defun ar-hide-show-doublequoted-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-hide-show))
 
(defun ar-hyphen-doublequoted-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-hyphen))
 
(defun ar-kill-doublequoted-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-doublequoted-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-left-right-singlequote))
 
(defun ar-length-doublequoted-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-length))
 
(defun ar-parentize-doublequoted-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-parentize))
 
(defun ar-quote-doublequoted-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-quote))
 
(defun ar-separate-doublequoted-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-separate))
 
(defun ar-show-doublequoted-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-show))
 
(defun ar-singlequote-doublequoted-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-singlequote))
 
(defun ar-slash-doublequoted-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-slash))
 
(defun ar-slashparen-doublequoted-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-slashparen))
 
(defun ar-sort-doublequoted-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-sort))
 
(defun ar-trim-doublequoted-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-trim))
 
(defun ar-trim-left-doublequoted-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-trim-left))
 
(defun ar-trim-right-doublequoted-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-trim-right))
 
(defun ar-underscore-doublequoted-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-underscore))
 
(defun ar-whitespace-doublequoted-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-whitespace))
 
(defun ar-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th))
 
(defun ar-greaterangle-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-greaterangle))
 
(defun ar-lesserangle-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-lesserangle))
 
(defun ar-backslash-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-backslash))
 
(defun ar-beg-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-beg))
 
(defun ar-blok-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-blok))
 
(defun ar-bounds-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-bounds))
 
(defun ar-brace-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-brace))
 
(defun ar-bracket-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-bracket))
 
(defun ar-commatize-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-commatize))
 
(defun ar-comment-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-comment))
 
(defun ar-dollar-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-dollar))
 
(defun ar-double-backslash-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-double-backslash))
 
(defun ar-doublequote-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-doublequote))
 
(defun ar-doubleslash-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-doubleslash-paren))
 
(defun ar-end-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-end))
 
(defun ar-escape-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-escape))
 
(defun ar-hide-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-hide))
 
(defun ar-hide-show-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-hide-show))
 
(defun ar-hyphen-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-hyphen))
 
(defun ar-kill-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-kill))
 
(defun ar-left-right-singlequote-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-left-right-singlequote))
 
(defun ar-length-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-length))
 
(defun ar-parentize-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-parentize))
 
(defun ar-quote-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-quote))
 
(defun ar-separate-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-separate))
 
(defun ar-show-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-show))
 
(defun ar-singlequote-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-singlequote))
 
(defun ar-slash-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-slash))
 
(defun ar-slashparen-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-slashparen))
 
(defun ar-sort-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-sort))
 
(defun ar-trim-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-trim))
 
(defun ar-trim-left-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-trim-left))
 
(defun ar-trim-right-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-trim-right))
 
(defun ar-underscore-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-underscore))
 
(defun ar-whitespace-doublequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-whitespace))
 
(defun ar-doublequoted-in-parentized-atpt ()
  "Employ actions of  at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th))
 
(defun ar-greaterangle-doublequoted-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-greaterangle))
 
(defun ar-lesserangle-doublequoted-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-lesserangle))
 
(defun ar-backslash-doublequoted-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-backslash))
 
(defun ar-beg-doublequoted-in-parentized-atpt ()
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-beg))
 
(defun ar-blok-doublequoted-in-parentized-atpt ()
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-blok))
 
(defun ar-bounds-doublequoted-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-bounds))
 
(defun ar-brace-doublequoted-in-parentized-atpt ()
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-brace))
 
(defun ar-bracket-doublequoted-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-bracket))
 
(defun ar-commatize-doublequoted-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-commatize))
 
(defun ar-comment-doublequoted-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-comment))
 
(defun ar-dollar-doublequoted-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-dollar))
 
(defun ar-double-backslash-doublequoted-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-double-backslash))
 
(defun ar-doublequote-doublequoted-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-doublequote))
 
(defun ar-doubleslash-doublequoted-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-doublequoted-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-doubleslash-paren))
 
(defun ar-end-doublequoted-in-parentized-atpt ()
  "Employ actions of END at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-end))
 
(defun ar-escape-doublequoted-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-escape))
 
(defun ar-hide-doublequoted-in-parentized-atpt ()
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-hide))
 
(defun ar-hide-show-doublequoted-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-hide-show))
 
(defun ar-hyphen-doublequoted-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-hyphen))
 
(defun ar-kill-doublequoted-in-parentized-atpt ()
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-kill))
 
(defun ar-left-right-singlequote-doublequoted-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-left-right-singlequote))
 
(defun ar-length-doublequoted-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-length))
 
(defun ar-parentize-doublequoted-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-parentize))
 
(defun ar-quote-doublequoted-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-quote))
 
(defun ar-separate-doublequoted-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-separate))
 
(defun ar-show-doublequoted-in-parentized-atpt ()
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-show))
 
(defun ar-singlequote-doublequoted-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-singlequote))
 
(defun ar-slash-doublequoted-in-parentized-atpt ()
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-slash))
 
(defun ar-slashparen-doublequoted-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-slashparen))
 
(defun ar-sort-doublequoted-in-parentized-atpt ()
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-sort))
 
(defun ar-trim-doublequoted-in-parentized-atpt ()
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-trim))
 
(defun ar-trim-left-doublequoted-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-trim-left))
 
(defun ar-trim-right-doublequoted-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-trim-right))
 
(defun ar-underscore-doublequoted-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-underscore))
 
(defun ar-whitespace-doublequoted-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-whitespace))
 
(defun ar-equalized-in-braced-atpt ()
  "Employ actions of  at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th))
 
(defun ar-greaterangle-equalized-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-greaterangle))
 
(defun ar-lesserangle-equalized-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-lesserangle))
 
(defun ar-backslash-equalized-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-backslash))
 
(defun ar-beg-equalized-in-braced-atpt ()
  "Employ actions of BEG at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-beg))
 
(defun ar-blok-equalized-in-braced-atpt ()
  "Employ actions of BLOK at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-blok))
 
(defun ar-bounds-equalized-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-bounds))
 
(defun ar-brace-equalized-in-braced-atpt ()
  "Employ actions of BRACE at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-brace))
 
(defun ar-bracket-equalized-in-braced-atpt ()
  "Employ actions of BRACKET at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-bracket))
 
(defun ar-commatize-equalized-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-commatize))
 
(defun ar-comment-equalized-in-braced-atpt ()
  "Employ actions of COMMENT at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-comment))
 
(defun ar-dollar-equalized-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-dollar))
 
(defun ar-double-backslash-equalized-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-double-backslash))
 
(defun ar-doublequote-equalized-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-doublequote))
 
(defun ar-doubleslash-equalized-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-equalized-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-doubleslash-paren))
 
(defun ar-end-equalized-in-braced-atpt ()
  "Employ actions of END at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-end))
 
(defun ar-escape-equalized-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-escape))
 
(defun ar-hide-equalized-in-braced-atpt ()
  "Employ actions of HIDE at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-hide))
 
(defun ar-hide-show-equalized-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-hide-show))
 
(defun ar-hyphen-equalized-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-hyphen))
 
(defun ar-kill-equalized-in-braced-atpt ()
  "Employ actions of KILL at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-kill))
 
(defun ar-left-right-singlequote-equalized-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-left-right-singlequote))
 
(defun ar-length-equalized-in-braced-atpt ()
  "Employ actions of LENGTH at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-length))
 
(defun ar-parentize-equalized-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-parentize))
 
(defun ar-quote-equalized-in-braced-atpt ()
  "Employ actions of QUOTE at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-quote))
 
(defun ar-separate-equalized-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-separate))
 
(defun ar-show-equalized-in-braced-atpt ()
  "Employ actions of SHOW at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-show))
 
(defun ar-singlequote-equalized-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-singlequote))
 
(defun ar-slash-equalized-in-braced-atpt ()
  "Employ actions of SLASH at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-slash))
 
(defun ar-slashparen-equalized-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-slashparen))
 
(defun ar-sort-equalized-in-braced-atpt ()
  "Employ actions of SORT at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-sort))
 
(defun ar-trim-equalized-in-braced-atpt ()
  "Employ actions of TRIM at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-trim))
 
(defun ar-trim-left-equalized-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-trim-left))
 
(defun ar-trim-right-equalized-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-trim-right))
 
(defun ar-underscore-equalized-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-underscore))
 
(defun ar-whitespace-equalized-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-whitespace))
 
(defun ar-equalized-in-bracketed-atpt ()
  "Employ actions of  at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th))
 
(defun ar-greaterangle-equalized-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-greaterangle))
 
(defun ar-lesserangle-equalized-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-lesserangle))
 
(defun ar-backslash-equalized-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-backslash))
 
(defun ar-beg-equalized-in-bracketed-atpt ()
  "Employ actions of BEG at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-beg))
 
(defun ar-blok-equalized-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-blok))
 
(defun ar-bounds-equalized-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-bounds))
 
(defun ar-brace-equalized-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-brace))
 
(defun ar-bracket-equalized-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-bracket))
 
(defun ar-commatize-equalized-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-commatize))
 
(defun ar-comment-equalized-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-comment))
 
(defun ar-dollar-equalized-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-dollar))
 
(defun ar-double-backslash-equalized-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-double-backslash))
 
(defun ar-doublequote-equalized-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-doublequote))
 
(defun ar-doubleslash-equalized-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-equalized-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-doubleslash-paren))
 
(defun ar-end-equalized-in-bracketed-atpt ()
  "Employ actions of END at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-end))
 
(defun ar-escape-equalized-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-escape))
 
(defun ar-hide-equalized-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-hide))
 
(defun ar-hide-show-equalized-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-hide-show))
 
(defun ar-hyphen-equalized-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-hyphen))
 
(defun ar-kill-equalized-in-bracketed-atpt ()
  "Employ actions of KILL at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-kill))
 
(defun ar-left-right-singlequote-equalized-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-left-right-singlequote))
 
(defun ar-length-equalized-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-length))
 
(defun ar-parentize-equalized-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-parentize))
 
(defun ar-quote-equalized-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-quote))
 
(defun ar-separate-equalized-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-separate))
 
(defun ar-show-equalized-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-show))
 
(defun ar-singlequote-equalized-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-singlequote))
 
(defun ar-slash-equalized-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-slash))
 
(defun ar-slashparen-equalized-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-slashparen))
 
(defun ar-sort-equalized-in-bracketed-atpt ()
  "Employ actions of SORT at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-sort))
 
(defun ar-trim-equalized-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-trim))
 
(defun ar-trim-left-equalized-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-trim-left))
 
(defun ar-trim-right-equalized-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-trim-right))
 
(defun ar-underscore-equalized-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-underscore))
 
(defun ar-whitespace-equalized-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-whitespace))
 
(defun ar-equalized-in-lesserangled-atpt ()
  "Employ actions of  at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th))
 
(defun ar-greaterangle-equalized-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-equalized-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-lesserangle))
 
(defun ar-backslash-equalized-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-backslash))
 
(defun ar-beg-equalized-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-beg))
 
(defun ar-blok-equalized-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-blok))
 
(defun ar-bounds-equalized-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-bounds))
 
(defun ar-brace-equalized-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-brace))
 
(defun ar-bracket-equalized-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-bracket))
 
(defun ar-commatize-equalized-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-commatize))
 
(defun ar-comment-equalized-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-comment))
 
(defun ar-dollar-equalized-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-dollar))
 
(defun ar-double-backslash-equalized-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-double-backslash))
 
(defun ar-doublequote-equalized-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-doublequote))
 
(defun ar-doubleslash-equalized-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-equalized-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-doubleslash-paren))
 
(defun ar-end-equalized-in-lesserangled-atpt ()
  "Employ actions of END at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-end))
 
(defun ar-escape-equalized-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-escape))
 
(defun ar-hide-equalized-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-hide))
 
(defun ar-hide-show-equalized-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-hide-show))
 
(defun ar-hyphen-equalized-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-hyphen))
 
(defun ar-kill-equalized-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-equalized-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-left-right-singlequote))
 
(defun ar-length-equalized-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-length))
 
(defun ar-parentize-equalized-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-parentize))
 
(defun ar-quote-equalized-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-quote))
 
(defun ar-separate-equalized-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-separate))
 
(defun ar-show-equalized-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-show))
 
(defun ar-singlequote-equalized-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-singlequote))
 
(defun ar-slash-equalized-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-slash))
 
(defun ar-slashparen-equalized-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-slashparen))
 
(defun ar-sort-equalized-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-sort))
 
(defun ar-trim-equalized-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-trim))
 
(defun ar-trim-left-equalized-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-trim-left))
 
(defun ar-trim-right-equalized-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-trim-right))
 
(defun ar-underscore-equalized-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-underscore))
 
(defun ar-whitespace-equalized-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-whitespace))
 
(defun ar-equalized-in-greaterangled-atpt ()
  "Employ actions of  at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th))
 
(defun ar-greaterangle-equalized-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-equalized-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-lesserangle))
 
(defun ar-backslash-equalized-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-backslash))
 
(defun ar-beg-equalized-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-beg))
 
(defun ar-blok-equalized-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-blok))
 
(defun ar-bounds-equalized-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-bounds))
 
(defun ar-brace-equalized-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-brace))
 
(defun ar-bracket-equalized-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-bracket))
 
(defun ar-commatize-equalized-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-commatize))
 
(defun ar-comment-equalized-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-comment))
 
(defun ar-dollar-equalized-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-dollar))
 
(defun ar-double-backslash-equalized-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-double-backslash))
 
(defun ar-doublequote-equalized-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-doublequote))
 
(defun ar-doubleslash-equalized-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-equalized-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-doubleslash-paren))
 
(defun ar-end-equalized-in-greaterangled-atpt ()
  "Employ actions of END at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-end))
 
(defun ar-escape-equalized-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-escape))
 
(defun ar-hide-equalized-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-hide))
 
(defun ar-hide-show-equalized-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-hide-show))
 
(defun ar-hyphen-equalized-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-hyphen))
 
(defun ar-kill-equalized-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-equalized-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-left-right-singlequote))
 
(defun ar-length-equalized-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-length))
 
(defun ar-parentize-equalized-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-parentize))
 
(defun ar-quote-equalized-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-quote))
 
(defun ar-separate-equalized-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-separate))
 
(defun ar-show-equalized-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-show))
 
(defun ar-singlequote-equalized-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-singlequote))
 
(defun ar-slash-equalized-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-slash))
 
(defun ar-slashparen-equalized-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-slashparen))
 
(defun ar-sort-equalized-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-sort))
 
(defun ar-trim-equalized-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-trim))
 
(defun ar-trim-left-equalized-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-trim-left))
 
(defun ar-trim-right-equalized-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-trim-right))
 
(defun ar-underscore-equalized-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-underscore))
 
(defun ar-whitespace-equalized-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-whitespace))
 
(defun ar-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th))
 
(defun ar-greaterangle-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-greaterangle))
 
(defun ar-lesserangle-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-lesserangle))
 
(defun ar-backslash-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-backslash))
 
(defun ar-beg-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-beg))
 
(defun ar-blok-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-blok))
 
(defun ar-bounds-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-bounds))
 
(defun ar-brace-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-brace))
 
(defun ar-bracket-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-bracket))
 
(defun ar-commatize-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-commatize))
 
(defun ar-comment-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-comment))
 
(defun ar-dollar-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-dollar))
 
(defun ar-double-backslash-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-double-backslash))
 
(defun ar-doublequote-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-doublequote))
 
(defun ar-doubleslash-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-doubleslash-paren))
 
(defun ar-end-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-end))
 
(defun ar-escape-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-escape))
 
(defun ar-hide-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-hide))
 
(defun ar-hide-show-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-hide-show))
 
(defun ar-hyphen-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-hyphen))
 
(defun ar-kill-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-kill))
 
(defun ar-left-right-singlequote-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-left-right-singlequote))
 
(defun ar-length-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-length))
 
(defun ar-parentize-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-parentize))
 
(defun ar-quote-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-quote))
 
(defun ar-separate-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-separate))
 
(defun ar-show-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-show))
 
(defun ar-singlequote-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-singlequote))
 
(defun ar-slash-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-slash))
 
(defun ar-slashparen-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-slashparen))
 
(defun ar-sort-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-sort))
 
(defun ar-trim-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-trim))
 
(defun ar-trim-left-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-trim-left))
 
(defun ar-trim-right-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-trim-right))
 
(defun ar-underscore-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-underscore))
 
(defun ar-whitespace-equalized-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-whitespace))
 
(defun ar-equalized-in-parentized-atpt ()
  "Employ actions of  at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th))
 
(defun ar-greaterangle-equalized-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-greaterangle))
 
(defun ar-lesserangle-equalized-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-lesserangle))
 
(defun ar-backslash-equalized-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-backslash))
 
(defun ar-beg-equalized-in-parentized-atpt ()
  "Employ actions of BEG at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-beg))
 
(defun ar-blok-equalized-in-parentized-atpt ()
  "Employ actions of BLOK at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-blok))
 
(defun ar-bounds-equalized-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-bounds))
 
(defun ar-brace-equalized-in-parentized-atpt ()
  "Employ actions of BRACE at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-brace))
 
(defun ar-bracket-equalized-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-bracket))
 
(defun ar-commatize-equalized-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-commatize))
 
(defun ar-comment-equalized-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-comment))
 
(defun ar-dollar-equalized-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-dollar))
 
(defun ar-double-backslash-equalized-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-double-backslash))
 
(defun ar-doublequote-equalized-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-doublequote))
 
(defun ar-doubleslash-equalized-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-equalized-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-doubleslash-paren))
 
(defun ar-end-equalized-in-parentized-atpt ()
  "Employ actions of END at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-end))
 
(defun ar-escape-equalized-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-escape))
 
(defun ar-hide-equalized-in-parentized-atpt ()
  "Employ actions of HIDE at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-hide))
 
(defun ar-hide-show-equalized-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-hide-show))
 
(defun ar-hyphen-equalized-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-hyphen))
 
(defun ar-kill-equalized-in-parentized-atpt ()
  "Employ actions of KILL at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-kill))
 
(defun ar-left-right-singlequote-equalized-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-left-right-singlequote))
 
(defun ar-length-equalized-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-length))
 
(defun ar-parentize-equalized-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-parentize))
 
(defun ar-quote-equalized-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-quote))
 
(defun ar-separate-equalized-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-separate))
 
(defun ar-show-equalized-in-parentized-atpt ()
  "Employ actions of SHOW at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-show))
 
(defun ar-singlequote-equalized-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-singlequote))
 
(defun ar-slash-equalized-in-parentized-atpt ()
  "Employ actions of SLASH at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-slash))
 
(defun ar-slashparen-equalized-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-slashparen))
 
(defun ar-sort-equalized-in-parentized-atpt ()
  "Employ actions of SORT at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-sort))
 
(defun ar-trim-equalized-in-parentized-atpt ()
  "Employ actions of TRIM at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-trim))
 
(defun ar-trim-left-equalized-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-trim-left))
 
(defun ar-trim-right-equalized-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-trim-right))
 
(defun ar-underscore-equalized-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-underscore))
 
(defun ar-whitespace-equalized-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-whitespace))
 
(defun ar-hyphened-in-braced-atpt ()
  "Employ actions of  at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th))
 
(defun ar-greaterangle-hyphened-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-greaterangle))
 
(defun ar-lesserangle-hyphened-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-lesserangle))
 
(defun ar-backslash-hyphened-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-backslash))
 
(defun ar-beg-hyphened-in-braced-atpt ()
  "Employ actions of BEG at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-beg))
 
(defun ar-blok-hyphened-in-braced-atpt ()
  "Employ actions of BLOK at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-blok))
 
(defun ar-bounds-hyphened-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-bounds))
 
(defun ar-brace-hyphened-in-braced-atpt ()
  "Employ actions of BRACE at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-brace))
 
(defun ar-bracket-hyphened-in-braced-atpt ()
  "Employ actions of BRACKET at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-bracket))
 
(defun ar-commatize-hyphened-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-commatize))
 
(defun ar-comment-hyphened-in-braced-atpt ()
  "Employ actions of COMMENT at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-comment))
 
(defun ar-dollar-hyphened-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-dollar))
 
(defun ar-double-backslash-hyphened-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-double-backslash))
 
(defun ar-doublequote-hyphened-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-doublequote))
 
(defun ar-doubleslash-hyphened-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-hyphened-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-doubleslash-paren))
 
(defun ar-end-hyphened-in-braced-atpt ()
  "Employ actions of END at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-end))
 
(defun ar-escape-hyphened-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-escape))
 
(defun ar-hide-hyphened-in-braced-atpt ()
  "Employ actions of HIDE at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-hide))
 
(defun ar-hide-show-hyphened-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-hide-show))
 
(defun ar-hyphen-hyphened-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-hyphen))
 
(defun ar-kill-hyphened-in-braced-atpt ()
  "Employ actions of KILL at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-kill))
 
(defun ar-left-right-singlequote-hyphened-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-left-right-singlequote))
 
(defun ar-length-hyphened-in-braced-atpt ()
  "Employ actions of LENGTH at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-length))
 
(defun ar-parentize-hyphened-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-parentize))
 
(defun ar-quote-hyphened-in-braced-atpt ()
  "Employ actions of QUOTE at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-quote))
 
(defun ar-separate-hyphened-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-separate))
 
(defun ar-show-hyphened-in-braced-atpt ()
  "Employ actions of SHOW at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-show))
 
(defun ar-singlequote-hyphened-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-singlequote))
 
(defun ar-slash-hyphened-in-braced-atpt ()
  "Employ actions of SLASH at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-slash))
 
(defun ar-slashparen-hyphened-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-slashparen))
 
(defun ar-sort-hyphened-in-braced-atpt ()
  "Employ actions of SORT at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-sort))
 
(defun ar-trim-hyphened-in-braced-atpt ()
  "Employ actions of TRIM at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-trim))
 
(defun ar-trim-left-hyphened-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-trim-left))
 
(defun ar-trim-right-hyphened-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-trim-right))
 
(defun ar-underscore-hyphened-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-underscore))
 
(defun ar-whitespace-hyphened-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-whitespace))
 
(defun ar-hyphened-in-bracketed-atpt ()
  "Employ actions of  at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th))
 
(defun ar-greaterangle-hyphened-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-greaterangle))
 
(defun ar-lesserangle-hyphened-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-lesserangle))
 
(defun ar-backslash-hyphened-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-backslash))
 
(defun ar-beg-hyphened-in-bracketed-atpt ()
  "Employ actions of BEG at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-beg))
 
(defun ar-blok-hyphened-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-blok))
 
(defun ar-bounds-hyphened-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-bounds))
 
(defun ar-brace-hyphened-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-brace))
 
(defun ar-bracket-hyphened-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-bracket))
 
(defun ar-commatize-hyphened-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-commatize))
 
(defun ar-comment-hyphened-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-comment))
 
(defun ar-dollar-hyphened-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-dollar))
 
(defun ar-double-backslash-hyphened-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-double-backslash))
 
(defun ar-doublequote-hyphened-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-doublequote))
 
(defun ar-doubleslash-hyphened-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-hyphened-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-doubleslash-paren))
 
(defun ar-end-hyphened-in-bracketed-atpt ()
  "Employ actions of END at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-end))
 
(defun ar-escape-hyphened-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-escape))
 
(defun ar-hide-hyphened-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-hide))
 
(defun ar-hide-show-hyphened-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-hide-show))
 
(defun ar-hyphen-hyphened-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-hyphen))
 
(defun ar-kill-hyphened-in-bracketed-atpt ()
  "Employ actions of KILL at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-kill))
 
(defun ar-left-right-singlequote-hyphened-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-left-right-singlequote))
 
(defun ar-length-hyphened-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-length))
 
(defun ar-parentize-hyphened-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-parentize))
 
(defun ar-quote-hyphened-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-quote))
 
(defun ar-separate-hyphened-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-separate))
 
(defun ar-show-hyphened-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-show))
 
(defun ar-singlequote-hyphened-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-singlequote))
 
(defun ar-slash-hyphened-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-slash))
 
(defun ar-slashparen-hyphened-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-slashparen))
 
(defun ar-sort-hyphened-in-bracketed-atpt ()
  "Employ actions of SORT at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-sort))
 
(defun ar-trim-hyphened-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-trim))
 
(defun ar-trim-left-hyphened-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-trim-left))
 
(defun ar-trim-right-hyphened-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-trim-right))
 
(defun ar-underscore-hyphened-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-underscore))
 
(defun ar-whitespace-hyphened-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-whitespace))
 
(defun ar-hyphened-in-lesserangled-atpt ()
  "Employ actions of  at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th))
 
(defun ar-greaterangle-hyphened-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-hyphened-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-lesserangle))
 
(defun ar-backslash-hyphened-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-backslash))
 
(defun ar-beg-hyphened-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-beg))
 
(defun ar-blok-hyphened-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-blok))
 
(defun ar-bounds-hyphened-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-bounds))
 
(defun ar-brace-hyphened-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-brace))
 
(defun ar-bracket-hyphened-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-bracket))
 
(defun ar-commatize-hyphened-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-commatize))
 
(defun ar-comment-hyphened-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-comment))
 
(defun ar-dollar-hyphened-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-dollar))
 
(defun ar-double-backslash-hyphened-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-double-backslash))
 
(defun ar-doublequote-hyphened-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-doublequote))
 
(defun ar-doubleslash-hyphened-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-hyphened-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-doubleslash-paren))
 
(defun ar-end-hyphened-in-lesserangled-atpt ()
  "Employ actions of END at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-end))
 
(defun ar-escape-hyphened-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-escape))
 
(defun ar-hide-hyphened-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-hide))
 
(defun ar-hide-show-hyphened-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-hide-show))
 
(defun ar-hyphen-hyphened-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-hyphen))
 
(defun ar-kill-hyphened-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-hyphened-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-left-right-singlequote))
 
(defun ar-length-hyphened-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-length))
 
(defun ar-parentize-hyphened-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-parentize))
 
(defun ar-quote-hyphened-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-quote))
 
(defun ar-separate-hyphened-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-separate))
 
(defun ar-show-hyphened-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-show))
 
(defun ar-singlequote-hyphened-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-singlequote))
 
(defun ar-slash-hyphened-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-slash))
 
(defun ar-slashparen-hyphened-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-slashparen))
 
(defun ar-sort-hyphened-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-sort))
 
(defun ar-trim-hyphened-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-trim))
 
(defun ar-trim-left-hyphened-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-trim-left))
 
(defun ar-trim-right-hyphened-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-trim-right))
 
(defun ar-underscore-hyphened-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-underscore))
 
(defun ar-whitespace-hyphened-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-whitespace))
 
(defun ar-hyphened-in-greaterangled-atpt ()
  "Employ actions of  at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th))
 
(defun ar-greaterangle-hyphened-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-hyphened-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-lesserangle))
 
(defun ar-backslash-hyphened-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-backslash))
 
(defun ar-beg-hyphened-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-beg))
 
(defun ar-blok-hyphened-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-blok))
 
(defun ar-bounds-hyphened-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-bounds))
 
(defun ar-brace-hyphened-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-brace))
 
(defun ar-bracket-hyphened-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-bracket))
 
(defun ar-commatize-hyphened-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-commatize))
 
(defun ar-comment-hyphened-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-comment))
 
(defun ar-dollar-hyphened-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-dollar))
 
(defun ar-double-backslash-hyphened-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-double-backslash))
 
(defun ar-doublequote-hyphened-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-doublequote))
 
(defun ar-doubleslash-hyphened-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-hyphened-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-doubleslash-paren))
 
(defun ar-end-hyphened-in-greaterangled-atpt ()
  "Employ actions of END at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-end))
 
(defun ar-escape-hyphened-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-escape))
 
(defun ar-hide-hyphened-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-hide))
 
(defun ar-hide-show-hyphened-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-hide-show))
 
(defun ar-hyphen-hyphened-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-hyphen))
 
(defun ar-kill-hyphened-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-hyphened-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-left-right-singlequote))
 
(defun ar-length-hyphened-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-length))
 
(defun ar-parentize-hyphened-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-parentize))
 
(defun ar-quote-hyphened-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-quote))
 
(defun ar-separate-hyphened-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-separate))
 
(defun ar-show-hyphened-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-show))
 
(defun ar-singlequote-hyphened-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-singlequote))
 
(defun ar-slash-hyphened-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-slash))
 
(defun ar-slashparen-hyphened-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-slashparen))
 
(defun ar-sort-hyphened-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-sort))
 
(defun ar-trim-hyphened-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-trim))
 
(defun ar-trim-left-hyphened-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-trim-left))
 
(defun ar-trim-right-hyphened-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-trim-right))
 
(defun ar-underscore-hyphened-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-underscore))
 
(defun ar-whitespace-hyphened-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-whitespace))
 
(defun ar-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th))
 
(defun ar-greaterangle-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-greaterangle))
 
(defun ar-lesserangle-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-lesserangle))
 
(defun ar-backslash-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-backslash))
 
(defun ar-beg-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-beg))
 
(defun ar-blok-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-blok))
 
(defun ar-bounds-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-bounds))
 
(defun ar-brace-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-brace))
 
(defun ar-bracket-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-bracket))
 
(defun ar-commatize-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-commatize))
 
(defun ar-comment-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-comment))
 
(defun ar-dollar-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-dollar))
 
(defun ar-double-backslash-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-double-backslash))
 
(defun ar-doublequote-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-doublequote))
 
(defun ar-doubleslash-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-doubleslash-paren))
 
(defun ar-end-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-end))
 
(defun ar-escape-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-escape))
 
(defun ar-hide-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-hide))
 
(defun ar-hide-show-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-hide-show))
 
(defun ar-hyphen-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-hyphen))
 
(defun ar-kill-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-kill))
 
(defun ar-left-right-singlequote-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-left-right-singlequote))
 
(defun ar-length-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-length))
 
(defun ar-parentize-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-parentize))
 
(defun ar-quote-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-quote))
 
(defun ar-separate-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-separate))
 
(defun ar-show-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-show))
 
(defun ar-singlequote-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-singlequote))
 
(defun ar-slash-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-slash))
 
(defun ar-slashparen-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-slashparen))
 
(defun ar-sort-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-sort))
 
(defun ar-trim-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-trim))
 
(defun ar-trim-left-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-trim-left))
 
(defun ar-trim-right-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-trim-right))
 
(defun ar-underscore-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-underscore))
 
(defun ar-whitespace-hyphened-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-whitespace))
 
(defun ar-hyphened-in-parentized-atpt ()
  "Employ actions of  at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th))
 
(defun ar-greaterangle-hyphened-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-greaterangle))
 
(defun ar-lesserangle-hyphened-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-lesserangle))
 
(defun ar-backslash-hyphened-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-backslash))
 
(defun ar-beg-hyphened-in-parentized-atpt ()
  "Employ actions of BEG at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-beg))
 
(defun ar-blok-hyphened-in-parentized-atpt ()
  "Employ actions of BLOK at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-blok))
 
(defun ar-bounds-hyphened-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-bounds))
 
(defun ar-brace-hyphened-in-parentized-atpt ()
  "Employ actions of BRACE at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-brace))
 
(defun ar-bracket-hyphened-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-bracket))
 
(defun ar-commatize-hyphened-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-commatize))
 
(defun ar-comment-hyphened-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-comment))
 
(defun ar-dollar-hyphened-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-dollar))
 
(defun ar-double-backslash-hyphened-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-double-backslash))
 
(defun ar-doublequote-hyphened-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-doublequote))
 
(defun ar-doubleslash-hyphened-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-hyphened-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-doubleslash-paren))
 
(defun ar-end-hyphened-in-parentized-atpt ()
  "Employ actions of END at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-end))
 
(defun ar-escape-hyphened-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-escape))
 
(defun ar-hide-hyphened-in-parentized-atpt ()
  "Employ actions of HIDE at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-hide))
 
(defun ar-hide-show-hyphened-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-hide-show))
 
(defun ar-hyphen-hyphened-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-hyphen))
 
(defun ar-kill-hyphened-in-parentized-atpt ()
  "Employ actions of KILL at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-kill))
 
(defun ar-left-right-singlequote-hyphened-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-left-right-singlequote))
 
(defun ar-length-hyphened-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-length))
 
(defun ar-parentize-hyphened-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-parentize))
 
(defun ar-quote-hyphened-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-quote))
 
(defun ar-separate-hyphened-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-separate))
 
(defun ar-show-hyphened-in-parentized-atpt ()
  "Employ actions of SHOW at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-show))
 
(defun ar-singlequote-hyphened-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-singlequote))
 
(defun ar-slash-hyphened-in-parentized-atpt ()
  "Employ actions of SLASH at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-slash))
 
(defun ar-slashparen-hyphened-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-slashparen))
 
(defun ar-sort-hyphened-in-parentized-atpt ()
  "Employ actions of SORT at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-sort))
 
(defun ar-trim-hyphened-in-parentized-atpt ()
  "Employ actions of TRIM at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-trim))
 
(defun ar-trim-left-hyphened-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-trim-left))
 
(defun ar-trim-right-hyphened-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-trim-right))
 
(defun ar-underscore-hyphened-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-underscore))
 
(defun ar-whitespace-hyphened-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-whitespace))
 
(defun ar-quoted-in-braced-atpt ()
  "Employ actions of  at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th))
 
(defun ar-greaterangle-quoted-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-greaterangle))
 
(defun ar-lesserangle-quoted-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-lesserangle))
 
(defun ar-backslash-quoted-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-backslash))
 
(defun ar-beg-quoted-in-braced-atpt ()
  "Employ actions of BEG at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-beg))
 
(defun ar-blok-quoted-in-braced-atpt ()
  "Employ actions of BLOK at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-blok))
 
(defun ar-bounds-quoted-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-bounds))
 
(defun ar-brace-quoted-in-braced-atpt ()
  "Employ actions of BRACE at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-brace))
 
(defun ar-bracket-quoted-in-braced-atpt ()
  "Employ actions of BRACKET at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-bracket))
 
(defun ar-commatize-quoted-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-commatize))
 
(defun ar-comment-quoted-in-braced-atpt ()
  "Employ actions of COMMENT at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-comment))
 
(defun ar-dollar-quoted-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-dollar))
 
(defun ar-double-backslash-quoted-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-double-backslash))
 
(defun ar-doublequote-quoted-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-doublequote))
 
(defun ar-doubleslash-quoted-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-quoted-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-doubleslash-paren))
 
(defun ar-end-quoted-in-braced-atpt ()
  "Employ actions of END at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-end))
 
(defun ar-escape-quoted-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-escape))
 
(defun ar-hide-quoted-in-braced-atpt ()
  "Employ actions of HIDE at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-hide))
 
(defun ar-hide-show-quoted-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-hide-show))
 
(defun ar-hyphen-quoted-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-hyphen))
 
(defun ar-kill-quoted-in-braced-atpt ()
  "Employ actions of KILL at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-kill))
 
(defun ar-left-right-singlequote-quoted-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-left-right-singlequote))
 
(defun ar-length-quoted-in-braced-atpt ()
  "Employ actions of LENGTH at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-length))
 
(defun ar-parentize-quoted-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-parentize))
 
(defun ar-quote-quoted-in-braced-atpt ()
  "Employ actions of QUOTE at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-quote))
 
(defun ar-separate-quoted-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-separate))
 
(defun ar-show-quoted-in-braced-atpt ()
  "Employ actions of SHOW at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-show))
 
(defun ar-singlequote-quoted-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-singlequote))
 
(defun ar-slash-quoted-in-braced-atpt ()
  "Employ actions of SLASH at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-slash))
 
(defun ar-slashparen-quoted-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-slashparen))
 
(defun ar-sort-quoted-in-braced-atpt ()
  "Employ actions of SORT at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-sort))
 
(defun ar-trim-quoted-in-braced-atpt ()
  "Employ actions of TRIM at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-trim))
 
(defun ar-trim-left-quoted-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-trim-left))
 
(defun ar-trim-right-quoted-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-trim-right))
 
(defun ar-underscore-quoted-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-underscore))
 
(defun ar-whitespace-quoted-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of QUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'braced 'ar-th-whitespace))
 
(defun ar-quoted-in-bracketed-atpt ()
  "Employ actions of  at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th))
 
(defun ar-greaterangle-quoted-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-greaterangle))
 
(defun ar-lesserangle-quoted-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-lesserangle))
 
(defun ar-backslash-quoted-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-backslash))
 
(defun ar-beg-quoted-in-bracketed-atpt ()
  "Employ actions of BEG at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-beg))
 
(defun ar-blok-quoted-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-blok))
 
(defun ar-bounds-quoted-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-bounds))
 
(defun ar-brace-quoted-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-brace))
 
(defun ar-bracket-quoted-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-bracket))
 
(defun ar-commatize-quoted-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-commatize))
 
(defun ar-comment-quoted-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-comment))
 
(defun ar-dollar-quoted-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-dollar))
 
(defun ar-double-backslash-quoted-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-double-backslash))
 
(defun ar-doublequote-quoted-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-doublequote))
 
(defun ar-doubleslash-quoted-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-quoted-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-doubleslash-paren))
 
(defun ar-end-quoted-in-bracketed-atpt ()
  "Employ actions of END at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-end))
 
(defun ar-escape-quoted-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-escape))
 
(defun ar-hide-quoted-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-hide))
 
(defun ar-hide-show-quoted-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-hide-show))
 
(defun ar-hyphen-quoted-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-hyphen))
 
(defun ar-kill-quoted-in-bracketed-atpt ()
  "Employ actions of KILL at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-kill))
 
(defun ar-left-right-singlequote-quoted-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-left-right-singlequote))
 
(defun ar-length-quoted-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-length))
 
(defun ar-parentize-quoted-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-parentize))
 
(defun ar-quote-quoted-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-quote))
 
(defun ar-separate-quoted-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-separate))
 
(defun ar-show-quoted-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-show))
 
(defun ar-singlequote-quoted-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-singlequote))
 
(defun ar-slash-quoted-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-slash))
 
(defun ar-slashparen-quoted-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-slashparen))
 
(defun ar-sort-quoted-in-bracketed-atpt ()
  "Employ actions of SORT at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-sort))
 
(defun ar-trim-quoted-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-trim))
 
(defun ar-trim-left-quoted-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-trim-left))
 
(defun ar-trim-right-quoted-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-trim-right))
 
(defun ar-underscore-quoted-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-underscore))
 
(defun ar-whitespace-quoted-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of QUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'bracketed 'ar-th-whitespace))
 
(defun ar-quoted-in-lesserangled-atpt ()
  "Employ actions of  at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th))
 
(defun ar-greaterangle-quoted-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-quoted-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-lesserangle))
 
(defun ar-backslash-quoted-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-backslash))
 
(defun ar-beg-quoted-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-beg))
 
(defun ar-blok-quoted-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-blok))
 
(defun ar-bounds-quoted-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-bounds))
 
(defun ar-brace-quoted-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-brace))
 
(defun ar-bracket-quoted-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-bracket))
 
(defun ar-commatize-quoted-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-commatize))
 
(defun ar-comment-quoted-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-comment))
 
(defun ar-dollar-quoted-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-dollar))
 
(defun ar-double-backslash-quoted-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-double-backslash))
 
(defun ar-doublequote-quoted-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-doublequote))
 
(defun ar-doubleslash-quoted-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-quoted-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-doubleslash-paren))
 
(defun ar-end-quoted-in-lesserangled-atpt ()
  "Employ actions of END at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-end))
 
(defun ar-escape-quoted-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-escape))
 
(defun ar-hide-quoted-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-hide))
 
(defun ar-hide-show-quoted-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-hide-show))
 
(defun ar-hyphen-quoted-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-hyphen))
 
(defun ar-kill-quoted-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-quoted-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-left-right-singlequote))
 
(defun ar-length-quoted-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-length))
 
(defun ar-parentize-quoted-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-parentize))
 
(defun ar-quote-quoted-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-quote))
 
(defun ar-separate-quoted-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-separate))
 
(defun ar-show-quoted-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-show))
 
(defun ar-singlequote-quoted-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-singlequote))
 
(defun ar-slash-quoted-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-slash))
 
(defun ar-slashparen-quoted-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-slashparen))
 
(defun ar-sort-quoted-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-sort))
 
(defun ar-trim-quoted-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-trim))
 
(defun ar-trim-left-quoted-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-trim-left))
 
(defun ar-trim-right-quoted-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-trim-right))
 
(defun ar-underscore-quoted-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-underscore))
 
(defun ar-whitespace-quoted-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of QUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'lesserangled 'ar-th-whitespace))
 
(defun ar-quoted-in-greaterangled-atpt ()
  "Employ actions of  at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th))
 
(defun ar-greaterangle-quoted-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-quoted-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-lesserangle))
 
(defun ar-backslash-quoted-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-backslash))
 
(defun ar-beg-quoted-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-beg))
 
(defun ar-blok-quoted-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-blok))
 
(defun ar-bounds-quoted-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-bounds))
 
(defun ar-brace-quoted-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-brace))
 
(defun ar-bracket-quoted-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-bracket))
 
(defun ar-commatize-quoted-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-commatize))
 
(defun ar-comment-quoted-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-comment))
 
(defun ar-dollar-quoted-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-dollar))
 
(defun ar-double-backslash-quoted-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-double-backslash))
 
(defun ar-doublequote-quoted-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-doublequote))
 
(defun ar-doubleslash-quoted-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-quoted-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-doubleslash-paren))
 
(defun ar-end-quoted-in-greaterangled-atpt ()
  "Employ actions of END at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-end))
 
(defun ar-escape-quoted-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-escape))
 
(defun ar-hide-quoted-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-hide))
 
(defun ar-hide-show-quoted-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-hide-show))
 
(defun ar-hyphen-quoted-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-hyphen))
 
(defun ar-kill-quoted-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-quoted-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-left-right-singlequote))
 
(defun ar-length-quoted-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-length))
 
(defun ar-parentize-quoted-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-parentize))
 
(defun ar-quote-quoted-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-quote))
 
(defun ar-separate-quoted-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-separate))
 
(defun ar-show-quoted-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-show))
 
(defun ar-singlequote-quoted-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-singlequote))
 
(defun ar-slash-quoted-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-slash))
 
(defun ar-slashparen-quoted-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-slashparen))
 
(defun ar-sort-quoted-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-sort))
 
(defun ar-trim-quoted-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-trim))
 
(defun ar-trim-left-quoted-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-trim-left))
 
(defun ar-trim-right-quoted-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-trim-right))
 
(defun ar-underscore-quoted-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-underscore))
 
(defun ar-whitespace-quoted-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of QUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'greaterangled 'ar-th-whitespace))
 
(defun ar-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th))
 
(defun ar-greaterangle-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-greaterangle))
 
(defun ar-lesserangle-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-lesserangle))
 
(defun ar-backslash-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-backslash))
 
(defun ar-beg-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-beg))
 
(defun ar-blok-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-blok))
 
(defun ar-bounds-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-bounds))
 
(defun ar-brace-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-brace))
 
(defun ar-bracket-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-bracket))
 
(defun ar-commatize-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-commatize))
 
(defun ar-comment-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-comment))
 
(defun ar-dollar-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-dollar))
 
(defun ar-double-backslash-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-double-backslash))
 
(defun ar-doublequote-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-doublequote))
 
(defun ar-doubleslash-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-doubleslash-paren))
 
(defun ar-end-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-end))
 
(defun ar-escape-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-escape))
 
(defun ar-hide-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-hide))
 
(defun ar-hide-show-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-hide-show))
 
(defun ar-hyphen-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-hyphen))
 
(defun ar-kill-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-kill))
 
(defun ar-left-right-singlequote-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-left-right-singlequote))
 
(defun ar-length-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-length))
 
(defun ar-parentize-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-parentize))
 
(defun ar-quote-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-quote))
 
(defun ar-separate-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-separate))
 
(defun ar-show-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-show))
 
(defun ar-singlequote-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-singlequote))
 
(defun ar-slash-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-slash))
 
(defun ar-slashparen-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-slashparen))
 
(defun ar-sort-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-sort))
 
(defun ar-trim-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-trim))
 
(defun ar-trim-left-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-trim-left))
 
(defun ar-trim-right-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-trim-right))
 
(defun ar-underscore-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-underscore))
 
(defun ar-whitespace-quoted-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of QUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'left-right-singlequoted 'ar-th-whitespace))
 
(defun ar-quoted-in-parentized-atpt ()
  "Employ actions of  at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th))
 
(defun ar-greaterangle-quoted-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-greaterangle))
 
(defun ar-lesserangle-quoted-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-lesserangle))
 
(defun ar-backslash-quoted-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-backslash))
 
(defun ar-beg-quoted-in-parentized-atpt ()
  "Employ actions of BEG at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-beg))
 
(defun ar-blok-quoted-in-parentized-atpt ()
  "Employ actions of BLOK at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-blok))
 
(defun ar-bounds-quoted-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-bounds))
 
(defun ar-brace-quoted-in-parentized-atpt ()
  "Employ actions of BRACE at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-brace))
 
(defun ar-bracket-quoted-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-bracket))
 
(defun ar-commatize-quoted-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-commatize))
 
(defun ar-comment-quoted-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-comment))
 
(defun ar-dollar-quoted-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-dollar))
 
(defun ar-double-backslash-quoted-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-double-backslash))
 
(defun ar-doublequote-quoted-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-doublequote))
 
(defun ar-doubleslash-quoted-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-quoted-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-doubleslash-paren))
 
(defun ar-end-quoted-in-parentized-atpt ()
  "Employ actions of END at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-end))
 
(defun ar-escape-quoted-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-escape))
 
(defun ar-hide-quoted-in-parentized-atpt ()
  "Employ actions of HIDE at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-hide))
 
(defun ar-hide-show-quoted-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-hide-show))
 
(defun ar-hyphen-quoted-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-hyphen))
 
(defun ar-kill-quoted-in-parentized-atpt ()
  "Employ actions of KILL at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-kill))
 
(defun ar-left-right-singlequote-quoted-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-left-right-singlequote))
 
(defun ar-length-quoted-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-length))
 
(defun ar-parentize-quoted-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-parentize))
 
(defun ar-quote-quoted-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-quote))
 
(defun ar-separate-quoted-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-separate))
 
(defun ar-show-quoted-in-parentized-atpt ()
  "Employ actions of SHOW at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-show))
 
(defun ar-singlequote-quoted-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-singlequote))
 
(defun ar-slash-quoted-in-parentized-atpt ()
  "Employ actions of SLASH at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-slash))
 
(defun ar-slashparen-quoted-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-slashparen))
 
(defun ar-sort-quoted-in-parentized-atpt ()
  "Employ actions of SORT at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-sort))
 
(defun ar-trim-quoted-in-parentized-atpt ()
  "Employ actions of TRIM at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-trim))
 
(defun ar-trim-left-quoted-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-trim-left))
 
(defun ar-trim-right-quoted-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-trim-right))
 
(defun ar-underscore-quoted-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-underscore))
 
(defun ar-whitespace-quoted-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of QUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'quoted 'parentized 'ar-th-whitespace))
 
(defun ar-singlequoted-in-braced-atpt ()
  "Employ actions of  at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th))
 
(defun ar-greaterangle-singlequoted-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-greaterangle))
 
(defun ar-lesserangle-singlequoted-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-lesserangle))
 
(defun ar-backslash-singlequoted-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-backslash))
 
(defun ar-beg-singlequoted-in-braced-atpt ()
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-beg))
 
(defun ar-blok-singlequoted-in-braced-atpt ()
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-blok))
 
(defun ar-bounds-singlequoted-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-bounds))
 
(defun ar-brace-singlequoted-in-braced-atpt ()
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-brace))
 
(defun ar-bracket-singlequoted-in-braced-atpt ()
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-bracket))
 
(defun ar-commatize-singlequoted-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-commatize))
 
(defun ar-comment-singlequoted-in-braced-atpt ()
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-comment))
 
(defun ar-dollar-singlequoted-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-dollar))
 
(defun ar-double-backslash-singlequoted-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-double-backslash))
 
(defun ar-doublequote-singlequoted-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-doublequote))
 
(defun ar-doubleslash-singlequoted-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-singlequoted-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-doubleslash-paren))
 
(defun ar-end-singlequoted-in-braced-atpt ()
  "Employ actions of END at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-end))
 
(defun ar-escape-singlequoted-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-escape))
 
(defun ar-hide-singlequoted-in-braced-atpt ()
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-hide))
 
(defun ar-hide-show-singlequoted-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-hide-show))
 
(defun ar-hyphen-singlequoted-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-hyphen))
 
(defun ar-kill-singlequoted-in-braced-atpt ()
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-kill))
 
(defun ar-left-right-singlequote-singlequoted-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-left-right-singlequote))
 
(defun ar-length-singlequoted-in-braced-atpt ()
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-length))
 
(defun ar-parentize-singlequoted-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-parentize))
 
(defun ar-quote-singlequoted-in-braced-atpt ()
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-quote))
 
(defun ar-separate-singlequoted-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-separate))
 
(defun ar-show-singlequoted-in-braced-atpt ()
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-show))
 
(defun ar-singlequote-singlequoted-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-singlequote))
 
(defun ar-slash-singlequoted-in-braced-atpt ()
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-slash))
 
(defun ar-slashparen-singlequoted-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-slashparen))
 
(defun ar-sort-singlequoted-in-braced-atpt ()
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-sort))
 
(defun ar-trim-singlequoted-in-braced-atpt ()
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-trim))
 
(defun ar-trim-left-singlequoted-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-trim-left))
 
(defun ar-trim-right-singlequoted-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-trim-right))
 
(defun ar-underscore-singlequoted-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-underscore))
 
(defun ar-whitespace-singlequoted-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-whitespace))
 
(defun ar-singlequoted-in-bracketed-atpt ()
  "Employ actions of  at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th))
 
(defun ar-greaterangle-singlequoted-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-greaterangle))
 
(defun ar-lesserangle-singlequoted-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-lesserangle))
 
(defun ar-backslash-singlequoted-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-backslash))
 
(defun ar-beg-singlequoted-in-bracketed-atpt ()
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-beg))
 
(defun ar-blok-singlequoted-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-blok))
 
(defun ar-bounds-singlequoted-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-bounds))
 
(defun ar-brace-singlequoted-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-brace))
 
(defun ar-bracket-singlequoted-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-bracket))
 
(defun ar-commatize-singlequoted-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-commatize))
 
(defun ar-comment-singlequoted-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-comment))
 
(defun ar-dollar-singlequoted-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-dollar))
 
(defun ar-double-backslash-singlequoted-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-double-backslash))
 
(defun ar-doublequote-singlequoted-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-doublequote))
 
(defun ar-doubleslash-singlequoted-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-singlequoted-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-doubleslash-paren))
 
(defun ar-end-singlequoted-in-bracketed-atpt ()
  "Employ actions of END at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-end))
 
(defun ar-escape-singlequoted-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-escape))
 
(defun ar-hide-singlequoted-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-hide))
 
(defun ar-hide-show-singlequoted-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-hide-show))
 
(defun ar-hyphen-singlequoted-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-hyphen))
 
(defun ar-kill-singlequoted-in-bracketed-atpt ()
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-kill))
 
(defun ar-left-right-singlequote-singlequoted-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-left-right-singlequote))
 
(defun ar-length-singlequoted-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-length))
 
(defun ar-parentize-singlequoted-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-parentize))
 
(defun ar-quote-singlequoted-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-quote))
 
(defun ar-separate-singlequoted-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-separate))
 
(defun ar-show-singlequoted-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-show))
 
(defun ar-singlequote-singlequoted-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-singlequote))
 
(defun ar-slash-singlequoted-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-slash))
 
(defun ar-slashparen-singlequoted-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-slashparen))
 
(defun ar-sort-singlequoted-in-bracketed-atpt ()
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-sort))
 
(defun ar-trim-singlequoted-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-trim))
 
(defun ar-trim-left-singlequoted-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-trim-left))
 
(defun ar-trim-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-trim-right))
 
(defun ar-underscore-singlequoted-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-underscore))
 
(defun ar-whitespace-singlequoted-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-whitespace))
 
(defun ar-singlequoted-in-lesserangled-atpt ()
  "Employ actions of  at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th))
 
(defun ar-greaterangle-singlequoted-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-singlequoted-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-lesserangle))
 
(defun ar-backslash-singlequoted-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-backslash))
 
(defun ar-beg-singlequoted-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-beg))
 
(defun ar-blok-singlequoted-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-blok))
 
(defun ar-bounds-singlequoted-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-bounds))
 
(defun ar-brace-singlequoted-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-brace))
 
(defun ar-bracket-singlequoted-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-bracket))
 
(defun ar-commatize-singlequoted-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-commatize))
 
(defun ar-comment-singlequoted-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-comment))
 
(defun ar-dollar-singlequoted-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-dollar))
 
(defun ar-double-backslash-singlequoted-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-double-backslash))
 
(defun ar-doublequote-singlequoted-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-doublequote))
 
(defun ar-doubleslash-singlequoted-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-singlequoted-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-doubleslash-paren))
 
(defun ar-end-singlequoted-in-lesserangled-atpt ()
  "Employ actions of END at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-end))
 
(defun ar-escape-singlequoted-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-escape))
 
(defun ar-hide-singlequoted-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-hide))
 
(defun ar-hide-show-singlequoted-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-hide-show))
 
(defun ar-hyphen-singlequoted-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-hyphen))
 
(defun ar-kill-singlequoted-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-singlequoted-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-left-right-singlequote))
 
(defun ar-length-singlequoted-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-length))
 
(defun ar-parentize-singlequoted-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-parentize))
 
(defun ar-quote-singlequoted-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-quote))
 
(defun ar-separate-singlequoted-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-separate))
 
(defun ar-show-singlequoted-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-show))
 
(defun ar-singlequote-singlequoted-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-singlequote))
 
(defun ar-slash-singlequoted-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-slash))
 
(defun ar-slashparen-singlequoted-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-slashparen))
 
(defun ar-sort-singlequoted-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-sort))
 
(defun ar-trim-singlequoted-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-trim))
 
(defun ar-trim-left-singlequoted-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-trim-left))
 
(defun ar-trim-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-trim-right))
 
(defun ar-underscore-singlequoted-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-underscore))
 
(defun ar-whitespace-singlequoted-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-whitespace))
 
(defun ar-singlequoted-in-greaterangled-atpt ()
  "Employ actions of  at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th))
 
(defun ar-greaterangle-singlequoted-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-singlequoted-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-lesserangle))
 
(defun ar-backslash-singlequoted-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-backslash))
 
(defun ar-beg-singlequoted-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-beg))
 
(defun ar-blok-singlequoted-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-blok))
 
(defun ar-bounds-singlequoted-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-bounds))
 
(defun ar-brace-singlequoted-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-brace))
 
(defun ar-bracket-singlequoted-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-bracket))
 
(defun ar-commatize-singlequoted-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-commatize))
 
(defun ar-comment-singlequoted-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-comment))
 
(defun ar-dollar-singlequoted-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-dollar))
 
(defun ar-double-backslash-singlequoted-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-double-backslash))
 
(defun ar-doublequote-singlequoted-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-doublequote))
 
(defun ar-doubleslash-singlequoted-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-singlequoted-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-doubleslash-paren))
 
(defun ar-end-singlequoted-in-greaterangled-atpt ()
  "Employ actions of END at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-end))
 
(defun ar-escape-singlequoted-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-escape))
 
(defun ar-hide-singlequoted-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-hide))
 
(defun ar-hide-show-singlequoted-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-hide-show))
 
(defun ar-hyphen-singlequoted-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-hyphen))
 
(defun ar-kill-singlequoted-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-singlequoted-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-left-right-singlequote))
 
(defun ar-length-singlequoted-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-length))
 
(defun ar-parentize-singlequoted-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-parentize))
 
(defun ar-quote-singlequoted-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-quote))
 
(defun ar-separate-singlequoted-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-separate))
 
(defun ar-show-singlequoted-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-show))
 
(defun ar-singlequote-singlequoted-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-singlequote))
 
(defun ar-slash-singlequoted-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-slash))
 
(defun ar-slashparen-singlequoted-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-slashparen))
 
(defun ar-sort-singlequoted-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-sort))
 
(defun ar-trim-singlequoted-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-trim))
 
(defun ar-trim-left-singlequoted-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-trim-left))
 
(defun ar-trim-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-trim-right))
 
(defun ar-underscore-singlequoted-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-underscore))
 
(defun ar-whitespace-singlequoted-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-whitespace))
 
(defun ar-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th))
 
(defun ar-greaterangle-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-greaterangle))
 
(defun ar-lesserangle-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-lesserangle))
 
(defun ar-backslash-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-backslash))
 
(defun ar-beg-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-beg))
 
(defun ar-blok-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-blok))
 
(defun ar-bounds-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-bounds))
 
(defun ar-brace-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-brace))
 
(defun ar-bracket-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-bracket))
 
(defun ar-commatize-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-commatize))
 
(defun ar-comment-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-comment))
 
(defun ar-dollar-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-dollar))
 
(defun ar-double-backslash-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-double-backslash))
 
(defun ar-doublequote-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-doublequote))
 
(defun ar-doubleslash-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-doubleslash-paren))
 
(defun ar-end-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-end))
 
(defun ar-escape-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-escape))
 
(defun ar-hide-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-hide))
 
(defun ar-hide-show-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-hide-show))
 
(defun ar-hyphen-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-hyphen))
 
(defun ar-kill-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-kill))
 
(defun ar-left-right-singlequote-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-left-right-singlequote))
 
(defun ar-length-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-length))
 
(defun ar-parentize-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-parentize))
 
(defun ar-quote-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-quote))
 
(defun ar-separate-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-separate))
 
(defun ar-show-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-show))
 
(defun ar-singlequote-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-singlequote))
 
(defun ar-slash-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-slash))
 
(defun ar-slashparen-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-slashparen))
 
(defun ar-sort-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-sort))
 
(defun ar-trim-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-trim))
 
(defun ar-trim-left-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-trim-left))
 
(defun ar-trim-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-trim-right))
 
(defun ar-underscore-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-underscore))
 
(defun ar-whitespace-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-whitespace))
 
(defun ar-singlequoted-in-parentized-atpt ()
  "Employ actions of  at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th))
 
(defun ar-greaterangle-singlequoted-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-greaterangle))
 
(defun ar-lesserangle-singlequoted-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-lesserangle))
 
(defun ar-backslash-singlequoted-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-backslash))
 
(defun ar-beg-singlequoted-in-parentized-atpt ()
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-beg))
 
(defun ar-blok-singlequoted-in-parentized-atpt ()
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-blok))
 
(defun ar-bounds-singlequoted-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-bounds))
 
(defun ar-brace-singlequoted-in-parentized-atpt ()
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-brace))
 
(defun ar-bracket-singlequoted-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-bracket))
 
(defun ar-commatize-singlequoted-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-commatize))
 
(defun ar-comment-singlequoted-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-comment))
 
(defun ar-dollar-singlequoted-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-dollar))
 
(defun ar-double-backslash-singlequoted-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-double-backslash))
 
(defun ar-doublequote-singlequoted-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-doublequote))
 
(defun ar-doubleslash-singlequoted-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-singlequoted-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-doubleslash-paren))
 
(defun ar-end-singlequoted-in-parentized-atpt ()
  "Employ actions of END at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-end))
 
(defun ar-escape-singlequoted-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-escape))
 
(defun ar-hide-singlequoted-in-parentized-atpt ()
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-hide))
 
(defun ar-hide-show-singlequoted-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-hide-show))
 
(defun ar-hyphen-singlequoted-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-hyphen))
 
(defun ar-kill-singlequoted-in-parentized-atpt ()
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-kill))
 
(defun ar-left-right-singlequote-singlequoted-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-left-right-singlequote))
 
(defun ar-length-singlequoted-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-length))
 
(defun ar-parentize-singlequoted-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-parentize))
 
(defun ar-quote-singlequoted-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-quote))
 
(defun ar-separate-singlequoted-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-separate))
 
(defun ar-show-singlequoted-in-parentized-atpt ()
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-show))
 
(defun ar-singlequote-singlequoted-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-singlequote))
 
(defun ar-slash-singlequoted-in-parentized-atpt ()
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-slash))
 
(defun ar-slashparen-singlequoted-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-slashparen))
 
(defun ar-sort-singlequoted-in-parentized-atpt ()
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-sort))
 
(defun ar-trim-singlequoted-in-parentized-atpt ()
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-trim))
 
(defun ar-trim-left-singlequoted-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-trim-left))
 
(defun ar-trim-right-singlequoted-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-trim-right))
 
(defun ar-underscore-singlequoted-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-underscore))
 
(defun ar-whitespace-singlequoted-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-whitespace))
 
(defun ar-slashed-in-braced-atpt ()
  "Employ actions of  at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th))
 
(defun ar-greaterangle-slashed-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-greaterangle))
 
(defun ar-lesserangle-slashed-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-lesserangle))
 
(defun ar-backslash-slashed-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-backslash))
 
(defun ar-beg-slashed-in-braced-atpt ()
  "Employ actions of BEG at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-beg))
 
(defun ar-blok-slashed-in-braced-atpt ()
  "Employ actions of BLOK at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-blok))
 
(defun ar-bounds-slashed-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-bounds))
 
(defun ar-brace-slashed-in-braced-atpt ()
  "Employ actions of BRACE at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-brace))
 
(defun ar-bracket-slashed-in-braced-atpt ()
  "Employ actions of BRACKET at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-bracket))
 
(defun ar-commatize-slashed-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-commatize))
 
(defun ar-comment-slashed-in-braced-atpt ()
  "Employ actions of COMMENT at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-comment))
 
(defun ar-dollar-slashed-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-dollar))
 
(defun ar-double-backslash-slashed-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-double-backslash))
 
(defun ar-doublequote-slashed-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-doublequote))
 
(defun ar-doubleslash-slashed-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-slashed-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-doubleslash-paren))
 
(defun ar-end-slashed-in-braced-atpt ()
  "Employ actions of END at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-end))
 
(defun ar-escape-slashed-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-escape))
 
(defun ar-hide-slashed-in-braced-atpt ()
  "Employ actions of HIDE at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-hide))
 
(defun ar-hide-show-slashed-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-hide-show))
 
(defun ar-hyphen-slashed-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-hyphen))
 
(defun ar-kill-slashed-in-braced-atpt ()
  "Employ actions of KILL at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-kill))
 
(defun ar-left-right-singlequote-slashed-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-left-right-singlequote))
 
(defun ar-length-slashed-in-braced-atpt ()
  "Employ actions of LENGTH at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-length))
 
(defun ar-parentize-slashed-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-parentize))
 
(defun ar-quote-slashed-in-braced-atpt ()
  "Employ actions of QUOTE at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-quote))
 
(defun ar-separate-slashed-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-separate))
 
(defun ar-show-slashed-in-braced-atpt ()
  "Employ actions of SHOW at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-show))
 
(defun ar-singlequote-slashed-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-singlequote))
 
(defun ar-slash-slashed-in-braced-atpt ()
  "Employ actions of SLASH at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-slash))
 
(defun ar-slashparen-slashed-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-slashparen))
 
(defun ar-sort-slashed-in-braced-atpt ()
  "Employ actions of SORT at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-sort))
 
(defun ar-trim-slashed-in-braced-atpt ()
  "Employ actions of TRIM at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-trim))
 
(defun ar-trim-left-slashed-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-trim-left))
 
(defun ar-trim-right-slashed-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-trim-right))
 
(defun ar-underscore-slashed-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-underscore))
 
(defun ar-whitespace-slashed-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of SLASHED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-whitespace))
 
(defun ar-slashed-in-bracketed-atpt ()
  "Employ actions of  at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th))
 
(defun ar-greaterangle-slashed-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-greaterangle))
 
(defun ar-lesserangle-slashed-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-lesserangle))
 
(defun ar-backslash-slashed-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-backslash))
 
(defun ar-beg-slashed-in-bracketed-atpt ()
  "Employ actions of BEG at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-beg))
 
(defun ar-blok-slashed-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-blok))
 
(defun ar-bounds-slashed-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-bounds))
 
(defun ar-brace-slashed-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-brace))
 
(defun ar-bracket-slashed-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-bracket))
 
(defun ar-commatize-slashed-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-commatize))
 
(defun ar-comment-slashed-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-comment))
 
(defun ar-dollar-slashed-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-dollar))
 
(defun ar-double-backslash-slashed-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-double-backslash))
 
(defun ar-doublequote-slashed-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-doublequote))
 
(defun ar-doubleslash-slashed-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-slashed-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-doubleslash-paren))
 
(defun ar-end-slashed-in-bracketed-atpt ()
  "Employ actions of END at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-end))
 
(defun ar-escape-slashed-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-escape))
 
(defun ar-hide-slashed-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-hide))
 
(defun ar-hide-show-slashed-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-hide-show))
 
(defun ar-hyphen-slashed-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-hyphen))
 
(defun ar-kill-slashed-in-bracketed-atpt ()
  "Employ actions of KILL at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-kill))
 
(defun ar-left-right-singlequote-slashed-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-left-right-singlequote))
 
(defun ar-length-slashed-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-length))
 
(defun ar-parentize-slashed-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-parentize))
 
(defun ar-quote-slashed-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-quote))
 
(defun ar-separate-slashed-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-separate))
 
(defun ar-show-slashed-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-show))
 
(defun ar-singlequote-slashed-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-singlequote))
 
(defun ar-slash-slashed-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-slash))
 
(defun ar-slashparen-slashed-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-slashparen))
 
(defun ar-sort-slashed-in-bracketed-atpt ()
  "Employ actions of SORT at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-sort))
 
(defun ar-trim-slashed-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-trim))
 
(defun ar-trim-left-slashed-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-trim-left))
 
(defun ar-trim-right-slashed-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-trim-right))
 
(defun ar-underscore-slashed-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-underscore))
 
(defun ar-whitespace-slashed-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of SLASHED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-whitespace))
 
(defun ar-slashed-in-lesserangled-atpt ()
  "Employ actions of  at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th))
 
(defun ar-greaterangle-slashed-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-slashed-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-lesserangle))
 
(defun ar-backslash-slashed-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-backslash))
 
(defun ar-beg-slashed-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-beg))
 
(defun ar-blok-slashed-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-blok))
 
(defun ar-bounds-slashed-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-bounds))
 
(defun ar-brace-slashed-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-brace))
 
(defun ar-bracket-slashed-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-bracket))
 
(defun ar-commatize-slashed-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-commatize))
 
(defun ar-comment-slashed-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-comment))
 
(defun ar-dollar-slashed-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-dollar))
 
(defun ar-double-backslash-slashed-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-double-backslash))
 
(defun ar-doublequote-slashed-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-doublequote))
 
(defun ar-doubleslash-slashed-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-slashed-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-doubleslash-paren))
 
(defun ar-end-slashed-in-lesserangled-atpt ()
  "Employ actions of END at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-end))
 
(defun ar-escape-slashed-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-escape))
 
(defun ar-hide-slashed-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-hide))
 
(defun ar-hide-show-slashed-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-hide-show))
 
(defun ar-hyphen-slashed-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-hyphen))
 
(defun ar-kill-slashed-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-slashed-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-left-right-singlequote))
 
(defun ar-length-slashed-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-length))
 
(defun ar-parentize-slashed-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-parentize))
 
(defun ar-quote-slashed-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-quote))
 
(defun ar-separate-slashed-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-separate))
 
(defun ar-show-slashed-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-show))
 
(defun ar-singlequote-slashed-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-singlequote))
 
(defun ar-slash-slashed-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-slash))
 
(defun ar-slashparen-slashed-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-slashparen))
 
(defun ar-sort-slashed-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-sort))
 
(defun ar-trim-slashed-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-trim))
 
(defun ar-trim-left-slashed-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-trim-left))
 
(defun ar-trim-right-slashed-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-trim-right))
 
(defun ar-underscore-slashed-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-underscore))
 
(defun ar-whitespace-slashed-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of SLASHED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-whitespace))
 
(defun ar-slashed-in-greaterangled-atpt ()
  "Employ actions of  at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th))
 
(defun ar-greaterangle-slashed-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-slashed-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-lesserangle))
 
(defun ar-backslash-slashed-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-backslash))
 
(defun ar-beg-slashed-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-beg))
 
(defun ar-blok-slashed-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-blok))
 
(defun ar-bounds-slashed-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-bounds))
 
(defun ar-brace-slashed-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-brace))
 
(defun ar-bracket-slashed-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-bracket))
 
(defun ar-commatize-slashed-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-commatize))
 
(defun ar-comment-slashed-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-comment))
 
(defun ar-dollar-slashed-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-dollar))
 
(defun ar-double-backslash-slashed-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-double-backslash))
 
(defun ar-doublequote-slashed-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-doublequote))
 
(defun ar-doubleslash-slashed-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-slashed-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-doubleslash-paren))
 
(defun ar-end-slashed-in-greaterangled-atpt ()
  "Employ actions of END at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-end))
 
(defun ar-escape-slashed-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-escape))
 
(defun ar-hide-slashed-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-hide))
 
(defun ar-hide-show-slashed-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-hide-show))
 
(defun ar-hyphen-slashed-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-hyphen))
 
(defun ar-kill-slashed-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-slashed-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-left-right-singlequote))
 
(defun ar-length-slashed-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-length))
 
(defun ar-parentize-slashed-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-parentize))
 
(defun ar-quote-slashed-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-quote))
 
(defun ar-separate-slashed-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-separate))
 
(defun ar-show-slashed-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-show))
 
(defun ar-singlequote-slashed-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-singlequote))
 
(defun ar-slash-slashed-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-slash))
 
(defun ar-slashparen-slashed-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-slashparen))
 
(defun ar-sort-slashed-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-sort))
 
(defun ar-trim-slashed-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-trim))
 
(defun ar-trim-left-slashed-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-trim-left))
 
(defun ar-trim-right-slashed-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-trim-right))
 
(defun ar-underscore-slashed-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-underscore))
 
(defun ar-whitespace-slashed-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of SLASHED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-whitespace))
 
(defun ar-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th))
 
(defun ar-greaterangle-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-greaterangle))
 
(defun ar-lesserangle-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-lesserangle))
 
(defun ar-backslash-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-backslash))
 
(defun ar-beg-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-beg))
 
(defun ar-blok-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-blok))
 
(defun ar-bounds-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-bounds))
 
(defun ar-brace-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-brace))
 
(defun ar-bracket-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-bracket))
 
(defun ar-commatize-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-commatize))
 
(defun ar-comment-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-comment))
 
(defun ar-dollar-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-dollar))
 
(defun ar-double-backslash-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-double-backslash))
 
(defun ar-doublequote-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-doublequote))
 
(defun ar-doubleslash-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-doubleslash-paren))
 
(defun ar-end-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-end))
 
(defun ar-escape-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-escape))
 
(defun ar-hide-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-hide))
 
(defun ar-hide-show-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-hide-show))
 
(defun ar-hyphen-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-hyphen))
 
(defun ar-kill-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-kill))
 
(defun ar-left-right-singlequote-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-left-right-singlequote))
 
(defun ar-length-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-length))
 
(defun ar-parentize-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-parentize))
 
(defun ar-quote-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-quote))
 
(defun ar-separate-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-separate))
 
(defun ar-show-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-show))
 
(defun ar-singlequote-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-singlequote))
 
(defun ar-slash-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-slash))
 
(defun ar-slashparen-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-slashparen))
 
(defun ar-sort-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-sort))
 
(defun ar-trim-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-trim))
 
(defun ar-trim-left-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-trim-left))
 
(defun ar-trim-right-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-trim-right))
 
(defun ar-underscore-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-underscore))
 
(defun ar-whitespace-slashed-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-whitespace))
 
(defun ar-slashed-in-parentized-atpt ()
  "Employ actions of  at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th))
 
(defun ar-greaterangle-slashed-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-greaterangle))
 
(defun ar-lesserangle-slashed-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-lesserangle))
 
(defun ar-backslash-slashed-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-backslash))
 
(defun ar-beg-slashed-in-parentized-atpt ()
  "Employ actions of BEG at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-beg))
 
(defun ar-blok-slashed-in-parentized-atpt ()
  "Employ actions of BLOK at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-blok))
 
(defun ar-bounds-slashed-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-bounds))
 
(defun ar-brace-slashed-in-parentized-atpt ()
  "Employ actions of BRACE at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-brace))
 
(defun ar-bracket-slashed-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-bracket))
 
(defun ar-commatize-slashed-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-commatize))
 
(defun ar-comment-slashed-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-comment))
 
(defun ar-dollar-slashed-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-dollar))
 
(defun ar-double-backslash-slashed-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-double-backslash))
 
(defun ar-doublequote-slashed-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-doublequote))
 
(defun ar-doubleslash-slashed-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-slashed-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-doubleslash-paren))
 
(defun ar-end-slashed-in-parentized-atpt ()
  "Employ actions of END at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-end))
 
(defun ar-escape-slashed-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-escape))
 
(defun ar-hide-slashed-in-parentized-atpt ()
  "Employ actions of HIDE at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-hide))
 
(defun ar-hide-show-slashed-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-hide-show))
 
(defun ar-hyphen-slashed-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-hyphen))
 
(defun ar-kill-slashed-in-parentized-atpt ()
  "Employ actions of KILL at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-kill))
 
(defun ar-left-right-singlequote-slashed-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-left-right-singlequote))
 
(defun ar-length-slashed-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-length))
 
(defun ar-parentize-slashed-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-parentize))
 
(defun ar-quote-slashed-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-quote))
 
(defun ar-separate-slashed-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-separate))
 
(defun ar-show-slashed-in-parentized-atpt ()
  "Employ actions of SHOW at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-show))
 
(defun ar-singlequote-slashed-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-singlequote))
 
(defun ar-slash-slashed-in-parentized-atpt ()
  "Employ actions of SLASH at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-slash))
 
(defun ar-slashparen-slashed-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-slashparen))
 
(defun ar-sort-slashed-in-parentized-atpt ()
  "Employ actions of SORT at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-sort))
 
(defun ar-trim-slashed-in-parentized-atpt ()
  "Employ actions of TRIM at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-trim))
 
(defun ar-trim-left-slashed-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-trim-left))
 
(defun ar-trim-right-slashed-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-trim-right))
 
(defun ar-underscore-slashed-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-underscore))
 
(defun ar-whitespace-slashed-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of SLASHED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-whitespace))
 
(defun ar-underscored-in-braced-atpt ()
  "Employ actions of  at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th))
 
(defun ar-greaterangle-underscored-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-greaterangle))
 
(defun ar-lesserangle-underscored-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-lesserangle))
 
(defun ar-backslash-underscored-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-backslash))
 
(defun ar-beg-underscored-in-braced-atpt ()
  "Employ actions of BEG at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-beg))
 
(defun ar-blok-underscored-in-braced-atpt ()
  "Employ actions of BLOK at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-blok))
 
(defun ar-bounds-underscored-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-bounds))
 
(defun ar-brace-underscored-in-braced-atpt ()
  "Employ actions of BRACE at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-brace))
 
(defun ar-bracket-underscored-in-braced-atpt ()
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-bracket))
 
(defun ar-commatize-underscored-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-commatize))
 
(defun ar-comment-underscored-in-braced-atpt ()
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-comment))
 
(defun ar-dollar-underscored-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-dollar))
 
(defun ar-double-backslash-underscored-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-double-backslash))
 
(defun ar-doublequote-underscored-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-doublequote))
 
(defun ar-doubleslash-underscored-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-underscored-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-doubleslash-paren))
 
(defun ar-end-underscored-in-braced-atpt ()
  "Employ actions of END at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-end))
 
(defun ar-escape-underscored-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-escape))
 
(defun ar-hide-underscored-in-braced-atpt ()
  "Employ actions of HIDE at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-hide))
 
(defun ar-hide-show-underscored-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-hide-show))
 
(defun ar-hyphen-underscored-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-hyphen))
 
(defun ar-kill-underscored-in-braced-atpt ()
  "Employ actions of KILL at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-kill))
 
(defun ar-left-right-singlequote-underscored-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-left-right-singlequote))
 
(defun ar-length-underscored-in-braced-atpt ()
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-length))
 
(defun ar-parentize-underscored-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-parentize))
 
(defun ar-quote-underscored-in-braced-atpt ()
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-quote))
 
(defun ar-separate-underscored-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-separate))
 
(defun ar-show-underscored-in-braced-atpt ()
  "Employ actions of SHOW at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-show))
 
(defun ar-singlequote-underscored-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-singlequote))
 
(defun ar-slash-underscored-in-braced-atpt ()
  "Employ actions of SLASH at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-slash))
 
(defun ar-slashparen-underscored-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-slashparen))
 
(defun ar-sort-underscored-in-braced-atpt ()
  "Employ actions of SORT at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-sort))
 
(defun ar-trim-underscored-in-braced-atpt ()
  "Employ actions of TRIM at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-trim))
 
(defun ar-trim-left-underscored-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-trim-left))
 
(defun ar-trim-right-underscored-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-trim-right))
 
(defun ar-underscore-underscored-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-underscore))
 
(defun ar-whitespace-underscored-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-whitespace))
 
(defun ar-underscored-in-bracketed-atpt ()
  "Employ actions of  at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th))
 
(defun ar-greaterangle-underscored-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-greaterangle))
 
(defun ar-lesserangle-underscored-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-lesserangle))
 
(defun ar-backslash-underscored-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-backslash))
 
(defun ar-beg-underscored-in-bracketed-atpt ()
  "Employ actions of BEG at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-beg))
 
(defun ar-blok-underscored-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-blok))
 
(defun ar-bounds-underscored-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-bounds))
 
(defun ar-brace-underscored-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-brace))
 
(defun ar-bracket-underscored-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-bracket))
 
(defun ar-commatize-underscored-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-commatize))
 
(defun ar-comment-underscored-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-comment))
 
(defun ar-dollar-underscored-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-dollar))
 
(defun ar-double-backslash-underscored-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-double-backslash))
 
(defun ar-doublequote-underscored-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-doublequote))
 
(defun ar-doubleslash-underscored-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-underscored-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-doubleslash-paren))
 
(defun ar-end-underscored-in-bracketed-atpt ()
  "Employ actions of END at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-end))
 
(defun ar-escape-underscored-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-escape))
 
(defun ar-hide-underscored-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-hide))
 
(defun ar-hide-show-underscored-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-hide-show))
 
(defun ar-hyphen-underscored-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-hyphen))
 
(defun ar-kill-underscored-in-bracketed-atpt ()
  "Employ actions of KILL at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-kill))
 
(defun ar-left-right-singlequote-underscored-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-left-right-singlequote))
 
(defun ar-length-underscored-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-length))
 
(defun ar-parentize-underscored-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-parentize))
 
(defun ar-quote-underscored-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-quote))
 
(defun ar-separate-underscored-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-separate))
 
(defun ar-show-underscored-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-show))
 
(defun ar-singlequote-underscored-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-singlequote))
 
(defun ar-slash-underscored-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-slash))
 
(defun ar-slashparen-underscored-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-slashparen))
 
(defun ar-sort-underscored-in-bracketed-atpt ()
  "Employ actions of SORT at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-sort))
 
(defun ar-trim-underscored-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-trim))
 
(defun ar-trim-left-underscored-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-trim-left))
 
(defun ar-trim-right-underscored-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-trim-right))
 
(defun ar-underscore-underscored-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-underscore))
 
(defun ar-whitespace-underscored-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-whitespace))
 
(defun ar-underscored-in-lesserangled-atpt ()
  "Employ actions of  at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th))
 
(defun ar-greaterangle-underscored-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-underscored-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-lesserangle))
 
(defun ar-backslash-underscored-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-backslash))
 
(defun ar-beg-underscored-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-beg))
 
(defun ar-blok-underscored-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-blok))
 
(defun ar-bounds-underscored-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-bounds))
 
(defun ar-brace-underscored-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-brace))
 
(defun ar-bracket-underscored-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-bracket))
 
(defun ar-commatize-underscored-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-commatize))
 
(defun ar-comment-underscored-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-comment))
 
(defun ar-dollar-underscored-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-dollar))
 
(defun ar-double-backslash-underscored-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-double-backslash))
 
(defun ar-doublequote-underscored-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-doublequote))
 
(defun ar-doubleslash-underscored-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-underscored-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-doubleslash-paren))
 
(defun ar-end-underscored-in-lesserangled-atpt ()
  "Employ actions of END at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-end))
 
(defun ar-escape-underscored-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-escape))
 
(defun ar-hide-underscored-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-hide))
 
(defun ar-hide-show-underscored-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-hide-show))
 
(defun ar-hyphen-underscored-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-hyphen))
 
(defun ar-kill-underscored-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-underscored-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-left-right-singlequote))
 
(defun ar-length-underscored-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-length))
 
(defun ar-parentize-underscored-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-parentize))
 
(defun ar-quote-underscored-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-quote))
 
(defun ar-separate-underscored-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-separate))
 
(defun ar-show-underscored-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-show))
 
(defun ar-singlequote-underscored-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-singlequote))
 
(defun ar-slash-underscored-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-slash))
 
(defun ar-slashparen-underscored-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-slashparen))
 
(defun ar-sort-underscored-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-sort))
 
(defun ar-trim-underscored-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-trim))
 
(defun ar-trim-left-underscored-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-trim-left))
 
(defun ar-trim-right-underscored-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-trim-right))
 
(defun ar-underscore-underscored-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-underscore))
 
(defun ar-whitespace-underscored-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-whitespace))
 
(defun ar-underscored-in-greaterangled-atpt ()
  "Employ actions of  at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th))
 
(defun ar-greaterangle-underscored-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-underscored-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-lesserangle))
 
(defun ar-backslash-underscored-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-backslash))
 
(defun ar-beg-underscored-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-beg))
 
(defun ar-blok-underscored-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-blok))
 
(defun ar-bounds-underscored-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-bounds))
 
(defun ar-brace-underscored-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-brace))
 
(defun ar-bracket-underscored-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-bracket))
 
(defun ar-commatize-underscored-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-commatize))
 
(defun ar-comment-underscored-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-comment))
 
(defun ar-dollar-underscored-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-dollar))
 
(defun ar-double-backslash-underscored-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-double-backslash))
 
(defun ar-doublequote-underscored-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-doublequote))
 
(defun ar-doubleslash-underscored-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-underscored-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-doubleslash-paren))
 
(defun ar-end-underscored-in-greaterangled-atpt ()
  "Employ actions of END at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-end))
 
(defun ar-escape-underscored-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-escape))
 
(defun ar-hide-underscored-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-hide))
 
(defun ar-hide-show-underscored-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-hide-show))
 
(defun ar-hyphen-underscored-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-hyphen))
 
(defun ar-kill-underscored-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-underscored-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-left-right-singlequote))
 
(defun ar-length-underscored-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-length))
 
(defun ar-parentize-underscored-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-parentize))
 
(defun ar-quote-underscored-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-quote))
 
(defun ar-separate-underscored-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-separate))
 
(defun ar-show-underscored-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-show))
 
(defun ar-singlequote-underscored-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-singlequote))
 
(defun ar-slash-underscored-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-slash))
 
(defun ar-slashparen-underscored-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-slashparen))
 
(defun ar-sort-underscored-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-sort))
 
(defun ar-trim-underscored-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-trim))
 
(defun ar-trim-left-underscored-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-trim-left))
 
(defun ar-trim-right-underscored-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-trim-right))
 
(defun ar-underscore-underscored-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-underscore))
 
(defun ar-whitespace-underscored-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-whitespace))
 
(defun ar-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th))
 
(defun ar-greaterangle-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-greaterangle))
 
(defun ar-lesserangle-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-lesserangle))
 
(defun ar-backslash-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-backslash))
 
(defun ar-beg-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-beg))
 
(defun ar-blok-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-blok))
 
(defun ar-bounds-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-bounds))
 
(defun ar-brace-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-brace))
 
(defun ar-bracket-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-bracket))
 
(defun ar-commatize-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-commatize))
 
(defun ar-comment-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-comment))
 
(defun ar-dollar-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-dollar))
 
(defun ar-double-backslash-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-double-backslash))
 
(defun ar-doublequote-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-doublequote))
 
(defun ar-doubleslash-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-doubleslash-paren))
 
(defun ar-end-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-end))
 
(defun ar-escape-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-escape))
 
(defun ar-hide-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-hide))
 
(defun ar-hide-show-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-hide-show))
 
(defun ar-hyphen-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-hyphen))
 
(defun ar-kill-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-kill))
 
(defun ar-left-right-singlequote-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-left-right-singlequote))
 
(defun ar-length-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-length))
 
(defun ar-parentize-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-parentize))
 
(defun ar-quote-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-quote))
 
(defun ar-separate-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-separate))
 
(defun ar-show-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-show))
 
(defun ar-singlequote-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-singlequote))
 
(defun ar-slash-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-slash))
 
(defun ar-slashparen-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-slashparen))
 
(defun ar-sort-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-sort))
 
(defun ar-trim-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-trim))
 
(defun ar-trim-left-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-trim-left))
 
(defun ar-trim-right-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-trim-right))
 
(defun ar-underscore-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-underscore))
 
(defun ar-whitespace-underscored-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-whitespace))
 
(defun ar-underscored-in-parentized-atpt ()
  "Employ actions of  at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th))
 
(defun ar-greaterangle-underscored-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-greaterangle))
 
(defun ar-lesserangle-underscored-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-lesserangle))
 
(defun ar-backslash-underscored-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-backslash))
 
(defun ar-beg-underscored-in-parentized-atpt ()
  "Employ actions of BEG at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-beg))
 
(defun ar-blok-underscored-in-parentized-atpt ()
  "Employ actions of BLOK at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-blok))
 
(defun ar-bounds-underscored-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-bounds))
 
(defun ar-brace-underscored-in-parentized-atpt ()
  "Employ actions of BRACE at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-brace))
 
(defun ar-bracket-underscored-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-bracket))
 
(defun ar-commatize-underscored-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-commatize))
 
(defun ar-comment-underscored-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-comment))
 
(defun ar-dollar-underscored-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-dollar))
 
(defun ar-double-backslash-underscored-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-double-backslash))
 
(defun ar-doublequote-underscored-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-doublequote))
 
(defun ar-doubleslash-underscored-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-underscored-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-doubleslash-paren))
 
(defun ar-end-underscored-in-parentized-atpt ()
  "Employ actions of END at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-end))
 
(defun ar-escape-underscored-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-escape))
 
(defun ar-hide-underscored-in-parentized-atpt ()
  "Employ actions of HIDE at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-hide))
 
(defun ar-hide-show-underscored-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-hide-show))
 
(defun ar-hyphen-underscored-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-hyphen))
 
(defun ar-kill-underscored-in-parentized-atpt ()
  "Employ actions of KILL at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-kill))
 
(defun ar-left-right-singlequote-underscored-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-left-right-singlequote))
 
(defun ar-length-underscored-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-length))
 
(defun ar-parentize-underscored-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-parentize))
 
(defun ar-quote-underscored-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-quote))
 
(defun ar-separate-underscored-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-separate))
 
(defun ar-show-underscored-in-parentized-atpt ()
  "Employ actions of SHOW at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-show))
 
(defun ar-singlequote-underscored-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-singlequote))
 
(defun ar-slash-underscored-in-parentized-atpt ()
  "Employ actions of SLASH at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-slash))
 
(defun ar-slashparen-underscored-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-slashparen))
 
(defun ar-sort-underscored-in-parentized-atpt ()
  "Employ actions of SORT at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-sort))
 
(defun ar-trim-underscored-in-parentized-atpt ()
  "Employ actions of TRIM at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-trim))
 
(defun ar-trim-left-underscored-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-trim-left))
 
(defun ar-trim-right-underscored-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-trim-right))
 
(defun ar-underscore-underscored-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-underscore))
 
(defun ar-whitespace-underscored-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-whitespace))
 
(defun ar-whitespaced-in-braced-atpt ()
  "Employ actions of  at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th))
 
(defun ar-greaterangle-whitespaced-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-greaterangle))
 
(defun ar-lesserangle-whitespaced-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-lesserangle))
 
(defun ar-backslash-whitespaced-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-backslash))
 
(defun ar-beg-whitespaced-in-braced-atpt ()
  "Employ actions of BEG at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-beg))
 
(defun ar-blok-whitespaced-in-braced-atpt ()
  "Employ actions of BLOK at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-blok))
 
(defun ar-bounds-whitespaced-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-bounds))
 
(defun ar-brace-whitespaced-in-braced-atpt ()
  "Employ actions of BRACE at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-brace))
 
(defun ar-bracket-whitespaced-in-braced-atpt ()
  "Employ actions of BRACKET at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-bracket))
 
(defun ar-commatize-whitespaced-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-commatize))
 
(defun ar-comment-whitespaced-in-braced-atpt ()
  "Employ actions of COMMENT at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-comment))
 
(defun ar-dollar-whitespaced-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-dollar))
 
(defun ar-double-backslash-whitespaced-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-double-backslash))
 
(defun ar-doublequote-whitespaced-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-doublequote))
 
(defun ar-doubleslash-whitespaced-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-whitespaced-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-doubleslash-paren))
 
(defun ar-end-whitespaced-in-braced-atpt ()
  "Employ actions of END at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-end))
 
(defun ar-escape-whitespaced-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-escape))
 
(defun ar-hide-whitespaced-in-braced-atpt ()
  "Employ actions of HIDE at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-hide))
 
(defun ar-hide-show-whitespaced-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-hide-show))
 
(defun ar-hyphen-whitespaced-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-hyphen))
 
(defun ar-kill-whitespaced-in-braced-atpt ()
  "Employ actions of KILL at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-kill))
 
(defun ar-left-right-singlequote-whitespaced-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-left-right-singlequote))
 
(defun ar-length-whitespaced-in-braced-atpt ()
  "Employ actions of LENGTH at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-length))
 
(defun ar-parentize-whitespaced-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-parentize))
 
(defun ar-quote-whitespaced-in-braced-atpt ()
  "Employ actions of QUOTE at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-quote))
 
(defun ar-separate-whitespaced-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-separate))
 
(defun ar-show-whitespaced-in-braced-atpt ()
  "Employ actions of SHOW at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-show))
 
(defun ar-singlequote-whitespaced-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-singlequote))
 
(defun ar-slash-whitespaced-in-braced-atpt ()
  "Employ actions of SLASH at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-slash))
 
(defun ar-slashparen-whitespaced-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-slashparen))
 
(defun ar-sort-whitespaced-in-braced-atpt ()
  "Employ actions of SORT at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-sort))
 
(defun ar-trim-whitespaced-in-braced-atpt ()
  "Employ actions of TRIM at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-trim))
 
(defun ar-trim-left-whitespaced-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-trim-left))
 
(defun ar-trim-right-whitespaced-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-trim-right))
 
(defun ar-underscore-whitespaced-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-underscore))
 
(defun ar-whitespace-whitespaced-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-whitespace))
 
(defun ar-whitespaced-in-bracketed-atpt ()
  "Employ actions of  at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th))
 
(defun ar-greaterangle-whitespaced-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-greaterangle))
 
(defun ar-lesserangle-whitespaced-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-lesserangle))
 
(defun ar-backslash-whitespaced-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-backslash))
 
(defun ar-beg-whitespaced-in-bracketed-atpt ()
  "Employ actions of BEG at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-beg))
 
(defun ar-blok-whitespaced-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-blok))
 
(defun ar-bounds-whitespaced-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-bounds))
 
(defun ar-brace-whitespaced-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-brace))
 
(defun ar-bracket-whitespaced-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-bracket))
 
(defun ar-commatize-whitespaced-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-commatize))
 
(defun ar-comment-whitespaced-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-comment))
 
(defun ar-dollar-whitespaced-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-dollar))
 
(defun ar-double-backslash-whitespaced-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-double-backslash))
 
(defun ar-doublequote-whitespaced-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-doublequote))
 
(defun ar-doubleslash-whitespaced-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-whitespaced-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-doubleslash-paren))
 
(defun ar-end-whitespaced-in-bracketed-atpt ()
  "Employ actions of END at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-end))
 
(defun ar-escape-whitespaced-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-escape))
 
(defun ar-hide-whitespaced-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-hide))
 
(defun ar-hide-show-whitespaced-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-hide-show))
 
(defun ar-hyphen-whitespaced-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-hyphen))
 
(defun ar-kill-whitespaced-in-bracketed-atpt ()
  "Employ actions of KILL at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-kill))
 
(defun ar-left-right-singlequote-whitespaced-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-left-right-singlequote))
 
(defun ar-length-whitespaced-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-length))
 
(defun ar-parentize-whitespaced-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-parentize))
 
(defun ar-quote-whitespaced-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-quote))
 
(defun ar-separate-whitespaced-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-separate))
 
(defun ar-show-whitespaced-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-show))
 
(defun ar-singlequote-whitespaced-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-singlequote))
 
(defun ar-slash-whitespaced-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-slash))
 
(defun ar-slashparen-whitespaced-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-slashparen))
 
(defun ar-sort-whitespaced-in-bracketed-atpt ()
  "Employ actions of SORT at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-sort))
 
(defun ar-trim-whitespaced-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-trim))
 
(defun ar-trim-left-whitespaced-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-trim-left))
 
(defun ar-trim-right-whitespaced-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-trim-right))
 
(defun ar-underscore-whitespaced-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-underscore))
 
(defun ar-whitespace-whitespaced-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-whitespace))
 
(defun ar-whitespaced-in-lesserangled-atpt ()
  "Employ actions of  at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th))
 
(defun ar-greaterangle-whitespaced-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-whitespaced-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-lesserangle))
 
(defun ar-backslash-whitespaced-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-backslash))
 
(defun ar-beg-whitespaced-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-beg))
 
(defun ar-blok-whitespaced-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-blok))
 
(defun ar-bounds-whitespaced-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-bounds))
 
(defun ar-brace-whitespaced-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-brace))
 
(defun ar-bracket-whitespaced-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-bracket))
 
(defun ar-commatize-whitespaced-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-commatize))
 
(defun ar-comment-whitespaced-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-comment))
 
(defun ar-dollar-whitespaced-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-dollar))
 
(defun ar-double-backslash-whitespaced-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-double-backslash))
 
(defun ar-doublequote-whitespaced-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-doublequote))
 
(defun ar-doubleslash-whitespaced-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-whitespaced-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-doubleslash-paren))
 
(defun ar-end-whitespaced-in-lesserangled-atpt ()
  "Employ actions of END at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-end))
 
(defun ar-escape-whitespaced-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-escape))
 
(defun ar-hide-whitespaced-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-hide))
 
(defun ar-hide-show-whitespaced-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-hide-show))
 
(defun ar-hyphen-whitespaced-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-hyphen))
 
(defun ar-kill-whitespaced-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-whitespaced-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-left-right-singlequote))
 
(defun ar-length-whitespaced-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-length))
 
(defun ar-parentize-whitespaced-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-parentize))
 
(defun ar-quote-whitespaced-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-quote))
 
(defun ar-separate-whitespaced-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-separate))
 
(defun ar-show-whitespaced-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-show))
 
(defun ar-singlequote-whitespaced-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-singlequote))
 
(defun ar-slash-whitespaced-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-slash))
 
(defun ar-slashparen-whitespaced-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-slashparen))
 
(defun ar-sort-whitespaced-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-sort))
 
(defun ar-trim-whitespaced-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-trim))
 
(defun ar-trim-left-whitespaced-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-trim-left))
 
(defun ar-trim-right-whitespaced-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-trim-right))
 
(defun ar-underscore-whitespaced-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-underscore))
 
(defun ar-whitespace-whitespaced-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-whitespace))
 
(defun ar-whitespaced-in-greaterangled-atpt ()
  "Employ actions of  at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th))
 
(defun ar-greaterangle-whitespaced-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-greaterangle))
 
(defun ar-lesserangle-whitespaced-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-lesserangle))
 
(defun ar-backslash-whitespaced-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-backslash))
 
(defun ar-beg-whitespaced-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-beg))
 
(defun ar-blok-whitespaced-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-blok))
 
(defun ar-bounds-whitespaced-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-bounds))
 
(defun ar-brace-whitespaced-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-brace))
 
(defun ar-bracket-whitespaced-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-bracket))
 
(defun ar-commatize-whitespaced-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-commatize))
 
(defun ar-comment-whitespaced-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-comment))
 
(defun ar-dollar-whitespaced-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-dollar))
 
(defun ar-double-backslash-whitespaced-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-double-backslash))
 
(defun ar-doublequote-whitespaced-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-doublequote))
 
(defun ar-doubleslash-whitespaced-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-whitespaced-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-doubleslash-paren))
 
(defun ar-end-whitespaced-in-greaterangled-atpt ()
  "Employ actions of END at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-end))
 
(defun ar-escape-whitespaced-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-escape))
 
(defun ar-hide-whitespaced-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-hide))
 
(defun ar-hide-show-whitespaced-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-hide-show))
 
(defun ar-hyphen-whitespaced-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-hyphen))
 
(defun ar-kill-whitespaced-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-kill))
 
(defun ar-left-right-singlequote-whitespaced-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-left-right-singlequote))
 
(defun ar-length-whitespaced-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-length))
 
(defun ar-parentize-whitespaced-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-parentize))
 
(defun ar-quote-whitespaced-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-quote))
 
(defun ar-separate-whitespaced-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-separate))
 
(defun ar-show-whitespaced-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-show))
 
(defun ar-singlequote-whitespaced-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-singlequote))
 
(defun ar-slash-whitespaced-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-slash))
 
(defun ar-slashparen-whitespaced-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-slashparen))
 
(defun ar-sort-whitespaced-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-sort))
 
(defun ar-trim-whitespaced-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-trim))
 
(defun ar-trim-left-whitespaced-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-trim-left))
 
(defun ar-trim-right-whitespaced-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-trim-right))
 
(defun ar-underscore-whitespaced-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-underscore))
 
(defun ar-whitespace-whitespaced-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-whitespace))
 
(defun ar-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th))
 
(defun ar-greaterangle-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-greaterangle))
 
(defun ar-lesserangle-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-lesserangle))
 
(defun ar-backslash-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-backslash))
 
(defun ar-beg-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-beg))
 
(defun ar-blok-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-blok))
 
(defun ar-bounds-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-bounds))
 
(defun ar-brace-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-brace))
 
(defun ar-bracket-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-bracket))
 
(defun ar-commatize-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-commatize))
 
(defun ar-comment-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-comment))
 
(defun ar-dollar-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-dollar))
 
(defun ar-double-backslash-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-double-backslash))
 
(defun ar-doublequote-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-doublequote))
 
(defun ar-doubleslash-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-doubleslash-paren))
 
(defun ar-end-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-end))
 
(defun ar-escape-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-escape))
 
(defun ar-hide-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-hide))
 
(defun ar-hide-show-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-hide-show))
 
(defun ar-hyphen-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-hyphen))
 
(defun ar-kill-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-kill))
 
(defun ar-left-right-singlequote-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-left-right-singlequote))
 
(defun ar-length-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-length))
 
(defun ar-parentize-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-parentize))
 
(defun ar-quote-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-quote))
 
(defun ar-separate-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-separate))
 
(defun ar-show-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-show))
 
(defun ar-singlequote-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-singlequote))
 
(defun ar-slash-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-slash))
 
(defun ar-slashparen-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-slashparen))
 
(defun ar-sort-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-sort))
 
(defun ar-trim-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-trim))
 
(defun ar-trim-left-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-trim-left))
 
(defun ar-trim-right-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-trim-right))
 
(defun ar-underscore-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-underscore))
 
(defun ar-whitespace-whitespaced-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-whitespace))
 
(defun ar-whitespaced-in-parentized-atpt ()
  "Employ actions of  at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th))
 
(defun ar-greaterangle-whitespaced-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-greaterangle))
 
(defun ar-lesserangle-whitespaced-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-lesserangle))
 
(defun ar-backslash-whitespaced-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-backslash))
 
(defun ar-beg-whitespaced-in-parentized-atpt ()
  "Employ actions of BEG at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-beg))
 
(defun ar-blok-whitespaced-in-parentized-atpt ()
  "Employ actions of BLOK at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-blok))
 
(defun ar-bounds-whitespaced-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-bounds))
 
(defun ar-brace-whitespaced-in-parentized-atpt ()
  "Employ actions of BRACE at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-brace))
 
(defun ar-bracket-whitespaced-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-bracket))
 
(defun ar-commatize-whitespaced-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-commatize))
 
(defun ar-comment-whitespaced-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-comment))
 
(defun ar-dollar-whitespaced-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-dollar))
 
(defun ar-double-backslash-whitespaced-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-double-backslash))
 
(defun ar-doublequote-whitespaced-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-doublequote))
 
(defun ar-doubleslash-whitespaced-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-doubleslash))
 
(defun ar-doubleslash-paren-whitespaced-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-doubleslash-paren))
 
(defun ar-end-whitespaced-in-parentized-atpt ()
  "Employ actions of END at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-end))
 
(defun ar-escape-whitespaced-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-escape))
 
(defun ar-hide-whitespaced-in-parentized-atpt ()
  "Employ actions of HIDE at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-hide))
 
(defun ar-hide-show-whitespaced-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-hide-show))
 
(defun ar-hyphen-whitespaced-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-hyphen))
 
(defun ar-kill-whitespaced-in-parentized-atpt ()
  "Employ actions of KILL at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-kill))
 
(defun ar-left-right-singlequote-whitespaced-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-left-right-singlequote))
 
(defun ar-length-whitespaced-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-length))
 
(defun ar-parentize-whitespaced-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-parentize))
 
(defun ar-quote-whitespaced-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-quote))
 
(defun ar-separate-whitespaced-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-separate))
 
(defun ar-show-whitespaced-in-parentized-atpt ()
  "Employ actions of SHOW at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-show))
 
(defun ar-singlequote-whitespaced-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-singlequote))
 
(defun ar-slash-whitespaced-in-parentized-atpt ()
  "Employ actions of SLASH at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-slash))
 
(defun ar-slashparen-whitespaced-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-slashparen))
 
(defun ar-sort-whitespaced-in-parentized-atpt ()
  "Employ actions of SORT at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-sort))
 
(defun ar-trim-whitespaced-in-parentized-atpt ()
  "Employ actions of TRIM at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-trim))
 
(defun ar-trim-left-whitespaced-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-trim-left))
 
(defun ar-trim-right-whitespaced-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-trim-right))
 
(defun ar-underscore-whitespaced-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-underscore))
 
(defun ar-whitespace-whitespaced-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-whitespace))

(provide 'ar-thingatpt-unpaired-delimited-list-in-delimited-list)
;;;thingatpt-unpaired-delimited-list-in-delimited-list.el ends here

