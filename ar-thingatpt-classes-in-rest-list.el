;;; thingatpt-classes-in-rest-list.el --- thing-in-thing functions -*- lexical-binding: t; -*-

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

(defun ar-alnum-in-abbrev-atpt ()
  "Employ actions of  at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th))

(defun ar-greaterangle-alnum-in-abbrev-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-greaterangle))

(defun ar-lesserangle-alnum-in-abbrev-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-lesserangle))

(defun ar-backslash-alnum-in-abbrev-atpt ()
  "Employ actions of BACKSLASH at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-backslash))

(defun ar-beg-alnum-in-abbrev-atpt ()
  "Employ actions of BEG at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-beg))

(defun ar-blok-alnum-in-abbrev-atpt ()
  "Employ actions of BLOK at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-blok))

(defun ar-bounds-alnum-in-abbrev-atpt ()
  "Employ actions of BOUNDS at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-bounds))

(defun ar-brace-alnum-in-abbrev-atpt ()
  "Employ actions of BRACE at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-brace))

(defun ar-bracket-alnum-in-abbrev-atpt ()
  "Employ actions of BRACKET at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-bracket))

(defun ar-commatize-alnum-in-abbrev-atpt ()
  "Employ actions of COMMATIZE at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-commatize))

(defun ar-comment-alnum-in-abbrev-atpt ()
  "Employ actions of COMMENT at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-comment))

(defun ar-dollar-alnum-in-abbrev-atpt ()
  "Employ actions of DOLLAR at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-dollar))

(defun ar-double-backslash-alnum-in-abbrev-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-double-backslash))

(defun ar-doublequote-alnum-in-abbrev-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-doublequote))

(defun ar-doubleslash-alnum-in-abbrev-atpt ()
  "Employ actions of DOUBLESLASH at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-doubleslash))

(defun ar-doubleslash-paren-alnum-in-abbrev-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-alnum-in-abbrev-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-doublebackslash-paren))

(defun ar-end-alnum-in-abbrev-atpt ()
  "Employ actions of END at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-end))

(defun ar-escape-alnum-in-abbrev-atpt ()
  "Employ actions of ESCAPE at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-escape))

(defun ar-hide-alnum-in-abbrev-atpt ()
  "Employ actions of HIDE at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-hide))

(defun ar-hide-show-alnum-in-abbrev-atpt ()
  "Employ actions of HIDE-SHOW at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-hide-show))

(defun ar-hyphen-alnum-in-abbrev-atpt ()
  "Employ actions of HYPHEN at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-hyphen))

(defun ar-kill-alnum-in-abbrev-atpt ()
  "Employ actions of KILL at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-kill))

(defun ar-left-right-singlequote-alnum-in-abbrev-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-left-right-singlequote))

(defun ar-length-alnum-in-abbrev-atpt ()
  "Employ actions of LENGTH at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-length))

(defun ar-parentize-alnum-in-abbrev-atpt ()
  "Employ actions of PARENTIZE at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-parentize))

(defun ar-quote-alnum-in-abbrev-atpt ()
  "Employ actions of QUOTE at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-quote))

(defun ar-separate-alnum-in-abbrev-atpt ()
  "Employ actions of SEPARATE at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-separate))

(defun ar-show-alnum-in-abbrev-atpt ()
  "Employ actions of SHOW at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-show))

(defun ar-singlequote-alnum-in-abbrev-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-singlequote))

(defun ar-slash-alnum-in-abbrev-atpt ()
  "Employ actions of SLASH at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-slash))

(defun ar-slashparen-alnum-in-abbrev-atpt ()
  "Employ actions of SLASHPAREN at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-slashparen))

(defun ar-sort-alnum-in-abbrev-atpt ()
  "Employ actions of SORT at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-sort))

(defun ar-trim-alnum-in-abbrev-atpt ()
  "Employ actions of TRIM at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-trim))

(defun ar-trim-left-alnum-in-abbrev-atpt ()
  "Employ actions of TRIM-LEFT at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-trim-left))

(defun ar-trim-right-alnum-in-abbrev-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-trim-right))

(defun ar-underscore-alnum-in-abbrev-atpt ()
  "Employ actions of UNDERSCORE at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-underscore))

(defun ar-whitespace-alnum-in-abbrev-atpt ()
  "Employ actions of WHITESPACE at things class of ALNUM residing withing ABBREV. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'abbrev 'ar-th-whitespace))

(defun ar-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th))

(defun ar-greaterangle-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-greaterangle))

(defun ar-lesserangle-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-lesserangle))

(defun ar-backslash-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-backslash))

(defun ar-beg-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-beg))

(defun ar-blok-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-blok))

(defun ar-bounds-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-bounds))

(defun ar-brace-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-brace))

(defun ar-bracket-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-bracket))

(defun ar-commatize-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-commatize))

(defun ar-comment-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-comment))

(defun ar-dollar-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-dollar))

(defun ar-double-backslash-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-double-backslash))

(defun ar-doublequote-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-doublequote))

(defun ar-doubleslash-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-doubleslash))

(defun ar-doubleslash-paren-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-doublebackslash-paren))

(defun ar-end-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-end))

(defun ar-escape-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-escape))

(defun ar-hide-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-hide))

(defun ar-hide-show-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-hide-show))

(defun ar-hyphen-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-hyphen))

(defun ar-kill-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-kill))

(defun ar-left-right-singlequote-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-left-right-singlequote))

(defun ar-length-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-length))

(defun ar-parentize-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-parentize))

(defun ar-quote-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-quote))

(defun ar-separate-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-separate))

(defun ar-show-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-show))

(defun ar-singlequote-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-singlequote))

(defun ar-slash-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-slash))

(defun ar-slashparen-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-slashparen))

(defun ar-sort-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-sort))

(defun ar-trim-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-trim))

(defun ar-trim-left-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-trim-left))

(defun ar-trim-right-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-trim-right))

(defun ar-underscore-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-underscore))

(defun ar-whitespace-xdigit-in-angled-no-nest-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'angled-no-nest 'ar-th-whitespace))

(defun ar-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th))

(defun ar-greaterangle-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-greaterangle))

(defun ar-lesserangle-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-lesserangle))

(defun ar-backslash-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-backslash))

(defun ar-beg-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-beg))

(defun ar-blok-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-blok))

(defun ar-bounds-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-bounds))

(defun ar-brace-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-brace))

(defun ar-bracket-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-bracket))

(defun ar-commatize-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-commatize))

(defun ar-comment-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-comment))

(defun ar-dollar-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-dollar))

(defun ar-double-backslash-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-double-backslash))

(defun ar-doublequote-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-doublequote))

(defun ar-doubleslash-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-doubleslash))

(defun ar-doubleslash-paren-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-doublebackslash-paren))

(defun ar-end-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-end))

(defun ar-escape-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-escape))

(defun ar-hide-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-hide))

(defun ar-hide-show-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-hide-show))

(defun ar-hyphen-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-hyphen))

(defun ar-kill-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-kill))

(defun ar-left-right-singlequote-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-left-right-singlequote))

(defun ar-length-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-length))

(defun ar-parentize-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-parentize))

(defun ar-quote-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-quote))

(defun ar-separate-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-separate))

(defun ar-show-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-show))

(defun ar-singlequote-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-singlequote))

(defun ar-slash-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-slash))

(defun ar-slashparen-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-slashparen))

(defun ar-sort-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-sort))

(defun ar-trim-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-trim))

(defun ar-trim-left-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-trim-left))

(defun ar-trim-right-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-trim-right))

(defun ar-underscore-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-underscore))

(defun ar-whitespace-xdigit-in-greaterangled-nested-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled-nested 'ar-th-whitespace))

(defun ar-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th))

(defun ar-greaterangle-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-greaterangle))

(defun ar-lesserangle-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-lesserangle))

(defun ar-backslash-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-backslash))

(defun ar-beg-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-beg))

(defun ar-blok-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-blok))

(defun ar-bounds-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-bounds))

(defun ar-brace-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-brace))

(defun ar-bracket-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-bracket))

(defun ar-commatize-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-commatize))

(defun ar-comment-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-comment))

(defun ar-dollar-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-dollar))

(defun ar-double-backslash-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-double-backslash))

(defun ar-doublequote-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-doublequote))

(defun ar-doubleslash-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-doubleslash))

(defun ar-doubleslash-paren-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-doublebackslash-paren))

(defun ar-end-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-end))

(defun ar-escape-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-escape))

(defun ar-hide-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-hide))

(defun ar-hide-show-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-hide-show))

(defun ar-hyphen-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-hyphen))

(defun ar-kill-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-kill))

(defun ar-left-right-singlequote-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-left-right-singlequote))

(defun ar-length-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-length))

(defun ar-parentize-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-parentize))

(defun ar-quote-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-quote))

(defun ar-separate-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-separate))

(defun ar-show-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-show))

(defun ar-singlequote-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-singlequote))

(defun ar-slash-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-slash))

(defun ar-slashparen-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-slashparen))

(defun ar-sort-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-sort))

(defun ar-trim-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-trim))

(defun ar-trim-left-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-trim-left))

(defun ar-trim-right-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-trim-right))

(defun ar-underscore-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-underscore))

(defun ar-whitespace-xdigit-in-lesserangled-nested-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled-nested 'ar-th-whitespace))

(defun ar-xdigit-in-buffer-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th))

(defun ar-greaterangle-xdigit-in-buffer-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-greaterangle))

(defun ar-lesserangle-xdigit-in-buffer-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-lesserangle))

(defun ar-backslash-xdigit-in-buffer-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-backslash))

(defun ar-beg-xdigit-in-buffer-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-beg))

(defun ar-blok-xdigit-in-buffer-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-blok))

(defun ar-bounds-xdigit-in-buffer-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-bounds))

(defun ar-brace-xdigit-in-buffer-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-brace))

(defun ar-bracket-xdigit-in-buffer-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-bracket))

(defun ar-commatize-xdigit-in-buffer-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-commatize))

(defun ar-comment-xdigit-in-buffer-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-comment))

(defun ar-dollar-xdigit-in-buffer-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-dollar))

(defun ar-double-backslash-xdigit-in-buffer-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-double-backslash))

(defun ar-doublequote-xdigit-in-buffer-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-doublequote))

(defun ar-doubleslash-xdigit-in-buffer-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-doubleslash))

(defun ar-doubleslash-paren-xdigit-in-buffer-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-xdigit-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-doublebackslash-paren))

(defun ar-end-xdigit-in-buffer-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-end))

(defun ar-escape-xdigit-in-buffer-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-escape))

(defun ar-hide-xdigit-in-buffer-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-hide))

(defun ar-hide-show-xdigit-in-buffer-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-hide-show))

(defun ar-hyphen-xdigit-in-buffer-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-hyphen))

(defun ar-kill-xdigit-in-buffer-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-kill))

(defun ar-left-right-singlequote-xdigit-in-buffer-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-left-right-singlequote))

(defun ar-length-xdigit-in-buffer-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-length))

(defun ar-parentize-xdigit-in-buffer-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-parentize))

(defun ar-quote-xdigit-in-buffer-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-quote))

(defun ar-separate-xdigit-in-buffer-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-separate))

(defun ar-show-xdigit-in-buffer-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-show))

(defun ar-singlequote-xdigit-in-buffer-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-singlequote))

(defun ar-slash-xdigit-in-buffer-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-slash))

(defun ar-slashparen-xdigit-in-buffer-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-slashparen))

(defun ar-sort-xdigit-in-buffer-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-sort))

(defun ar-trim-xdigit-in-buffer-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-trim))

(defun ar-trim-left-xdigit-in-buffer-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-trim-left))

(defun ar-trim-right-xdigit-in-buffer-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-trim-right))

(defun ar-underscore-xdigit-in-buffer-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-underscore))

(defun ar-whitespace-xdigit-in-buffer-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'buffer 'ar-th-whitespace))

(defun ar-xdigit-in-comment-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th))

(defun ar-greaterangle-xdigit-in-comment-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-greaterangle))

(defun ar-lesserangle-xdigit-in-comment-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-lesserangle))

(defun ar-backslash-xdigit-in-comment-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-backslash))

(defun ar-beg-xdigit-in-comment-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-beg))

(defun ar-blok-xdigit-in-comment-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-blok))

(defun ar-bounds-xdigit-in-comment-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-bounds))

(defun ar-brace-xdigit-in-comment-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-brace))

(defun ar-bracket-xdigit-in-comment-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-bracket))

(defun ar-commatize-xdigit-in-comment-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-commatize))

(defun ar-comment-xdigit-in-comment-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-comment))

(defun ar-dollar-xdigit-in-comment-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-dollar))

(defun ar-double-backslash-xdigit-in-comment-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-double-backslash))

(defun ar-doublequote-xdigit-in-comment-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-doublequote))

(defun ar-doubleslash-xdigit-in-comment-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-doubleslash))

(defun ar-doubleslash-paren-xdigit-in-comment-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-xdigit-in-comment-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-doublebackslash-paren))

(defun ar-end-xdigit-in-comment-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-end))

(defun ar-escape-xdigit-in-comment-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-escape))

(defun ar-hide-xdigit-in-comment-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-hide))

(defun ar-hide-show-xdigit-in-comment-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-hide-show))

(defun ar-hyphen-xdigit-in-comment-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-hyphen))

(defun ar-kill-xdigit-in-comment-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-kill))

(defun ar-left-right-singlequote-xdigit-in-comment-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-left-right-singlequote))

(defun ar-length-xdigit-in-comment-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-length))

(defun ar-parentize-xdigit-in-comment-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-parentize))

(defun ar-quote-xdigit-in-comment-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-quote))

(defun ar-separate-xdigit-in-comment-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-separate))

(defun ar-show-xdigit-in-comment-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-show))

(defun ar-singlequote-xdigit-in-comment-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-singlequote))

(defun ar-slash-xdigit-in-comment-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-slash))

(defun ar-slashparen-xdigit-in-comment-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-slashparen))

(defun ar-sort-xdigit-in-comment-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-sort))

(defun ar-trim-xdigit-in-comment-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-trim))

(defun ar-trim-left-xdigit-in-comment-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-trim-left))

(defun ar-trim-right-xdigit-in-comment-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-trim-right))

(defun ar-underscore-xdigit-in-comment-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-underscore))

(defun ar-whitespace-xdigit-in-comment-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing COMMENT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'comment 'ar-th-whitespace))

(defun ar-xdigit-in-csv-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th))

(defun ar-greaterangle-xdigit-in-csv-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-greaterangle))

(defun ar-lesserangle-xdigit-in-csv-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-lesserangle))

(defun ar-backslash-xdigit-in-csv-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-backslash))

(defun ar-beg-xdigit-in-csv-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-beg))

(defun ar-blok-xdigit-in-csv-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-blok))

(defun ar-bounds-xdigit-in-csv-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-bounds))

(defun ar-brace-xdigit-in-csv-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-brace))

(defun ar-bracket-xdigit-in-csv-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-bracket))

(defun ar-commatize-xdigit-in-csv-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-commatize))

(defun ar-comment-xdigit-in-csv-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-comment))

(defun ar-dollar-xdigit-in-csv-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-dollar))

(defun ar-double-backslash-xdigit-in-csv-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-double-backslash))

(defun ar-doublequote-xdigit-in-csv-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-doublequote))

(defun ar-doubleslash-xdigit-in-csv-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-doubleslash))

(defun ar-doubleslash-paren-xdigit-in-csv-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-xdigit-in-csv-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-doublebackslash-paren))

(defun ar-end-xdigit-in-csv-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-end))

(defun ar-escape-xdigit-in-csv-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-escape))

(defun ar-hide-xdigit-in-csv-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-hide))

(defun ar-hide-show-xdigit-in-csv-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-hide-show))

(defun ar-hyphen-xdigit-in-csv-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-hyphen))

(defun ar-kill-xdigit-in-csv-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-kill))

(defun ar-left-right-singlequote-xdigit-in-csv-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-left-right-singlequote))

(defun ar-length-xdigit-in-csv-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-length))

(defun ar-parentize-xdigit-in-csv-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-parentize))

(defun ar-quote-xdigit-in-csv-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-quote))

(defun ar-separate-xdigit-in-csv-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-separate))

(defun ar-show-xdigit-in-csv-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-show))

(defun ar-singlequote-xdigit-in-csv-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-singlequote))

(defun ar-slash-xdigit-in-csv-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-slash))

(defun ar-slashparen-xdigit-in-csv-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-slashparen))

(defun ar-sort-xdigit-in-csv-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-sort))

(defun ar-trim-xdigit-in-csv-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-trim))

(defun ar-trim-left-xdigit-in-csv-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-trim-left))

(defun ar-trim-right-xdigit-in-csv-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-trim-right))

(defun ar-underscore-xdigit-in-csv-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-underscore))

(defun ar-whitespace-xdigit-in-csv-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'csv 'ar-th-whitespace))

(defun ar-xdigit-in-delimited-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th))

(defun ar-greaterangle-xdigit-in-delimited-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-greaterangle))

(defun ar-lesserangle-xdigit-in-delimited-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-lesserangle))

(defun ar-backslash-xdigit-in-delimited-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-backslash))

(defun ar-beg-xdigit-in-delimited-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-beg))

(defun ar-blok-xdigit-in-delimited-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-blok))

(defun ar-bounds-xdigit-in-delimited-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-bounds))

(defun ar-brace-xdigit-in-delimited-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-brace))

(defun ar-bracket-xdigit-in-delimited-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-bracket))

(defun ar-commatize-xdigit-in-delimited-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-commatize))

(defun ar-comment-xdigit-in-delimited-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-comment))

(defun ar-dollar-xdigit-in-delimited-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-dollar))

(defun ar-double-backslash-xdigit-in-delimited-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-double-backslash))

(defun ar-doublequote-xdigit-in-delimited-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-doublequote))

(defun ar-doubleslash-xdigit-in-delimited-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-doubleslash))

(defun ar-doubleslash-paren-xdigit-in-delimited-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-xdigit-in-delimited-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-doublebackslash-paren))

(defun ar-end-xdigit-in-delimited-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-end))

(defun ar-escape-xdigit-in-delimited-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-escape))

(defun ar-hide-xdigit-in-delimited-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-hide))

(defun ar-hide-show-xdigit-in-delimited-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-hide-show))

(defun ar-hyphen-xdigit-in-delimited-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-hyphen))

(defun ar-kill-xdigit-in-delimited-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-kill))

(defun ar-left-right-singlequote-xdigit-in-delimited-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-left-right-singlequote))

(defun ar-length-xdigit-in-delimited-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-length))

(defun ar-parentize-xdigit-in-delimited-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-parentize))

(defun ar-quote-xdigit-in-delimited-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-quote))

(defun ar-separate-xdigit-in-delimited-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-separate))

(defun ar-show-xdigit-in-delimited-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-show))

(defun ar-singlequote-xdigit-in-delimited-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-singlequote))

(defun ar-slash-xdigit-in-delimited-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-slash))

(defun ar-slashparen-xdigit-in-delimited-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-slashparen))

(defun ar-sort-xdigit-in-delimited-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-sort))

(defun ar-trim-xdigit-in-delimited-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-trim))

(defun ar-trim-left-xdigit-in-delimited-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-trim-left))

(defun ar-trim-right-xdigit-in-delimited-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-trim-right))

(defun ar-underscore-xdigit-in-delimited-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-underscore))

(defun ar-whitespace-xdigit-in-delimited-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing DELIMITED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'delimited 'ar-th-whitespace))

(defun ar-xdigit-in-function-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th))

(defun ar-greaterangle-xdigit-in-function-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-greaterangle))

(defun ar-lesserangle-xdigit-in-function-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-lesserangle))

(defun ar-backslash-xdigit-in-function-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-backslash))

(defun ar-beg-xdigit-in-function-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-beg))

(defun ar-blok-xdigit-in-function-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-blok))

(defun ar-bounds-xdigit-in-function-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-bounds))

(defun ar-brace-xdigit-in-function-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-brace))

(defun ar-bracket-xdigit-in-function-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-bracket))

(defun ar-commatize-xdigit-in-function-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-commatize))

(defun ar-comment-xdigit-in-function-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-comment))

(defun ar-dollar-xdigit-in-function-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-dollar))

(defun ar-double-backslash-xdigit-in-function-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-double-backslash))

(defun ar-doublequote-xdigit-in-function-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-doublequote))

(defun ar-doubleslash-xdigit-in-function-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-doubleslash))

(defun ar-doubleslash-paren-xdigit-in-function-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-xdigit-in-function-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-doublebackslash-paren))

(defun ar-end-xdigit-in-function-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-end))

(defun ar-escape-xdigit-in-function-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-escape))

(defun ar-hide-xdigit-in-function-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-hide))

(defun ar-hide-show-xdigit-in-function-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-hide-show))

(defun ar-hyphen-xdigit-in-function-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-hyphen))

(defun ar-kill-xdigit-in-function-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-kill))

(defun ar-left-right-singlequote-xdigit-in-function-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-left-right-singlequote))

(defun ar-length-xdigit-in-function-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-length))

(defun ar-parentize-xdigit-in-function-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-parentize))

(defun ar-quote-xdigit-in-function-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-quote))

(defun ar-separate-xdigit-in-function-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-separate))

(defun ar-show-xdigit-in-function-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-show))

(defun ar-singlequote-xdigit-in-function-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-singlequote))

(defun ar-slash-xdigit-in-function-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-slash))

(defun ar-slashparen-xdigit-in-function-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-slashparen))

(defun ar-sort-xdigit-in-function-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-sort))

(defun ar-trim-xdigit-in-function-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-trim))

(defun ar-trim-left-xdigit-in-function-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-trim-left))

(defun ar-trim-right-xdigit-in-function-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-trim-right))

(defun ar-underscore-xdigit-in-function-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-underscore))

(defun ar-whitespace-xdigit-in-function-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing FUNCTION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'function 'ar-th-whitespace))

(defun ar-xdigit-in-line-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th))

(defun ar-greaterangle-xdigit-in-line-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-greaterangle))

(defun ar-lesserangle-xdigit-in-line-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-lesserangle))

(defun ar-backslash-xdigit-in-line-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-backslash))

(defun ar-beg-xdigit-in-line-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-beg))

(defun ar-blok-xdigit-in-line-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-blok))

(defun ar-bounds-xdigit-in-line-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-bounds))

(defun ar-brace-xdigit-in-line-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-brace))

(defun ar-bracket-xdigit-in-line-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-bracket))

(defun ar-commatize-xdigit-in-line-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-commatize))

(defun ar-comment-xdigit-in-line-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-comment))

(defun ar-dollar-xdigit-in-line-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-dollar))

(defun ar-double-backslash-xdigit-in-line-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-double-backslash))

(defun ar-doublequote-xdigit-in-line-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-doublequote))

(defun ar-doubleslash-xdigit-in-line-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-doubleslash))

(defun ar-doubleslash-paren-xdigit-in-line-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-xdigit-in-line-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-doublebackslash-paren))

(defun ar-end-xdigit-in-line-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-end))

(defun ar-escape-xdigit-in-line-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-escape))

(defun ar-hide-xdigit-in-line-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-hide))

(defun ar-hide-show-xdigit-in-line-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-hide-show))

(defun ar-hyphen-xdigit-in-line-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-hyphen))

(defun ar-kill-xdigit-in-line-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-kill))

(defun ar-left-right-singlequote-xdigit-in-line-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-left-right-singlequote))

(defun ar-length-xdigit-in-line-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-length))

(defun ar-parentize-xdigit-in-line-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-parentize))

(defun ar-quote-xdigit-in-line-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-quote))

(defun ar-separate-xdigit-in-line-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-separate))

(defun ar-show-xdigit-in-line-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-show))

(defun ar-singlequote-xdigit-in-line-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-singlequote))

(defun ar-slash-xdigit-in-line-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-slash))

(defun ar-slashparen-xdigit-in-line-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-slashparen))

(defun ar-sort-xdigit-in-line-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-sort))

(defun ar-trim-xdigit-in-line-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-trim))

(defun ar-trim-left-xdigit-in-line-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-trim-left))

(defun ar-trim-right-xdigit-in-line-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-trim-right))

(defun ar-underscore-xdigit-in-line-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-underscore))

(defun ar-whitespace-xdigit-in-line-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'line 'ar-th-whitespace))

(defun ar-xdigit-in-name-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th))

(defun ar-greaterangle-xdigit-in-name-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-greaterangle))

(defun ar-lesserangle-xdigit-in-name-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-lesserangle))

(defun ar-backslash-xdigit-in-name-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-backslash))

(defun ar-beg-xdigit-in-name-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-beg))

(defun ar-blok-xdigit-in-name-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-blok))

(defun ar-bounds-xdigit-in-name-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-bounds))

(defun ar-brace-xdigit-in-name-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-brace))

(defun ar-bracket-xdigit-in-name-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-bracket))

(defun ar-commatize-xdigit-in-name-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-commatize))

(defun ar-comment-xdigit-in-name-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-comment))

(defun ar-dollar-xdigit-in-name-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-dollar))

(defun ar-double-backslash-xdigit-in-name-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-double-backslash))

(defun ar-doublequote-xdigit-in-name-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-doublequote))

(defun ar-doubleslash-xdigit-in-name-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-doubleslash))

(defun ar-doubleslash-paren-xdigit-in-name-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-xdigit-in-name-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-doublebackslash-paren))

(defun ar-end-xdigit-in-name-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-end))

(defun ar-escape-xdigit-in-name-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-escape))

(defun ar-hide-xdigit-in-name-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-hide))

(defun ar-hide-show-xdigit-in-name-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-hide-show))

(defun ar-hyphen-xdigit-in-name-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-hyphen))

(defun ar-kill-xdigit-in-name-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-kill))

(defun ar-left-right-singlequote-xdigit-in-name-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-left-right-singlequote))

(defun ar-length-xdigit-in-name-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-length))

(defun ar-parentize-xdigit-in-name-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-parentize))

(defun ar-quote-xdigit-in-name-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-quote))

(defun ar-separate-xdigit-in-name-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-separate))

(defun ar-show-xdigit-in-name-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-show))

(defun ar-singlequote-xdigit-in-name-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-singlequote))

(defun ar-slash-xdigit-in-name-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-slash))

(defun ar-slashparen-xdigit-in-name-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-slashparen))

(defun ar-sort-xdigit-in-name-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-sort))

(defun ar-trim-xdigit-in-name-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-trim))

(defun ar-trim-left-xdigit-in-name-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-trim-left))

(defun ar-trim-right-xdigit-in-name-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-trim-right))

(defun ar-underscore-xdigit-in-name-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-underscore))

(defun ar-whitespace-xdigit-in-name-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing NAME. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'name 'ar-th-whitespace))

(defun ar-xdigit-in-page-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th))

(defun ar-greaterangle-xdigit-in-page-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-greaterangle))

(defun ar-lesserangle-xdigit-in-page-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-lesserangle))

(defun ar-backslash-xdigit-in-page-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-backslash))

(defun ar-beg-xdigit-in-page-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-beg))

(defun ar-blok-xdigit-in-page-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-blok))

(defun ar-bounds-xdigit-in-page-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-bounds))

(defun ar-brace-xdigit-in-page-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-brace))

(defun ar-bracket-xdigit-in-page-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-bracket))

(defun ar-commatize-xdigit-in-page-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-commatize))

(defun ar-comment-xdigit-in-page-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-comment))

(defun ar-dollar-xdigit-in-page-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-dollar))

(defun ar-double-backslash-xdigit-in-page-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-double-backslash))

(defun ar-doublequote-xdigit-in-page-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-doublequote))

(defun ar-doubleslash-xdigit-in-page-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-doubleslash))

(defun ar-doubleslash-paren-xdigit-in-page-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-xdigit-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-doublebackslash-paren))

(defun ar-end-xdigit-in-page-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-end))

(defun ar-escape-xdigit-in-page-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-escape))

(defun ar-hide-xdigit-in-page-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-hide))

(defun ar-hide-show-xdigit-in-page-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-hide-show))

(defun ar-hyphen-xdigit-in-page-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-hyphen))

(defun ar-kill-xdigit-in-page-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-kill))

(defun ar-left-right-singlequote-xdigit-in-page-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-left-right-singlequote))

(defun ar-length-xdigit-in-page-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-length))

(defun ar-parentize-xdigit-in-page-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-parentize))

(defun ar-quote-xdigit-in-page-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-quote))

(defun ar-separate-xdigit-in-page-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-separate))

(defun ar-show-xdigit-in-page-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-show))

(defun ar-singlequote-xdigit-in-page-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-singlequote))

(defun ar-slash-xdigit-in-page-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-slash))

(defun ar-slashparen-xdigit-in-page-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-slashparen))

(defun ar-sort-xdigit-in-page-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-sort))

(defun ar-trim-xdigit-in-page-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-trim))

(defun ar-trim-left-xdigit-in-page-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-trim-left))

(defun ar-trim-right-xdigit-in-page-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-trim-right))

(defun ar-underscore-xdigit-in-page-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-underscore))

(defun ar-whitespace-xdigit-in-page-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'page 'ar-th-whitespace))

(defun ar-xdigit-in-paragraph-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th))

(defun ar-greaterangle-xdigit-in-paragraph-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-greaterangle))

(defun ar-lesserangle-xdigit-in-paragraph-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-lesserangle))

(defun ar-backslash-xdigit-in-paragraph-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-backslash))

(defun ar-beg-xdigit-in-paragraph-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-beg))

(defun ar-blok-xdigit-in-paragraph-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-blok))

(defun ar-bounds-xdigit-in-paragraph-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-bounds))

(defun ar-brace-xdigit-in-paragraph-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-brace))

(defun ar-bracket-xdigit-in-paragraph-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-bracket))

(defun ar-commatize-xdigit-in-paragraph-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-commatize))

(defun ar-comment-xdigit-in-paragraph-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-comment))

(defun ar-dollar-xdigit-in-paragraph-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-dollar))

(defun ar-double-backslash-xdigit-in-paragraph-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-double-backslash))

(defun ar-doublequote-xdigit-in-paragraph-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-doublequote))

(defun ar-doubleslash-xdigit-in-paragraph-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-doubleslash))

(defun ar-doubleslash-paren-xdigit-in-paragraph-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-xdigit-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-doublebackslash-paren))

(defun ar-end-xdigit-in-paragraph-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-end))

(defun ar-escape-xdigit-in-paragraph-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-escape))

(defun ar-hide-xdigit-in-paragraph-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-hide))

(defun ar-hide-show-xdigit-in-paragraph-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-hide-show))

(defun ar-hyphen-xdigit-in-paragraph-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-hyphen))

(defun ar-kill-xdigit-in-paragraph-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-kill))

(defun ar-left-right-singlequote-xdigit-in-paragraph-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-left-right-singlequote))

(defun ar-length-xdigit-in-paragraph-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-length))

(defun ar-parentize-xdigit-in-paragraph-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-parentize))

(defun ar-quote-xdigit-in-paragraph-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-quote))

(defun ar-separate-xdigit-in-paragraph-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-separate))

(defun ar-show-xdigit-in-paragraph-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-show))

(defun ar-singlequote-xdigit-in-paragraph-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-singlequote))

(defun ar-slash-xdigit-in-paragraph-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-slash))

(defun ar-slashparen-xdigit-in-paragraph-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-slashparen))

(defun ar-sort-xdigit-in-paragraph-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-sort))

(defun ar-trim-xdigit-in-paragraph-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-trim))

(defun ar-trim-left-xdigit-in-paragraph-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-trim-left))

(defun ar-trim-right-xdigit-in-paragraph-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-trim-right))

(defun ar-underscore-xdigit-in-paragraph-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-underscore))

(defun ar-whitespace-xdigit-in-paragraph-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paragraph 'ar-th-whitespace))

(defun ar-xdigit-in-paren-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th))

(defun ar-greaterangle-xdigit-in-paren-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-greaterangle))

(defun ar-lesserangle-xdigit-in-paren-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-lesserangle))

(defun ar-backslash-xdigit-in-paren-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-backslash))

(defun ar-beg-xdigit-in-paren-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-beg))

(defun ar-blok-xdigit-in-paren-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-blok))

(defun ar-bounds-xdigit-in-paren-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-bounds))

(defun ar-brace-xdigit-in-paren-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-brace))

(defun ar-bracket-xdigit-in-paren-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-bracket))

(defun ar-commatize-xdigit-in-paren-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-commatize))

(defun ar-comment-xdigit-in-paren-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-comment))

(defun ar-dollar-xdigit-in-paren-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-dollar))

(defun ar-double-backslash-xdigit-in-paren-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-double-backslash))

(defun ar-doublequote-xdigit-in-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-doublequote))

(defun ar-doubleslash-xdigit-in-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-doubleslash))

(defun ar-doubleslash-paren-xdigit-in-paren-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-xdigit-in-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-doublebackslash-paren))

(defun ar-end-xdigit-in-paren-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-end))

(defun ar-escape-xdigit-in-paren-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-escape))

(defun ar-hide-xdigit-in-paren-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-hide))

(defun ar-hide-show-xdigit-in-paren-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-hide-show))

(defun ar-hyphen-xdigit-in-paren-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-hyphen))

(defun ar-kill-xdigit-in-paren-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-kill))

(defun ar-left-right-singlequote-xdigit-in-paren-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-left-right-singlequote))

(defun ar-length-xdigit-in-paren-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-length))

(defun ar-parentize-xdigit-in-paren-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-parentize))

(defun ar-quote-xdigit-in-paren-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-quote))

(defun ar-separate-xdigit-in-paren-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-separate))

(defun ar-show-xdigit-in-paren-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-show))

(defun ar-singlequote-xdigit-in-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-singlequote))

(defun ar-slash-xdigit-in-paren-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-slash))

(defun ar-slashparen-xdigit-in-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-slashparen))

(defun ar-sort-xdigit-in-paren-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-sort))

(defun ar-trim-xdigit-in-paren-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-trim))

(defun ar-trim-left-xdigit-in-paren-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-trim-left))

(defun ar-trim-right-xdigit-in-paren-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-trim-right))

(defun ar-underscore-xdigit-in-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-underscore))

(defun ar-whitespace-xdigit-in-paren-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'paren 'ar-th-whitespace))

(defun ar-xdigit-in-region-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th))

(defun ar-greaterangle-xdigit-in-region-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-greaterangle))

(defun ar-lesserangle-xdigit-in-region-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-lesserangle))

(defun ar-backslash-xdigit-in-region-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-backslash))

(defun ar-beg-xdigit-in-region-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-beg))

(defun ar-blok-xdigit-in-region-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-blok))

(defun ar-bounds-xdigit-in-region-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-bounds))

(defun ar-brace-xdigit-in-region-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-brace))

(defun ar-bracket-xdigit-in-region-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-bracket))

(defun ar-commatize-xdigit-in-region-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-commatize))

(defun ar-comment-xdigit-in-region-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-comment))

(defun ar-dollar-xdigit-in-region-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-dollar))

(defun ar-double-backslash-xdigit-in-region-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-double-backslash))

(defun ar-doublequote-xdigit-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-doublequote))

(defun ar-doubleslash-xdigit-in-region-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-doubleslash))

(defun ar-doubleslash-paren-xdigit-in-region-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-xdigit-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-doublebackslash-paren))

(defun ar-end-xdigit-in-region-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-end))

(defun ar-escape-xdigit-in-region-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-escape))

(defun ar-hide-xdigit-in-region-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-hide))

(defun ar-hide-show-xdigit-in-region-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-hide-show))

(defun ar-hyphen-xdigit-in-region-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-hyphen))

(defun ar-kill-xdigit-in-region-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-kill))

(defun ar-left-right-singlequote-xdigit-in-region-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-left-right-singlequote))

(defun ar-length-xdigit-in-region-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-length))

(defun ar-parentize-xdigit-in-region-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-parentize))

(defun ar-quote-xdigit-in-region-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-quote))

(defun ar-separate-xdigit-in-region-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-separate))

(defun ar-show-xdigit-in-region-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-show))

(defun ar-singlequote-xdigit-in-region-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-singlequote))

(defun ar-slash-xdigit-in-region-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-slash))

(defun ar-slashparen-xdigit-in-region-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-slashparen))

(defun ar-sort-xdigit-in-region-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-sort))

(defun ar-trim-xdigit-in-region-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-trim))

(defun ar-trim-left-xdigit-in-region-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-trim-left))

(defun ar-trim-right-xdigit-in-region-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-trim-right))

(defun ar-underscore-xdigit-in-region-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-underscore))

(defun ar-whitespace-xdigit-in-region-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'region 'ar-th-whitespace))

(defun ar-xdigit-in-sentence-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th))

(defun ar-greaterangle-xdigit-in-sentence-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-greaterangle))

(defun ar-lesserangle-xdigit-in-sentence-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-lesserangle))

(defun ar-backslash-xdigit-in-sentence-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-backslash))

(defun ar-beg-xdigit-in-sentence-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-beg))

(defun ar-blok-xdigit-in-sentence-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-blok))

(defun ar-bounds-xdigit-in-sentence-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-bounds))

(defun ar-brace-xdigit-in-sentence-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-brace))

(defun ar-bracket-xdigit-in-sentence-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-bracket))

(defun ar-commatize-xdigit-in-sentence-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-commatize))

(defun ar-comment-xdigit-in-sentence-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-comment))

(defun ar-dollar-xdigit-in-sentence-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-dollar))

(defun ar-double-backslash-xdigit-in-sentence-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-double-backslash))

(defun ar-doublequote-xdigit-in-sentence-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-doublequote))

(defun ar-doubleslash-xdigit-in-sentence-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-doubleslash))

(defun ar-doubleslash-paren-xdigit-in-sentence-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-xdigit-in-sentence-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-doublebackslash-paren))

(defun ar-end-xdigit-in-sentence-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-end))

(defun ar-escape-xdigit-in-sentence-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-escape))

(defun ar-hide-xdigit-in-sentence-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-hide))

(defun ar-hide-show-xdigit-in-sentence-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-hide-show))

(defun ar-hyphen-xdigit-in-sentence-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-hyphen))

(defun ar-kill-xdigit-in-sentence-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-kill))

(defun ar-left-right-singlequote-xdigit-in-sentence-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-left-right-singlequote))

(defun ar-length-xdigit-in-sentence-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-length))

(defun ar-parentize-xdigit-in-sentence-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-parentize))

(defun ar-quote-xdigit-in-sentence-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-quote))

(defun ar-separate-xdigit-in-sentence-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-separate))

(defun ar-show-xdigit-in-sentence-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-show))

(defun ar-singlequote-xdigit-in-sentence-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-singlequote))

(defun ar-slash-xdigit-in-sentence-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-slash))

(defun ar-slashparen-xdigit-in-sentence-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-slashparen))

(defun ar-sort-xdigit-in-sentence-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-sort))

(defun ar-trim-xdigit-in-sentence-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-trim))

(defun ar-trim-left-xdigit-in-sentence-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-trim-left))

(defun ar-trim-right-xdigit-in-sentence-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-trim-right))

(defun ar-underscore-xdigit-in-sentence-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-underscore))

(defun ar-whitespace-xdigit-in-sentence-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sentence 'ar-th-whitespace))

(defun ar-xdigit-in-sexp-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th))

(defun ar-greaterangle-xdigit-in-sexp-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-greaterangle))

(defun ar-lesserangle-xdigit-in-sexp-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-lesserangle))

(defun ar-backslash-xdigit-in-sexp-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-backslash))

(defun ar-beg-xdigit-in-sexp-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-beg))

(defun ar-blok-xdigit-in-sexp-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-blok))

(defun ar-bounds-xdigit-in-sexp-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-bounds))

(defun ar-brace-xdigit-in-sexp-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-brace))

(defun ar-bracket-xdigit-in-sexp-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-bracket))

(defun ar-commatize-xdigit-in-sexp-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-commatize))

(defun ar-comment-xdigit-in-sexp-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-comment))

(defun ar-dollar-xdigit-in-sexp-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-dollar))

(defun ar-double-backslash-xdigit-in-sexp-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-double-backslash))

(defun ar-doublequote-xdigit-in-sexp-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-doublequote))

(defun ar-doubleslash-xdigit-in-sexp-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-doubleslash))

(defun ar-doubleslash-paren-xdigit-in-sexp-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-xdigit-in-sexp-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-doublebackslash-paren))

(defun ar-end-xdigit-in-sexp-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-end))

(defun ar-escape-xdigit-in-sexp-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-escape))

(defun ar-hide-xdigit-in-sexp-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-hide))

(defun ar-hide-show-xdigit-in-sexp-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-hide-show))

(defun ar-hyphen-xdigit-in-sexp-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-hyphen))

(defun ar-kill-xdigit-in-sexp-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-kill))

(defun ar-left-right-singlequote-xdigit-in-sexp-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-left-right-singlequote))

(defun ar-length-xdigit-in-sexp-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-length))

(defun ar-parentize-xdigit-in-sexp-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-parentize))

(defun ar-quote-xdigit-in-sexp-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-quote))

(defun ar-separate-xdigit-in-sexp-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-separate))

(defun ar-show-xdigit-in-sexp-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-show))

(defun ar-singlequote-xdigit-in-sexp-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-singlequote))

(defun ar-slash-xdigit-in-sexp-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-slash))

(defun ar-slashparen-xdigit-in-sexp-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-slashparen))

(defun ar-sort-xdigit-in-sexp-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-sort))

(defun ar-trim-xdigit-in-sexp-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-trim))

(defun ar-trim-left-xdigit-in-sexp-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-trim-left))

(defun ar-trim-right-xdigit-in-sexp-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-trim-right))

(defun ar-underscore-xdigit-in-sexp-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-underscore))

(defun ar-whitespace-xdigit-in-sexp-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing SEXP. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sexp 'ar-th-whitespace))

(defun ar-xdigit-in-string-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th))

(defun ar-greaterangle-xdigit-in-string-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-greaterangle))

(defun ar-lesserangle-xdigit-in-string-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-lesserangle))

(defun ar-backslash-xdigit-in-string-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-backslash))

(defun ar-beg-xdigit-in-string-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-beg))

(defun ar-blok-xdigit-in-string-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-blok))

(defun ar-bounds-xdigit-in-string-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-bounds))

(defun ar-brace-xdigit-in-string-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-brace))

(defun ar-bracket-xdigit-in-string-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-bracket))

(defun ar-commatize-xdigit-in-string-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-commatize))

(defun ar-comment-xdigit-in-string-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-comment))

(defun ar-dollar-xdigit-in-string-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-dollar))

(defun ar-double-backslash-xdigit-in-string-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-double-backslash))

(defun ar-doublequote-xdigit-in-string-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-doublequote))

(defun ar-doubleslash-xdigit-in-string-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-doubleslash))

(defun ar-doubleslash-paren-xdigit-in-string-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-xdigit-in-string-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-doublebackslash-paren))

(defun ar-end-xdigit-in-string-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-end))

(defun ar-escape-xdigit-in-string-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-escape))

(defun ar-hide-xdigit-in-string-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-hide))

(defun ar-hide-show-xdigit-in-string-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-hide-show))

(defun ar-hyphen-xdigit-in-string-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-hyphen))

(defun ar-kill-xdigit-in-string-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-kill))

(defun ar-left-right-singlequote-xdigit-in-string-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-left-right-singlequote))

(defun ar-length-xdigit-in-string-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-length))

(defun ar-parentize-xdigit-in-string-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-parentize))

(defun ar-quote-xdigit-in-string-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-quote))

(defun ar-separate-xdigit-in-string-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-separate))

(defun ar-show-xdigit-in-string-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-show))

(defun ar-singlequote-xdigit-in-string-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-singlequote))

(defun ar-slash-xdigit-in-string-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-slash))

(defun ar-slashparen-xdigit-in-string-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-slashparen))

(defun ar-sort-xdigit-in-string-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-sort))

(defun ar-trim-xdigit-in-string-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-trim))

(defun ar-trim-left-xdigit-in-string-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-trim-left))

(defun ar-trim-right-xdigit-in-string-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-trim-right))

(defun ar-underscore-xdigit-in-string-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-underscore))

(defun ar-whitespace-xdigit-in-string-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'string 'ar-th-whitespace))

(defun ar-xdigit-in-sh-struct-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th))

(defun ar-greaterangle-xdigit-in-sh-struct-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-greaterangle))

(defun ar-lesserangle-xdigit-in-sh-struct-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-lesserangle))

(defun ar-backslash-xdigit-in-sh-struct-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-backslash))

(defun ar-beg-xdigit-in-sh-struct-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-beg))

(defun ar-blok-xdigit-in-sh-struct-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-blok))

(defun ar-bounds-xdigit-in-sh-struct-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-bounds))

(defun ar-brace-xdigit-in-sh-struct-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-brace))

(defun ar-bracket-xdigit-in-sh-struct-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-bracket))

(defun ar-commatize-xdigit-in-sh-struct-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-commatize))

(defun ar-comment-xdigit-in-sh-struct-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-comment))

(defun ar-dollar-xdigit-in-sh-struct-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-dollar))

(defun ar-double-backslash-xdigit-in-sh-struct-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-double-backslash))

(defun ar-doublequote-xdigit-in-sh-struct-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-doublequote))

(defun ar-doubleslash-xdigit-in-sh-struct-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-doubleslash))

(defun ar-doubleslash-paren-xdigit-in-sh-struct-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-doubleslash-paren))

(defun ar-doublebackslash-paren-xdigit-in-sh-struct-atpt ()
  "Employ actions of DOUBLEBACKSLASH-PAREN at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-doublebackslash-paren))

(defun ar-end-xdigit-in-sh-struct-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-end))

(defun ar-escape-xdigit-in-sh-struct-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-escape))

(defun ar-hide-xdigit-in-sh-struct-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-hide))

(defun ar-hide-show-xdigit-in-sh-struct-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-hide-show))

(defun ar-hyphen-xdigit-in-sh-struct-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-hyphen))

(defun ar-kill-xdigit-in-sh-struct-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-kill))

(defun ar-left-right-singlequote-xdigit-in-sh-struct-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-left-right-singlequote))

(defun ar-length-xdigit-in-sh-struct-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-length))

(defun ar-parentize-xdigit-in-sh-struct-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-parentize))

(defun ar-quote-xdigit-in-sh-struct-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-quote))

(defun ar-separate-xdigit-in-sh-struct-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-separate))

(defun ar-show-xdigit-in-sh-struct-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-show))

(defun ar-singlequote-xdigit-in-sh-struct-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-singlequote))

(defun ar-slash-xdigit-in-sh-struct-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-slash))

(defun ar-slashparen-xdigit-in-sh-struct-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-slashparen))

(defun ar-sort-xdigit-in-sh-struct-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-sort))

(defun ar-trim-xdigit-in-sh-struct-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-trim))

(defun ar-trim-left-xdigit-in-sh-struct-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-trim-left))

(defun ar-trim-right-xdigit-in-sh-struct-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-trim-right))

(defun ar-underscore-xdigit-in-sh-struct-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-underscore))

(defun ar-whitespace-xdigit-in-sh-struct-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing SH-STRUCT. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'sh-struct 'ar-th-whitespace))

(provide 'ar-thingatpt-classes-in-rest-list)
;;;thingatpt-classes-in-rest-list.el ends here
