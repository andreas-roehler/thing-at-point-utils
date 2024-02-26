;;; thing-delimited-list-in-counts-list.el --- thing-in-thing functions

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
;; contents lists at the bottom of thingatpt-utils-core.el which are
;; cross-used in general.

;; Further information is given with thingatpt-utils-core.el

(defun ar-braced-in-angled-no-nest-atpt ()
  "Employ actions of  at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th))

;;;###autoload
(defun ar-greaterangle-braced-in-angled-no-nest-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-braced-in-angled-no-nest-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-braced-in-angled-no-nest-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-braced-in-angled-no-nest-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-backtick))

;;;###autoload
(defun ar-beg-braced-in-angled-no-nest-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-beg))

;;;###autoload
(defun ar-blok-braced-in-angled-no-nest-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-blok))

;;;###autoload
(defun ar-bounds-braced-in-angled-no-nest-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-bounds))

;;;###autoload
(defun ar-brace-braced-in-angled-no-nest-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-brace))

;;;###autoload
(defun ar-bracket-braced-in-angled-no-nest-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-braced-in-angled-no-nest-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-commatize))

;;;###autoload
(defun ar-comment-braced-in-angled-no-nest-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-comment))

;;;###autoload
(defun ar-dollar-braced-in-angled-no-nest-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-braced-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-braced-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-braced-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-braced-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-braced-in-angled-no-nest-atpt ()
  "Employ actions of END at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-end))

;;;###autoload
(defun ar-escape-braced-in-angled-no-nest-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-escape))

;;;###autoload
(defun ar-hide-braced-in-angled-no-nest-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-braced-in-angled-no-nest-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-braced-in-angled-no-nest-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-braced-in-angled-no-nest-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-braced-in-angled-no-nest-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-braced-in-angled-no-nest-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-length))

;;;###autoload
(defun ar-parentize-braced-in-angled-no-nest-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-parentize))

;;;###autoload
(defun ar-quote-braced-in-angled-no-nest-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-quote))

;;;###autoload
(defun ar-separate-braced-in-angled-no-nest-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-separate))

;;;###autoload
(defun ar-show-braced-in-angled-no-nest-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-show))

;;;###autoload
(defun ar-singlequote-braced-in-angled-no-nest-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-braced-in-angled-no-nest-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-braced-in-angled-no-nest-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-braced-in-angled-no-nest-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-sort))

;;;###autoload
(defun ar-trim-braced-in-angled-no-nest-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-braced-in-angled-no-nest-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-braced-in-angled-no-nest-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-braced-in-angled-no-nest-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-braced-in-angled-no-nest-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'angled-no-nest 'ar-th-whitespace))

;;;###autoload
(defun ar-braced-in-greaterangled-nested-atpt ()
  "Employ actions of  at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th))

;;;###autoload
(defun ar-greaterangle-braced-in-greaterangled-nested-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-braced-in-greaterangled-nested-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-braced-in-greaterangled-nested-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-braced-in-greaterangled-nested-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-backtick))

;;;###autoload
(defun ar-beg-braced-in-greaterangled-nested-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-beg))

;;;###autoload
(defun ar-blok-braced-in-greaterangled-nested-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-blok))

;;;###autoload
(defun ar-bounds-braced-in-greaterangled-nested-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-bounds))

;;;###autoload
(defun ar-brace-braced-in-greaterangled-nested-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-brace))

;;;###autoload
(defun ar-bracket-braced-in-greaterangled-nested-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-braced-in-greaterangled-nested-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-commatize))

;;;###autoload
(defun ar-comment-braced-in-greaterangled-nested-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-comment))

;;;###autoload
(defun ar-dollar-braced-in-greaterangled-nested-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-braced-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-braced-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-braced-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-braced-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-braced-in-greaterangled-nested-atpt ()
  "Employ actions of END at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-end))

;;;###autoload
(defun ar-escape-braced-in-greaterangled-nested-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-escape))

;;;###autoload
(defun ar-hide-braced-in-greaterangled-nested-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-braced-in-greaterangled-nested-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-braced-in-greaterangled-nested-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-braced-in-greaterangled-nested-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-braced-in-greaterangled-nested-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-braced-in-greaterangled-nested-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-length))

;;;###autoload
(defun ar-parentize-braced-in-greaterangled-nested-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-parentize))

;;;###autoload
(defun ar-quote-braced-in-greaterangled-nested-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-quote))

;;;###autoload
(defun ar-separate-braced-in-greaterangled-nested-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-separate))

;;;###autoload
(defun ar-show-braced-in-greaterangled-nested-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-show))

;;;###autoload
(defun ar-singlequote-braced-in-greaterangled-nested-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-braced-in-greaterangled-nested-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-braced-in-greaterangled-nested-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-braced-in-greaterangled-nested-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-sort))

;;;###autoload
(defun ar-trim-braced-in-greaterangled-nested-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-braced-in-greaterangled-nested-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-braced-in-greaterangled-nested-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-braced-in-greaterangled-nested-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-braced-in-greaterangled-nested-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled-nested 'ar-th-whitespace))

;;;###autoload
(defun ar-braced-in-lesserangled-nested-atpt ()
  "Employ actions of  at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th))

;;;###autoload
(defun ar-greaterangle-braced-in-lesserangled-nested-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-braced-in-lesserangled-nested-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-braced-in-lesserangled-nested-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-braced-in-lesserangled-nested-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-backtick))

;;;###autoload
(defun ar-beg-braced-in-lesserangled-nested-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-beg))

;;;###autoload
(defun ar-blok-braced-in-lesserangled-nested-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-blok))

;;;###autoload
(defun ar-bounds-braced-in-lesserangled-nested-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-bounds))

;;;###autoload
(defun ar-brace-braced-in-lesserangled-nested-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-brace))

;;;###autoload
(defun ar-bracket-braced-in-lesserangled-nested-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-braced-in-lesserangled-nested-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-commatize))

;;;###autoload
(defun ar-comment-braced-in-lesserangled-nested-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-comment))

;;;###autoload
(defun ar-dollar-braced-in-lesserangled-nested-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-braced-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-braced-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-braced-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-braced-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-braced-in-lesserangled-nested-atpt ()
  "Employ actions of END at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-end))

;;;###autoload
(defun ar-escape-braced-in-lesserangled-nested-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-escape))

;;;###autoload
(defun ar-hide-braced-in-lesserangled-nested-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-braced-in-lesserangled-nested-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-braced-in-lesserangled-nested-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-braced-in-lesserangled-nested-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-braced-in-lesserangled-nested-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-braced-in-lesserangled-nested-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-length))

;;;###autoload
(defun ar-parentize-braced-in-lesserangled-nested-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-parentize))

;;;###autoload
(defun ar-quote-braced-in-lesserangled-nested-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-quote))

;;;###autoload
(defun ar-separate-braced-in-lesserangled-nested-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-separate))

;;;###autoload
(defun ar-show-braced-in-lesserangled-nested-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-show))

;;;###autoload
(defun ar-singlequote-braced-in-lesserangled-nested-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-braced-in-lesserangled-nested-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-braced-in-lesserangled-nested-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-braced-in-lesserangled-nested-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-sort))

;;;###autoload
(defun ar-trim-braced-in-lesserangled-nested-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-braced-in-lesserangled-nested-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-braced-in-lesserangled-nested-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-braced-in-lesserangled-nested-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-braced-in-lesserangled-nested-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled-nested 'ar-th-whitespace))

;;;###autoload
(defun ar-braced-in-csv-atpt ()
  "Employ actions of  at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th))

;;;###autoload
(defun ar-greaterangle-braced-in-csv-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-braced-in-csv-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-braced-in-csv-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-braced-in-csv-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-backtick))

;;;###autoload
(defun ar-beg-braced-in-csv-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-beg))

;;;###autoload
(defun ar-blok-braced-in-csv-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-blok))

;;;###autoload
(defun ar-bounds-braced-in-csv-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-bounds))

;;;###autoload
(defun ar-brace-braced-in-csv-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-brace))

;;;###autoload
(defun ar-bracket-braced-in-csv-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-braced-in-csv-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-commatize))

;;;###autoload
(defun ar-comment-braced-in-csv-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-comment))

;;;###autoload
(defun ar-dollar-braced-in-csv-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-braced-in-csv-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-braced-in-csv-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-braced-in-csv-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-braced-in-csv-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-braced-in-csv-atpt ()
  "Employ actions of END at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-end))

;;;###autoload
(defun ar-escape-braced-in-csv-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-escape))

;;;###autoload
(defun ar-hide-braced-in-csv-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-braced-in-csv-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-braced-in-csv-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-braced-in-csv-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-braced-in-csv-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-braced-in-csv-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-length))

;;;###autoload
(defun ar-parentize-braced-in-csv-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-parentize))

;;;###autoload
(defun ar-quote-braced-in-csv-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-quote))

;;;###autoload
(defun ar-separate-braced-in-csv-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-separate))

;;;###autoload
(defun ar-show-braced-in-csv-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-show))

;;;###autoload
(defun ar-singlequote-braced-in-csv-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-braced-in-csv-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-braced-in-csv-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-braced-in-csv-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-sort))

;;;###autoload
(defun ar-trim-braced-in-csv-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-braced-in-csv-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-braced-in-csv-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-braced-in-csv-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-braced-in-csv-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'csv 'ar-th-whitespace))

;;;###autoload
(defun ar-braced-in-line-atpt ()
  "Employ actions of  at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th))

;;;###autoload
(defun ar-greaterangle-braced-in-line-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-braced-in-line-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-braced-in-line-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-braced-in-line-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-backtick))

;;;###autoload
(defun ar-beg-braced-in-line-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-beg))

;;;###autoload
(defun ar-blok-braced-in-line-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-blok))

;;;###autoload
(defun ar-bounds-braced-in-line-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-bounds))

;;;###autoload
(defun ar-brace-braced-in-line-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-brace))

;;;###autoload
(defun ar-bracket-braced-in-line-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-braced-in-line-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-commatize))

;;;###autoload
(defun ar-comment-braced-in-line-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-comment))

;;;###autoload
(defun ar-dollar-braced-in-line-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-braced-in-line-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-braced-in-line-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-braced-in-line-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-braced-in-line-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-braced-in-line-atpt ()
  "Employ actions of END at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-end))

;;;###autoload
(defun ar-escape-braced-in-line-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-escape))

;;;###autoload
(defun ar-hide-braced-in-line-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-braced-in-line-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-braced-in-line-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-braced-in-line-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-braced-in-line-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-braced-in-line-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-length))

;;;###autoload
(defun ar-parentize-braced-in-line-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-parentize))

;;;###autoload
(defun ar-quote-braced-in-line-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-quote))

;;;###autoload
(defun ar-separate-braced-in-line-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-separate))

;;;###autoload
(defun ar-show-braced-in-line-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-show))

;;;###autoload
(defun ar-singlequote-braced-in-line-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-braced-in-line-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-braced-in-line-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-braced-in-line-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-sort))

;;;###autoload
(defun ar-trim-braced-in-line-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-braced-in-line-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-braced-in-line-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-braced-in-line-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-braced-in-line-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'line 'ar-th-whitespace))

;;;###autoload
(defun ar-braced-in-paragraph-atpt ()
  "Employ actions of  at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th))

;;;###autoload
(defun ar-greaterangle-braced-in-paragraph-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-braced-in-paragraph-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-braced-in-paragraph-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-braced-in-paragraph-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-backtick))

;;;###autoload
(defun ar-beg-braced-in-paragraph-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-beg))

;;;###autoload
(defun ar-blok-braced-in-paragraph-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-blok))

;;;###autoload
(defun ar-bounds-braced-in-paragraph-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-bounds))

;;;###autoload
(defun ar-brace-braced-in-paragraph-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-brace))

;;;###autoload
(defun ar-bracket-braced-in-paragraph-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-braced-in-paragraph-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-commatize))

;;;###autoload
(defun ar-comment-braced-in-paragraph-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-comment))

;;;###autoload
(defun ar-dollar-braced-in-paragraph-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-braced-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-braced-in-paragraph-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-braced-in-paragraph-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-braced-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-braced-in-paragraph-atpt ()
  "Employ actions of END at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-end))

;;;###autoload
(defun ar-escape-braced-in-paragraph-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-escape))

;;;###autoload
(defun ar-hide-braced-in-paragraph-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-braced-in-paragraph-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-braced-in-paragraph-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-braced-in-paragraph-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-braced-in-paragraph-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-braced-in-paragraph-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-length))

;;;###autoload
(defun ar-parentize-braced-in-paragraph-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-parentize))

;;;###autoload
(defun ar-quote-braced-in-paragraph-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-quote))

;;;###autoload
(defun ar-separate-braced-in-paragraph-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-separate))

;;;###autoload
(defun ar-show-braced-in-paragraph-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-show))

;;;###autoload
(defun ar-singlequote-braced-in-paragraph-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-braced-in-paragraph-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-braced-in-paragraph-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-braced-in-paragraph-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-sort))

;;;###autoload
(defun ar-trim-braced-in-paragraph-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-braced-in-paragraph-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-braced-in-paragraph-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-braced-in-paragraph-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-braced-in-paragraph-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'paragraph 'ar-th-whitespace))

;;;###autoload
(defun ar-braced-in-region-atpt ()
  "Employ actions of  at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th))

;;;###autoload
(defun ar-greaterangle-braced-in-region-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-braced-in-region-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-braced-in-region-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-braced-in-region-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-backtick))

;;;###autoload
(defun ar-beg-braced-in-region-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-beg))

;;;###autoload
(defun ar-blok-braced-in-region-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-blok))

;;;###autoload
(defun ar-bounds-braced-in-region-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-bounds))

;;;###autoload
(defun ar-brace-braced-in-region-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-brace))

;;;###autoload
(defun ar-bracket-braced-in-region-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-braced-in-region-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-commatize))

;;;###autoload
(defun ar-comment-braced-in-region-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-comment))

;;;###autoload
(defun ar-dollar-braced-in-region-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-braced-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-braced-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-braced-in-region-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-braced-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-braced-in-region-atpt ()
  "Employ actions of END at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-end))

;;;###autoload
(defun ar-escape-braced-in-region-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-escape))

;;;###autoload
(defun ar-hide-braced-in-region-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-braced-in-region-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-braced-in-region-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-braced-in-region-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-braced-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-braced-in-region-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-length))

;;;###autoload
(defun ar-parentize-braced-in-region-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-parentize))

;;;###autoload
(defun ar-quote-braced-in-region-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-quote))

;;;###autoload
(defun ar-separate-braced-in-region-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-separate))

;;;###autoload
(defun ar-show-braced-in-region-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-show))

;;;###autoload
(defun ar-singlequote-braced-in-region-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-braced-in-region-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-braced-in-region-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-braced-in-region-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-sort))

;;;###autoload
(defun ar-trim-braced-in-region-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-braced-in-region-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-braced-in-region-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-braced-in-region-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-braced-in-region-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'region 'ar-th-whitespace))

;;;###autoload
(defun ar-braced-in-sentence-atpt ()
  "Employ actions of  at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th))

;;;###autoload
(defun ar-greaterangle-braced-in-sentence-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-braced-in-sentence-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-braced-in-sentence-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-braced-in-sentence-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-backtick))

;;;###autoload
(defun ar-beg-braced-in-sentence-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-beg))

;;;###autoload
(defun ar-blok-braced-in-sentence-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-blok))

;;;###autoload
(defun ar-bounds-braced-in-sentence-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-bounds))

;;;###autoload
(defun ar-brace-braced-in-sentence-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-brace))

;;;###autoload
(defun ar-bracket-braced-in-sentence-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-braced-in-sentence-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-commatize))

;;;###autoload
(defun ar-comment-braced-in-sentence-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-comment))

;;;###autoload
(defun ar-dollar-braced-in-sentence-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-braced-in-sentence-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-braced-in-sentence-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-braced-in-sentence-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-braced-in-sentence-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-braced-in-sentence-atpt ()
  "Employ actions of END at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-end))

;;;###autoload
(defun ar-escape-braced-in-sentence-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-escape))

;;;###autoload
(defun ar-hide-braced-in-sentence-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-braced-in-sentence-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-braced-in-sentence-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-braced-in-sentence-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-braced-in-sentence-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-braced-in-sentence-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-length))

;;;###autoload
(defun ar-parentize-braced-in-sentence-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-parentize))

;;;###autoload
(defun ar-quote-braced-in-sentence-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-quote))

;;;###autoload
(defun ar-separate-braced-in-sentence-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-separate))

;;;###autoload
(defun ar-show-braced-in-sentence-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-show))

;;;###autoload
(defun ar-singlequote-braced-in-sentence-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-braced-in-sentence-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-braced-in-sentence-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-braced-in-sentence-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-sort))

;;;###autoload
(defun ar-trim-braced-in-sentence-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-braced-in-sentence-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-braced-in-sentence-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-braced-in-sentence-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-braced-in-sentence-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'sentence 'ar-th-whitespace))

;;;###autoload
(defun ar-braced-in-string-atpt ()
  "Employ actions of  at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th))

;;;###autoload
(defun ar-greaterangle-braced-in-string-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-braced-in-string-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-braced-in-string-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-braced-in-string-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-backtick))

;;;###autoload
(defun ar-beg-braced-in-string-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-beg))

;;;###autoload
(defun ar-blok-braced-in-string-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-blok))

;;;###autoload
(defun ar-bounds-braced-in-string-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-bounds))

;;;###autoload
(defun ar-brace-braced-in-string-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-brace))

;;;###autoload
(defun ar-bracket-braced-in-string-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-braced-in-string-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-commatize))

;;;###autoload
(defun ar-comment-braced-in-string-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-comment))

;;;###autoload
(defun ar-dollar-braced-in-string-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-braced-in-string-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-braced-in-string-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-braced-in-string-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-braced-in-string-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-braced-in-string-atpt ()
  "Employ actions of END at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-end))

;;;###autoload
(defun ar-escape-braced-in-string-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-escape))

;;;###autoload
(defun ar-hide-braced-in-string-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-braced-in-string-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-braced-in-string-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-braced-in-string-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-braced-in-string-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-braced-in-string-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-length))

;;;###autoload
(defun ar-parentize-braced-in-string-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-parentize))

;;;###autoload
(defun ar-quote-braced-in-string-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-quote))

;;;###autoload
(defun ar-separate-braced-in-string-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-separate))

;;;###autoload
(defun ar-show-braced-in-string-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-show))

;;;###autoload
(defun ar-singlequote-braced-in-string-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-braced-in-string-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-braced-in-string-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-braced-in-string-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-sort))

;;;###autoload
(defun ar-trim-braced-in-string-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-braced-in-string-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-braced-in-string-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-braced-in-string-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-braced-in-string-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'string 'ar-th-whitespace))

;;;###autoload
(defun ar-braced-in-buffer-atpt ()
  "Employ actions of  at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th))

;;;###autoload
(defun ar-greaterangle-braced-in-buffer-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-braced-in-buffer-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-braced-in-buffer-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-braced-in-buffer-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-backtick))

;;;###autoload
(defun ar-beg-braced-in-buffer-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-beg))

;;;###autoload
(defun ar-blok-braced-in-buffer-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-blok))

;;;###autoload
(defun ar-bounds-braced-in-buffer-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-bounds))

;;;###autoload
(defun ar-brace-braced-in-buffer-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-brace))

;;;###autoload
(defun ar-bracket-braced-in-buffer-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-braced-in-buffer-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-commatize))

;;;###autoload
(defun ar-comment-braced-in-buffer-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-comment))

;;;###autoload
(defun ar-dollar-braced-in-buffer-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-braced-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-braced-in-buffer-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-braced-in-buffer-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-braced-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-braced-in-buffer-atpt ()
  "Employ actions of END at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-end))

;;;###autoload
(defun ar-escape-braced-in-buffer-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-escape))

;;;###autoload
(defun ar-hide-braced-in-buffer-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-braced-in-buffer-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-braced-in-buffer-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-braced-in-buffer-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-braced-in-buffer-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-braced-in-buffer-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-length))

;;;###autoload
(defun ar-parentize-braced-in-buffer-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-parentize))

;;;###autoload
(defun ar-quote-braced-in-buffer-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-quote))

;;;###autoload
(defun ar-separate-braced-in-buffer-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-separate))

;;;###autoload
(defun ar-show-braced-in-buffer-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-show))

;;;###autoload
(defun ar-singlequote-braced-in-buffer-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-braced-in-buffer-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-braced-in-buffer-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-braced-in-buffer-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-sort))

;;;###autoload
(defun ar-trim-braced-in-buffer-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-braced-in-buffer-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-braced-in-buffer-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-braced-in-buffer-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-braced-in-buffer-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'buffer 'ar-th-whitespace))

;;;###autoload
(defun ar-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th))

;;;###autoload
(defun ar-greaterangle-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-backtick))

;;;###autoload
(defun ar-beg-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-beg))

;;;###autoload
(defun ar-blok-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-blok))

;;;###autoload
(defun ar-bounds-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-bounds))

;;;###autoload
(defun ar-brace-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-brace))

;;;###autoload
(defun ar-bracket-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-commatize))

;;;###autoload
(defun ar-comment-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-comment))

;;;###autoload
(defun ar-dollar-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-end))

;;;###autoload
(defun ar-escape-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-escape))

;;;###autoload
(defun ar-hide-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-length))

;;;###autoload
(defun ar-parentize-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-parentize))

;;;###autoload
(defun ar-quote-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-quote))

;;;###autoload
(defun ar-separate-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-separate))

;;;###autoload
(defun ar-show-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-show))

;;;###autoload
(defun ar-singlequote-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-sort))

;;;###autoload
(defun ar-trim-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-bracketed-in-angled-no-nest-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'angled-no-nest 'ar-th-whitespace))

;;;###autoload
(defun ar-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th))

;;;###autoload
(defun ar-greaterangle-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-backtick))

;;;###autoload
(defun ar-beg-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-beg))

;;;###autoload
(defun ar-blok-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-blok))

;;;###autoload
(defun ar-bounds-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-bounds))

;;;###autoload
(defun ar-brace-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-brace))

;;;###autoload
(defun ar-bracket-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-commatize))

;;;###autoload
(defun ar-comment-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-comment))

;;;###autoload
(defun ar-dollar-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-end))

;;;###autoload
(defun ar-escape-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-escape))

;;;###autoload
(defun ar-hide-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-length))

;;;###autoload
(defun ar-parentize-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-parentize))

;;;###autoload
(defun ar-quote-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-quote))

;;;###autoload
(defun ar-separate-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-separate))

;;;###autoload
(defun ar-show-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-show))

;;;###autoload
(defun ar-singlequote-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-sort))

;;;###autoload
(defun ar-trim-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-bracketed-in-greaterangled-nested-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled-nested 'ar-th-whitespace))

;;;###autoload
(defun ar-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th))

;;;###autoload
(defun ar-greaterangle-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-backtick))

;;;###autoload
(defun ar-beg-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-beg))

;;;###autoload
(defun ar-blok-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-blok))

;;;###autoload
(defun ar-bounds-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-bounds))

;;;###autoload
(defun ar-brace-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-brace))

;;;###autoload
(defun ar-bracket-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-commatize))

;;;###autoload
(defun ar-comment-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-comment))

;;;###autoload
(defun ar-dollar-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-end))

;;;###autoload
(defun ar-escape-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-escape))

;;;###autoload
(defun ar-hide-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-length))

;;;###autoload
(defun ar-parentize-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-parentize))

;;;###autoload
(defun ar-quote-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-quote))

;;;###autoload
(defun ar-separate-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-separate))

;;;###autoload
(defun ar-show-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-show))

;;;###autoload
(defun ar-singlequote-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-sort))

;;;###autoload
(defun ar-trim-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-bracketed-in-lesserangled-nested-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled-nested 'ar-th-whitespace))

;;;###autoload
(defun ar-bracketed-in-csv-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th))

;;;###autoload
(defun ar-greaterangle-bracketed-in-csv-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-bracketed-in-csv-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-bracketed-in-csv-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-bracketed-in-csv-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-backtick))

;;;###autoload
(defun ar-beg-bracketed-in-csv-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-beg))

;;;###autoload
(defun ar-blok-bracketed-in-csv-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-blok))

;;;###autoload
(defun ar-bounds-bracketed-in-csv-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-bounds))

;;;###autoload
(defun ar-brace-bracketed-in-csv-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-brace))

;;;###autoload
(defun ar-bracket-bracketed-in-csv-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-bracketed-in-csv-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-commatize))

;;;###autoload
(defun ar-comment-bracketed-in-csv-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-comment))

;;;###autoload
(defun ar-dollar-bracketed-in-csv-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-bracketed-in-csv-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-bracketed-in-csv-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-bracketed-in-csv-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-bracketed-in-csv-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-bracketed-in-csv-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-end))

;;;###autoload
(defun ar-escape-bracketed-in-csv-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-escape))

;;;###autoload
(defun ar-hide-bracketed-in-csv-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-bracketed-in-csv-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-bracketed-in-csv-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-bracketed-in-csv-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-bracketed-in-csv-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-bracketed-in-csv-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-length))

;;;###autoload
(defun ar-parentize-bracketed-in-csv-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-parentize))

;;;###autoload
(defun ar-quote-bracketed-in-csv-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-quote))

;;;###autoload
(defun ar-separate-bracketed-in-csv-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-separate))

;;;###autoload
(defun ar-show-bracketed-in-csv-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-show))

;;;###autoload
(defun ar-singlequote-bracketed-in-csv-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-bracketed-in-csv-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-bracketed-in-csv-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-bracketed-in-csv-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-sort))

;;;###autoload
(defun ar-trim-bracketed-in-csv-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-bracketed-in-csv-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-bracketed-in-csv-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-bracketed-in-csv-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-bracketed-in-csv-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'csv 'ar-th-whitespace))

;;;###autoload
(defun ar-bracketed-in-line-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th))

;;;###autoload
(defun ar-greaterangle-bracketed-in-line-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-bracketed-in-line-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-bracketed-in-line-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-bracketed-in-line-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-backtick))

;;;###autoload
(defun ar-beg-bracketed-in-line-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-beg))

;;;###autoload
(defun ar-blok-bracketed-in-line-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-blok))

;;;###autoload
(defun ar-bounds-bracketed-in-line-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-bounds))

;;;###autoload
(defun ar-brace-bracketed-in-line-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-brace))

;;;###autoload
(defun ar-bracket-bracketed-in-line-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-bracketed-in-line-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-commatize))

;;;###autoload
(defun ar-comment-bracketed-in-line-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-comment))

;;;###autoload
(defun ar-dollar-bracketed-in-line-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-bracketed-in-line-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-bracketed-in-line-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-bracketed-in-line-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-bracketed-in-line-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-bracketed-in-line-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-end))

;;;###autoload
(defun ar-escape-bracketed-in-line-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-escape))

;;;###autoload
(defun ar-hide-bracketed-in-line-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-bracketed-in-line-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-bracketed-in-line-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-bracketed-in-line-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-bracketed-in-line-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-bracketed-in-line-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-length))

;;;###autoload
(defun ar-parentize-bracketed-in-line-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-parentize))

;;;###autoload
(defun ar-quote-bracketed-in-line-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-quote))

;;;###autoload
(defun ar-separate-bracketed-in-line-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-separate))

;;;###autoload
(defun ar-show-bracketed-in-line-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-show))

;;;###autoload
(defun ar-singlequote-bracketed-in-line-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-bracketed-in-line-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-bracketed-in-line-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-bracketed-in-line-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-sort))

;;;###autoload
(defun ar-trim-bracketed-in-line-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-bracketed-in-line-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-bracketed-in-line-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-bracketed-in-line-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-bracketed-in-line-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'line 'ar-th-whitespace))

;;;###autoload
(defun ar-bracketed-in-paragraph-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th))

;;;###autoload
(defun ar-greaterangle-bracketed-in-paragraph-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-bracketed-in-paragraph-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-bracketed-in-paragraph-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-bracketed-in-paragraph-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-backtick))

;;;###autoload
(defun ar-beg-bracketed-in-paragraph-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-beg))

;;;###autoload
(defun ar-blok-bracketed-in-paragraph-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-blok))

;;;###autoload
(defun ar-bounds-bracketed-in-paragraph-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-bounds))

;;;###autoload
(defun ar-brace-bracketed-in-paragraph-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-brace))

;;;###autoload
(defun ar-bracket-bracketed-in-paragraph-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-bracketed-in-paragraph-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-commatize))

;;;###autoload
(defun ar-comment-bracketed-in-paragraph-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-comment))

;;;###autoload
(defun ar-dollar-bracketed-in-paragraph-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-bracketed-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-bracketed-in-paragraph-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-bracketed-in-paragraph-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-bracketed-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-bracketed-in-paragraph-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-end))

;;;###autoload
(defun ar-escape-bracketed-in-paragraph-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-escape))

;;;###autoload
(defun ar-hide-bracketed-in-paragraph-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-bracketed-in-paragraph-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-bracketed-in-paragraph-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-bracketed-in-paragraph-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-bracketed-in-paragraph-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-bracketed-in-paragraph-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-length))

;;;###autoload
(defun ar-parentize-bracketed-in-paragraph-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-parentize))

;;;###autoload
(defun ar-quote-bracketed-in-paragraph-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-quote))

;;;###autoload
(defun ar-separate-bracketed-in-paragraph-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-separate))

;;;###autoload
(defun ar-show-bracketed-in-paragraph-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-show))

;;;###autoload
(defun ar-singlequote-bracketed-in-paragraph-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-bracketed-in-paragraph-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-bracketed-in-paragraph-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-bracketed-in-paragraph-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-sort))

;;;###autoload
(defun ar-trim-bracketed-in-paragraph-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-bracketed-in-paragraph-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-bracketed-in-paragraph-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-bracketed-in-paragraph-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-bracketed-in-paragraph-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'paragraph 'ar-th-whitespace))

;;;###autoload
(defun ar-bracketed-in-region-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th))

;;;###autoload
(defun ar-greaterangle-bracketed-in-region-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-bracketed-in-region-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-bracketed-in-region-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-bracketed-in-region-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-backtick))

;;;###autoload
(defun ar-beg-bracketed-in-region-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-beg))

;;;###autoload
(defun ar-blok-bracketed-in-region-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-blok))

;;;###autoload
(defun ar-bounds-bracketed-in-region-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-bounds))

;;;###autoload
(defun ar-brace-bracketed-in-region-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-brace))

;;;###autoload
(defun ar-bracket-bracketed-in-region-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-bracketed-in-region-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-commatize))

;;;###autoload
(defun ar-comment-bracketed-in-region-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-comment))

;;;###autoload
(defun ar-dollar-bracketed-in-region-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-bracketed-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-bracketed-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-bracketed-in-region-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-bracketed-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-bracketed-in-region-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-end))

;;;###autoload
(defun ar-escape-bracketed-in-region-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-escape))

;;;###autoload
(defun ar-hide-bracketed-in-region-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-bracketed-in-region-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-bracketed-in-region-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-bracketed-in-region-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-bracketed-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-bracketed-in-region-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-length))

;;;###autoload
(defun ar-parentize-bracketed-in-region-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-parentize))

;;;###autoload
(defun ar-quote-bracketed-in-region-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-quote))

;;;###autoload
(defun ar-separate-bracketed-in-region-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-separate))

;;;###autoload
(defun ar-show-bracketed-in-region-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-show))

;;;###autoload
(defun ar-singlequote-bracketed-in-region-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-bracketed-in-region-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-bracketed-in-region-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-bracketed-in-region-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-sort))

;;;###autoload
(defun ar-trim-bracketed-in-region-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-bracketed-in-region-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-bracketed-in-region-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-bracketed-in-region-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-bracketed-in-region-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'region 'ar-th-whitespace))

;;;###autoload
(defun ar-bracketed-in-sentence-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th))

;;;###autoload
(defun ar-greaterangle-bracketed-in-sentence-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-bracketed-in-sentence-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-bracketed-in-sentence-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-bracketed-in-sentence-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-backtick))

;;;###autoload
(defun ar-beg-bracketed-in-sentence-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-beg))

;;;###autoload
(defun ar-blok-bracketed-in-sentence-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-blok))

;;;###autoload
(defun ar-bounds-bracketed-in-sentence-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-bounds))

;;;###autoload
(defun ar-brace-bracketed-in-sentence-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-brace))

;;;###autoload
(defun ar-bracket-bracketed-in-sentence-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-bracketed-in-sentence-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-commatize))

;;;###autoload
(defun ar-comment-bracketed-in-sentence-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-comment))

;;;###autoload
(defun ar-dollar-bracketed-in-sentence-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-bracketed-in-sentence-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-bracketed-in-sentence-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-bracketed-in-sentence-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-bracketed-in-sentence-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-bracketed-in-sentence-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-end))

;;;###autoload
(defun ar-escape-bracketed-in-sentence-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-escape))

;;;###autoload
(defun ar-hide-bracketed-in-sentence-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-bracketed-in-sentence-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-bracketed-in-sentence-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-bracketed-in-sentence-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-bracketed-in-sentence-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-bracketed-in-sentence-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-length))

;;;###autoload
(defun ar-parentize-bracketed-in-sentence-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-parentize))

;;;###autoload
(defun ar-quote-bracketed-in-sentence-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-quote))

;;;###autoload
(defun ar-separate-bracketed-in-sentence-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-separate))

;;;###autoload
(defun ar-show-bracketed-in-sentence-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-show))

;;;###autoload
(defun ar-singlequote-bracketed-in-sentence-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-bracketed-in-sentence-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-bracketed-in-sentence-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-bracketed-in-sentence-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-sort))

;;;###autoload
(defun ar-trim-bracketed-in-sentence-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-bracketed-in-sentence-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-bracketed-in-sentence-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-bracketed-in-sentence-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-bracketed-in-sentence-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'sentence 'ar-th-whitespace))

;;;###autoload
(defun ar-bracketed-in-string-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th))

;;;###autoload
(defun ar-greaterangle-bracketed-in-string-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-bracketed-in-string-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-bracketed-in-string-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-bracketed-in-string-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-backtick))

;;;###autoload
(defun ar-beg-bracketed-in-string-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-beg))

;;;###autoload
(defun ar-blok-bracketed-in-string-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-blok))

;;;###autoload
(defun ar-bounds-bracketed-in-string-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-bounds))

;;;###autoload
(defun ar-brace-bracketed-in-string-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-brace))

;;;###autoload
(defun ar-bracket-bracketed-in-string-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-bracketed-in-string-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-commatize))

;;;###autoload
(defun ar-comment-bracketed-in-string-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-comment))

;;;###autoload
(defun ar-dollar-bracketed-in-string-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-bracketed-in-string-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-bracketed-in-string-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-bracketed-in-string-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-bracketed-in-string-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-bracketed-in-string-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-end))

;;;###autoload
(defun ar-escape-bracketed-in-string-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-escape))

;;;###autoload
(defun ar-hide-bracketed-in-string-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-bracketed-in-string-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-bracketed-in-string-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-bracketed-in-string-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-bracketed-in-string-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-bracketed-in-string-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-length))

;;;###autoload
(defun ar-parentize-bracketed-in-string-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-parentize))

;;;###autoload
(defun ar-quote-bracketed-in-string-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-quote))

;;;###autoload
(defun ar-separate-bracketed-in-string-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-separate))

;;;###autoload
(defun ar-show-bracketed-in-string-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-show))

;;;###autoload
(defun ar-singlequote-bracketed-in-string-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-bracketed-in-string-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-bracketed-in-string-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-bracketed-in-string-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-sort))

;;;###autoload
(defun ar-trim-bracketed-in-string-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-bracketed-in-string-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-bracketed-in-string-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-bracketed-in-string-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-bracketed-in-string-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'string 'ar-th-whitespace))

;;;###autoload
(defun ar-bracketed-in-buffer-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th))

;;;###autoload
(defun ar-greaterangle-bracketed-in-buffer-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-bracketed-in-buffer-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-bracketed-in-buffer-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-bracketed-in-buffer-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-backtick))

;;;###autoload
(defun ar-beg-bracketed-in-buffer-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-beg))

;;;###autoload
(defun ar-blok-bracketed-in-buffer-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-blok))

;;;###autoload
(defun ar-bounds-bracketed-in-buffer-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-bounds))

;;;###autoload
(defun ar-brace-bracketed-in-buffer-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-brace))

;;;###autoload
(defun ar-bracket-bracketed-in-buffer-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-bracketed-in-buffer-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-commatize))

;;;###autoload
(defun ar-comment-bracketed-in-buffer-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-comment))

;;;###autoload
(defun ar-dollar-bracketed-in-buffer-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-bracketed-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-bracketed-in-buffer-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-bracketed-in-buffer-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-bracketed-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-bracketed-in-buffer-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-end))

;;;###autoload
(defun ar-escape-bracketed-in-buffer-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-escape))

;;;###autoload
(defun ar-hide-bracketed-in-buffer-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-bracketed-in-buffer-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-bracketed-in-buffer-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-bracketed-in-buffer-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-bracketed-in-buffer-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-bracketed-in-buffer-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-length))

;;;###autoload
(defun ar-parentize-bracketed-in-buffer-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-parentize))

;;;###autoload
(defun ar-quote-bracketed-in-buffer-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-quote))

;;;###autoload
(defun ar-separate-bracketed-in-buffer-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-separate))

;;;###autoload
(defun ar-show-bracketed-in-buffer-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-show))

;;;###autoload
(defun ar-singlequote-bracketed-in-buffer-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-bracketed-in-buffer-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-bracketed-in-buffer-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-bracketed-in-buffer-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-sort))

;;;###autoload
(defun ar-trim-bracketed-in-buffer-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-bracketed-in-buffer-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-bracketed-in-buffer-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-bracketed-in-buffer-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-bracketed-in-buffer-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'buffer 'ar-th-whitespace))

;;;###autoload
(defun ar-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th))

;;;###autoload
(defun ar-greaterangle-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-backtick))

;;;###autoload
(defun ar-beg-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-beg))

;;;###autoload
(defun ar-blok-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-blok))

;;;###autoload
(defun ar-bounds-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-bounds))

;;;###autoload
(defun ar-brace-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-brace))

;;;###autoload
(defun ar-bracket-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-commatize))

;;;###autoload
(defun ar-comment-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-comment))

;;;###autoload
(defun ar-dollar-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-end))

;;;###autoload
(defun ar-escape-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-escape))

;;;###autoload
(defun ar-hide-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-length))

;;;###autoload
(defun ar-parentize-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-parentize))

;;;###autoload
(defun ar-quote-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-quote))

;;;###autoload
(defun ar-separate-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-separate))

;;;###autoload
(defun ar-show-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-show))

;;;###autoload
(defun ar-singlequote-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-sort))

;;;###autoload
(defun ar-trim-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-lesserangled-in-angled-no-nest-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'angled-no-nest 'ar-th-whitespace))

;;;###autoload
(defun ar-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th))

;;;###autoload
(defun ar-greaterangle-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-backtick))

;;;###autoload
(defun ar-beg-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-beg))

;;;###autoload
(defun ar-blok-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-blok))

;;;###autoload
(defun ar-bounds-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-bounds))

;;;###autoload
(defun ar-brace-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-brace))

;;;###autoload
(defun ar-bracket-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-commatize))

;;;###autoload
(defun ar-comment-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-comment))

;;;###autoload
(defun ar-dollar-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-end))

;;;###autoload
(defun ar-escape-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-escape))

;;;###autoload
(defun ar-hide-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-length))

;;;###autoload
(defun ar-parentize-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-parentize))

;;;###autoload
(defun ar-quote-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-quote))

;;;###autoload
(defun ar-separate-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-separate))

;;;###autoload
(defun ar-show-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-show))

;;;###autoload
(defun ar-singlequote-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-sort))

;;;###autoload
(defun ar-trim-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-lesserangled-in-greaterangled-nested-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled-nested 'ar-th-whitespace))

;;;###autoload
(defun ar-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th))

;;;###autoload
(defun ar-greaterangle-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-backtick))

;;;###autoload
(defun ar-beg-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-beg))

;;;###autoload
(defun ar-blok-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-blok))

;;;###autoload
(defun ar-bounds-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-bounds))

;;;###autoload
(defun ar-brace-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-brace))

;;;###autoload
(defun ar-bracket-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-commatize))

;;;###autoload
(defun ar-comment-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-comment))

;;;###autoload
(defun ar-dollar-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-end))

;;;###autoload
(defun ar-escape-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-escape))

;;;###autoload
(defun ar-hide-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-length))

;;;###autoload
(defun ar-parentize-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-parentize))

;;;###autoload
(defun ar-quote-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-quote))

;;;###autoload
(defun ar-separate-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-separate))

;;;###autoload
(defun ar-show-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-show))

;;;###autoload
(defun ar-singlequote-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-sort))

;;;###autoload
(defun ar-trim-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-lesserangled-in-lesserangled-nested-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled-nested 'ar-th-whitespace))

;;;###autoload
(defun ar-lesserangled-in-csv-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th))

;;;###autoload
(defun ar-greaterangle-lesserangled-in-csv-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-lesserangled-in-csv-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-lesserangled-in-csv-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-lesserangled-in-csv-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-backtick))

;;;###autoload
(defun ar-beg-lesserangled-in-csv-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-beg))

;;;###autoload
(defun ar-blok-lesserangled-in-csv-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-blok))

;;;###autoload
(defun ar-bounds-lesserangled-in-csv-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-bounds))

;;;###autoload
(defun ar-brace-lesserangled-in-csv-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-brace))

;;;###autoload
(defun ar-bracket-lesserangled-in-csv-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-lesserangled-in-csv-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-commatize))

;;;###autoload
(defun ar-comment-lesserangled-in-csv-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-comment))

;;;###autoload
(defun ar-dollar-lesserangled-in-csv-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-lesserangled-in-csv-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-lesserangled-in-csv-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-lesserangled-in-csv-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-lesserangled-in-csv-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-lesserangled-in-csv-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-end))

;;;###autoload
(defun ar-escape-lesserangled-in-csv-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-escape))

;;;###autoload
(defun ar-hide-lesserangled-in-csv-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-lesserangled-in-csv-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-lesserangled-in-csv-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-lesserangled-in-csv-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-lesserangled-in-csv-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-lesserangled-in-csv-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-length))

;;;###autoload
(defun ar-parentize-lesserangled-in-csv-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-parentize))

;;;###autoload
(defun ar-quote-lesserangled-in-csv-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-quote))

;;;###autoload
(defun ar-separate-lesserangled-in-csv-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-separate))

;;;###autoload
(defun ar-show-lesserangled-in-csv-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-show))

;;;###autoload
(defun ar-singlequote-lesserangled-in-csv-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-lesserangled-in-csv-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-lesserangled-in-csv-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-lesserangled-in-csv-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-sort))

;;;###autoload
(defun ar-trim-lesserangled-in-csv-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-lesserangled-in-csv-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-lesserangled-in-csv-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-lesserangled-in-csv-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-lesserangled-in-csv-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'csv 'ar-th-whitespace))

;;;###autoload
(defun ar-lesserangled-in-line-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th))

;;;###autoload
(defun ar-greaterangle-lesserangled-in-line-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-lesserangled-in-line-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-lesserangled-in-line-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-lesserangled-in-line-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-backtick))

;;;###autoload
(defun ar-beg-lesserangled-in-line-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-beg))

;;;###autoload
(defun ar-blok-lesserangled-in-line-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-blok))

;;;###autoload
(defun ar-bounds-lesserangled-in-line-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-bounds))

;;;###autoload
(defun ar-brace-lesserangled-in-line-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-brace))

;;;###autoload
(defun ar-bracket-lesserangled-in-line-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-lesserangled-in-line-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-commatize))

;;;###autoload
(defun ar-comment-lesserangled-in-line-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-comment))

;;;###autoload
(defun ar-dollar-lesserangled-in-line-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-lesserangled-in-line-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-lesserangled-in-line-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-lesserangled-in-line-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-lesserangled-in-line-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-lesserangled-in-line-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-end))

;;;###autoload
(defun ar-escape-lesserangled-in-line-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-escape))

;;;###autoload
(defun ar-hide-lesserangled-in-line-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-lesserangled-in-line-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-lesserangled-in-line-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-lesserangled-in-line-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-lesserangled-in-line-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-lesserangled-in-line-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-length))

;;;###autoload
(defun ar-parentize-lesserangled-in-line-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-parentize))

;;;###autoload
(defun ar-quote-lesserangled-in-line-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-quote))

;;;###autoload
(defun ar-separate-lesserangled-in-line-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-separate))

;;;###autoload
(defun ar-show-lesserangled-in-line-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-show))

;;;###autoload
(defun ar-singlequote-lesserangled-in-line-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-lesserangled-in-line-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-lesserangled-in-line-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-lesserangled-in-line-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-sort))

;;;###autoload
(defun ar-trim-lesserangled-in-line-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-lesserangled-in-line-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-lesserangled-in-line-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-lesserangled-in-line-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-lesserangled-in-line-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'line 'ar-th-whitespace))

;;;###autoload
(defun ar-lesserangled-in-paragraph-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th))

;;;###autoload
(defun ar-greaterangle-lesserangled-in-paragraph-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-lesserangled-in-paragraph-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-lesserangled-in-paragraph-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-lesserangled-in-paragraph-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-backtick))

;;;###autoload
(defun ar-beg-lesserangled-in-paragraph-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-beg))

;;;###autoload
(defun ar-blok-lesserangled-in-paragraph-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-blok))

;;;###autoload
(defun ar-bounds-lesserangled-in-paragraph-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-bounds))

;;;###autoload
(defun ar-brace-lesserangled-in-paragraph-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-brace))

;;;###autoload
(defun ar-bracket-lesserangled-in-paragraph-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-lesserangled-in-paragraph-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-commatize))

;;;###autoload
(defun ar-comment-lesserangled-in-paragraph-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-comment))

;;;###autoload
(defun ar-dollar-lesserangled-in-paragraph-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-lesserangled-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-lesserangled-in-paragraph-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-lesserangled-in-paragraph-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-lesserangled-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-lesserangled-in-paragraph-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-end))

;;;###autoload
(defun ar-escape-lesserangled-in-paragraph-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-escape))

;;;###autoload
(defun ar-hide-lesserangled-in-paragraph-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-lesserangled-in-paragraph-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-lesserangled-in-paragraph-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-lesserangled-in-paragraph-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-lesserangled-in-paragraph-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-lesserangled-in-paragraph-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-length))

;;;###autoload
(defun ar-parentize-lesserangled-in-paragraph-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-parentize))

;;;###autoload
(defun ar-quote-lesserangled-in-paragraph-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-quote))

;;;###autoload
(defun ar-separate-lesserangled-in-paragraph-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-separate))

;;;###autoload
(defun ar-show-lesserangled-in-paragraph-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-show))

;;;###autoload
(defun ar-singlequote-lesserangled-in-paragraph-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-lesserangled-in-paragraph-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-lesserangled-in-paragraph-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-lesserangled-in-paragraph-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-sort))

;;;###autoload
(defun ar-trim-lesserangled-in-paragraph-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-lesserangled-in-paragraph-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-lesserangled-in-paragraph-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-lesserangled-in-paragraph-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-lesserangled-in-paragraph-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'paragraph 'ar-th-whitespace))

;;;###autoload
(defun ar-lesserangled-in-region-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th))

;;;###autoload
(defun ar-greaterangle-lesserangled-in-region-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-lesserangled-in-region-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-lesserangled-in-region-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-lesserangled-in-region-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-backtick))

;;;###autoload
(defun ar-beg-lesserangled-in-region-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-beg))

;;;###autoload
(defun ar-blok-lesserangled-in-region-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-blok))

;;;###autoload
(defun ar-bounds-lesserangled-in-region-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-bounds))

;;;###autoload
(defun ar-brace-lesserangled-in-region-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-brace))

;;;###autoload
(defun ar-bracket-lesserangled-in-region-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-lesserangled-in-region-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-commatize))

;;;###autoload
(defun ar-comment-lesserangled-in-region-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-comment))

;;;###autoload
(defun ar-dollar-lesserangled-in-region-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-lesserangled-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-lesserangled-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-lesserangled-in-region-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-lesserangled-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-lesserangled-in-region-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-end))

;;;###autoload
(defun ar-escape-lesserangled-in-region-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-escape))

;;;###autoload
(defun ar-hide-lesserangled-in-region-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-lesserangled-in-region-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-lesserangled-in-region-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-lesserangled-in-region-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-lesserangled-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-lesserangled-in-region-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-length))

;;;###autoload
(defun ar-parentize-lesserangled-in-region-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-parentize))

;;;###autoload
(defun ar-quote-lesserangled-in-region-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-quote))

;;;###autoload
(defun ar-separate-lesserangled-in-region-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-separate))

;;;###autoload
(defun ar-show-lesserangled-in-region-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-show))

;;;###autoload
(defun ar-singlequote-lesserangled-in-region-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-lesserangled-in-region-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-lesserangled-in-region-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-lesserangled-in-region-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-sort))

;;;###autoload
(defun ar-trim-lesserangled-in-region-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-lesserangled-in-region-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-lesserangled-in-region-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-lesserangled-in-region-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-lesserangled-in-region-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'region 'ar-th-whitespace))

;;;###autoload
(defun ar-lesserangled-in-sentence-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th))

;;;###autoload
(defun ar-greaterangle-lesserangled-in-sentence-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-lesserangled-in-sentence-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-lesserangled-in-sentence-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-lesserangled-in-sentence-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-backtick))

;;;###autoload
(defun ar-beg-lesserangled-in-sentence-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-beg))

;;;###autoload
(defun ar-blok-lesserangled-in-sentence-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-blok))

;;;###autoload
(defun ar-bounds-lesserangled-in-sentence-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-bounds))

;;;###autoload
(defun ar-brace-lesserangled-in-sentence-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-brace))

;;;###autoload
(defun ar-bracket-lesserangled-in-sentence-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-lesserangled-in-sentence-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-commatize))

;;;###autoload
(defun ar-comment-lesserangled-in-sentence-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-comment))

;;;###autoload
(defun ar-dollar-lesserangled-in-sentence-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-lesserangled-in-sentence-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-lesserangled-in-sentence-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-lesserangled-in-sentence-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-lesserangled-in-sentence-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-lesserangled-in-sentence-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-end))

;;;###autoload
(defun ar-escape-lesserangled-in-sentence-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-escape))

;;;###autoload
(defun ar-hide-lesserangled-in-sentence-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-lesserangled-in-sentence-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-lesserangled-in-sentence-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-lesserangled-in-sentence-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-lesserangled-in-sentence-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-lesserangled-in-sentence-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-length))

;;;###autoload
(defun ar-parentize-lesserangled-in-sentence-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-parentize))

;;;###autoload
(defun ar-quote-lesserangled-in-sentence-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-quote))

;;;###autoload
(defun ar-separate-lesserangled-in-sentence-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-separate))

;;;###autoload
(defun ar-show-lesserangled-in-sentence-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-show))

;;;###autoload
(defun ar-singlequote-lesserangled-in-sentence-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-lesserangled-in-sentence-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-lesserangled-in-sentence-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-lesserangled-in-sentence-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-sort))

;;;###autoload
(defun ar-trim-lesserangled-in-sentence-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-lesserangled-in-sentence-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-lesserangled-in-sentence-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-lesserangled-in-sentence-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-lesserangled-in-sentence-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'sentence 'ar-th-whitespace))

;;;###autoload
(defun ar-lesserangled-in-string-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th))

;;;###autoload
(defun ar-greaterangle-lesserangled-in-string-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-lesserangled-in-string-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-lesserangled-in-string-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-lesserangled-in-string-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-backtick))

;;;###autoload
(defun ar-beg-lesserangled-in-string-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-beg))

;;;###autoload
(defun ar-blok-lesserangled-in-string-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-blok))

;;;###autoload
(defun ar-bounds-lesserangled-in-string-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-bounds))

;;;###autoload
(defun ar-brace-lesserangled-in-string-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-brace))

;;;###autoload
(defun ar-bracket-lesserangled-in-string-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-lesserangled-in-string-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-commatize))

;;;###autoload
(defun ar-comment-lesserangled-in-string-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-comment))

;;;###autoload
(defun ar-dollar-lesserangled-in-string-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-lesserangled-in-string-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-lesserangled-in-string-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-lesserangled-in-string-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-lesserangled-in-string-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-lesserangled-in-string-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-end))

;;;###autoload
(defun ar-escape-lesserangled-in-string-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-escape))

;;;###autoload
(defun ar-hide-lesserangled-in-string-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-lesserangled-in-string-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-lesserangled-in-string-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-lesserangled-in-string-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-lesserangled-in-string-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-lesserangled-in-string-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-length))

;;;###autoload
(defun ar-parentize-lesserangled-in-string-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-parentize))

;;;###autoload
(defun ar-quote-lesserangled-in-string-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-quote))

;;;###autoload
(defun ar-separate-lesserangled-in-string-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-separate))

;;;###autoload
(defun ar-show-lesserangled-in-string-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-show))

;;;###autoload
(defun ar-singlequote-lesserangled-in-string-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-lesserangled-in-string-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-lesserangled-in-string-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-lesserangled-in-string-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-sort))

;;;###autoload
(defun ar-trim-lesserangled-in-string-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-lesserangled-in-string-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-lesserangled-in-string-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-lesserangled-in-string-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-lesserangled-in-string-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'string 'ar-th-whitespace))

;;;###autoload
(defun ar-lesserangled-in-buffer-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th))

;;;###autoload
(defun ar-greaterangle-lesserangled-in-buffer-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-lesserangled-in-buffer-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-lesserangled-in-buffer-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-lesserangled-in-buffer-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-backtick))

;;;###autoload
(defun ar-beg-lesserangled-in-buffer-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-beg))

;;;###autoload
(defun ar-blok-lesserangled-in-buffer-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-blok))

;;;###autoload
(defun ar-bounds-lesserangled-in-buffer-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-bounds))

;;;###autoload
(defun ar-brace-lesserangled-in-buffer-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-brace))

;;;###autoload
(defun ar-bracket-lesserangled-in-buffer-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-lesserangled-in-buffer-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-commatize))

;;;###autoload
(defun ar-comment-lesserangled-in-buffer-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-comment))

;;;###autoload
(defun ar-dollar-lesserangled-in-buffer-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-lesserangled-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-lesserangled-in-buffer-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-lesserangled-in-buffer-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-lesserangled-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-lesserangled-in-buffer-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-end))

;;;###autoload
(defun ar-escape-lesserangled-in-buffer-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-escape))

;;;###autoload
(defun ar-hide-lesserangled-in-buffer-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-lesserangled-in-buffer-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-lesserangled-in-buffer-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-lesserangled-in-buffer-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-lesserangled-in-buffer-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-lesserangled-in-buffer-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-length))

;;;###autoload
(defun ar-parentize-lesserangled-in-buffer-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-parentize))

;;;###autoload
(defun ar-quote-lesserangled-in-buffer-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-quote))

;;;###autoload
(defun ar-separate-lesserangled-in-buffer-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-separate))

;;;###autoload
(defun ar-show-lesserangled-in-buffer-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-show))

;;;###autoload
(defun ar-singlequote-lesserangled-in-buffer-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-lesserangled-in-buffer-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-lesserangled-in-buffer-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-lesserangled-in-buffer-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-sort))

;;;###autoload
(defun ar-trim-lesserangled-in-buffer-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-lesserangled-in-buffer-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-lesserangled-in-buffer-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-lesserangled-in-buffer-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-lesserangled-in-buffer-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'buffer 'ar-th-whitespace))

;;;###autoload
(defun ar-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th))

;;;###autoload
(defun ar-greaterangle-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-backtick))

;;;###autoload
(defun ar-beg-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-beg))

;;;###autoload
(defun ar-blok-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-blok))

;;;###autoload
(defun ar-bounds-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-bounds))

;;;###autoload
(defun ar-brace-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-brace))

;;;###autoload
(defun ar-bracket-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-commatize))

;;;###autoload
(defun ar-comment-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-comment))

;;;###autoload
(defun ar-dollar-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-end))

;;;###autoload
(defun ar-escape-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-escape))

;;;###autoload
(defun ar-hide-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-length))

;;;###autoload
(defun ar-parentize-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-parentize))

;;;###autoload
(defun ar-quote-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-quote))

;;;###autoload
(defun ar-separate-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-separate))

;;;###autoload
(defun ar-show-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-show))

;;;###autoload
(defun ar-singlequote-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-sort))

;;;###autoload
(defun ar-trim-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-greaterangled-in-angled-no-nest-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'angled-no-nest 'ar-th-whitespace))

;;;###autoload
(defun ar-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th))

;;;###autoload
(defun ar-greaterangle-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-backtick))

;;;###autoload
(defun ar-beg-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-beg))

;;;###autoload
(defun ar-blok-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-blok))

;;;###autoload
(defun ar-bounds-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-bounds))

;;;###autoload
(defun ar-brace-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-brace))

;;;###autoload
(defun ar-bracket-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-commatize))

;;;###autoload
(defun ar-comment-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-comment))

;;;###autoload
(defun ar-dollar-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-end))

;;;###autoload
(defun ar-escape-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-escape))

;;;###autoload
(defun ar-hide-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-length))

;;;###autoload
(defun ar-parentize-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-parentize))

;;;###autoload
(defun ar-quote-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-quote))

;;;###autoload
(defun ar-separate-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-separate))

;;;###autoload
(defun ar-show-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-show))

;;;###autoload
(defun ar-singlequote-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-sort))

;;;###autoload
(defun ar-trim-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-greaterangled-in-greaterangled-nested-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled-nested 'ar-th-whitespace))

;;;###autoload
(defun ar-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th))

;;;###autoload
(defun ar-greaterangle-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-backtick))

;;;###autoload
(defun ar-beg-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-beg))

;;;###autoload
(defun ar-blok-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-blok))

;;;###autoload
(defun ar-bounds-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-bounds))

;;;###autoload
(defun ar-brace-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-brace))

;;;###autoload
(defun ar-bracket-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-commatize))

;;;###autoload
(defun ar-comment-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-comment))

;;;###autoload
(defun ar-dollar-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-end))

;;;###autoload
(defun ar-escape-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-escape))

;;;###autoload
(defun ar-hide-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-length))

;;;###autoload
(defun ar-parentize-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-parentize))

;;;###autoload
(defun ar-quote-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-quote))

;;;###autoload
(defun ar-separate-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-separate))

;;;###autoload
(defun ar-show-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-show))

;;;###autoload
(defun ar-singlequote-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-sort))

;;;###autoload
(defun ar-trim-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-greaterangled-in-lesserangled-nested-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled-nested 'ar-th-whitespace))

;;;###autoload
(defun ar-greaterangled-in-csv-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th))

;;;###autoload
(defun ar-greaterangle-greaterangled-in-csv-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-greaterangled-in-csv-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-greaterangled-in-csv-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-greaterangled-in-csv-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-backtick))

;;;###autoload
(defun ar-beg-greaterangled-in-csv-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-beg))

;;;###autoload
(defun ar-blok-greaterangled-in-csv-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-blok))

;;;###autoload
(defun ar-bounds-greaterangled-in-csv-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-bounds))

;;;###autoload
(defun ar-brace-greaterangled-in-csv-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-brace))

;;;###autoload
(defun ar-bracket-greaterangled-in-csv-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-greaterangled-in-csv-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-commatize))

;;;###autoload
(defun ar-comment-greaterangled-in-csv-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-comment))

;;;###autoload
(defun ar-dollar-greaterangled-in-csv-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-greaterangled-in-csv-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-greaterangled-in-csv-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-greaterangled-in-csv-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-greaterangled-in-csv-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-greaterangled-in-csv-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-end))

;;;###autoload
(defun ar-escape-greaterangled-in-csv-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-escape))

;;;###autoload
(defun ar-hide-greaterangled-in-csv-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-greaterangled-in-csv-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-greaterangled-in-csv-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-greaterangled-in-csv-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-greaterangled-in-csv-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-greaterangled-in-csv-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-length))

;;;###autoload
(defun ar-parentize-greaterangled-in-csv-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-parentize))

;;;###autoload
(defun ar-quote-greaterangled-in-csv-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-quote))

;;;###autoload
(defun ar-separate-greaterangled-in-csv-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-separate))

;;;###autoload
(defun ar-show-greaterangled-in-csv-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-show))

;;;###autoload
(defun ar-singlequote-greaterangled-in-csv-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-greaterangled-in-csv-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-greaterangled-in-csv-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-greaterangled-in-csv-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-sort))

;;;###autoload
(defun ar-trim-greaterangled-in-csv-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-greaterangled-in-csv-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-greaterangled-in-csv-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-greaterangled-in-csv-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-greaterangled-in-csv-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'csv 'ar-th-whitespace))

;;;###autoload
(defun ar-greaterangled-in-line-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th))

;;;###autoload
(defun ar-greaterangle-greaterangled-in-line-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-greaterangled-in-line-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-greaterangled-in-line-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-greaterangled-in-line-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-backtick))

;;;###autoload
(defun ar-beg-greaterangled-in-line-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-beg))

;;;###autoload
(defun ar-blok-greaterangled-in-line-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-blok))

;;;###autoload
(defun ar-bounds-greaterangled-in-line-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-bounds))

;;;###autoload
(defun ar-brace-greaterangled-in-line-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-brace))

;;;###autoload
(defun ar-bracket-greaterangled-in-line-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-greaterangled-in-line-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-commatize))

;;;###autoload
(defun ar-comment-greaterangled-in-line-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-comment))

;;;###autoload
(defun ar-dollar-greaterangled-in-line-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-greaterangled-in-line-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-greaterangled-in-line-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-greaterangled-in-line-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-greaterangled-in-line-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-greaterangled-in-line-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-end))

;;;###autoload
(defun ar-escape-greaterangled-in-line-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-escape))

;;;###autoload
(defun ar-hide-greaterangled-in-line-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-greaterangled-in-line-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-greaterangled-in-line-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-greaterangled-in-line-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-greaterangled-in-line-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-greaterangled-in-line-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-length))

;;;###autoload
(defun ar-parentize-greaterangled-in-line-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-parentize))

;;;###autoload
(defun ar-quote-greaterangled-in-line-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-quote))

;;;###autoload
(defun ar-separate-greaterangled-in-line-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-separate))

;;;###autoload
(defun ar-show-greaterangled-in-line-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-show))

;;;###autoload
(defun ar-singlequote-greaterangled-in-line-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-greaterangled-in-line-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-greaterangled-in-line-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-greaterangled-in-line-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-sort))

;;;###autoload
(defun ar-trim-greaterangled-in-line-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-greaterangled-in-line-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-greaterangled-in-line-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-greaterangled-in-line-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-greaterangled-in-line-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'line 'ar-th-whitespace))

;;;###autoload
(defun ar-greaterangled-in-paragraph-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th))

;;;###autoload
(defun ar-greaterangle-greaterangled-in-paragraph-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-greaterangled-in-paragraph-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-greaterangled-in-paragraph-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-greaterangled-in-paragraph-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-backtick))

;;;###autoload
(defun ar-beg-greaterangled-in-paragraph-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-beg))

;;;###autoload
(defun ar-blok-greaterangled-in-paragraph-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-blok))

;;;###autoload
(defun ar-bounds-greaterangled-in-paragraph-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-bounds))

;;;###autoload
(defun ar-brace-greaterangled-in-paragraph-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-brace))

;;;###autoload
(defun ar-bracket-greaterangled-in-paragraph-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-greaterangled-in-paragraph-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-commatize))

;;;###autoload
(defun ar-comment-greaterangled-in-paragraph-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-comment))

;;;###autoload
(defun ar-dollar-greaterangled-in-paragraph-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-greaterangled-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-greaterangled-in-paragraph-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-greaterangled-in-paragraph-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-greaterangled-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-greaterangled-in-paragraph-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-end))

;;;###autoload
(defun ar-escape-greaterangled-in-paragraph-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-escape))

;;;###autoload
(defun ar-hide-greaterangled-in-paragraph-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-greaterangled-in-paragraph-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-greaterangled-in-paragraph-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-greaterangled-in-paragraph-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-greaterangled-in-paragraph-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-greaterangled-in-paragraph-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-length))

;;;###autoload
(defun ar-parentize-greaterangled-in-paragraph-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-parentize))

;;;###autoload
(defun ar-quote-greaterangled-in-paragraph-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-quote))

;;;###autoload
(defun ar-separate-greaterangled-in-paragraph-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-separate))

;;;###autoload
(defun ar-show-greaterangled-in-paragraph-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-show))

;;;###autoload
(defun ar-singlequote-greaterangled-in-paragraph-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-greaterangled-in-paragraph-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-greaterangled-in-paragraph-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-greaterangled-in-paragraph-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-sort))

;;;###autoload
(defun ar-trim-greaterangled-in-paragraph-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-greaterangled-in-paragraph-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-greaterangled-in-paragraph-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-greaterangled-in-paragraph-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-greaterangled-in-paragraph-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'paragraph 'ar-th-whitespace))

;;;###autoload
(defun ar-greaterangled-in-region-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th))

;;;###autoload
(defun ar-greaterangle-greaterangled-in-region-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-greaterangled-in-region-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-greaterangled-in-region-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-greaterangled-in-region-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-backtick))

;;;###autoload
(defun ar-beg-greaterangled-in-region-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-beg))

;;;###autoload
(defun ar-blok-greaterangled-in-region-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-blok))

;;;###autoload
(defun ar-bounds-greaterangled-in-region-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-bounds))

;;;###autoload
(defun ar-brace-greaterangled-in-region-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-brace))

;;;###autoload
(defun ar-bracket-greaterangled-in-region-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-greaterangled-in-region-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-commatize))

;;;###autoload
(defun ar-comment-greaterangled-in-region-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-comment))

;;;###autoload
(defun ar-dollar-greaterangled-in-region-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-greaterangled-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-greaterangled-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-greaterangled-in-region-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-greaterangled-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-greaterangled-in-region-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-end))

;;;###autoload
(defun ar-escape-greaterangled-in-region-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-escape))

;;;###autoload
(defun ar-hide-greaterangled-in-region-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-greaterangled-in-region-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-greaterangled-in-region-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-greaterangled-in-region-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-greaterangled-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-greaterangled-in-region-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-length))

;;;###autoload
(defun ar-parentize-greaterangled-in-region-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-parentize))

;;;###autoload
(defun ar-quote-greaterangled-in-region-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-quote))

;;;###autoload
(defun ar-separate-greaterangled-in-region-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-separate))

;;;###autoload
(defun ar-show-greaterangled-in-region-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-show))

;;;###autoload
(defun ar-singlequote-greaterangled-in-region-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-greaterangled-in-region-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-greaterangled-in-region-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-greaterangled-in-region-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-sort))

;;;###autoload
(defun ar-trim-greaterangled-in-region-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-greaterangled-in-region-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-greaterangled-in-region-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-greaterangled-in-region-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-greaterangled-in-region-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'region 'ar-th-whitespace))

;;;###autoload
(defun ar-greaterangled-in-sentence-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th))

;;;###autoload
(defun ar-greaterangle-greaterangled-in-sentence-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-greaterangled-in-sentence-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-greaterangled-in-sentence-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-greaterangled-in-sentence-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-backtick))

;;;###autoload
(defun ar-beg-greaterangled-in-sentence-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-beg))

;;;###autoload
(defun ar-blok-greaterangled-in-sentence-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-blok))

;;;###autoload
(defun ar-bounds-greaterangled-in-sentence-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-bounds))

;;;###autoload
(defun ar-brace-greaterangled-in-sentence-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-brace))

;;;###autoload
(defun ar-bracket-greaterangled-in-sentence-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-greaterangled-in-sentence-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-commatize))

;;;###autoload
(defun ar-comment-greaterangled-in-sentence-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-comment))

;;;###autoload
(defun ar-dollar-greaterangled-in-sentence-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-greaterangled-in-sentence-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-greaterangled-in-sentence-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-greaterangled-in-sentence-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-greaterangled-in-sentence-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-greaterangled-in-sentence-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-end))

;;;###autoload
(defun ar-escape-greaterangled-in-sentence-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-escape))

;;;###autoload
(defun ar-hide-greaterangled-in-sentence-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-greaterangled-in-sentence-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-greaterangled-in-sentence-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-greaterangled-in-sentence-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-greaterangled-in-sentence-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-greaterangled-in-sentence-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-length))

;;;###autoload
(defun ar-parentize-greaterangled-in-sentence-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-parentize))

;;;###autoload
(defun ar-quote-greaterangled-in-sentence-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-quote))

;;;###autoload
(defun ar-separate-greaterangled-in-sentence-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-separate))

;;;###autoload
(defun ar-show-greaterangled-in-sentence-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-show))

;;;###autoload
(defun ar-singlequote-greaterangled-in-sentence-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-greaterangled-in-sentence-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-greaterangled-in-sentence-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-greaterangled-in-sentence-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-sort))

;;;###autoload
(defun ar-trim-greaterangled-in-sentence-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-greaterangled-in-sentence-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-greaterangled-in-sentence-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-greaterangled-in-sentence-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-greaterangled-in-sentence-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'sentence 'ar-th-whitespace))

;;;###autoload
(defun ar-greaterangled-in-string-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th))

;;;###autoload
(defun ar-greaterangle-greaterangled-in-string-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-greaterangled-in-string-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-greaterangled-in-string-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-greaterangled-in-string-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-backtick))

;;;###autoload
(defun ar-beg-greaterangled-in-string-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-beg))

;;;###autoload
(defun ar-blok-greaterangled-in-string-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-blok))

;;;###autoload
(defun ar-bounds-greaterangled-in-string-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-bounds))

;;;###autoload
(defun ar-brace-greaterangled-in-string-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-brace))

;;;###autoload
(defun ar-bracket-greaterangled-in-string-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-greaterangled-in-string-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-commatize))

;;;###autoload
(defun ar-comment-greaterangled-in-string-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-comment))

;;;###autoload
(defun ar-dollar-greaterangled-in-string-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-greaterangled-in-string-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-greaterangled-in-string-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-greaterangled-in-string-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-greaterangled-in-string-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-greaterangled-in-string-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-end))

;;;###autoload
(defun ar-escape-greaterangled-in-string-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-escape))

;;;###autoload
(defun ar-hide-greaterangled-in-string-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-greaterangled-in-string-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-greaterangled-in-string-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-greaterangled-in-string-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-greaterangled-in-string-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-greaterangled-in-string-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-length))

;;;###autoload
(defun ar-parentize-greaterangled-in-string-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-parentize))

;;;###autoload
(defun ar-quote-greaterangled-in-string-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-quote))

;;;###autoload
(defun ar-separate-greaterangled-in-string-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-separate))

;;;###autoload
(defun ar-show-greaterangled-in-string-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-show))

;;;###autoload
(defun ar-singlequote-greaterangled-in-string-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-greaterangled-in-string-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-greaterangled-in-string-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-greaterangled-in-string-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-sort))

;;;###autoload
(defun ar-trim-greaterangled-in-string-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-greaterangled-in-string-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-greaterangled-in-string-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-greaterangled-in-string-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-greaterangled-in-string-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'string 'ar-th-whitespace))

;;;###autoload
(defun ar-greaterangled-in-buffer-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th))

;;;###autoload
(defun ar-greaterangle-greaterangled-in-buffer-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-greaterangled-in-buffer-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-greaterangled-in-buffer-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-greaterangled-in-buffer-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-backtick))

;;;###autoload
(defun ar-beg-greaterangled-in-buffer-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-beg))

;;;###autoload
(defun ar-blok-greaterangled-in-buffer-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-blok))

;;;###autoload
(defun ar-bounds-greaterangled-in-buffer-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-bounds))

;;;###autoload
(defun ar-brace-greaterangled-in-buffer-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-brace))

;;;###autoload
(defun ar-bracket-greaterangled-in-buffer-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-greaterangled-in-buffer-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-commatize))

;;;###autoload
(defun ar-comment-greaterangled-in-buffer-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-comment))

;;;###autoload
(defun ar-dollar-greaterangled-in-buffer-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-greaterangled-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-greaterangled-in-buffer-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-greaterangled-in-buffer-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-greaterangled-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-greaterangled-in-buffer-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-end))

;;;###autoload
(defun ar-escape-greaterangled-in-buffer-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-escape))

;;;###autoload
(defun ar-hide-greaterangled-in-buffer-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-greaterangled-in-buffer-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-greaterangled-in-buffer-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-greaterangled-in-buffer-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-greaterangled-in-buffer-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-greaterangled-in-buffer-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-length))

;;;###autoload
(defun ar-parentize-greaterangled-in-buffer-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-parentize))

;;;###autoload
(defun ar-quote-greaterangled-in-buffer-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-quote))

;;;###autoload
(defun ar-separate-greaterangled-in-buffer-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-separate))

;;;###autoload
(defun ar-show-greaterangled-in-buffer-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-show))

;;;###autoload
(defun ar-singlequote-greaterangled-in-buffer-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-greaterangled-in-buffer-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-greaterangled-in-buffer-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-greaterangled-in-buffer-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-sort))

;;;###autoload
(defun ar-trim-greaterangled-in-buffer-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-greaterangled-in-buffer-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-greaterangled-in-buffer-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-greaterangled-in-buffer-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-greaterangled-in-buffer-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'buffer 'ar-th-whitespace))

;;;###autoload
(defun ar-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th))

;;;###autoload
(defun ar-greaterangle-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-backtick))

;;;###autoload
(defun ar-beg-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-beg))

;;;###autoload
(defun ar-blok-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-blok))

;;;###autoload
(defun ar-bounds-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-bounds))

;;;###autoload
(defun ar-brace-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-brace))

;;;###autoload
(defun ar-bracket-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-commatize))

;;;###autoload
(defun ar-comment-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-comment))

;;;###autoload
(defun ar-dollar-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-end))

;;;###autoload
(defun ar-escape-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-escape))

;;;###autoload
(defun ar-hide-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-length))

;;;###autoload
(defun ar-parentize-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-parentize))

;;;###autoload
(defun ar-quote-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-quote))

;;;###autoload
(defun ar-separate-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-separate))

;;;###autoload
(defun ar-show-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-show))

;;;###autoload
(defun ar-singlequote-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-sort))

;;;###autoload
(defun ar-trim-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-left-right-singlequoted-in-angled-no-nest-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'angled-no-nest 'ar-th-whitespace))

;;;###autoload
(defun ar-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th))

;;;###autoload
(defun ar-greaterangle-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-backtick))

;;;###autoload
(defun ar-beg-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-beg))

;;;###autoload
(defun ar-blok-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-blok))

;;;###autoload
(defun ar-bounds-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-bounds))

;;;###autoload
(defun ar-brace-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-brace))

;;;###autoload
(defun ar-bracket-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-commatize))

;;;###autoload
(defun ar-comment-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-comment))

;;;###autoload
(defun ar-dollar-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-end))

;;;###autoload
(defun ar-escape-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-escape))

;;;###autoload
(defun ar-hide-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-length))

;;;###autoload
(defun ar-parentize-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-parentize))

;;;###autoload
(defun ar-quote-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-quote))

;;;###autoload
(defun ar-separate-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-separate))

;;;###autoload
(defun ar-show-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-show))

;;;###autoload
(defun ar-singlequote-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-sort))

;;;###autoload
(defun ar-trim-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-left-right-singlequoted-in-greaterangled-nested-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled-nested 'ar-th-whitespace))

;;;###autoload
(defun ar-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th))

;;;###autoload
(defun ar-greaterangle-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-backtick))

;;;###autoload
(defun ar-beg-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-beg))

;;;###autoload
(defun ar-blok-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-blok))

;;;###autoload
(defun ar-bounds-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-bounds))

;;;###autoload
(defun ar-brace-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-brace))

;;;###autoload
(defun ar-bracket-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-commatize))

;;;###autoload
(defun ar-comment-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-comment))

;;;###autoload
(defun ar-dollar-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-end))

;;;###autoload
(defun ar-escape-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-escape))

;;;###autoload
(defun ar-hide-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-length))

;;;###autoload
(defun ar-parentize-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-parentize))

;;;###autoload
(defun ar-quote-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-quote))

;;;###autoload
(defun ar-separate-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-separate))

;;;###autoload
(defun ar-show-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-show))

;;;###autoload
(defun ar-singlequote-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-sort))

;;;###autoload
(defun ar-trim-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-left-right-singlequoted-in-lesserangled-nested-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled-nested 'ar-th-whitespace))

;;;###autoload
(defun ar-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th))

;;;###autoload
(defun ar-greaterangle-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-backtick))

;;;###autoload
(defun ar-beg-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-beg))

;;;###autoload
(defun ar-blok-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-blok))

;;;###autoload
(defun ar-bounds-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-bounds))

;;;###autoload
(defun ar-brace-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-brace))

;;;###autoload
(defun ar-bracket-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-commatize))

;;;###autoload
(defun ar-comment-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-comment))

;;;###autoload
(defun ar-dollar-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-end))

;;;###autoload
(defun ar-escape-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-escape))

;;;###autoload
(defun ar-hide-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-length))

;;;###autoload
(defun ar-parentize-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-parentize))

;;;###autoload
(defun ar-quote-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-quote))

;;;###autoload
(defun ar-separate-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-separate))

;;;###autoload
(defun ar-show-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-show))

;;;###autoload
(defun ar-singlequote-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-sort))

;;;###autoload
(defun ar-trim-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-left-right-singlequoted-in-csv-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'csv 'ar-th-whitespace))

;;;###autoload
(defun ar-left-right-singlequoted-in-line-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th))

;;;###autoload
(defun ar-greaterangle-left-right-singlequoted-in-line-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-left-right-singlequoted-in-line-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-left-right-singlequoted-in-line-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-left-right-singlequoted-in-line-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-backtick))

;;;###autoload
(defun ar-beg-left-right-singlequoted-in-line-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-beg))

;;;###autoload
(defun ar-blok-left-right-singlequoted-in-line-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-blok))

;;;###autoload
(defun ar-bounds-left-right-singlequoted-in-line-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-bounds))

;;;###autoload
(defun ar-brace-left-right-singlequoted-in-line-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-brace))

;;;###autoload
(defun ar-bracket-left-right-singlequoted-in-line-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-left-right-singlequoted-in-line-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-commatize))

;;;###autoload
(defun ar-comment-left-right-singlequoted-in-line-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-comment))

;;;###autoload
(defun ar-dollar-left-right-singlequoted-in-line-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-left-right-singlequoted-in-line-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-left-right-singlequoted-in-line-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-left-right-singlequoted-in-line-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-left-right-singlequoted-in-line-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-left-right-singlequoted-in-line-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-end))

;;;###autoload
(defun ar-escape-left-right-singlequoted-in-line-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-escape))

;;;###autoload
(defun ar-hide-left-right-singlequoted-in-line-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-left-right-singlequoted-in-line-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-left-right-singlequoted-in-line-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-left-right-singlequoted-in-line-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-left-right-singlequoted-in-line-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-left-right-singlequoted-in-line-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-length))

;;;###autoload
(defun ar-parentize-left-right-singlequoted-in-line-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-parentize))

;;;###autoload
(defun ar-quote-left-right-singlequoted-in-line-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-quote))

;;;###autoload
(defun ar-separate-left-right-singlequoted-in-line-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-separate))

;;;###autoload
(defun ar-show-left-right-singlequoted-in-line-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-show))

;;;###autoload
(defun ar-singlequote-left-right-singlequoted-in-line-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-left-right-singlequoted-in-line-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-left-right-singlequoted-in-line-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-left-right-singlequoted-in-line-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-sort))

;;;###autoload
(defun ar-trim-left-right-singlequoted-in-line-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-left-right-singlequoted-in-line-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-left-right-singlequoted-in-line-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-left-right-singlequoted-in-line-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-left-right-singlequoted-in-line-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'line 'ar-th-whitespace))

;;;###autoload
(defun ar-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th))

;;;###autoload
(defun ar-greaterangle-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-backtick))

;;;###autoload
(defun ar-beg-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-beg))

;;;###autoload
(defun ar-blok-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-blok))

;;;###autoload
(defun ar-bounds-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-bounds))

;;;###autoload
(defun ar-brace-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-brace))

;;;###autoload
(defun ar-bracket-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-commatize))

;;;###autoload
(defun ar-comment-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-comment))

;;;###autoload
(defun ar-dollar-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-end))

;;;###autoload
(defun ar-escape-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-escape))

;;;###autoload
(defun ar-hide-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-length))

;;;###autoload
(defun ar-parentize-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-parentize))

;;;###autoload
(defun ar-quote-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-quote))

;;;###autoload
(defun ar-separate-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-separate))

;;;###autoload
(defun ar-show-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-show))

;;;###autoload
(defun ar-singlequote-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-sort))

;;;###autoload
(defun ar-trim-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-left-right-singlequoted-in-paragraph-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'paragraph 'ar-th-whitespace))

;;;###autoload
(defun ar-left-right-singlequoted-in-region-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th))

;;;###autoload
(defun ar-greaterangle-left-right-singlequoted-in-region-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-left-right-singlequoted-in-region-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-left-right-singlequoted-in-region-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-left-right-singlequoted-in-region-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-backtick))

;;;###autoload
(defun ar-beg-left-right-singlequoted-in-region-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-beg))

;;;###autoload
(defun ar-blok-left-right-singlequoted-in-region-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-blok))

;;;###autoload
(defun ar-bounds-left-right-singlequoted-in-region-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-bounds))

;;;###autoload
(defun ar-brace-left-right-singlequoted-in-region-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-brace))

;;;###autoload
(defun ar-bracket-left-right-singlequoted-in-region-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-left-right-singlequoted-in-region-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-commatize))

;;;###autoload
(defun ar-comment-left-right-singlequoted-in-region-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-comment))

;;;###autoload
(defun ar-dollar-left-right-singlequoted-in-region-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-left-right-singlequoted-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-left-right-singlequoted-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-left-right-singlequoted-in-region-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-left-right-singlequoted-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-left-right-singlequoted-in-region-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-end))

;;;###autoload
(defun ar-escape-left-right-singlequoted-in-region-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-escape))

;;;###autoload
(defun ar-hide-left-right-singlequoted-in-region-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-left-right-singlequoted-in-region-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-left-right-singlequoted-in-region-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-left-right-singlequoted-in-region-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-left-right-singlequoted-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-left-right-singlequoted-in-region-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-length))

;;;###autoload
(defun ar-parentize-left-right-singlequoted-in-region-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-parentize))

;;;###autoload
(defun ar-quote-left-right-singlequoted-in-region-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-quote))

;;;###autoload
(defun ar-separate-left-right-singlequoted-in-region-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-separate))

;;;###autoload
(defun ar-show-left-right-singlequoted-in-region-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-show))

;;;###autoload
(defun ar-singlequote-left-right-singlequoted-in-region-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-left-right-singlequoted-in-region-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-left-right-singlequoted-in-region-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-left-right-singlequoted-in-region-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-sort))

;;;###autoload
(defun ar-trim-left-right-singlequoted-in-region-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-left-right-singlequoted-in-region-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-left-right-singlequoted-in-region-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-left-right-singlequoted-in-region-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-left-right-singlequoted-in-region-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'region 'ar-th-whitespace))

;;;###autoload
(defun ar-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th))

;;;###autoload
(defun ar-greaterangle-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-backtick))

;;;###autoload
(defun ar-beg-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-beg))

;;;###autoload
(defun ar-blok-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-blok))

;;;###autoload
(defun ar-bounds-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-bounds))

;;;###autoload
(defun ar-brace-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-brace))

;;;###autoload
(defun ar-bracket-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-commatize))

;;;###autoload
(defun ar-comment-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-comment))

;;;###autoload
(defun ar-dollar-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-end))

;;;###autoload
(defun ar-escape-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-escape))

;;;###autoload
(defun ar-hide-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-length))

;;;###autoload
(defun ar-parentize-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-parentize))

;;;###autoload
(defun ar-quote-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-quote))

;;;###autoload
(defun ar-separate-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-separate))

;;;###autoload
(defun ar-show-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-show))

;;;###autoload
(defun ar-singlequote-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-sort))

;;;###autoload
(defun ar-trim-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-left-right-singlequoted-in-sentence-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'sentence 'ar-th-whitespace))

;;;###autoload
(defun ar-left-right-singlequoted-in-string-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th))

;;;###autoload
(defun ar-greaterangle-left-right-singlequoted-in-string-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-left-right-singlequoted-in-string-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-left-right-singlequoted-in-string-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-left-right-singlequoted-in-string-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-backtick))

;;;###autoload
(defun ar-beg-left-right-singlequoted-in-string-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-beg))

;;;###autoload
(defun ar-blok-left-right-singlequoted-in-string-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-blok))

;;;###autoload
(defun ar-bounds-left-right-singlequoted-in-string-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-bounds))

;;;###autoload
(defun ar-brace-left-right-singlequoted-in-string-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-brace))

;;;###autoload
(defun ar-bracket-left-right-singlequoted-in-string-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-left-right-singlequoted-in-string-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-commatize))

;;;###autoload
(defun ar-comment-left-right-singlequoted-in-string-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-comment))

;;;###autoload
(defun ar-dollar-left-right-singlequoted-in-string-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-left-right-singlequoted-in-string-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-left-right-singlequoted-in-string-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-left-right-singlequoted-in-string-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-left-right-singlequoted-in-string-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-left-right-singlequoted-in-string-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-end))

;;;###autoload
(defun ar-escape-left-right-singlequoted-in-string-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-escape))

;;;###autoload
(defun ar-hide-left-right-singlequoted-in-string-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-left-right-singlequoted-in-string-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-left-right-singlequoted-in-string-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-left-right-singlequoted-in-string-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-left-right-singlequoted-in-string-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-left-right-singlequoted-in-string-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-length))

;;;###autoload
(defun ar-parentize-left-right-singlequoted-in-string-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-parentize))

;;;###autoload
(defun ar-quote-left-right-singlequoted-in-string-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-quote))

;;;###autoload
(defun ar-separate-left-right-singlequoted-in-string-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-separate))

;;;###autoload
(defun ar-show-left-right-singlequoted-in-string-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-show))

;;;###autoload
(defun ar-singlequote-left-right-singlequoted-in-string-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-left-right-singlequoted-in-string-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-left-right-singlequoted-in-string-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-left-right-singlequoted-in-string-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-sort))

;;;###autoload
(defun ar-trim-left-right-singlequoted-in-string-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-left-right-singlequoted-in-string-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-left-right-singlequoted-in-string-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-left-right-singlequoted-in-string-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-left-right-singlequoted-in-string-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'string 'ar-th-whitespace))

;;;###autoload
(defun ar-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th))

;;;###autoload
(defun ar-greaterangle-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-backtick))

;;;###autoload
(defun ar-beg-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-beg))

;;;###autoload
(defun ar-blok-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-blok))

;;;###autoload
(defun ar-bounds-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-bounds))

;;;###autoload
(defun ar-brace-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-brace))

;;;###autoload
(defun ar-bracket-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-commatize))

;;;###autoload
(defun ar-comment-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-comment))

;;;###autoload
(defun ar-dollar-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-end))

;;;###autoload
(defun ar-escape-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-escape))

;;;###autoload
(defun ar-hide-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-length))

;;;###autoload
(defun ar-parentize-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-parentize))

;;;###autoload
(defun ar-quote-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-quote))

;;;###autoload
(defun ar-separate-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-separate))

;;;###autoload
(defun ar-show-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-show))

;;;###autoload
(defun ar-singlequote-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-sort))

;;;###autoload
(defun ar-trim-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-left-right-singlequoted-in-buffer-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'buffer 'ar-th-whitespace))

;;;###autoload
(defun ar-parentized-in-angled-no-nest-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th))

;;;###autoload
(defun ar-greaterangle-parentized-in-angled-no-nest-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-parentized-in-angled-no-nest-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-parentized-in-angled-no-nest-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-parentized-in-angled-no-nest-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-backtick))

;;;###autoload
(defun ar-beg-parentized-in-angled-no-nest-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-beg))

;;;###autoload
(defun ar-blok-parentized-in-angled-no-nest-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-blok))

;;;###autoload
(defun ar-bounds-parentized-in-angled-no-nest-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-bounds))

;;;###autoload
(defun ar-brace-parentized-in-angled-no-nest-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-brace))

;;;###autoload
(defun ar-bracket-parentized-in-angled-no-nest-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-parentized-in-angled-no-nest-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-commatize))

;;;###autoload
(defun ar-comment-parentized-in-angled-no-nest-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-comment))

;;;###autoload
(defun ar-dollar-parentized-in-angled-no-nest-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-parentized-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-parentized-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-parentized-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-parentized-in-angled-no-nest-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-parentized-in-angled-no-nest-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-end))

;;;###autoload
(defun ar-escape-parentized-in-angled-no-nest-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-escape))

;;;###autoload
(defun ar-hide-parentized-in-angled-no-nest-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-parentized-in-angled-no-nest-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-parentized-in-angled-no-nest-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-parentized-in-angled-no-nest-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-parentized-in-angled-no-nest-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-parentized-in-angled-no-nest-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-length))

;;;###autoload
(defun ar-parentize-parentized-in-angled-no-nest-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-parentize))

;;;###autoload
(defun ar-quote-parentized-in-angled-no-nest-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-quote))

;;;###autoload
(defun ar-separate-parentized-in-angled-no-nest-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-separate))

;;;###autoload
(defun ar-show-parentized-in-angled-no-nest-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-show))

;;;###autoload
(defun ar-singlequote-parentized-in-angled-no-nest-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-parentized-in-angled-no-nest-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-parentized-in-angled-no-nest-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-parentized-in-angled-no-nest-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-sort))

;;;###autoload
(defun ar-trim-parentized-in-angled-no-nest-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-parentized-in-angled-no-nest-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-parentized-in-angled-no-nest-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-parentized-in-angled-no-nest-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-parentized-in-angled-no-nest-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing ANGLED-NO-NEST. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'angled-no-nest 'ar-th-whitespace))

;;;###autoload
(defun ar-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th))

;;;###autoload
(defun ar-greaterangle-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-backtick))

;;;###autoload
(defun ar-beg-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-beg))

;;;###autoload
(defun ar-blok-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-blok))

;;;###autoload
(defun ar-bounds-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-bounds))

;;;###autoload
(defun ar-brace-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-brace))

;;;###autoload
(defun ar-bracket-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-commatize))

;;;###autoload
(defun ar-comment-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-comment))

;;;###autoload
(defun ar-dollar-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-end))

;;;###autoload
(defun ar-escape-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-escape))

;;;###autoload
(defun ar-hide-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-length))

;;;###autoload
(defun ar-parentize-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-parentize))

;;;###autoload
(defun ar-quote-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-quote))

;;;###autoload
(defun ar-separate-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-separate))

;;;###autoload
(defun ar-show-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-show))

;;;###autoload
(defun ar-singlequote-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-sort))

;;;###autoload
(defun ar-trim-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-parentized-in-greaterangled-nested-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing GREATER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled-nested 'ar-th-whitespace))

;;;###autoload
(defun ar-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th))

;;;###autoload
(defun ar-greaterangle-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-backtick))

;;;###autoload
(defun ar-beg-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-beg))

;;;###autoload
(defun ar-blok-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-blok))

;;;###autoload
(defun ar-bounds-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-bounds))

;;;###autoload
(defun ar-brace-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-brace))

;;;###autoload
(defun ar-bracket-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-commatize))

;;;###autoload
(defun ar-comment-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-comment))

;;;###autoload
(defun ar-dollar-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-end))

;;;###autoload
(defun ar-escape-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-escape))

;;;###autoload
(defun ar-hide-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-length))

;;;###autoload
(defun ar-parentize-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-parentize))

;;;###autoload
(defun ar-quote-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-quote))

;;;###autoload
(defun ar-separate-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-separate))

;;;###autoload
(defun ar-show-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-show))

;;;###autoload
(defun ar-singlequote-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-sort))

;;;###autoload
(defun ar-trim-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-parentized-in-lesserangled-nested-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing LESSER-ANGLED-NESTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled-nested 'ar-th-whitespace))

;;;###autoload
(defun ar-parentized-in-csv-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th))

;;;###autoload
(defun ar-greaterangle-parentized-in-csv-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-parentized-in-csv-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-parentized-in-csv-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-parentized-in-csv-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-backtick))

;;;###autoload
(defun ar-beg-parentized-in-csv-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-beg))

;;;###autoload
(defun ar-blok-parentized-in-csv-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-blok))

;;;###autoload
(defun ar-bounds-parentized-in-csv-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-bounds))

;;;###autoload
(defun ar-brace-parentized-in-csv-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-brace))

;;;###autoload
(defun ar-bracket-parentized-in-csv-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-parentized-in-csv-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-commatize))

;;;###autoload
(defun ar-comment-parentized-in-csv-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-comment))

;;;###autoload
(defun ar-dollar-parentized-in-csv-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-parentized-in-csv-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-parentized-in-csv-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-parentized-in-csv-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-parentized-in-csv-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-parentized-in-csv-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-end))

;;;###autoload
(defun ar-escape-parentized-in-csv-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-escape))

;;;###autoload
(defun ar-hide-parentized-in-csv-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-parentized-in-csv-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-parentized-in-csv-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-parentized-in-csv-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-parentized-in-csv-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-parentized-in-csv-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-length))

;;;###autoload
(defun ar-parentize-parentized-in-csv-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-parentize))

;;;###autoload
(defun ar-quote-parentized-in-csv-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-quote))

;;;###autoload
(defun ar-separate-parentized-in-csv-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-separate))

;;;###autoload
(defun ar-show-parentized-in-csv-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-show))

;;;###autoload
(defun ar-singlequote-parentized-in-csv-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-parentized-in-csv-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-parentized-in-csv-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-parentized-in-csv-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-sort))

;;;###autoload
(defun ar-trim-parentized-in-csv-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-parentized-in-csv-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-parentized-in-csv-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-parentized-in-csv-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-parentized-in-csv-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing CSV. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'csv 'ar-th-whitespace))

;;;###autoload
(defun ar-parentized-in-line-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th))

;;;###autoload
(defun ar-greaterangle-parentized-in-line-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-parentized-in-line-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-parentized-in-line-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-parentized-in-line-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-backtick))

;;;###autoload
(defun ar-beg-parentized-in-line-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-beg))

;;;###autoload
(defun ar-blok-parentized-in-line-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-blok))

;;;###autoload
(defun ar-bounds-parentized-in-line-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-bounds))

;;;###autoload
(defun ar-brace-parentized-in-line-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-brace))

;;;###autoload
(defun ar-bracket-parentized-in-line-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-parentized-in-line-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-commatize))

;;;###autoload
(defun ar-comment-parentized-in-line-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-comment))

;;;###autoload
(defun ar-dollar-parentized-in-line-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-parentized-in-line-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-parentized-in-line-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-parentized-in-line-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-parentized-in-line-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-parentized-in-line-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-end))

;;;###autoload
(defun ar-escape-parentized-in-line-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-escape))

;;;###autoload
(defun ar-hide-parentized-in-line-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-parentized-in-line-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-parentized-in-line-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-parentized-in-line-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-parentized-in-line-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-parentized-in-line-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-length))

;;;###autoload
(defun ar-parentize-parentized-in-line-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-parentize))

;;;###autoload
(defun ar-quote-parentized-in-line-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-quote))

;;;###autoload
(defun ar-separate-parentized-in-line-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-separate))

;;;###autoload
(defun ar-show-parentized-in-line-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-show))

;;;###autoload
(defun ar-singlequote-parentized-in-line-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-parentized-in-line-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-parentized-in-line-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-parentized-in-line-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-sort))

;;;###autoload
(defun ar-trim-parentized-in-line-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-parentized-in-line-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-parentized-in-line-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-parentized-in-line-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-parentized-in-line-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing LINE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'line 'ar-th-whitespace))

;;;###autoload
(defun ar-parentized-in-paragraph-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th))

;;;###autoload
(defun ar-greaterangle-parentized-in-paragraph-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-parentized-in-paragraph-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-parentized-in-paragraph-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-parentized-in-paragraph-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-backtick))

;;;###autoload
(defun ar-beg-parentized-in-paragraph-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-beg))

;;;###autoload
(defun ar-blok-parentized-in-paragraph-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-blok))

;;;###autoload
(defun ar-bounds-parentized-in-paragraph-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-bounds))

;;;###autoload
(defun ar-brace-parentized-in-paragraph-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-brace))

;;;###autoload
(defun ar-bracket-parentized-in-paragraph-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-parentized-in-paragraph-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-commatize))

;;;###autoload
(defun ar-comment-parentized-in-paragraph-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-comment))

;;;###autoload
(defun ar-dollar-parentized-in-paragraph-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-parentized-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-parentized-in-paragraph-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-parentized-in-paragraph-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-parentized-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-parentized-in-paragraph-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-end))

;;;###autoload
(defun ar-escape-parentized-in-paragraph-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-escape))

;;;###autoload
(defun ar-hide-parentized-in-paragraph-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-parentized-in-paragraph-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-parentized-in-paragraph-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-parentized-in-paragraph-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-parentized-in-paragraph-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-parentized-in-paragraph-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-length))

;;;###autoload
(defun ar-parentize-parentized-in-paragraph-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-parentize))

;;;###autoload
(defun ar-quote-parentized-in-paragraph-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-quote))

;;;###autoload
(defun ar-separate-parentized-in-paragraph-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-separate))

;;;###autoload
(defun ar-show-parentized-in-paragraph-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-show))

;;;###autoload
(defun ar-singlequote-parentized-in-paragraph-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-parentized-in-paragraph-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-parentized-in-paragraph-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-parentized-in-paragraph-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-sort))

;;;###autoload
(defun ar-trim-parentized-in-paragraph-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-parentized-in-paragraph-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-parentized-in-paragraph-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-parentized-in-paragraph-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-parentized-in-paragraph-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'paragraph 'ar-th-whitespace))

;;;###autoload
(defun ar-parentized-in-region-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th))

;;;###autoload
(defun ar-greaterangle-parentized-in-region-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-parentized-in-region-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-parentized-in-region-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-parentized-in-region-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-backtick))

;;;###autoload
(defun ar-beg-parentized-in-region-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-beg))

;;;###autoload
(defun ar-blok-parentized-in-region-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-blok))

;;;###autoload
(defun ar-bounds-parentized-in-region-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-bounds))

;;;###autoload
(defun ar-brace-parentized-in-region-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-brace))

;;;###autoload
(defun ar-bracket-parentized-in-region-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-parentized-in-region-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-commatize))

;;;###autoload
(defun ar-comment-parentized-in-region-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-comment))

;;;###autoload
(defun ar-dollar-parentized-in-region-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-parentized-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-parentized-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-parentized-in-region-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-parentized-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-parentized-in-region-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-end))

;;;###autoload
(defun ar-escape-parentized-in-region-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-escape))

;;;###autoload
(defun ar-hide-parentized-in-region-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-parentized-in-region-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-parentized-in-region-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-parentized-in-region-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-parentized-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-parentized-in-region-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-length))

;;;###autoload
(defun ar-parentize-parentized-in-region-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-parentize))

;;;###autoload
(defun ar-quote-parentized-in-region-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-quote))

;;;###autoload
(defun ar-separate-parentized-in-region-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-separate))

;;;###autoload
(defun ar-show-parentized-in-region-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-show))

;;;###autoload
(defun ar-singlequote-parentized-in-region-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-parentized-in-region-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-parentized-in-region-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-parentized-in-region-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-sort))

;;;###autoload
(defun ar-trim-parentized-in-region-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-parentized-in-region-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-parentized-in-region-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-parentized-in-region-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-parentized-in-region-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'region 'ar-th-whitespace))

;;;###autoload
(defun ar-parentized-in-sentence-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th))

;;;###autoload
(defun ar-greaterangle-parentized-in-sentence-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-parentized-in-sentence-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-parentized-in-sentence-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-parentized-in-sentence-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-backtick))

;;;###autoload
(defun ar-beg-parentized-in-sentence-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-beg))

;;;###autoload
(defun ar-blok-parentized-in-sentence-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-blok))

;;;###autoload
(defun ar-bounds-parentized-in-sentence-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-bounds))

;;;###autoload
(defun ar-brace-parentized-in-sentence-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-brace))

;;;###autoload
(defun ar-bracket-parentized-in-sentence-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-parentized-in-sentence-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-commatize))

;;;###autoload
(defun ar-comment-parentized-in-sentence-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-comment))

;;;###autoload
(defun ar-dollar-parentized-in-sentence-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-parentized-in-sentence-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-parentized-in-sentence-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-parentized-in-sentence-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-parentized-in-sentence-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-parentized-in-sentence-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-end))

;;;###autoload
(defun ar-escape-parentized-in-sentence-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-escape))

;;;###autoload
(defun ar-hide-parentized-in-sentence-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-parentized-in-sentence-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-parentized-in-sentence-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-parentized-in-sentence-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-parentized-in-sentence-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-parentized-in-sentence-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-length))

;;;###autoload
(defun ar-parentize-parentized-in-sentence-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-parentize))

;;;###autoload
(defun ar-quote-parentized-in-sentence-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-quote))

;;;###autoload
(defun ar-separate-parentized-in-sentence-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-separate))

;;;###autoload
(defun ar-show-parentized-in-sentence-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-show))

;;;###autoload
(defun ar-singlequote-parentized-in-sentence-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-parentized-in-sentence-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-parentized-in-sentence-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-parentized-in-sentence-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-sort))

;;;###autoload
(defun ar-trim-parentized-in-sentence-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-parentized-in-sentence-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-parentized-in-sentence-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-parentized-in-sentence-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-parentized-in-sentence-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing SENTENCE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'sentence 'ar-th-whitespace))

;;;###autoload
(defun ar-parentized-in-string-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th))

;;;###autoload
(defun ar-greaterangle-parentized-in-string-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-parentized-in-string-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-parentized-in-string-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-parentized-in-string-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-backtick))

;;;###autoload
(defun ar-beg-parentized-in-string-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-beg))

;;;###autoload
(defun ar-blok-parentized-in-string-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-blok))

;;;###autoload
(defun ar-bounds-parentized-in-string-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-bounds))

;;;###autoload
(defun ar-brace-parentized-in-string-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-brace))

;;;###autoload
(defun ar-bracket-parentized-in-string-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-parentized-in-string-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-commatize))

;;;###autoload
(defun ar-comment-parentized-in-string-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-comment))

;;;###autoload
(defun ar-dollar-parentized-in-string-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-parentized-in-string-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-parentized-in-string-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-parentized-in-string-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-parentized-in-string-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-parentized-in-string-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-end))

;;;###autoload
(defun ar-escape-parentized-in-string-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-escape))

;;;###autoload
(defun ar-hide-parentized-in-string-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-parentized-in-string-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-parentized-in-string-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-parentized-in-string-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-parentized-in-string-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-parentized-in-string-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-length))

;;;###autoload
(defun ar-parentize-parentized-in-string-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-parentize))

;;;###autoload
(defun ar-quote-parentized-in-string-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-quote))

;;;###autoload
(defun ar-separate-parentized-in-string-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-separate))

;;;###autoload
(defun ar-show-parentized-in-string-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-show))

;;;###autoload
(defun ar-singlequote-parentized-in-string-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-parentized-in-string-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-parentized-in-string-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-parentized-in-string-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-sort))

;;;###autoload
(defun ar-trim-parentized-in-string-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-parentized-in-string-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-parentized-in-string-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-parentized-in-string-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-parentized-in-string-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing STRING. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'string 'ar-th-whitespace))

;;;###autoload
(defun ar-parentized-in-buffer-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th))

;;;###autoload
(defun ar-greaterangle-parentized-in-buffer-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-greaterangle))

;;;###autoload
(defun ar-lesserangle-parentized-in-buffer-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-lesserangle))

;;;###autoload
(defun ar-backslash-parentized-in-buffer-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-backslash))

;;;###autoload
(defun ar-backtick-parentized-in-buffer-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-backtick))

;;;###autoload
(defun ar-beg-parentized-in-buffer-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-beg))

;;;###autoload
(defun ar-blok-parentized-in-buffer-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-blok))

;;;###autoload
(defun ar-bounds-parentized-in-buffer-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-bounds))

;;;###autoload
(defun ar-brace-parentized-in-buffer-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-brace))

;;;###autoload
(defun ar-bracket-parentized-in-buffer-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-bracket))

;;;###autoload
(defun ar-commatize-parentized-in-buffer-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-commatize))

;;;###autoload
(defun ar-comment-parentized-in-buffer-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-comment))

;;;###autoload
(defun ar-dollar-parentized-in-buffer-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-dollar))

;;;###autoload
(defun ar-double-backslash-parentized-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-double-backslash))

;;;###autoload
(defun ar-doublequote-parentized-in-buffer-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-doublequote))

;;;###autoload
(defun ar-doubleslash-parentized-in-buffer-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-doubleslash))

;;;###autoload
(defun ar-double-backslash-paren-parentized-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-double-backslash-paren))

;;;###autoload
(defun ar-end-parentized-in-buffer-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-end))

;;;###autoload
(defun ar-escape-parentized-in-buffer-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-escape))

;;;###autoload
(defun ar-hide-parentized-in-buffer-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-hide))

;;;###autoload
(defun ar-hide-show-parentized-in-buffer-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-hide-show))

;;;###autoload
(defun ar-hyphen-parentized-in-buffer-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-hyphen))

;;;###autoload
(defun ar-kill-parentized-in-buffer-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-kill))

;;;###autoload
(defun ar-left-right-singlequote-parentized-in-buffer-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-left-right-singlequote))

;;;###autoload
(defun ar-length-parentized-in-buffer-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-length))

;;;###autoload
(defun ar-parentize-parentized-in-buffer-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-parentize))

;;;###autoload
(defun ar-quote-parentized-in-buffer-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-quote))

;;;###autoload
(defun ar-separate-parentized-in-buffer-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-separate))

;;;###autoload
(defun ar-show-parentized-in-buffer-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-show))

;;;###autoload
(defun ar-singlequote-parentized-in-buffer-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-singlequote))

;;;###autoload
(defun ar-slash-parentized-in-buffer-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-slash))

;;;###autoload
(defun ar-slashparen-parentized-in-buffer-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-slashparen))

;;;###autoload
(defun ar-sort-parentized-in-buffer-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-sort))

;;;###autoload
(defun ar-trim-parentized-in-buffer-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-trim))

;;;###autoload
(defun ar-trim-left-parentized-in-buffer-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-trim-left))

;;;###autoload
(defun ar-trim-right-parentized-in-buffer-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-trim-right))

;;;###autoload
(defun ar-underscore-parentized-in-buffer-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-underscore))

;;;###autoload
(defun ar-whitespace-parentized-in-buffer-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'buffer 'ar-th-whitespace))

(provide 'ar-thingatpt-delimited-list-in-counts-list)
;;;thing-delimited-list-in-counts-list.el ends here
