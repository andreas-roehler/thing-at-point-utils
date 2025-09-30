;;; thing-rest-list-in-region-only.el --- thing-in-thing functions -*- lexical-binding: t; -*-
;; Built by ar-thing-in-thing-anlegen-intern ar-atpt-rest-list ar-atpt-region-only


;; Copyright (C) 2010-2025 Andreas Röhler, unless
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

(defun ar-greateranglednested-in-region-atpt ()
  "Employ actions of  at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th))

(defun ar-greaterangle-greateranglednested-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-greaterangle))

(defun ar-lesserangle-greateranglednested-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-lesserangle))

(defun ar-backslash-greateranglednested-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-backslash))

(defun ar-colon-greateranglednested-in-region-atpt ()
  "Employ actions of COLON- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-colon))

(defun ar-beg-greateranglednested-in-region-atpt ()
  "Employ actions of BEG- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-beg))

(defun ar-blok-greateranglednested-in-region-atpt ()
  "Employ actions of BLOK- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-blok))

(defun ar-bounds-greateranglednested-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-bounds))

(defun ar-brace-greateranglednested-in-region-atpt ()
  "Employ actions of BRACE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-brace))

(defun ar-bracket-greateranglednested-in-region-atpt ()
  "Employ actions of BRACKET- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-bracket))

(defun ar-commatize-greateranglednested-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-commatize))

(defun ar-comment-greateranglednested-in-region-atpt ()
  "Employ actions of COMMENT- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-comment))

(defun ar-dollar-greateranglednested-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-dollar))

(defun ar-doublebackslash-greateranglednested-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-greateranglednested-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-doublebacktick))

(defun ar-doublequote-greateranglednested-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-doublequote))

(defun ar-doubleslash-greateranglednested-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-greateranglednested-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-doublebackslashparen))

(defun ar-end-greateranglednested-in-region-atpt ()
  "Employ actions of END- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-end))

(defun ar-escape-greateranglednested-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-escape))

(defun ar-hide-greateranglednested-in-region-atpt ()
  "Employ actions of HIDE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-hide))

(defun ar-hide-show-greateranglednested-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-hide-show))

(defun ar-hyphen-greateranglednested-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-hyphen))

(defun ar-kill-greateranglednested-in-region-atpt ()
  "Employ actions of KILL- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-kill))

(defun ar-curvedsinglequote-greateranglednested-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-curvedsinglequote))

(defun ar-length-greateranglednested-in-region-atpt ()
  "Employ actions of LENGTH- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-length))

(defun ar-parentize-greateranglednested-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-parentize))

(defun ar-quote-greateranglednested-in-region-atpt ()
  "Employ actions of QUOTE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-quote))

(defun ar-separate-greateranglednested-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-separate))

(defun ar-show-greateranglednested-in-region-atpt ()
  "Employ actions of SHOW- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-show))

(defun ar-singlequote-greateranglednested-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-singlequote))

(defun ar-slash-greateranglednested-in-region-atpt ()
  "Employ actions of SLASH- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-slash))

(defun ar-star-greateranglednested-in-region-atpt ()
  "Employ actions of STAR- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-star))

(defun ar-slashparen-greateranglednested-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-slashparen))

(defun ar-sort-greateranglednested-in-region-atpt ()
  "Employ actions of SORT- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-sort))

(defun ar-trim-greateranglednested-in-region-atpt ()
  "Employ actions of TRIM- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-trim))

(defun ar-trim-left-greateranglednested-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-trim-left))

(defun ar-trim-right-greateranglednested-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-trim-right))

(defun ar-underscore-greateranglednested-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-underscore))

(defun ar-whitespace-greateranglednested-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-whitespace))

(defun ar-lesseranglednested-in-region-atpt ()
  "Employ actions of  at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th))

(defun ar-greaterangle-lesseranglednested-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-greaterangle))

(defun ar-lesserangle-lesseranglednested-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-lesserangle))

(defun ar-backslash-lesseranglednested-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-backslash))

(defun ar-colon-lesseranglednested-in-region-atpt ()
  "Employ actions of COLON- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-colon))

(defun ar-beg-lesseranglednested-in-region-atpt ()
  "Employ actions of BEG- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-beg))

(defun ar-blok-lesseranglednested-in-region-atpt ()
  "Employ actions of BLOK- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-blok))

(defun ar-bounds-lesseranglednested-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-bounds))

(defun ar-brace-lesseranglednested-in-region-atpt ()
  "Employ actions of BRACE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-brace))

(defun ar-bracket-lesseranglednested-in-region-atpt ()
  "Employ actions of BRACKET- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-bracket))

(defun ar-commatize-lesseranglednested-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-commatize))

(defun ar-comment-lesseranglednested-in-region-atpt ()
  "Employ actions of COMMENT- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-comment))

(defun ar-dollar-lesseranglednested-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-dollar))

(defun ar-doublebackslash-lesseranglednested-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-lesseranglednested-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-doublebacktick))

(defun ar-doublequote-lesseranglednested-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-doublequote))

(defun ar-doubleslash-lesseranglednested-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-lesseranglednested-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-doublebackslashparen))

(defun ar-end-lesseranglednested-in-region-atpt ()
  "Employ actions of END- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-end))

(defun ar-escape-lesseranglednested-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-escape))

(defun ar-hide-lesseranglednested-in-region-atpt ()
  "Employ actions of HIDE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-hide))

(defun ar-hide-show-lesseranglednested-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-hide-show))

(defun ar-hyphen-lesseranglednested-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-hyphen))

(defun ar-kill-lesseranglednested-in-region-atpt ()
  "Employ actions of KILL- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-kill))

(defun ar-curvedsinglequote-lesseranglednested-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-curvedsinglequote))

(defun ar-length-lesseranglednested-in-region-atpt ()
  "Employ actions of LENGTH- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-length))

(defun ar-parentize-lesseranglednested-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-parentize))

(defun ar-quote-lesseranglednested-in-region-atpt ()
  "Employ actions of QUOTE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-quote))

(defun ar-separate-lesseranglednested-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-separate))

(defun ar-show-lesseranglednested-in-region-atpt ()
  "Employ actions of SHOW- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-show))

(defun ar-singlequote-lesseranglednested-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-singlequote))

(defun ar-slash-lesseranglednested-in-region-atpt ()
  "Employ actions of SLASH- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-slash))

(defun ar-star-lesseranglednested-in-region-atpt ()
  "Employ actions of STAR- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-star))

(defun ar-slashparen-lesseranglednested-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-slashparen))

(defun ar-sort-lesseranglednested-in-region-atpt ()
  "Employ actions of SORT- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-sort))

(defun ar-trim-lesseranglednested-in-region-atpt ()
  "Employ actions of TRIM- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-trim))

(defun ar-trim-left-lesseranglednested-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-trim-left))

(defun ar-trim-right-lesseranglednested-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-trim-right))

(defun ar-underscore-lesseranglednested-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-underscore))

(defun ar-whitespace-lesseranglednested-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-whitespace))

(defun ar-buffer-in-region-atpt ()
  "Employ actions of  at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th))

(defun ar-greaterangle-buffer-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-greaterangle))

(defun ar-lesserangle-buffer-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-lesserangle))

(defun ar-backslash-buffer-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-backslash))

(defun ar-colon-buffer-in-region-atpt ()
  "Employ actions of COLON- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-colon))

(defun ar-beg-buffer-in-region-atpt ()
  "Employ actions of BEG- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-beg))

(defun ar-blok-buffer-in-region-atpt ()
  "Employ actions of BLOK- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-blok))

(defun ar-bounds-buffer-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-bounds))

(defun ar-brace-buffer-in-region-atpt ()
  "Employ actions of BRACE- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-brace))

(defun ar-bracket-buffer-in-region-atpt ()
  "Employ actions of BRACKET- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-bracket))

(defun ar-commatize-buffer-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-commatize))

(defun ar-comment-buffer-in-region-atpt ()
  "Employ actions of COMMENT- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-comment))

(defun ar-dollar-buffer-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-dollar))

(defun ar-doublebackslash-buffer-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-buffer-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-doublebacktick))

(defun ar-doublequote-buffer-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-doublequote))

(defun ar-doubleslash-buffer-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-buffer-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-doublebackslashparen))

(defun ar-end-buffer-in-region-atpt ()
  "Employ actions of END- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-end))

(defun ar-escape-buffer-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-escape))

(defun ar-hide-buffer-in-region-atpt ()
  "Employ actions of HIDE- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-hide))

(defun ar-hide-show-buffer-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-hide-show))

(defun ar-hyphen-buffer-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-hyphen))

(defun ar-kill-buffer-in-region-atpt ()
  "Employ actions of KILL- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-kill))

(defun ar-curvedsinglequote-buffer-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-curvedsinglequote))

(defun ar-length-buffer-in-region-atpt ()
  "Employ actions of LENGTH- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-length))

(defun ar-parentize-buffer-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-parentize))

(defun ar-quote-buffer-in-region-atpt ()
  "Employ actions of QUOTE- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-quote))

(defun ar-separate-buffer-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-separate))

(defun ar-show-buffer-in-region-atpt ()
  "Employ actions of SHOW- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-show))

(defun ar-singlequote-buffer-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-singlequote))

(defun ar-slash-buffer-in-region-atpt ()
  "Employ actions of SLASH- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-slash))

(defun ar-star-buffer-in-region-atpt ()
  "Employ actions of STAR- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-star))

(defun ar-slashparen-buffer-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-slashparen))

(defun ar-sort-buffer-in-region-atpt ()
  "Employ actions of SORT- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-sort))

(defun ar-trim-buffer-in-region-atpt ()
  "Employ actions of TRIM- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-trim))

(defun ar-trim-left-buffer-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-trim-left))

(defun ar-trim-right-buffer-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-trim-right))

(defun ar-underscore-buffer-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-underscore))

(defun ar-whitespace-buffer-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of BUFFER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'buffer 'region 'ar-th-whitespace))

(defun ar-char-in-region-atpt ()
  "Employ actions of  at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th))

(defun ar-greaterangle-char-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-greaterangle))

(defun ar-lesserangle-char-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-lesserangle))

(defun ar-backslash-char-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-backslash))

(defun ar-colon-char-in-region-atpt ()
  "Employ actions of COLON- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-colon))

(defun ar-beg-char-in-region-atpt ()
  "Employ actions of BEG- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-beg))

(defun ar-blok-char-in-region-atpt ()
  "Employ actions of BLOK- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-blok))

(defun ar-bounds-char-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-bounds))

(defun ar-brace-char-in-region-atpt ()
  "Employ actions of BRACE- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-brace))

(defun ar-bracket-char-in-region-atpt ()
  "Employ actions of BRACKET- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-bracket))

(defun ar-commatize-char-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-commatize))

(defun ar-comment-char-in-region-atpt ()
  "Employ actions of COMMENT- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-comment))

(defun ar-dollar-char-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-dollar))

(defun ar-doublebackslash-char-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-char-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-doublebacktick))

(defun ar-doublequote-char-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-doublequote))

(defun ar-doubleslash-char-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-char-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-doublebackslashparen))

(defun ar-end-char-in-region-atpt ()
  "Employ actions of END- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-end))

(defun ar-escape-char-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-escape))

(defun ar-hide-char-in-region-atpt ()
  "Employ actions of HIDE- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-hide))

(defun ar-hide-show-char-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-hide-show))

(defun ar-hyphen-char-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-hyphen))

(defun ar-kill-char-in-region-atpt ()
  "Employ actions of KILL- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-kill))

(defun ar-curvedsinglequote-char-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-curvedsinglequote))

(defun ar-length-char-in-region-atpt ()
  "Employ actions of LENGTH- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-length))

(defun ar-parentize-char-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-parentize))

(defun ar-quote-char-in-region-atpt ()
  "Employ actions of QUOTE- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-quote))

(defun ar-separate-char-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-separate))

(defun ar-show-char-in-region-atpt ()
  "Employ actions of SHOW- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-show))

(defun ar-singlequote-char-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-singlequote))

(defun ar-slash-char-in-region-atpt ()
  "Employ actions of SLASH- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-slash))

(defun ar-star-char-in-region-atpt ()
  "Employ actions of STAR- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-star))

(defun ar-slashparen-char-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-slashparen))

(defun ar-sort-char-in-region-atpt ()
  "Employ actions of SORT- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-sort))

(defun ar-trim-char-in-region-atpt ()
  "Employ actions of TRIM- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-trim))

(defun ar-trim-left-char-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-trim-left))

(defun ar-trim-right-char-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-trim-right))

(defun ar-underscore-char-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-underscore))

(defun ar-whitespace-char-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of CHAR residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'char 'region 'ar-th-whitespace))

(defun ar-comment-in-region-atpt ()
  "Employ actions of  at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th))

(defun ar-greaterangle-comment-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-greaterangle))

(defun ar-lesserangle-comment-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-lesserangle))

(defun ar-backslash-comment-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-backslash))

(defun ar-colon-comment-in-region-atpt ()
  "Employ actions of COLON- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-colon))

(defun ar-beg-comment-in-region-atpt ()
  "Employ actions of BEG- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-beg))

(defun ar-blok-comment-in-region-atpt ()
  "Employ actions of BLOK- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-blok))

(defun ar-bounds-comment-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-bounds))

(defun ar-brace-comment-in-region-atpt ()
  "Employ actions of BRACE- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-brace))

(defun ar-bracket-comment-in-region-atpt ()
  "Employ actions of BRACKET- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-bracket))

(defun ar-commatize-comment-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-commatize))

(defun ar-comment-comment-in-region-atpt ()
  "Employ actions of COMMENT- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-comment))

(defun ar-dollar-comment-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-dollar))

(defun ar-doublebackslash-comment-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-comment-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-doublebacktick))

(defun ar-doublequote-comment-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-doublequote))

(defun ar-doubleslash-comment-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-comment-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-doublebackslashparen))

(defun ar-end-comment-in-region-atpt ()
  "Employ actions of END- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-end))

(defun ar-escape-comment-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-escape))

(defun ar-hide-comment-in-region-atpt ()
  "Employ actions of HIDE- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-hide))

(defun ar-hide-show-comment-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-hide-show))

(defun ar-hyphen-comment-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-hyphen))

(defun ar-kill-comment-in-region-atpt ()
  "Employ actions of KILL- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-kill))

(defun ar-curvedsinglequote-comment-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-curvedsinglequote))

(defun ar-length-comment-in-region-atpt ()
  "Employ actions of LENGTH- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-length))

(defun ar-parentize-comment-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-parentize))

(defun ar-quote-comment-in-region-atpt ()
  "Employ actions of QUOTE- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-quote))

(defun ar-separate-comment-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-separate))

(defun ar-show-comment-in-region-atpt ()
  "Employ actions of SHOW- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-show))

(defun ar-singlequote-comment-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-singlequote))

(defun ar-slash-comment-in-region-atpt ()
  "Employ actions of SLASH- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-slash))

(defun ar-star-comment-in-region-atpt ()
  "Employ actions of STAR- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-star))

(defun ar-slashparen-comment-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-slashparen))

(defun ar-sort-comment-in-region-atpt ()
  "Employ actions of SORT- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-sort))

(defun ar-trim-comment-in-region-atpt ()
  "Employ actions of TRIM- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-trim))

(defun ar-trim-left-comment-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-trim-left))

(defun ar-trim-right-comment-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-trim-right))

(defun ar-underscore-comment-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-underscore))

(defun ar-whitespace-comment-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of COMMENT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'comment 'region 'ar-th-whitespace))

(defun ar-csv-in-region-atpt ()
  "Employ actions of  at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th))

(defun ar-greaterangle-csv-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-greaterangle))

(defun ar-lesserangle-csv-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-lesserangle))

(defun ar-backslash-csv-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-backslash))

(defun ar-colon-csv-in-region-atpt ()
  "Employ actions of COLON- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-colon))

(defun ar-beg-csv-in-region-atpt ()
  "Employ actions of BEG- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-beg))

(defun ar-blok-csv-in-region-atpt ()
  "Employ actions of BLOK- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-blok))

(defun ar-bounds-csv-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-bounds))

(defun ar-brace-csv-in-region-atpt ()
  "Employ actions of BRACE- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-brace))

(defun ar-bracket-csv-in-region-atpt ()
  "Employ actions of BRACKET- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-bracket))

(defun ar-commatize-csv-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-commatize))

(defun ar-comment-csv-in-region-atpt ()
  "Employ actions of COMMENT- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-comment))

(defun ar-dollar-csv-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-dollar))

(defun ar-doublebackslash-csv-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-csv-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-doublebacktick))

(defun ar-doublequote-csv-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-doublequote))

(defun ar-doubleslash-csv-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-csv-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-doublebackslashparen))

(defun ar-end-csv-in-region-atpt ()
  "Employ actions of END- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-end))

(defun ar-escape-csv-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-escape))

(defun ar-hide-csv-in-region-atpt ()
  "Employ actions of HIDE- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-hide))

(defun ar-hide-show-csv-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-hide-show))

(defun ar-hyphen-csv-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-hyphen))

(defun ar-kill-csv-in-region-atpt ()
  "Employ actions of KILL- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-kill))

(defun ar-curvedsinglequote-csv-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-curvedsinglequote))

(defun ar-length-csv-in-region-atpt ()
  "Employ actions of LENGTH- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-length))

(defun ar-parentize-csv-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-parentize))

(defun ar-quote-csv-in-region-atpt ()
  "Employ actions of QUOTE- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-quote))

(defun ar-separate-csv-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-separate))

(defun ar-show-csv-in-region-atpt ()
  "Employ actions of SHOW- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-show))

(defun ar-singlequote-csv-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-singlequote))

(defun ar-slash-csv-in-region-atpt ()
  "Employ actions of SLASH- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-slash))

(defun ar-star-csv-in-region-atpt ()
  "Employ actions of STAR- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-star))

(defun ar-slashparen-csv-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-slashparen))

(defun ar-sort-csv-in-region-atpt ()
  "Employ actions of SORT- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-sort))

(defun ar-trim-csv-in-region-atpt ()
  "Employ actions of TRIM- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-trim))

(defun ar-trim-left-csv-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-trim-left))

(defun ar-trim-right-csv-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-trim-right))

(defun ar-underscore-csv-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-underscore))

(defun ar-whitespace-csv-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of CSV residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'csv 'region 'ar-th-whitespace))

(defun ar-date-in-region-atpt ()
  "Employ actions of  at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th))

(defun ar-greaterangle-date-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-greaterangle))

(defun ar-lesserangle-date-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-lesserangle))

(defun ar-backslash-date-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-backslash))

(defun ar-colon-date-in-region-atpt ()
  "Employ actions of COLON- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-colon))

(defun ar-beg-date-in-region-atpt ()
  "Employ actions of BEG- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-beg))

(defun ar-blok-date-in-region-atpt ()
  "Employ actions of BLOK- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-blok))

(defun ar-bounds-date-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-bounds))

(defun ar-brace-date-in-region-atpt ()
  "Employ actions of BRACE- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-brace))

(defun ar-bracket-date-in-region-atpt ()
  "Employ actions of BRACKET- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-bracket))

(defun ar-commatize-date-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-commatize))

(defun ar-comment-date-in-region-atpt ()
  "Employ actions of COMMENT- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-comment))

(defun ar-dollar-date-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-dollar))

(defun ar-doublebackslash-date-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-date-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-doublebacktick))

(defun ar-doublequote-date-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-doublequote))

(defun ar-doubleslash-date-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-date-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-doublebackslashparen))

(defun ar-end-date-in-region-atpt ()
  "Employ actions of END- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-end))

(defun ar-escape-date-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-escape))

(defun ar-hide-date-in-region-atpt ()
  "Employ actions of HIDE- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-hide))

(defun ar-hide-show-date-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-hide-show))

(defun ar-hyphen-date-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-hyphen))

(defun ar-kill-date-in-region-atpt ()
  "Employ actions of KILL- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-kill))

(defun ar-curvedsinglequote-date-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-curvedsinglequote))

(defun ar-length-date-in-region-atpt ()
  "Employ actions of LENGTH- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-length))

(defun ar-parentize-date-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-parentize))

(defun ar-quote-date-in-region-atpt ()
  "Employ actions of QUOTE- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-quote))

(defun ar-separate-date-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-separate))

(defun ar-show-date-in-region-atpt ()
  "Employ actions of SHOW- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-show))

(defun ar-singlequote-date-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-singlequote))

(defun ar-slash-date-in-region-atpt ()
  "Employ actions of SLASH- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-slash))

(defun ar-star-date-in-region-atpt ()
  "Employ actions of STAR- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-star))

(defun ar-slashparen-date-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-slashparen))

(defun ar-sort-date-in-region-atpt ()
  "Employ actions of SORT- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-sort))

(defun ar-trim-date-in-region-atpt ()
  "Employ actions of TRIM- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-trim))

(defun ar-trim-left-date-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-trim-left))

(defun ar-trim-right-date-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-trim-right))

(defun ar-underscore-date-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-underscore))

(defun ar-whitespace-date-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of DATE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'date 'region 'ar-th-whitespace))

(defun ar-email-in-region-atpt ()
  "Employ actions of  at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th))

(defun ar-greaterangle-email-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-greaterangle))

(defun ar-lesserangle-email-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-lesserangle))

(defun ar-backslash-email-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-backslash))

(defun ar-colon-email-in-region-atpt ()
  "Employ actions of COLON- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-colon))

(defun ar-beg-email-in-region-atpt ()
  "Employ actions of BEG- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-beg))

(defun ar-blok-email-in-region-atpt ()
  "Employ actions of BLOK- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-blok))

(defun ar-bounds-email-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-bounds))

(defun ar-brace-email-in-region-atpt ()
  "Employ actions of BRACE- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-brace))

(defun ar-bracket-email-in-region-atpt ()
  "Employ actions of BRACKET- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-bracket))

(defun ar-commatize-email-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-commatize))

(defun ar-comment-email-in-region-atpt ()
  "Employ actions of COMMENT- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-comment))

(defun ar-dollar-email-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-dollar))

(defun ar-doublebackslash-email-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-email-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-doublebacktick))

(defun ar-doublequote-email-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-doublequote))

(defun ar-doubleslash-email-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-email-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-doublebackslashparen))

(defun ar-end-email-in-region-atpt ()
  "Employ actions of END- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-end))

(defun ar-escape-email-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-escape))

(defun ar-hide-email-in-region-atpt ()
  "Employ actions of HIDE- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-hide))

(defun ar-hide-show-email-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-hide-show))

(defun ar-hyphen-email-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-hyphen))

(defun ar-kill-email-in-region-atpt ()
  "Employ actions of KILL- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-kill))

(defun ar-curvedsinglequote-email-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-curvedsinglequote))

(defun ar-length-email-in-region-atpt ()
  "Employ actions of LENGTH- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-length))

(defun ar-parentize-email-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-parentize))

(defun ar-quote-email-in-region-atpt ()
  "Employ actions of QUOTE- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-quote))

(defun ar-separate-email-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-separate))

(defun ar-show-email-in-region-atpt ()
  "Employ actions of SHOW- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-show))

(defun ar-singlequote-email-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-singlequote))

(defun ar-slash-email-in-region-atpt ()
  "Employ actions of SLASH- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-slash))

(defun ar-star-email-in-region-atpt ()
  "Employ actions of STAR- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-star))

(defun ar-slashparen-email-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-slashparen))

(defun ar-sort-email-in-region-atpt ()
  "Employ actions of SORT- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-sort))

(defun ar-trim-email-in-region-atpt ()
  "Employ actions of TRIM- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-trim))

(defun ar-trim-left-email-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-trim-left))

(defun ar-trim-right-email-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-trim-right))

(defun ar-underscore-email-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-underscore))

(defun ar-whitespace-email-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of EMAIL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'email 'region 'ar-th-whitespace))

(defun ar-filename-in-region-atpt ()
  "Employ actions of  at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th))

(defun ar-greaterangle-filename-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-greaterangle))

(defun ar-lesserangle-filename-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-lesserangle))

(defun ar-backslash-filename-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-backslash))

(defun ar-colon-filename-in-region-atpt ()
  "Employ actions of COLON- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-colon))

(defun ar-beg-filename-in-region-atpt ()
  "Employ actions of BEG- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-beg))

(defun ar-blok-filename-in-region-atpt ()
  "Employ actions of BLOK- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-blok))

(defun ar-bounds-filename-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-bounds))

(defun ar-brace-filename-in-region-atpt ()
  "Employ actions of BRACE- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-brace))

(defun ar-bracket-filename-in-region-atpt ()
  "Employ actions of BRACKET- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-bracket))

(defun ar-commatize-filename-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-commatize))

(defun ar-comment-filename-in-region-atpt ()
  "Employ actions of COMMENT- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-comment))

(defun ar-dollar-filename-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-dollar))

(defun ar-doublebackslash-filename-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-filename-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-doublebacktick))

(defun ar-doublequote-filename-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-doublequote))

(defun ar-doubleslash-filename-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-filename-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-doublebackslashparen))

(defun ar-end-filename-in-region-atpt ()
  "Employ actions of END- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-end))

(defun ar-escape-filename-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-escape))

(defun ar-hide-filename-in-region-atpt ()
  "Employ actions of HIDE- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-hide))

(defun ar-hide-show-filename-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-hide-show))

(defun ar-hyphen-filename-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-hyphen))

(defun ar-kill-filename-in-region-atpt ()
  "Employ actions of KILL- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-kill))

(defun ar-curvedsinglequote-filename-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-curvedsinglequote))

(defun ar-length-filename-in-region-atpt ()
  "Employ actions of LENGTH- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-length))

(defun ar-parentize-filename-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-parentize))

(defun ar-quote-filename-in-region-atpt ()
  "Employ actions of QUOTE- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-quote))

(defun ar-separate-filename-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-separate))

(defun ar-show-filename-in-region-atpt ()
  "Employ actions of SHOW- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-show))

(defun ar-singlequote-filename-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-singlequote))

(defun ar-slash-filename-in-region-atpt ()
  "Employ actions of SLASH- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-slash))

(defun ar-star-filename-in-region-atpt ()
  "Employ actions of STAR- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-star))

(defun ar-slashparen-filename-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-slashparen))

(defun ar-sort-filename-in-region-atpt ()
  "Employ actions of SORT- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-sort))

(defun ar-trim-filename-in-region-atpt ()
  "Employ actions of TRIM- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-trim))

(defun ar-trim-left-filename-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-trim-left))

(defun ar-trim-right-filename-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-trim-right))

(defun ar-underscore-filename-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-underscore))

(defun ar-whitespace-filename-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of FILENAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filename 'region 'ar-th-whitespace))

(defun ar-filenamenondirectory-in-region-atpt ()
  "Employ actions of  at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th))

(defun ar-greaterangle-filenamenondirectory-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-greaterangle))

(defun ar-lesserangle-filenamenondirectory-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-lesserangle))

(defun ar-backslash-filenamenondirectory-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-backslash))

(defun ar-colon-filenamenondirectory-in-region-atpt ()
  "Employ actions of COLON- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-colon))

(defun ar-beg-filenamenondirectory-in-region-atpt ()
  "Employ actions of BEG- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-beg))

(defun ar-blok-filenamenondirectory-in-region-atpt ()
  "Employ actions of BLOK- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-blok))

(defun ar-bounds-filenamenondirectory-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-bounds))

(defun ar-brace-filenamenondirectory-in-region-atpt ()
  "Employ actions of BRACE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-brace))

(defun ar-bracket-filenamenondirectory-in-region-atpt ()
  "Employ actions of BRACKET- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-bracket))

(defun ar-commatize-filenamenondirectory-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-commatize))

(defun ar-comment-filenamenondirectory-in-region-atpt ()
  "Employ actions of COMMENT- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-comment))

(defun ar-dollar-filenamenondirectory-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-dollar))

(defun ar-doublebackslash-filenamenondirectory-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-filenamenondirectory-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-doublebacktick))

(defun ar-doublequote-filenamenondirectory-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-doublequote))

(defun ar-doubleslash-filenamenondirectory-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-filenamenondirectory-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-doublebackslashparen))

(defun ar-end-filenamenondirectory-in-region-atpt ()
  "Employ actions of END- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-end))

(defun ar-escape-filenamenondirectory-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-escape))

(defun ar-hide-filenamenondirectory-in-region-atpt ()
  "Employ actions of HIDE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-hide))

(defun ar-hide-show-filenamenondirectory-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-hide-show))

(defun ar-hyphen-filenamenondirectory-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-hyphen))

(defun ar-kill-filenamenondirectory-in-region-atpt ()
  "Employ actions of KILL- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-kill))

(defun ar-curvedsinglequote-filenamenondirectory-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-curvedsinglequote))

(defun ar-length-filenamenondirectory-in-region-atpt ()
  "Employ actions of LENGTH- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-length))

(defun ar-parentize-filenamenondirectory-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-parentize))

(defun ar-quote-filenamenondirectory-in-region-atpt ()
  "Employ actions of QUOTE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-quote))

(defun ar-separate-filenamenondirectory-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-separate))

(defun ar-show-filenamenondirectory-in-region-atpt ()
  "Employ actions of SHOW- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-show))

(defun ar-singlequote-filenamenondirectory-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-singlequote))

(defun ar-slash-filenamenondirectory-in-region-atpt ()
  "Employ actions of SLASH- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-slash))

(defun ar-star-filenamenondirectory-in-region-atpt ()
  "Employ actions of STAR- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-star))

(defun ar-slashparen-filenamenondirectory-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-slashparen))

(defun ar-sort-filenamenondirectory-in-region-atpt ()
  "Employ actions of SORT- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-sort))

(defun ar-trim-filenamenondirectory-in-region-atpt ()
  "Employ actions of TRIM- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-trim))

(defun ar-trim-left-filenamenondirectory-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-trim-left))

(defun ar-trim-right-filenamenondirectory-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-trim-right))

(defun ar-underscore-filenamenondirectory-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-underscore))

(defun ar-whitespace-filenamenondirectory-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-whitespace))

(defun ar-float-in-region-atpt ()
  "Employ actions of  at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th))

(defun ar-greaterangle-float-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-greaterangle))

(defun ar-lesserangle-float-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-lesserangle))

(defun ar-backslash-float-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-backslash))

(defun ar-colon-float-in-region-atpt ()
  "Employ actions of COLON- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-colon))

(defun ar-beg-float-in-region-atpt ()
  "Employ actions of BEG- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-beg))

(defun ar-blok-float-in-region-atpt ()
  "Employ actions of BLOK- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-blok))

(defun ar-bounds-float-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-bounds))

(defun ar-brace-float-in-region-atpt ()
  "Employ actions of BRACE- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-brace))

(defun ar-bracket-float-in-region-atpt ()
  "Employ actions of BRACKET- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-bracket))

(defun ar-commatize-float-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-commatize))

(defun ar-comment-float-in-region-atpt ()
  "Employ actions of COMMENT- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-comment))

(defun ar-dollar-float-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-dollar))

(defun ar-doublebackslash-float-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-float-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-doublebacktick))

(defun ar-doublequote-float-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-doublequote))

(defun ar-doubleslash-float-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-float-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-doublebackslashparen))

(defun ar-end-float-in-region-atpt ()
  "Employ actions of END- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-end))

(defun ar-escape-float-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-escape))

(defun ar-hide-float-in-region-atpt ()
  "Employ actions of HIDE- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-hide))

(defun ar-hide-show-float-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-hide-show))

(defun ar-hyphen-float-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-hyphen))

(defun ar-kill-float-in-region-atpt ()
  "Employ actions of KILL- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-kill))

(defun ar-curvedsinglequote-float-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-curvedsinglequote))

(defun ar-length-float-in-region-atpt ()
  "Employ actions of LENGTH- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-length))

(defun ar-parentize-float-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-parentize))

(defun ar-quote-float-in-region-atpt ()
  "Employ actions of QUOTE- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-quote))

(defun ar-separate-float-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-separate))

(defun ar-show-float-in-region-atpt ()
  "Employ actions of SHOW- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-show))

(defun ar-singlequote-float-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-singlequote))

(defun ar-slash-float-in-region-atpt ()
  "Employ actions of SLASH- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-slash))

(defun ar-star-float-in-region-atpt ()
  "Employ actions of STAR- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-star))

(defun ar-slashparen-float-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-slashparen))

(defun ar-sort-float-in-region-atpt ()
  "Employ actions of SORT- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-sort))

(defun ar-trim-float-in-region-atpt ()
  "Employ actions of TRIM- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-trim))

(defun ar-trim-left-float-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-trim-left))

(defun ar-trim-right-float-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-trim-right))

(defun ar-underscore-float-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-underscore))

(defun ar-whitespace-float-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of FLOAT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'float 'region 'ar-th-whitespace))

(defun ar-function-in-region-atpt ()
  "Employ actions of  at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th))

(defun ar-greaterangle-function-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-greaterangle))

(defun ar-lesserangle-function-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-lesserangle))

(defun ar-backslash-function-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-backslash))

(defun ar-colon-function-in-region-atpt ()
  "Employ actions of COLON- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-colon))

(defun ar-beg-function-in-region-atpt ()
  "Employ actions of BEG- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-beg))

(defun ar-blok-function-in-region-atpt ()
  "Employ actions of BLOK- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-blok))

(defun ar-bounds-function-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-bounds))

(defun ar-brace-function-in-region-atpt ()
  "Employ actions of BRACE- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-brace))

(defun ar-bracket-function-in-region-atpt ()
  "Employ actions of BRACKET- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-bracket))

(defun ar-commatize-function-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-commatize))

(defun ar-comment-function-in-region-atpt ()
  "Employ actions of COMMENT- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-comment))

(defun ar-dollar-function-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-dollar))

(defun ar-doublebackslash-function-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-function-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-doublebacktick))

(defun ar-doublequote-function-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-doublequote))

(defun ar-doubleslash-function-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-function-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-doublebackslashparen))

(defun ar-end-function-in-region-atpt ()
  "Employ actions of END- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-end))

(defun ar-escape-function-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-escape))

(defun ar-hide-function-in-region-atpt ()
  "Employ actions of HIDE- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-hide))

(defun ar-hide-show-function-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-hide-show))

(defun ar-hyphen-function-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-hyphen))

(defun ar-kill-function-in-region-atpt ()
  "Employ actions of KILL- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-kill))

(defun ar-curvedsinglequote-function-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-curvedsinglequote))

(defun ar-length-function-in-region-atpt ()
  "Employ actions of LENGTH- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-length))

(defun ar-parentize-function-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-parentize))

(defun ar-quote-function-in-region-atpt ()
  "Employ actions of QUOTE- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-quote))

(defun ar-separate-function-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-separate))

(defun ar-show-function-in-region-atpt ()
  "Employ actions of SHOW- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-show))

(defun ar-singlequote-function-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-singlequote))

(defun ar-slash-function-in-region-atpt ()
  "Employ actions of SLASH- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-slash))

(defun ar-star-function-in-region-atpt ()
  "Employ actions of STAR- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-star))

(defun ar-slashparen-function-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-slashparen))

(defun ar-sort-function-in-region-atpt ()
  "Employ actions of SORT- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-sort))

(defun ar-trim-function-in-region-atpt ()
  "Employ actions of TRIM- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-trim))

(defun ar-trim-left-function-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-trim-left))

(defun ar-trim-right-function-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-trim-right))

(defun ar-underscore-function-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-underscore))

(defun ar-whitespace-function-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of FUNCTION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'function 'region 'ar-th-whitespace))

(defun ar-ip-in-region-atpt ()
  "Employ actions of  at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th))

(defun ar-greaterangle-ip-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-greaterangle))

(defun ar-lesserangle-ip-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-lesserangle))

(defun ar-backslash-ip-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-backslash))

(defun ar-colon-ip-in-region-atpt ()
  "Employ actions of COLON- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-colon))

(defun ar-beg-ip-in-region-atpt ()
  "Employ actions of BEG- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-beg))

(defun ar-blok-ip-in-region-atpt ()
  "Employ actions of BLOK- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-blok))

(defun ar-bounds-ip-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-bounds))

(defun ar-brace-ip-in-region-atpt ()
  "Employ actions of BRACE- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-brace))

(defun ar-bracket-ip-in-region-atpt ()
  "Employ actions of BRACKET- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-bracket))

(defun ar-commatize-ip-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-commatize))

(defun ar-comment-ip-in-region-atpt ()
  "Employ actions of COMMENT- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-comment))

(defun ar-dollar-ip-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-dollar))

(defun ar-doublebackslash-ip-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-ip-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-doublebacktick))

(defun ar-doublequote-ip-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-doublequote))

(defun ar-doubleslash-ip-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-ip-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-doublebackslashparen))

(defun ar-end-ip-in-region-atpt ()
  "Employ actions of END- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-end))

(defun ar-escape-ip-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-escape))

(defun ar-hide-ip-in-region-atpt ()
  "Employ actions of HIDE- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-hide))

(defun ar-hide-show-ip-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-hide-show))

(defun ar-hyphen-ip-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-hyphen))

(defun ar-kill-ip-in-region-atpt ()
  "Employ actions of KILL- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-kill))

(defun ar-curvedsinglequote-ip-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-curvedsinglequote))

(defun ar-length-ip-in-region-atpt ()
  "Employ actions of LENGTH- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-length))

(defun ar-parentize-ip-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-parentize))

(defun ar-quote-ip-in-region-atpt ()
  "Employ actions of QUOTE- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-quote))

(defun ar-separate-ip-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-separate))

(defun ar-show-ip-in-region-atpt ()
  "Employ actions of SHOW- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-show))

(defun ar-singlequote-ip-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-singlequote))

(defun ar-slash-ip-in-region-atpt ()
  "Employ actions of SLASH- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-slash))

(defun ar-star-ip-in-region-atpt ()
  "Employ actions of STAR- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-star))

(defun ar-slashparen-ip-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-slashparen))

(defun ar-sort-ip-in-region-atpt ()
  "Employ actions of SORT- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-sort))

(defun ar-trim-ip-in-region-atpt ()
  "Employ actions of TRIM- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-trim))

(defun ar-trim-left-ip-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-trim-left))

(defun ar-trim-right-ip-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-trim-right))

(defun ar-underscore-ip-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-underscore))

(defun ar-whitespace-ip-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of IP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'ip 'region 'ar-th-whitespace))

(defun ar-isbn-in-region-atpt ()
  "Employ actions of  at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th))

(defun ar-greaterangle-isbn-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-greaterangle))

(defun ar-lesserangle-isbn-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-lesserangle))

(defun ar-backslash-isbn-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-backslash))

(defun ar-colon-isbn-in-region-atpt ()
  "Employ actions of COLON- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-colon))

(defun ar-beg-isbn-in-region-atpt ()
  "Employ actions of BEG- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-beg))

(defun ar-blok-isbn-in-region-atpt ()
  "Employ actions of BLOK- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-blok))

(defun ar-bounds-isbn-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-bounds))

(defun ar-brace-isbn-in-region-atpt ()
  "Employ actions of BRACE- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-brace))

(defun ar-bracket-isbn-in-region-atpt ()
  "Employ actions of BRACKET- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-bracket))

(defun ar-commatize-isbn-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-commatize))

(defun ar-comment-isbn-in-region-atpt ()
  "Employ actions of COMMENT- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-comment))

(defun ar-dollar-isbn-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-dollar))

(defun ar-doublebackslash-isbn-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-isbn-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-doublebacktick))

(defun ar-doublequote-isbn-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-doublequote))

(defun ar-doubleslash-isbn-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-isbn-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-doublebackslashparen))

(defun ar-end-isbn-in-region-atpt ()
  "Employ actions of END- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-end))

(defun ar-escape-isbn-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-escape))

(defun ar-hide-isbn-in-region-atpt ()
  "Employ actions of HIDE- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-hide))

(defun ar-hide-show-isbn-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-hide-show))

(defun ar-hyphen-isbn-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-hyphen))

(defun ar-kill-isbn-in-region-atpt ()
  "Employ actions of KILL- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-kill))

(defun ar-curvedsinglequote-isbn-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-curvedsinglequote))

(defun ar-length-isbn-in-region-atpt ()
  "Employ actions of LENGTH- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-length))

(defun ar-parentize-isbn-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-parentize))

(defun ar-quote-isbn-in-region-atpt ()
  "Employ actions of QUOTE- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-quote))

(defun ar-separate-isbn-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-separate))

(defun ar-show-isbn-in-region-atpt ()
  "Employ actions of SHOW- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-show))

(defun ar-singlequote-isbn-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-singlequote))

(defun ar-slash-isbn-in-region-atpt ()
  "Employ actions of SLASH- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-slash))

(defun ar-star-isbn-in-region-atpt ()
  "Employ actions of STAR- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-star))

(defun ar-slashparen-isbn-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-slashparen))

(defun ar-sort-isbn-in-region-atpt ()
  "Employ actions of SORT- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-sort))

(defun ar-trim-isbn-in-region-atpt ()
  "Employ actions of TRIM- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-trim))

(defun ar-trim-left-isbn-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-trim-left))

(defun ar-trim-right-isbn-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-trim-right))

(defun ar-underscore-isbn-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-underscore))

(defun ar-whitespace-isbn-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of ISBN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'isbn 'region 'ar-th-whitespace))

(defun ar-line-in-region-atpt ()
  "Employ actions of  at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th))

(defun ar-greaterangle-line-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-greaterangle))

(defun ar-lesserangle-line-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-lesserangle))

(defun ar-backslash-line-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-backslash))

(defun ar-colon-line-in-region-atpt ()
  "Employ actions of COLON- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-colon))

(defun ar-beg-line-in-region-atpt ()
  "Employ actions of BEG- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-beg))

(defun ar-blok-line-in-region-atpt ()
  "Employ actions of BLOK- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-blok))

(defun ar-bounds-line-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-bounds))

(defun ar-brace-line-in-region-atpt ()
  "Employ actions of BRACE- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-brace))

(defun ar-bracket-line-in-region-atpt ()
  "Employ actions of BRACKET- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-bracket))

(defun ar-commatize-line-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-commatize))

(defun ar-comment-line-in-region-atpt ()
  "Employ actions of COMMENT- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-comment))

(defun ar-dollar-line-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-dollar))

(defun ar-doublebackslash-line-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-line-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-doublebacktick))

(defun ar-doublequote-line-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-doublequote))

(defun ar-doubleslash-line-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-line-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-doublebackslashparen))

(defun ar-end-line-in-region-atpt ()
  "Employ actions of END- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-end))

(defun ar-escape-line-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-escape))

(defun ar-hide-line-in-region-atpt ()
  "Employ actions of HIDE- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-hide))

(defun ar-hide-show-line-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-hide-show))

(defun ar-hyphen-line-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-hyphen))

(defun ar-kill-line-in-region-atpt ()
  "Employ actions of KILL- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-kill))

(defun ar-curvedsinglequote-line-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-curvedsinglequote))

(defun ar-length-line-in-region-atpt ()
  "Employ actions of LENGTH- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-length))

(defun ar-parentize-line-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-parentize))

(defun ar-quote-line-in-region-atpt ()
  "Employ actions of QUOTE- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-quote))

(defun ar-separate-line-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-separate))

(defun ar-show-line-in-region-atpt ()
  "Employ actions of SHOW- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-show))

(defun ar-singlequote-line-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-singlequote))

(defun ar-slash-line-in-region-atpt ()
  "Employ actions of SLASH- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-slash))

(defun ar-star-line-in-region-atpt ()
  "Employ actions of STAR- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-star))

(defun ar-slashparen-line-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-slashparen))

(defun ar-sort-line-in-region-atpt ()
  "Employ actions of SORT- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-sort))

(defun ar-trim-line-in-region-atpt ()
  "Employ actions of TRIM- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-trim))

(defun ar-trim-left-line-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-trim-left))

(defun ar-trim-right-line-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-trim-right))

(defun ar-underscore-line-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-underscore))

(defun ar-whitespace-line-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of LINE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'line 'region 'ar-th-whitespace))

(defun ar-list-in-region-atpt ()
  "Employ actions of  at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th))

(defun ar-greaterangle-list-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-greaterangle))

(defun ar-lesserangle-list-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-lesserangle))

(defun ar-backslash-list-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-backslash))

(defun ar-colon-list-in-region-atpt ()
  "Employ actions of COLON- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-colon))

(defun ar-beg-list-in-region-atpt ()
  "Employ actions of BEG- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-beg))

(defun ar-blok-list-in-region-atpt ()
  "Employ actions of BLOK- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-blok))

(defun ar-bounds-list-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-bounds))

(defun ar-brace-list-in-region-atpt ()
  "Employ actions of BRACE- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-brace))

(defun ar-bracket-list-in-region-atpt ()
  "Employ actions of BRACKET- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-bracket))

(defun ar-commatize-list-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-commatize))

(defun ar-comment-list-in-region-atpt ()
  "Employ actions of COMMENT- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-comment))

(defun ar-dollar-list-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-dollar))

(defun ar-doublebackslash-list-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-list-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-doublebacktick))

(defun ar-doublequote-list-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-doublequote))

(defun ar-doubleslash-list-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-list-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-doublebackslashparen))

(defun ar-end-list-in-region-atpt ()
  "Employ actions of END- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-end))

(defun ar-escape-list-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-escape))

(defun ar-hide-list-in-region-atpt ()
  "Employ actions of HIDE- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-hide))

(defun ar-hide-show-list-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-hide-show))

(defun ar-hyphen-list-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-hyphen))

(defun ar-kill-list-in-region-atpt ()
  "Employ actions of KILL- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-kill))

(defun ar-curvedsinglequote-list-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-curvedsinglequote))

(defun ar-length-list-in-region-atpt ()
  "Employ actions of LENGTH- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-length))

(defun ar-parentize-list-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-parentize))

(defun ar-quote-list-in-region-atpt ()
  "Employ actions of QUOTE- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-quote))

(defun ar-separate-list-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-separate))

(defun ar-show-list-in-region-atpt ()
  "Employ actions of SHOW- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-show))

(defun ar-singlequote-list-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-singlequote))

(defun ar-slash-list-in-region-atpt ()
  "Employ actions of SLASH- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-slash))

(defun ar-star-list-in-region-atpt ()
  "Employ actions of STAR- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-star))

(defun ar-slashparen-list-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-slashparen))

(defun ar-sort-list-in-region-atpt ()
  "Employ actions of SORT- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-sort))

(defun ar-trim-list-in-region-atpt ()
  "Employ actions of TRIM- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-trim))

(defun ar-trim-left-list-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-trim-left))

(defun ar-trim-right-list-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-trim-right))

(defun ar-underscore-list-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-underscore))

(defun ar-whitespace-list-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of LIST residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'list 'region 'ar-th-whitespace))

(defun ar-name-in-region-atpt ()
  "Employ actions of  at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th))

(defun ar-greaterangle-name-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-greaterangle))

(defun ar-lesserangle-name-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-lesserangle))

(defun ar-backslash-name-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-backslash))

(defun ar-colon-name-in-region-atpt ()
  "Employ actions of COLON- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-colon))

(defun ar-beg-name-in-region-atpt ()
  "Employ actions of BEG- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-beg))

(defun ar-blok-name-in-region-atpt ()
  "Employ actions of BLOK- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-blok))

(defun ar-bounds-name-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-bounds))

(defun ar-brace-name-in-region-atpt ()
  "Employ actions of BRACE- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-brace))

(defun ar-bracket-name-in-region-atpt ()
  "Employ actions of BRACKET- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-bracket))

(defun ar-commatize-name-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-commatize))

(defun ar-comment-name-in-region-atpt ()
  "Employ actions of COMMENT- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-comment))

(defun ar-dollar-name-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-dollar))

(defun ar-doublebackslash-name-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-name-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-doublebacktick))

(defun ar-doublequote-name-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-doublequote))

(defun ar-doubleslash-name-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-name-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-doublebackslashparen))

(defun ar-end-name-in-region-atpt ()
  "Employ actions of END- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-end))

(defun ar-escape-name-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-escape))

(defun ar-hide-name-in-region-atpt ()
  "Employ actions of HIDE- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-hide))

(defun ar-hide-show-name-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-hide-show))

(defun ar-hyphen-name-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-hyphen))

(defun ar-kill-name-in-region-atpt ()
  "Employ actions of KILL- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-kill))

(defun ar-curvedsinglequote-name-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-curvedsinglequote))

(defun ar-length-name-in-region-atpt ()
  "Employ actions of LENGTH- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-length))

(defun ar-parentize-name-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-parentize))

(defun ar-quote-name-in-region-atpt ()
  "Employ actions of QUOTE- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-quote))

(defun ar-separate-name-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-separate))

(defun ar-show-name-in-region-atpt ()
  "Employ actions of SHOW- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-show))

(defun ar-singlequote-name-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-singlequote))

(defun ar-slash-name-in-region-atpt ()
  "Employ actions of SLASH- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-slash))

(defun ar-star-name-in-region-atpt ()
  "Employ actions of STAR- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-star))

(defun ar-slashparen-name-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-slashparen))

(defun ar-sort-name-in-region-atpt ()
  "Employ actions of SORT- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-sort))

(defun ar-trim-name-in-region-atpt ()
  "Employ actions of TRIM- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-trim))

(defun ar-trim-left-name-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-trim-left))

(defun ar-trim-right-name-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-trim-right))

(defun ar-underscore-name-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-underscore))

(defun ar-whitespace-name-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of NAME residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'name 'region 'ar-th-whitespace))

(defun ar-number-in-region-atpt ()
  "Employ actions of  at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th))

(defun ar-greaterangle-number-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-greaterangle))

(defun ar-lesserangle-number-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-lesserangle))

(defun ar-backslash-number-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-backslash))

(defun ar-colon-number-in-region-atpt ()
  "Employ actions of COLON- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-colon))

(defun ar-beg-number-in-region-atpt ()
  "Employ actions of BEG- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-beg))

(defun ar-blok-number-in-region-atpt ()
  "Employ actions of BLOK- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-blok))

(defun ar-bounds-number-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-bounds))

(defun ar-brace-number-in-region-atpt ()
  "Employ actions of BRACE- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-brace))

(defun ar-bracket-number-in-region-atpt ()
  "Employ actions of BRACKET- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-bracket))

(defun ar-commatize-number-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-commatize))

(defun ar-comment-number-in-region-atpt ()
  "Employ actions of COMMENT- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-comment))

(defun ar-dollar-number-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-dollar))

(defun ar-doublebackslash-number-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-number-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-doublebacktick))

(defun ar-doublequote-number-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-doublequote))

(defun ar-doubleslash-number-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-number-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-doublebackslashparen))

(defun ar-end-number-in-region-atpt ()
  "Employ actions of END- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-end))

(defun ar-escape-number-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-escape))

(defun ar-hide-number-in-region-atpt ()
  "Employ actions of HIDE- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-hide))

(defun ar-hide-show-number-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-hide-show))

(defun ar-hyphen-number-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-hyphen))

(defun ar-kill-number-in-region-atpt ()
  "Employ actions of KILL- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-kill))

(defun ar-curvedsinglequote-number-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-curvedsinglequote))

(defun ar-length-number-in-region-atpt ()
  "Employ actions of LENGTH- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-length))

(defun ar-parentize-number-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-parentize))

(defun ar-quote-number-in-region-atpt ()
  "Employ actions of QUOTE- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-quote))

(defun ar-separate-number-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-separate))

(defun ar-show-number-in-region-atpt ()
  "Employ actions of SHOW- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-show))

(defun ar-singlequote-number-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-singlequote))

(defun ar-slash-number-in-region-atpt ()
  "Employ actions of SLASH- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-slash))

(defun ar-star-number-in-region-atpt ()
  "Employ actions of STAR- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-star))

(defun ar-slashparen-number-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-slashparen))

(defun ar-sort-number-in-region-atpt ()
  "Employ actions of SORT- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-sort))

(defun ar-trim-number-in-region-atpt ()
  "Employ actions of TRIM- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-trim))

(defun ar-trim-left-number-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-trim-left))

(defun ar-trim-right-number-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-trim-right))

(defun ar-underscore-number-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-underscore))

(defun ar-whitespace-number-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of NUMBER residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'number 'region 'ar-th-whitespace))

(defun ar-page-in-region-atpt ()
  "Employ actions of  at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th))

(defun ar-greaterangle-page-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-greaterangle))

(defun ar-lesserangle-page-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-lesserangle))

(defun ar-backslash-page-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-backslash))

(defun ar-colon-page-in-region-atpt ()
  "Employ actions of COLON- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-colon))

(defun ar-beg-page-in-region-atpt ()
  "Employ actions of BEG- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-beg))

(defun ar-blok-page-in-region-atpt ()
  "Employ actions of BLOK- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-blok))

(defun ar-bounds-page-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-bounds))

(defun ar-brace-page-in-region-atpt ()
  "Employ actions of BRACE- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-brace))

(defun ar-bracket-page-in-region-atpt ()
  "Employ actions of BRACKET- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-bracket))

(defun ar-commatize-page-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-commatize))

(defun ar-comment-page-in-region-atpt ()
  "Employ actions of COMMENT- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-comment))

(defun ar-dollar-page-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-dollar))

(defun ar-doublebackslash-page-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-page-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-doublebacktick))

(defun ar-doublequote-page-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-doublequote))

(defun ar-doubleslash-page-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-page-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-doublebackslashparen))

(defun ar-end-page-in-region-atpt ()
  "Employ actions of END- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-end))

(defun ar-escape-page-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-escape))

(defun ar-hide-page-in-region-atpt ()
  "Employ actions of HIDE- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-hide))

(defun ar-hide-show-page-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-hide-show))

(defun ar-hyphen-page-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-hyphen))

(defun ar-kill-page-in-region-atpt ()
  "Employ actions of KILL- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-kill))

(defun ar-curvedsinglequote-page-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-curvedsinglequote))

(defun ar-length-page-in-region-atpt ()
  "Employ actions of LENGTH- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-length))

(defun ar-parentize-page-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-parentize))

(defun ar-quote-page-in-region-atpt ()
  "Employ actions of QUOTE- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-quote))

(defun ar-separate-page-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-separate))

(defun ar-show-page-in-region-atpt ()
  "Employ actions of SHOW- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-show))

(defun ar-singlequote-page-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-singlequote))

(defun ar-slash-page-in-region-atpt ()
  "Employ actions of SLASH- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-slash))

(defun ar-star-page-in-region-atpt ()
  "Employ actions of STAR- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-star))

(defun ar-slashparen-page-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-slashparen))

(defun ar-sort-page-in-region-atpt ()
  "Employ actions of SORT- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-sort))

(defun ar-trim-page-in-region-atpt ()
  "Employ actions of TRIM- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-trim))

(defun ar-trim-left-page-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-trim-left))

(defun ar-trim-right-page-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-trim-right))

(defun ar-underscore-page-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-underscore))

(defun ar-whitespace-page-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of PAGE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'page 'region 'ar-th-whitespace))

(defun ar-paragraph-in-region-atpt ()
  "Employ actions of  at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th))

(defun ar-greaterangle-paragraph-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-greaterangle))

(defun ar-lesserangle-paragraph-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-lesserangle))

(defun ar-backslash-paragraph-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-backslash))

(defun ar-colon-paragraph-in-region-atpt ()
  "Employ actions of COLON- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-colon))

(defun ar-beg-paragraph-in-region-atpt ()
  "Employ actions of BEG- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-beg))

(defun ar-blok-paragraph-in-region-atpt ()
  "Employ actions of BLOK- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-blok))

(defun ar-bounds-paragraph-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-bounds))

(defun ar-brace-paragraph-in-region-atpt ()
  "Employ actions of BRACE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-brace))

(defun ar-bracket-paragraph-in-region-atpt ()
  "Employ actions of BRACKET- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-bracket))

(defun ar-commatize-paragraph-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-commatize))

(defun ar-comment-paragraph-in-region-atpt ()
  "Employ actions of COMMENT- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-comment))

(defun ar-dollar-paragraph-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-dollar))

(defun ar-doublebackslash-paragraph-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-paragraph-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-doublebacktick))

(defun ar-doublequote-paragraph-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-doublequote))

(defun ar-doubleslash-paragraph-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-paragraph-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-doublebackslashparen))

(defun ar-end-paragraph-in-region-atpt ()
  "Employ actions of END- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-end))

(defun ar-escape-paragraph-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-escape))

(defun ar-hide-paragraph-in-region-atpt ()
  "Employ actions of HIDE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-hide))

(defun ar-hide-show-paragraph-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-hide-show))

(defun ar-hyphen-paragraph-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-hyphen))

(defun ar-kill-paragraph-in-region-atpt ()
  "Employ actions of KILL- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-kill))

(defun ar-curvedsinglequote-paragraph-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-curvedsinglequote))

(defun ar-length-paragraph-in-region-atpt ()
  "Employ actions of LENGTH- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-length))

(defun ar-parentize-paragraph-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-parentize))

(defun ar-quote-paragraph-in-region-atpt ()
  "Employ actions of QUOTE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-quote))

(defun ar-separate-paragraph-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-separate))

(defun ar-show-paragraph-in-region-atpt ()
  "Employ actions of SHOW- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-show))

(defun ar-singlequote-paragraph-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-singlequote))

(defun ar-slash-paragraph-in-region-atpt ()
  "Employ actions of SLASH- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-slash))

(defun ar-star-paragraph-in-region-atpt ()
  "Employ actions of STAR- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-star))

(defun ar-slashparen-paragraph-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-slashparen))

(defun ar-sort-paragraph-in-region-atpt ()
  "Employ actions of SORT- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-sort))

(defun ar-trim-paragraph-in-region-atpt ()
  "Employ actions of TRIM- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-trim))

(defun ar-trim-left-paragraph-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-trim-left))

(defun ar-trim-right-paragraph-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-trim-right))

(defun ar-underscore-paragraph-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-underscore))

(defun ar-whitespace-paragraph-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-whitespace))

(defun ar-phone-in-region-atpt ()
  "Employ actions of  at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th))

(defun ar-greaterangle-phone-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-greaterangle))

(defun ar-lesserangle-phone-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-lesserangle))

(defun ar-backslash-phone-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-backslash))

(defun ar-colon-phone-in-region-atpt ()
  "Employ actions of COLON- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-colon))

(defun ar-beg-phone-in-region-atpt ()
  "Employ actions of BEG- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-beg))

(defun ar-blok-phone-in-region-atpt ()
  "Employ actions of BLOK- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-blok))

(defun ar-bounds-phone-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-bounds))

(defun ar-brace-phone-in-region-atpt ()
  "Employ actions of BRACE- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-brace))

(defun ar-bracket-phone-in-region-atpt ()
  "Employ actions of BRACKET- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-bracket))

(defun ar-commatize-phone-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-commatize))

(defun ar-comment-phone-in-region-atpt ()
  "Employ actions of COMMENT- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-comment))

(defun ar-dollar-phone-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-dollar))

(defun ar-doublebackslash-phone-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-phone-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-doublebacktick))

(defun ar-doublequote-phone-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-doublequote))

(defun ar-doubleslash-phone-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-phone-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-doublebackslashparen))

(defun ar-end-phone-in-region-atpt ()
  "Employ actions of END- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-end))

(defun ar-escape-phone-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-escape))

(defun ar-hide-phone-in-region-atpt ()
  "Employ actions of HIDE- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-hide))

(defun ar-hide-show-phone-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-hide-show))

(defun ar-hyphen-phone-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-hyphen))

(defun ar-kill-phone-in-region-atpt ()
  "Employ actions of KILL- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-kill))

(defun ar-curvedsinglequote-phone-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-curvedsinglequote))

(defun ar-length-phone-in-region-atpt ()
  "Employ actions of LENGTH- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-length))

(defun ar-parentize-phone-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-parentize))

(defun ar-quote-phone-in-region-atpt ()
  "Employ actions of QUOTE- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-quote))

(defun ar-separate-phone-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-separate))

(defun ar-show-phone-in-region-atpt ()
  "Employ actions of SHOW- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-show))

(defun ar-singlequote-phone-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-singlequote))

(defun ar-slash-phone-in-region-atpt ()
  "Employ actions of SLASH- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-slash))

(defun ar-star-phone-in-region-atpt ()
  "Employ actions of STAR- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-star))

(defun ar-slashparen-phone-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-slashparen))

(defun ar-sort-phone-in-region-atpt ()
  "Employ actions of SORT- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-sort))

(defun ar-trim-phone-in-region-atpt ()
  "Employ actions of TRIM- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-trim))

(defun ar-trim-left-phone-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-trim-left))

(defun ar-trim-right-phone-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-trim-right))

(defun ar-underscore-phone-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-underscore))

(defun ar-whitespace-phone-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of PHONE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'phone 'region 'ar-th-whitespace))

(defun ar-region-in-region-atpt ()
  "Employ actions of  at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th))

(defun ar-greaterangle-region-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-greaterangle))

(defun ar-lesserangle-region-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-lesserangle))

(defun ar-backslash-region-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-backslash))

(defun ar-colon-region-in-region-atpt ()
  "Employ actions of COLON- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-colon))

(defun ar-beg-region-in-region-atpt ()
  "Employ actions of BEG- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-beg))

(defun ar-blok-region-in-region-atpt ()
  "Employ actions of BLOK- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-blok))

(defun ar-bounds-region-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-bounds))

(defun ar-brace-region-in-region-atpt ()
  "Employ actions of BRACE- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-brace))

(defun ar-bracket-region-in-region-atpt ()
  "Employ actions of BRACKET- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-bracket))

(defun ar-commatize-region-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-commatize))

(defun ar-comment-region-in-region-atpt ()
  "Employ actions of COMMENT- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-comment))

(defun ar-dollar-region-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-dollar))

(defun ar-doublebackslash-region-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-region-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-doublebacktick))

(defun ar-doublequote-region-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-doublequote))

(defun ar-doubleslash-region-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-region-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-doublebackslashparen))

(defun ar-end-region-in-region-atpt ()
  "Employ actions of END- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-end))

(defun ar-escape-region-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-escape))

(defun ar-hide-region-in-region-atpt ()
  "Employ actions of HIDE- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-hide))

(defun ar-hide-show-region-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-hide-show))

(defun ar-hyphen-region-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-hyphen))

(defun ar-kill-region-in-region-atpt ()
  "Employ actions of KILL- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-kill))

(defun ar-curvedsinglequote-region-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-curvedsinglequote))

(defun ar-length-region-in-region-atpt ()
  "Employ actions of LENGTH- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-length))

(defun ar-parentize-region-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-parentize))

(defun ar-quote-region-in-region-atpt ()
  "Employ actions of QUOTE- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-quote))

(defun ar-separate-region-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-separate))

(defun ar-show-region-in-region-atpt ()
  "Employ actions of SHOW- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-show))

(defun ar-singlequote-region-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-singlequote))

(defun ar-slash-region-in-region-atpt ()
  "Employ actions of SLASH- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-slash))

(defun ar-star-region-in-region-atpt ()
  "Employ actions of STAR- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-star))

(defun ar-slashparen-region-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-slashparen))

(defun ar-sort-region-in-region-atpt ()
  "Employ actions of SORT- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-sort))

(defun ar-trim-region-in-region-atpt ()
  "Employ actions of TRIM- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-trim))

(defun ar-trim-left-region-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-trim-left))

(defun ar-trim-right-region-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-trim-right))

(defun ar-underscore-region-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-underscore))

(defun ar-whitespace-region-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of REGION residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'region 'region 'ar-th-whitespace))

(defun ar-sentence-in-region-atpt ()
  "Employ actions of  at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th))

(defun ar-greaterangle-sentence-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-greaterangle))

(defun ar-lesserangle-sentence-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-lesserangle))

(defun ar-backslash-sentence-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-backslash))

(defun ar-colon-sentence-in-region-atpt ()
  "Employ actions of COLON- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-colon))

(defun ar-beg-sentence-in-region-atpt ()
  "Employ actions of BEG- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-beg))

(defun ar-blok-sentence-in-region-atpt ()
  "Employ actions of BLOK- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-blok))

(defun ar-bounds-sentence-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-bounds))

(defun ar-brace-sentence-in-region-atpt ()
  "Employ actions of BRACE- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-brace))

(defun ar-bracket-sentence-in-region-atpt ()
  "Employ actions of BRACKET- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-bracket))

(defun ar-commatize-sentence-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-commatize))

(defun ar-comment-sentence-in-region-atpt ()
  "Employ actions of COMMENT- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-comment))

(defun ar-dollar-sentence-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-dollar))

(defun ar-doublebackslash-sentence-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-sentence-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-doublebacktick))

(defun ar-doublequote-sentence-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-doublequote))

(defun ar-doubleslash-sentence-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-sentence-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-doublebackslashparen))

(defun ar-end-sentence-in-region-atpt ()
  "Employ actions of END- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-end))

(defun ar-escape-sentence-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-escape))

(defun ar-hide-sentence-in-region-atpt ()
  "Employ actions of HIDE- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-hide))

(defun ar-hide-show-sentence-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-hide-show))

(defun ar-hyphen-sentence-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-hyphen))

(defun ar-kill-sentence-in-region-atpt ()
  "Employ actions of KILL- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-kill))

(defun ar-curvedsinglequote-sentence-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-curvedsinglequote))

(defun ar-length-sentence-in-region-atpt ()
  "Employ actions of LENGTH- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-length))

(defun ar-parentize-sentence-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-parentize))

(defun ar-quote-sentence-in-region-atpt ()
  "Employ actions of QUOTE- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-quote))

(defun ar-separate-sentence-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-separate))

(defun ar-show-sentence-in-region-atpt ()
  "Employ actions of SHOW- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-show))

(defun ar-singlequote-sentence-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-singlequote))

(defun ar-slash-sentence-in-region-atpt ()
  "Employ actions of SLASH- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-slash))

(defun ar-star-sentence-in-region-atpt ()
  "Employ actions of STAR- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-star))

(defun ar-slashparen-sentence-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-slashparen))

(defun ar-sort-sentence-in-region-atpt ()
  "Employ actions of SORT- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-sort))

(defun ar-trim-sentence-in-region-atpt ()
  "Employ actions of TRIM- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-trim))

(defun ar-trim-left-sentence-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-trim-left))

(defun ar-trim-right-sentence-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-trim-right))

(defun ar-underscore-sentence-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-underscore))

(defun ar-whitespace-sentence-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of SENTENCE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sentence 'region 'ar-th-whitespace))

(defun ar-sexp-in-region-atpt ()
  "Employ actions of  at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th))

(defun ar-greaterangle-sexp-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-greaterangle))

(defun ar-lesserangle-sexp-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-lesserangle))

(defun ar-backslash-sexp-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-backslash))

(defun ar-colon-sexp-in-region-atpt ()
  "Employ actions of COLON- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-colon))

(defun ar-beg-sexp-in-region-atpt ()
  "Employ actions of BEG- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-beg))

(defun ar-blok-sexp-in-region-atpt ()
  "Employ actions of BLOK- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-blok))

(defun ar-bounds-sexp-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-bounds))

(defun ar-brace-sexp-in-region-atpt ()
  "Employ actions of BRACE- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-brace))

(defun ar-bracket-sexp-in-region-atpt ()
  "Employ actions of BRACKET- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-bracket))

(defun ar-commatize-sexp-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-commatize))

(defun ar-comment-sexp-in-region-atpt ()
  "Employ actions of COMMENT- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-comment))

(defun ar-dollar-sexp-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-dollar))

(defun ar-doublebackslash-sexp-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-sexp-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-doublebacktick))

(defun ar-doublequote-sexp-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-doublequote))

(defun ar-doubleslash-sexp-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-sexp-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-doublebackslashparen))

(defun ar-end-sexp-in-region-atpt ()
  "Employ actions of END- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-end))

(defun ar-escape-sexp-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-escape))

(defun ar-hide-sexp-in-region-atpt ()
  "Employ actions of HIDE- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-hide))

(defun ar-hide-show-sexp-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-hide-show))

(defun ar-hyphen-sexp-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-hyphen))

(defun ar-kill-sexp-in-region-atpt ()
  "Employ actions of KILL- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-kill))

(defun ar-curvedsinglequote-sexp-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-curvedsinglequote))

(defun ar-length-sexp-in-region-atpt ()
  "Employ actions of LENGTH- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-length))

(defun ar-parentize-sexp-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-parentize))

(defun ar-quote-sexp-in-region-atpt ()
  "Employ actions of QUOTE- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-quote))

(defun ar-separate-sexp-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-separate))

(defun ar-show-sexp-in-region-atpt ()
  "Employ actions of SHOW- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-show))

(defun ar-singlequote-sexp-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-singlequote))

(defun ar-slash-sexp-in-region-atpt ()
  "Employ actions of SLASH- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-slash))

(defun ar-star-sexp-in-region-atpt ()
  "Employ actions of STAR- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-star))

(defun ar-slashparen-sexp-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-slashparen))

(defun ar-sort-sexp-in-region-atpt ()
  "Employ actions of SORT- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-sort))

(defun ar-trim-sexp-in-region-atpt ()
  "Employ actions of TRIM- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-trim))

(defun ar-trim-left-sexp-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-trim-left))

(defun ar-trim-right-sexp-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-trim-right))

(defun ar-underscore-sexp-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-underscore))

(defun ar-whitespace-sexp-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of SEXP residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'sexp 'region 'ar-th-whitespace))

(defun ar-shstruct-in-region-atpt ()
  "Employ actions of  at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th))

(defun ar-greaterangle-shstruct-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-greaterangle))

(defun ar-lesserangle-shstruct-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-lesserangle))

(defun ar-backslash-shstruct-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-backslash))

(defun ar-colon-shstruct-in-region-atpt ()
  "Employ actions of COLON- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-colon))

(defun ar-beg-shstruct-in-region-atpt ()
  "Employ actions of BEG- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-beg))

(defun ar-blok-shstruct-in-region-atpt ()
  "Employ actions of BLOK- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-blok))

(defun ar-bounds-shstruct-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-bounds))

(defun ar-brace-shstruct-in-region-atpt ()
  "Employ actions of BRACE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-brace))

(defun ar-bracket-shstruct-in-region-atpt ()
  "Employ actions of BRACKET- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-bracket))

(defun ar-commatize-shstruct-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-commatize))

(defun ar-comment-shstruct-in-region-atpt ()
  "Employ actions of COMMENT- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-comment))

(defun ar-dollar-shstruct-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-dollar))

(defun ar-doublebackslash-shstruct-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-shstruct-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-doublebacktick))

(defun ar-doublequote-shstruct-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-doublequote))

(defun ar-doubleslash-shstruct-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-shstruct-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-doublebackslashparen))

(defun ar-end-shstruct-in-region-atpt ()
  "Employ actions of END- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-end))

(defun ar-escape-shstruct-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-escape))

(defun ar-hide-shstruct-in-region-atpt ()
  "Employ actions of HIDE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-hide))

(defun ar-hide-show-shstruct-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-hide-show))

(defun ar-hyphen-shstruct-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-hyphen))

(defun ar-kill-shstruct-in-region-atpt ()
  "Employ actions of KILL- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-kill))

(defun ar-curvedsinglequote-shstruct-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-curvedsinglequote))

(defun ar-length-shstruct-in-region-atpt ()
  "Employ actions of LENGTH- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-length))

(defun ar-parentize-shstruct-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-parentize))

(defun ar-quote-shstruct-in-region-atpt ()
  "Employ actions of QUOTE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-quote))

(defun ar-separate-shstruct-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-separate))

(defun ar-show-shstruct-in-region-atpt ()
  "Employ actions of SHOW- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-show))

(defun ar-singlequote-shstruct-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-singlequote))

(defun ar-slash-shstruct-in-region-atpt ()
  "Employ actions of SLASH- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-slash))

(defun ar-star-shstruct-in-region-atpt ()
  "Employ actions of STAR- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-star))

(defun ar-slashparen-shstruct-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-slashparen))

(defun ar-sort-shstruct-in-region-atpt ()
  "Employ actions of SORT- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-sort))

(defun ar-trim-shstruct-in-region-atpt ()
  "Employ actions of TRIM- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-trim))

(defun ar-trim-left-shstruct-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-trim-left))

(defun ar-trim-right-shstruct-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-trim-right))

(defun ar-underscore-shstruct-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-underscore))

(defun ar-whitespace-shstruct-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-whitespace))

(defun ar-symbol-in-region-atpt ()
  "Employ actions of  at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th))

(defun ar-greaterangle-symbol-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-greaterangle))

(defun ar-lesserangle-symbol-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-lesserangle))

(defun ar-backslash-symbol-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-backslash))

(defun ar-colon-symbol-in-region-atpt ()
  "Employ actions of COLON- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-colon))

(defun ar-beg-symbol-in-region-atpt ()
  "Employ actions of BEG- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-beg))

(defun ar-blok-symbol-in-region-atpt ()
  "Employ actions of BLOK- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-blok))

(defun ar-bounds-symbol-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-bounds))

(defun ar-brace-symbol-in-region-atpt ()
  "Employ actions of BRACE- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-brace))

(defun ar-bracket-symbol-in-region-atpt ()
  "Employ actions of BRACKET- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-bracket))

(defun ar-commatize-symbol-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-commatize))

(defun ar-comment-symbol-in-region-atpt ()
  "Employ actions of COMMENT- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-comment))

(defun ar-dollar-symbol-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-dollar))

(defun ar-doublebackslash-symbol-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-symbol-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-doublebacktick))

(defun ar-doublequote-symbol-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-doublequote))

(defun ar-doubleslash-symbol-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-symbol-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-doublebackslashparen))

(defun ar-end-symbol-in-region-atpt ()
  "Employ actions of END- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-end))

(defun ar-escape-symbol-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-escape))

(defun ar-hide-symbol-in-region-atpt ()
  "Employ actions of HIDE- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-hide))

(defun ar-hide-show-symbol-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-hide-show))

(defun ar-hyphen-symbol-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-hyphen))

(defun ar-kill-symbol-in-region-atpt ()
  "Employ actions of KILL- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-kill))

(defun ar-curvedsinglequote-symbol-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-curvedsinglequote))

(defun ar-length-symbol-in-region-atpt ()
  "Employ actions of LENGTH- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-length))

(defun ar-parentize-symbol-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-parentize))

(defun ar-quote-symbol-in-region-atpt ()
  "Employ actions of QUOTE- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-quote))

(defun ar-separate-symbol-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-separate))

(defun ar-show-symbol-in-region-atpt ()
  "Employ actions of SHOW- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-show))

(defun ar-singlequote-symbol-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-singlequote))

(defun ar-slash-symbol-in-region-atpt ()
  "Employ actions of SLASH- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-slash))

(defun ar-star-symbol-in-region-atpt ()
  "Employ actions of STAR- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-star))

(defun ar-slashparen-symbol-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-slashparen))

(defun ar-sort-symbol-in-region-atpt ()
  "Employ actions of SORT- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-sort))

(defun ar-trim-symbol-in-region-atpt ()
  "Employ actions of TRIM- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-trim))

(defun ar-trim-left-symbol-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-trim-left))

(defun ar-trim-right-symbol-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-trim-right))

(defun ar-underscore-symbol-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-underscore))

(defun ar-whitespace-symbol-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of SYMBOL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'symbol 'region 'ar-th-whitespace))

(defun ar-url-in-region-atpt ()
  "Employ actions of  at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th))

(defun ar-greaterangle-url-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-greaterangle))

(defun ar-lesserangle-url-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-lesserangle))

(defun ar-backslash-url-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-backslash))

(defun ar-colon-url-in-region-atpt ()
  "Employ actions of COLON- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-colon))

(defun ar-beg-url-in-region-atpt ()
  "Employ actions of BEG- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-beg))

(defun ar-blok-url-in-region-atpt ()
  "Employ actions of BLOK- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-blok))

(defun ar-bounds-url-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-bounds))

(defun ar-brace-url-in-region-atpt ()
  "Employ actions of BRACE- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-brace))

(defun ar-bracket-url-in-region-atpt ()
  "Employ actions of BRACKET- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-bracket))

(defun ar-commatize-url-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-commatize))

(defun ar-comment-url-in-region-atpt ()
  "Employ actions of COMMENT- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-comment))

(defun ar-dollar-url-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-dollar))

(defun ar-doublebackslash-url-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-url-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-doublebacktick))

(defun ar-doublequote-url-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-doublequote))

(defun ar-doubleslash-url-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-url-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-doublebackslashparen))

(defun ar-end-url-in-region-atpt ()
  "Employ actions of END- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-end))

(defun ar-escape-url-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-escape))

(defun ar-hide-url-in-region-atpt ()
  "Employ actions of HIDE- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-hide))

(defun ar-hide-show-url-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-hide-show))

(defun ar-hyphen-url-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-hyphen))

(defun ar-kill-url-in-region-atpt ()
  "Employ actions of KILL- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-kill))

(defun ar-curvedsinglequote-url-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-curvedsinglequote))

(defun ar-length-url-in-region-atpt ()
  "Employ actions of LENGTH- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-length))

(defun ar-parentize-url-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-parentize))

(defun ar-quote-url-in-region-atpt ()
  "Employ actions of QUOTE- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-quote))

(defun ar-separate-url-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-separate))

(defun ar-show-url-in-region-atpt ()
  "Employ actions of SHOW- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-show))

(defun ar-singlequote-url-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-singlequote))

(defun ar-slash-url-in-region-atpt ()
  "Employ actions of SLASH- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-slash))

(defun ar-star-url-in-region-atpt ()
  "Employ actions of STAR- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-star))

(defun ar-slashparen-url-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-slashparen))

(defun ar-sort-url-in-region-atpt ()
  "Employ actions of SORT- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-sort))

(defun ar-trim-url-in-region-atpt ()
  "Employ actions of TRIM- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-trim))

(defun ar-trim-left-url-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-trim-left))

(defun ar-trim-right-url-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-trim-right))

(defun ar-underscore-url-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-underscore))

(defun ar-whitespace-url-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of URL residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'url 'region 'ar-th-whitespace))

(defun ar-word-in-region-atpt ()
  "Employ actions of  at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th))

(defun ar-greaterangle-word-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-greaterangle))

(defun ar-lesserangle-word-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-lesserangle))

(defun ar-backslash-word-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-backslash))

(defun ar-colon-word-in-region-atpt ()
  "Employ actions of COLON- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-colon))

(defun ar-beg-word-in-region-atpt ()
  "Employ actions of BEG- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-beg))

(defun ar-blok-word-in-region-atpt ()
  "Employ actions of BLOK- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-blok))

(defun ar-bounds-word-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-bounds))

(defun ar-brace-word-in-region-atpt ()
  "Employ actions of BRACE- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-brace))

(defun ar-bracket-word-in-region-atpt ()
  "Employ actions of BRACKET- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-bracket))

(defun ar-commatize-word-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-commatize))

(defun ar-comment-word-in-region-atpt ()
  "Employ actions of COMMENT- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-comment))

(defun ar-dollar-word-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-dollar))

(defun ar-doublebackslash-word-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-word-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-doublebacktick))

(defun ar-doublequote-word-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-doublequote))

(defun ar-doubleslash-word-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-word-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-doublebackslashparen))

(defun ar-end-word-in-region-atpt ()
  "Employ actions of END- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-end))

(defun ar-escape-word-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-escape))

(defun ar-hide-word-in-region-atpt ()
  "Employ actions of HIDE- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-hide))

(defun ar-hide-show-word-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-hide-show))

(defun ar-hyphen-word-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-hyphen))

(defun ar-kill-word-in-region-atpt ()
  "Employ actions of KILL- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-kill))

(defun ar-curvedsinglequote-word-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-curvedsinglequote))

(defun ar-length-word-in-region-atpt ()
  "Employ actions of LENGTH- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-length))

(defun ar-parentize-word-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-parentize))

(defun ar-quote-word-in-region-atpt ()
  "Employ actions of QUOTE- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-quote))

(defun ar-separate-word-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-separate))

(defun ar-show-word-in-region-atpt ()
  "Employ actions of SHOW- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-show))

(defun ar-singlequote-word-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-singlequote))

(defun ar-slash-word-in-region-atpt ()
  "Employ actions of SLASH- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-slash))

(defun ar-star-word-in-region-atpt ()
  "Employ actions of STAR- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-star))

(defun ar-slashparen-word-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-slashparen))

(defun ar-sort-word-in-region-atpt ()
  "Employ actions of SORT- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-sort))

(defun ar-trim-word-in-region-atpt ()
  "Employ actions of TRIM- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-trim))

(defun ar-trim-left-word-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-trim-left))

(defun ar-trim-right-word-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-trim-right))

(defun ar-underscore-word-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-underscore))

(defun ar-whitespace-word-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of WORD residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'word 'region 'ar-th-whitespace))

(defun ar-wordalphaonly-in-region-atpt ()
  "Employ actions of  at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th))

(defun ar-greaterangle-wordalphaonly-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-greaterangle))

(defun ar-lesserangle-wordalphaonly-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-lesserangle))

(defun ar-backslash-wordalphaonly-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-backslash))

(defun ar-colon-wordalphaonly-in-region-atpt ()
  "Employ actions of COLON- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-colon))

(defun ar-beg-wordalphaonly-in-region-atpt ()
  "Employ actions of BEG- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-beg))

(defun ar-blok-wordalphaonly-in-region-atpt ()
  "Employ actions of BLOK- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-blok))

(defun ar-bounds-wordalphaonly-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-bounds))

(defun ar-brace-wordalphaonly-in-region-atpt ()
  "Employ actions of BRACE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-brace))

(defun ar-bracket-wordalphaonly-in-region-atpt ()
  "Employ actions of BRACKET- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-bracket))

(defun ar-commatize-wordalphaonly-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-commatize))

(defun ar-comment-wordalphaonly-in-region-atpt ()
  "Employ actions of COMMENT- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-comment))

(defun ar-dollar-wordalphaonly-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-dollar))

(defun ar-doublebackslash-wordalphaonly-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-wordalphaonly-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-doublebacktick))

(defun ar-doublequote-wordalphaonly-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-doublequote))

(defun ar-doubleslash-wordalphaonly-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-wordalphaonly-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-doublebackslashparen))

(defun ar-end-wordalphaonly-in-region-atpt ()
  "Employ actions of END- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-end))

(defun ar-escape-wordalphaonly-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-escape))

(defun ar-hide-wordalphaonly-in-region-atpt ()
  "Employ actions of HIDE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-hide))

(defun ar-hide-show-wordalphaonly-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-hide-show))

(defun ar-hyphen-wordalphaonly-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-hyphen))

(defun ar-kill-wordalphaonly-in-region-atpt ()
  "Employ actions of KILL- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-kill))

(defun ar-curvedsinglequote-wordalphaonly-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-curvedsinglequote))

(defun ar-length-wordalphaonly-in-region-atpt ()
  "Employ actions of LENGTH- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-length))

(defun ar-parentize-wordalphaonly-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-parentize))

(defun ar-quote-wordalphaonly-in-region-atpt ()
  "Employ actions of QUOTE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-quote))

(defun ar-separate-wordalphaonly-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-separate))

(defun ar-show-wordalphaonly-in-region-atpt ()
  "Employ actions of SHOW- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-show))

(defun ar-singlequote-wordalphaonly-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-singlequote))

(defun ar-slash-wordalphaonly-in-region-atpt ()
  "Employ actions of SLASH- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-slash))

(defun ar-star-wordalphaonly-in-region-atpt ()
  "Employ actions of STAR- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-star))

(defun ar-slashparen-wordalphaonly-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-slashparen))

(defun ar-sort-wordalphaonly-in-region-atpt ()
  "Employ actions of SORT- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-sort))

(defun ar-trim-wordalphaonly-in-region-atpt ()
  "Employ actions of TRIM- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-trim))

(defun ar-trim-left-wordalphaonly-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-trim-left))

(defun ar-trim-right-wordalphaonly-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-trim-right))

(defun ar-underscore-wordalphaonly-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-underscore))

(defun ar-whitespace-wordalphaonly-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-whitespace))

(provide 'ar-thingatpt-rest-list-in-region-only)
;;;thing-rest-list-in-region-only.el ends here

