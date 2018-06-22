;;; thing-rest-list-in-region-only.el --- thing-in-thing functions
;; Built by ar-thing-in-thing-anlegen-intern ar-atpt-rest-list ar-atpt-region-only


;; Copyright (C) 2010-2018 Andreas Röhler, unless
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

(defun ar-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of  at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th arg))

(defun ar-greaterangle-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-lesserangle arg))

(defun ar-backslash-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-backslash arg))

(defun ar-colon-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-colon arg))

(defun ar-beg-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-beg arg))

(defun ar-blok-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-blok arg))

(defun ar-bounds-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-bounds arg))

(defun ar-brace-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-brace arg))

(defun ar-bracket-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-bracket arg))

(defun ar-commatize-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-commatize arg))

(defun ar-comment-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-comment arg))

(defun ar-dollar-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-end arg))

(defun ar-escape-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-escape arg))

(defun ar-hide-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-hide arg))

(defun ar-hide-show-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-hide-show arg))

(defun ar-hyphen-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-hyphen arg))

(defun ar-kill-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-length arg))

(defun ar-parentize-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-parentize arg))

(defun ar-quote-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-quote arg))

(defun ar-separate-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-separate arg))

(defun ar-show-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-show arg))

(defun ar-singlequote-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-singlequote arg))

(defun ar-slash-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-slash arg))

(defun ar-star-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-star arg))

(defun ar-slashparen-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-slashparen arg))

(defun ar-sort-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-sort arg))

(defun ar-trim-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-trim arg))

(defun ar-trim-left-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-trim-left arg))

(defun ar-trim-right-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-trim-right arg))

(defun ar-underscore-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-underscore arg))

(defun ar-whitespace-greateranglednested-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of GREATERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'greateranglednested 'region 'ar-th-whitespace arg))

(defun ar-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of  at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th arg))

(defun ar-greaterangle-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-lesserangle arg))

(defun ar-backslash-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-backslash arg))

(defun ar-colon-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-colon arg))

(defun ar-beg-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-beg arg))

(defun ar-blok-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-blok arg))

(defun ar-bounds-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-bounds arg))

(defun ar-brace-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-brace arg))

(defun ar-bracket-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-bracket arg))

(defun ar-commatize-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-commatize arg))

(defun ar-comment-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-comment arg))

(defun ar-dollar-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-end arg))

(defun ar-escape-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-escape arg))

(defun ar-hide-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-hide arg))

(defun ar-hide-show-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-hide-show arg))

(defun ar-hyphen-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-hyphen arg))

(defun ar-kill-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-length arg))

(defun ar-parentize-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-parentize arg))

(defun ar-quote-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-quote arg))

(defun ar-separate-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-separate arg))

(defun ar-show-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-show arg))

(defun ar-singlequote-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-singlequote arg))

(defun ar-slash-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-slash arg))

(defun ar-star-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-star arg))

(defun ar-slashparen-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-slashparen arg))

(defun ar-sort-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-sort arg))

(defun ar-trim-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-trim arg))

(defun ar-trim-left-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-trim-left arg))

(defun ar-trim-right-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-trim-right arg))

(defun ar-underscore-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-underscore arg))

(defun ar-whitespace-lesseranglednested-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of LESSERANGLEDNESTED residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'lesseranglednested 'region 'ar-th-whitespace arg))

(defun ar-buffer-in-region-atpt (&optional arg)
  "Employ actions of  at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th arg))

(defun ar-greaterangle-buffer-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-buffer-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-lesserangle arg))

(defun ar-backslash-buffer-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-backslash arg))

(defun ar-colon-buffer-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-colon arg))

(defun ar-beg-buffer-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-beg arg))

(defun ar-blok-buffer-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-blok arg))

(defun ar-bounds-buffer-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-bounds arg))

(defun ar-brace-buffer-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-brace arg))

(defun ar-bracket-buffer-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-bracket arg))

(defun ar-commatize-buffer-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-commatize arg))

(defun ar-comment-buffer-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-comment arg))

(defun ar-dollar-buffer-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-buffer-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-buffer-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-buffer-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-buffer-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-buffer-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-buffer-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-end arg))

(defun ar-escape-buffer-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-escape arg))

(defun ar-hide-buffer-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-hide arg))

(defun ar-hide-show-buffer-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-hide-show arg))

(defun ar-hyphen-buffer-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-hyphen arg))

(defun ar-kill-buffer-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-buffer-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-buffer-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-length arg))

(defun ar-parentize-buffer-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-parentize arg))

(defun ar-quote-buffer-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-quote arg))

(defun ar-separate-buffer-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-separate arg))

(defun ar-show-buffer-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-show arg))

(defun ar-singlequote-buffer-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-singlequote arg))

(defun ar-slash-buffer-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-slash arg))

(defun ar-star-buffer-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-star arg))

(defun ar-slashparen-buffer-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-slashparen arg))

(defun ar-sort-buffer-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-sort arg))

(defun ar-trim-buffer-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-trim arg))

(defun ar-trim-left-buffer-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-trim-left arg))

(defun ar-trim-right-buffer-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-trim-right arg))

(defun ar-underscore-buffer-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-underscore arg))

(defun ar-whitespace-buffer-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of BUFFER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'buffer 'region 'ar-th-whitespace arg))

(defun ar-char-in-region-atpt (&optional arg)
  "Employ actions of  at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th arg))

(defun ar-greaterangle-char-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-char-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-lesserangle arg))

(defun ar-backslash-char-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-backslash arg))

(defun ar-colon-char-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-colon arg))

(defun ar-beg-char-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-beg arg))

(defun ar-blok-char-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-blok arg))

(defun ar-bounds-char-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-bounds arg))

(defun ar-brace-char-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-brace arg))

(defun ar-bracket-char-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-bracket arg))

(defun ar-commatize-char-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-commatize arg))

(defun ar-comment-char-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-comment arg))

(defun ar-dollar-char-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-char-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-char-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-char-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-char-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-char-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-char-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-end arg))

(defun ar-escape-char-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-escape arg))

(defun ar-hide-char-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-hide arg))

(defun ar-hide-show-char-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-hide-show arg))

(defun ar-hyphen-char-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-hyphen arg))

(defun ar-kill-char-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-char-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-char-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-length arg))

(defun ar-parentize-char-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-parentize arg))

(defun ar-quote-char-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-quote arg))

(defun ar-separate-char-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-separate arg))

(defun ar-show-char-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-show arg))

(defun ar-singlequote-char-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-singlequote arg))

(defun ar-slash-char-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-slash arg))

(defun ar-star-char-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-star arg))

(defun ar-slashparen-char-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-slashparen arg))

(defun ar-sort-char-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-sort arg))

(defun ar-trim-char-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-trim arg))

(defun ar-trim-left-char-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-trim-left arg))

(defun ar-trim-right-char-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-trim-right arg))

(defun ar-underscore-char-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-underscore arg))

(defun ar-whitespace-char-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of CHAR residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'char 'region 'ar-th-whitespace arg))

(defun ar-comment-in-region-atpt (&optional arg)
  "Employ actions of  at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th arg))

(defun ar-greaterangle-comment-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-comment-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-lesserangle arg))

(defun ar-backslash-comment-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-backslash arg))

(defun ar-colon-comment-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-colon arg))

(defun ar-beg-comment-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-beg arg))

(defun ar-blok-comment-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-blok arg))

(defun ar-bounds-comment-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-bounds arg))

(defun ar-brace-comment-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-brace arg))

(defun ar-bracket-comment-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-bracket arg))

(defun ar-commatize-comment-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-commatize arg))

(defun ar-comment-comment-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-comment arg))

(defun ar-dollar-comment-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-comment-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-comment-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-comment-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-comment-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-comment-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-comment-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-end arg))

(defun ar-escape-comment-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-escape arg))

(defun ar-hide-comment-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-hide arg))

(defun ar-hide-show-comment-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-hide-show arg))

(defun ar-hyphen-comment-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-hyphen arg))

(defun ar-kill-comment-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-comment-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-comment-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-length arg))

(defun ar-parentize-comment-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-parentize arg))

(defun ar-quote-comment-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-quote arg))

(defun ar-separate-comment-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-separate arg))

(defun ar-show-comment-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-show arg))

(defun ar-singlequote-comment-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-singlequote arg))

(defun ar-slash-comment-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-slash arg))

(defun ar-star-comment-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-star arg))

(defun ar-slashparen-comment-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-slashparen arg))

(defun ar-sort-comment-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-sort arg))

(defun ar-trim-comment-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-trim arg))

(defun ar-trim-left-comment-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-trim-left arg))

(defun ar-trim-right-comment-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-trim-right arg))

(defun ar-underscore-comment-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-underscore arg))

(defun ar-whitespace-comment-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of COMMENT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'comment 'region 'ar-th-whitespace arg))

(defun ar-csv-in-region-atpt (&optional arg)
  "Employ actions of  at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th arg))

(defun ar-greaterangle-csv-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-csv-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-lesserangle arg))

(defun ar-backslash-csv-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-backslash arg))

(defun ar-colon-csv-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-colon arg))

(defun ar-beg-csv-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-beg arg))

(defun ar-blok-csv-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-blok arg))

(defun ar-bounds-csv-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-bounds arg))

(defun ar-brace-csv-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-brace arg))

(defun ar-bracket-csv-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-bracket arg))

(defun ar-commatize-csv-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-commatize arg))

(defun ar-comment-csv-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-comment arg))

(defun ar-dollar-csv-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-csv-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-csv-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-csv-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-csv-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-csv-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-csv-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-end arg))

(defun ar-escape-csv-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-escape arg))

(defun ar-hide-csv-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-hide arg))

(defun ar-hide-show-csv-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-hide-show arg))

(defun ar-hyphen-csv-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-hyphen arg))

(defun ar-kill-csv-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-csv-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-csv-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-length arg))

(defun ar-parentize-csv-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-parentize arg))

(defun ar-quote-csv-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-quote arg))

(defun ar-separate-csv-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-separate arg))

(defun ar-show-csv-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-show arg))

(defun ar-singlequote-csv-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-singlequote arg))

(defun ar-slash-csv-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-slash arg))

(defun ar-star-csv-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-star arg))

(defun ar-slashparen-csv-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-slashparen arg))

(defun ar-sort-csv-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-sort arg))

(defun ar-trim-csv-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-trim arg))

(defun ar-trim-left-csv-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-trim-left arg))

(defun ar-trim-right-csv-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-trim-right arg))

(defun ar-underscore-csv-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-underscore arg))

(defun ar-whitespace-csv-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of CSV residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'csv 'region 'ar-th-whitespace arg))

(defun ar-date-in-region-atpt (&optional arg)
  "Employ actions of  at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th arg))

(defun ar-greaterangle-date-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-date-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-lesserangle arg))

(defun ar-backslash-date-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-backslash arg))

(defun ar-colon-date-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-colon arg))

(defun ar-beg-date-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-beg arg))

(defun ar-blok-date-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-blok arg))

(defun ar-bounds-date-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-bounds arg))

(defun ar-brace-date-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-brace arg))

(defun ar-bracket-date-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-bracket arg))

(defun ar-commatize-date-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-commatize arg))

(defun ar-comment-date-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-comment arg))

(defun ar-dollar-date-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-date-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-date-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-date-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-date-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-date-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-date-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-end arg))

(defun ar-escape-date-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-escape arg))

(defun ar-hide-date-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-hide arg))

(defun ar-hide-show-date-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-hide-show arg))

(defun ar-hyphen-date-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-hyphen arg))

(defun ar-kill-date-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-date-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-date-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-length arg))

(defun ar-parentize-date-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-parentize arg))

(defun ar-quote-date-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-quote arg))

(defun ar-separate-date-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-separate arg))

(defun ar-show-date-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-show arg))

(defun ar-singlequote-date-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-singlequote arg))

(defun ar-slash-date-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-slash arg))

(defun ar-star-date-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-star arg))

(defun ar-slashparen-date-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-slashparen arg))

(defun ar-sort-date-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-sort arg))

(defun ar-trim-date-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-trim arg))

(defun ar-trim-left-date-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-trim-left arg))

(defun ar-trim-right-date-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-trim-right arg))

(defun ar-underscore-date-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-underscore arg))

(defun ar-whitespace-date-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of DATE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'date 'region 'ar-th-whitespace arg))

(defun ar-email-in-region-atpt (&optional arg)
  "Employ actions of  at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th arg))

(defun ar-greaterangle-email-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-email-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-lesserangle arg))

(defun ar-backslash-email-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-backslash arg))

(defun ar-colon-email-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-colon arg))

(defun ar-beg-email-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-beg arg))

(defun ar-blok-email-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-blok arg))

(defun ar-bounds-email-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-bounds arg))

(defun ar-brace-email-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-brace arg))

(defun ar-bracket-email-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-bracket arg))

(defun ar-commatize-email-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-commatize arg))

(defun ar-comment-email-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-comment arg))

(defun ar-dollar-email-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-email-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-email-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-email-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-email-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-email-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-email-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-end arg))

(defun ar-escape-email-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-escape arg))

(defun ar-hide-email-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-hide arg))

(defun ar-hide-show-email-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-hide-show arg))

(defun ar-hyphen-email-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-hyphen arg))

(defun ar-kill-email-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-email-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-email-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-length arg))

(defun ar-parentize-email-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-parentize arg))

(defun ar-quote-email-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-quote arg))

(defun ar-separate-email-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-separate arg))

(defun ar-show-email-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-show arg))

(defun ar-singlequote-email-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-singlequote arg))

(defun ar-slash-email-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-slash arg))

(defun ar-star-email-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-star arg))

(defun ar-slashparen-email-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-slashparen arg))

(defun ar-sort-email-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-sort arg))

(defun ar-trim-email-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-trim arg))

(defun ar-trim-left-email-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-trim-left arg))

(defun ar-trim-right-email-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-trim-right arg))

(defun ar-underscore-email-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-underscore arg))

(defun ar-whitespace-email-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of EMAIL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'email 'region 'ar-th-whitespace arg))

(defun ar-filename-in-region-atpt (&optional arg)
  "Employ actions of  at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th arg))

(defun ar-greaterangle-filename-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-filename-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-lesserangle arg))

(defun ar-backslash-filename-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-backslash arg))

(defun ar-colon-filename-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-colon arg))

(defun ar-beg-filename-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-beg arg))

(defun ar-blok-filename-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-blok arg))

(defun ar-bounds-filename-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-bounds arg))

(defun ar-brace-filename-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-brace arg))

(defun ar-bracket-filename-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-bracket arg))

(defun ar-commatize-filename-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-commatize arg))

(defun ar-comment-filename-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-comment arg))

(defun ar-dollar-filename-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-filename-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-filename-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-filename-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-filename-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-filename-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-filename-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-end arg))

(defun ar-escape-filename-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-escape arg))

(defun ar-hide-filename-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-hide arg))

(defun ar-hide-show-filename-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-hide-show arg))

(defun ar-hyphen-filename-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-hyphen arg))

(defun ar-kill-filename-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-filename-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-filename-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-length arg))

(defun ar-parentize-filename-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-parentize arg))

(defun ar-quote-filename-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-quote arg))

(defun ar-separate-filename-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-separate arg))

(defun ar-show-filename-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-show arg))

(defun ar-singlequote-filename-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-singlequote arg))

(defun ar-slash-filename-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-slash arg))

(defun ar-star-filename-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-star arg))

(defun ar-slashparen-filename-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-slashparen arg))

(defun ar-sort-filename-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-sort arg))

(defun ar-trim-filename-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-trim arg))

(defun ar-trim-left-filename-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-trim-left arg))

(defun ar-trim-right-filename-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-trim-right arg))

(defun ar-underscore-filename-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-underscore arg))

(defun ar-whitespace-filename-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of FILENAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filename 'region 'ar-th-whitespace arg))

(defun ar-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of  at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th arg))

(defun ar-greaterangle-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-lesserangle arg))

(defun ar-backslash-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-backslash arg))

(defun ar-colon-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-colon arg))

(defun ar-beg-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-beg arg))

(defun ar-blok-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-blok arg))

(defun ar-bounds-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-bounds arg))

(defun ar-brace-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-brace arg))

(defun ar-bracket-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-bracket arg))

(defun ar-commatize-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-commatize arg))

(defun ar-comment-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-comment arg))

(defun ar-dollar-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-end arg))

(defun ar-escape-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-escape arg))

(defun ar-hide-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-hide arg))

(defun ar-hide-show-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-hide-show arg))

(defun ar-hyphen-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-hyphen arg))

(defun ar-kill-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-length arg))

(defun ar-parentize-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-parentize arg))

(defun ar-quote-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-quote arg))

(defun ar-separate-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-separate arg))

(defun ar-show-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-show arg))

(defun ar-singlequote-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-singlequote arg))

(defun ar-slash-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-slash arg))

(defun ar-star-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-star arg))

(defun ar-slashparen-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-slashparen arg))

(defun ar-sort-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-sort arg))

(defun ar-trim-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-trim arg))

(defun ar-trim-left-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-trim-left arg))

(defun ar-trim-right-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-trim-right arg))

(defun ar-underscore-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-underscore arg))

(defun ar-whitespace-filenamenondirectory-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of FILENAMENONDIRECTORY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'filenamenondirectory 'region 'ar-th-whitespace arg))

(defun ar-float-in-region-atpt (&optional arg)
  "Employ actions of  at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th arg))

(defun ar-greaterangle-float-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-float-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-lesserangle arg))

(defun ar-backslash-float-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-backslash arg))

(defun ar-colon-float-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-colon arg))

(defun ar-beg-float-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-beg arg))

(defun ar-blok-float-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-blok arg))

(defun ar-bounds-float-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-bounds arg))

(defun ar-brace-float-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-brace arg))

(defun ar-bracket-float-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-bracket arg))

(defun ar-commatize-float-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-commatize arg))

(defun ar-comment-float-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-comment arg))

(defun ar-dollar-float-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-float-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-float-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-float-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-float-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-float-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-float-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-end arg))

(defun ar-escape-float-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-escape arg))

(defun ar-hide-float-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-hide arg))

(defun ar-hide-show-float-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-hide-show arg))

(defun ar-hyphen-float-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-hyphen arg))

(defun ar-kill-float-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-float-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-float-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-length arg))

(defun ar-parentize-float-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-parentize arg))

(defun ar-quote-float-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-quote arg))

(defun ar-separate-float-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-separate arg))

(defun ar-show-float-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-show arg))

(defun ar-singlequote-float-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-singlequote arg))

(defun ar-slash-float-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-slash arg))

(defun ar-star-float-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-star arg))

(defun ar-slashparen-float-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-slashparen arg))

(defun ar-sort-float-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-sort arg))

(defun ar-trim-float-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-trim arg))

(defun ar-trim-left-float-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-trim-left arg))

(defun ar-trim-right-float-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-trim-right arg))

(defun ar-underscore-float-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-underscore arg))

(defun ar-whitespace-float-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of FLOAT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'float 'region 'ar-th-whitespace arg))

(defun ar-function-in-region-atpt (&optional arg)
  "Employ actions of  at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th arg))

(defun ar-greaterangle-function-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-function-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-lesserangle arg))

(defun ar-backslash-function-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-backslash arg))

(defun ar-colon-function-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-colon arg))

(defun ar-beg-function-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-beg arg))

(defun ar-blok-function-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-blok arg))

(defun ar-bounds-function-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-bounds arg))

(defun ar-brace-function-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-brace arg))

(defun ar-bracket-function-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-bracket arg))

(defun ar-commatize-function-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-commatize arg))

(defun ar-comment-function-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-comment arg))

(defun ar-dollar-function-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-function-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-function-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-function-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-function-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-function-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-function-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-end arg))

(defun ar-escape-function-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-escape arg))

(defun ar-hide-function-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-hide arg))

(defun ar-hide-show-function-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-hide-show arg))

(defun ar-hyphen-function-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-hyphen arg))

(defun ar-kill-function-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-function-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-function-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-length arg))

(defun ar-parentize-function-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-parentize arg))

(defun ar-quote-function-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-quote arg))

(defun ar-separate-function-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-separate arg))

(defun ar-show-function-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-show arg))

(defun ar-singlequote-function-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-singlequote arg))

(defun ar-slash-function-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-slash arg))

(defun ar-star-function-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-star arg))

(defun ar-slashparen-function-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-slashparen arg))

(defun ar-sort-function-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-sort arg))

(defun ar-trim-function-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-trim arg))

(defun ar-trim-left-function-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-trim-left arg))

(defun ar-trim-right-function-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-trim-right arg))

(defun ar-underscore-function-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-underscore arg))

(defun ar-whitespace-function-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of FUNCTION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'function 'region 'ar-th-whitespace arg))

(defun ar-ip-in-region-atpt (&optional arg)
  "Employ actions of  at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th arg))

(defun ar-greaterangle-ip-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-ip-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-lesserangle arg))

(defun ar-backslash-ip-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-backslash arg))

(defun ar-colon-ip-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-colon arg))

(defun ar-beg-ip-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-beg arg))

(defun ar-blok-ip-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-blok arg))

(defun ar-bounds-ip-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-bounds arg))

(defun ar-brace-ip-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-brace arg))

(defun ar-bracket-ip-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-bracket arg))

(defun ar-commatize-ip-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-commatize arg))

(defun ar-comment-ip-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-comment arg))

(defun ar-dollar-ip-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-ip-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-ip-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-ip-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-ip-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-ip-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-ip-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-end arg))

(defun ar-escape-ip-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-escape arg))

(defun ar-hide-ip-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-hide arg))

(defun ar-hide-show-ip-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-hide-show arg))

(defun ar-hyphen-ip-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-hyphen arg))

(defun ar-kill-ip-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-ip-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-ip-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-length arg))

(defun ar-parentize-ip-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-parentize arg))

(defun ar-quote-ip-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-quote arg))

(defun ar-separate-ip-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-separate arg))

(defun ar-show-ip-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-show arg))

(defun ar-singlequote-ip-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-singlequote arg))

(defun ar-slash-ip-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-slash arg))

(defun ar-star-ip-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-star arg))

(defun ar-slashparen-ip-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-slashparen arg))

(defun ar-sort-ip-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-sort arg))

(defun ar-trim-ip-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-trim arg))

(defun ar-trim-left-ip-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-trim-left arg))

(defun ar-trim-right-ip-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-trim-right arg))

(defun ar-underscore-ip-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-underscore arg))

(defun ar-whitespace-ip-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of IP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'ip 'region 'ar-th-whitespace arg))

(defun ar-isbn-in-region-atpt (&optional arg)
  "Employ actions of  at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th arg))

(defun ar-greaterangle-isbn-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-isbn-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-lesserangle arg))

(defun ar-backslash-isbn-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-backslash arg))

(defun ar-colon-isbn-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-colon arg))

(defun ar-beg-isbn-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-beg arg))

(defun ar-blok-isbn-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-blok arg))

(defun ar-bounds-isbn-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-bounds arg))

(defun ar-brace-isbn-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-brace arg))

(defun ar-bracket-isbn-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-bracket arg))

(defun ar-commatize-isbn-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-commatize arg))

(defun ar-comment-isbn-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-comment arg))

(defun ar-dollar-isbn-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-isbn-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-isbn-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-isbn-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-isbn-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-isbn-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-isbn-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-end arg))

(defun ar-escape-isbn-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-escape arg))

(defun ar-hide-isbn-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-hide arg))

(defun ar-hide-show-isbn-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-hide-show arg))

(defun ar-hyphen-isbn-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-hyphen arg))

(defun ar-kill-isbn-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-isbn-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-isbn-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-length arg))

(defun ar-parentize-isbn-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-parentize arg))

(defun ar-quote-isbn-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-quote arg))

(defun ar-separate-isbn-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-separate arg))

(defun ar-show-isbn-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-show arg))

(defun ar-singlequote-isbn-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-singlequote arg))

(defun ar-slash-isbn-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-slash arg))

(defun ar-star-isbn-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-star arg))

(defun ar-slashparen-isbn-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-slashparen arg))

(defun ar-sort-isbn-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-sort arg))

(defun ar-trim-isbn-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-trim arg))

(defun ar-trim-left-isbn-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-trim-left arg))

(defun ar-trim-right-isbn-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-trim-right arg))

(defun ar-underscore-isbn-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-underscore arg))

(defun ar-whitespace-isbn-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of ISBN residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'isbn 'region 'ar-th-whitespace arg))

(defun ar-line-in-region-atpt (&optional arg)
  "Employ actions of  at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th arg))

(defun ar-greaterangle-line-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-line-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-lesserangle arg))

(defun ar-backslash-line-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-backslash arg))

(defun ar-colon-line-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-colon arg))

(defun ar-beg-line-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-beg arg))

(defun ar-blok-line-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-blok arg))

(defun ar-bounds-line-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-bounds arg))

(defun ar-brace-line-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-brace arg))

(defun ar-bracket-line-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-bracket arg))

(defun ar-commatize-line-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-commatize arg))

(defun ar-comment-line-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-comment arg))

(defun ar-dollar-line-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-line-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-line-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-line-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-line-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-line-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-line-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-end arg))

(defun ar-escape-line-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-escape arg))

(defun ar-hide-line-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-hide arg))

(defun ar-hide-show-line-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-hide-show arg))

(defun ar-hyphen-line-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-hyphen arg))

(defun ar-kill-line-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-line-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-line-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-length arg))

(defun ar-parentize-line-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-parentize arg))

(defun ar-quote-line-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-quote arg))

(defun ar-separate-line-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-separate arg))

(defun ar-show-line-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-show arg))

(defun ar-singlequote-line-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-singlequote arg))

(defun ar-slash-line-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-slash arg))

(defun ar-star-line-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-star arg))

(defun ar-slashparen-line-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-slashparen arg))

(defun ar-sort-line-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-sort arg))

(defun ar-trim-line-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-trim arg))

(defun ar-trim-left-line-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-trim-left arg))

(defun ar-trim-right-line-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-trim-right arg))

(defun ar-underscore-line-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-underscore arg))

(defun ar-whitespace-line-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of LINE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'line 'region 'ar-th-whitespace arg))

(defun ar-list-in-region-atpt (&optional arg)
  "Employ actions of  at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th arg))

(defun ar-greaterangle-list-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-list-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-lesserangle arg))

(defun ar-backslash-list-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-backslash arg))

(defun ar-colon-list-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-colon arg))

(defun ar-beg-list-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-beg arg))

(defun ar-blok-list-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-blok arg))

(defun ar-bounds-list-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-bounds arg))

(defun ar-brace-list-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-brace arg))

(defun ar-bracket-list-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-bracket arg))

(defun ar-commatize-list-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-commatize arg))

(defun ar-comment-list-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-comment arg))

(defun ar-dollar-list-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-list-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-list-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-list-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-list-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-list-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-list-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-end arg))

(defun ar-escape-list-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-escape arg))

(defun ar-hide-list-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-hide arg))

(defun ar-hide-show-list-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-hide-show arg))

(defun ar-hyphen-list-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-hyphen arg))

(defun ar-kill-list-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-list-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-list-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-length arg))

(defun ar-parentize-list-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-parentize arg))

(defun ar-quote-list-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-quote arg))

(defun ar-separate-list-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-separate arg))

(defun ar-show-list-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-show arg))

(defun ar-singlequote-list-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-singlequote arg))

(defun ar-slash-list-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-slash arg))

(defun ar-star-list-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-star arg))

(defun ar-slashparen-list-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-slashparen arg))

(defun ar-sort-list-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-sort arg))

(defun ar-trim-list-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-trim arg))

(defun ar-trim-left-list-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-trim-left arg))

(defun ar-trim-right-list-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-trim-right arg))

(defun ar-underscore-list-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-underscore arg))

(defun ar-whitespace-list-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of LIST residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'list 'region 'ar-th-whitespace arg))

(defun ar-name-in-region-atpt (&optional arg)
  "Employ actions of  at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th arg))

(defun ar-greaterangle-name-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-name-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-lesserangle arg))

(defun ar-backslash-name-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-backslash arg))

(defun ar-colon-name-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-colon arg))

(defun ar-beg-name-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-beg arg))

(defun ar-blok-name-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-blok arg))

(defun ar-bounds-name-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-bounds arg))

(defun ar-brace-name-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-brace arg))

(defun ar-bracket-name-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-bracket arg))

(defun ar-commatize-name-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-commatize arg))

(defun ar-comment-name-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-comment arg))

(defun ar-dollar-name-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-name-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-name-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-name-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-name-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-name-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-name-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-end arg))

(defun ar-escape-name-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-escape arg))

(defun ar-hide-name-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-hide arg))

(defun ar-hide-show-name-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-hide-show arg))

(defun ar-hyphen-name-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-hyphen arg))

(defun ar-kill-name-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-name-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-name-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-length arg))

(defun ar-parentize-name-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-parentize arg))

(defun ar-quote-name-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-quote arg))

(defun ar-separate-name-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-separate arg))

(defun ar-show-name-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-show arg))

(defun ar-singlequote-name-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-singlequote arg))

(defun ar-slash-name-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-slash arg))

(defun ar-star-name-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-star arg))

(defun ar-slashparen-name-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-slashparen arg))

(defun ar-sort-name-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-sort arg))

(defun ar-trim-name-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-trim arg))

(defun ar-trim-left-name-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-trim-left arg))

(defun ar-trim-right-name-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-trim-right arg))

(defun ar-underscore-name-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-underscore arg))

(defun ar-whitespace-name-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of NAME residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'name 'region 'ar-th-whitespace arg))

(defun ar-number-in-region-atpt (&optional arg)
  "Employ actions of  at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th arg))

(defun ar-greaterangle-number-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-number-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-lesserangle arg))

(defun ar-backslash-number-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-backslash arg))

(defun ar-colon-number-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-colon arg))

(defun ar-beg-number-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-beg arg))

(defun ar-blok-number-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-blok arg))

(defun ar-bounds-number-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-bounds arg))

(defun ar-brace-number-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-brace arg))

(defun ar-bracket-number-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-bracket arg))

(defun ar-commatize-number-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-commatize arg))

(defun ar-comment-number-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-comment arg))

(defun ar-dollar-number-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-number-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-number-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-number-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-number-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-number-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-number-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-end arg))

(defun ar-escape-number-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-escape arg))

(defun ar-hide-number-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-hide arg))

(defun ar-hide-show-number-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-hide-show arg))

(defun ar-hyphen-number-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-hyphen arg))

(defun ar-kill-number-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-number-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-number-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-length arg))

(defun ar-parentize-number-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-parentize arg))

(defun ar-quote-number-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-quote arg))

(defun ar-separate-number-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-separate arg))

(defun ar-show-number-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-show arg))

(defun ar-singlequote-number-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-singlequote arg))

(defun ar-slash-number-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-slash arg))

(defun ar-star-number-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-star arg))

(defun ar-slashparen-number-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-slashparen arg))

(defun ar-sort-number-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-sort arg))

(defun ar-trim-number-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-trim arg))

(defun ar-trim-left-number-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-trim-left arg))

(defun ar-trim-right-number-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-trim-right arg))

(defun ar-underscore-number-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-underscore arg))

(defun ar-whitespace-number-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of NUMBER residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'number 'region 'ar-th-whitespace arg))

(defun ar-page-in-region-atpt (&optional arg)
  "Employ actions of  at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th arg))

(defun ar-greaterangle-page-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-page-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-lesserangle arg))

(defun ar-backslash-page-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-backslash arg))

(defun ar-colon-page-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-colon arg))

(defun ar-beg-page-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-beg arg))

(defun ar-blok-page-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-blok arg))

(defun ar-bounds-page-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-bounds arg))

(defun ar-brace-page-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-brace arg))

(defun ar-bracket-page-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-bracket arg))

(defun ar-commatize-page-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-commatize arg))

(defun ar-comment-page-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-comment arg))

(defun ar-dollar-page-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-page-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-page-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-page-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-page-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-page-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-page-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-end arg))

(defun ar-escape-page-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-escape arg))

(defun ar-hide-page-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-hide arg))

(defun ar-hide-show-page-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-hide-show arg))

(defun ar-hyphen-page-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-hyphen arg))

(defun ar-kill-page-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-page-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-page-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-length arg))

(defun ar-parentize-page-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-parentize arg))

(defun ar-quote-page-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-quote arg))

(defun ar-separate-page-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-separate arg))

(defun ar-show-page-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-show arg))

(defun ar-singlequote-page-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-singlequote arg))

(defun ar-slash-page-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-slash arg))

(defun ar-star-page-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-star arg))

(defun ar-slashparen-page-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-slashparen arg))

(defun ar-sort-page-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-sort arg))

(defun ar-trim-page-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-trim arg))

(defun ar-trim-left-page-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-trim-left arg))

(defun ar-trim-right-page-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-trim-right arg))

(defun ar-underscore-page-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-underscore arg))

(defun ar-whitespace-page-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of PAGE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'page 'region 'ar-th-whitespace arg))

(defun ar-paragraph-in-region-atpt (&optional arg)
  "Employ actions of  at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th arg))

(defun ar-greaterangle-paragraph-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-paragraph-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-lesserangle arg))

(defun ar-backslash-paragraph-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-backslash arg))

(defun ar-colon-paragraph-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-colon arg))

(defun ar-beg-paragraph-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-beg arg))

(defun ar-blok-paragraph-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-blok arg))

(defun ar-bounds-paragraph-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-bounds arg))

(defun ar-brace-paragraph-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-brace arg))

(defun ar-bracket-paragraph-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-bracket arg))

(defun ar-commatize-paragraph-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-commatize arg))

(defun ar-comment-paragraph-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-comment arg))

(defun ar-dollar-paragraph-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-paragraph-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-paragraph-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-paragraph-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-paragraph-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-paragraph-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-paragraph-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-end arg))

(defun ar-escape-paragraph-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-escape arg))

(defun ar-hide-paragraph-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-hide arg))

(defun ar-hide-show-paragraph-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-hide-show arg))

(defun ar-hyphen-paragraph-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-hyphen arg))

(defun ar-kill-paragraph-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-paragraph-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-paragraph-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-length arg))

(defun ar-parentize-paragraph-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-parentize arg))

(defun ar-quote-paragraph-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-quote arg))

(defun ar-separate-paragraph-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-separate arg))

(defun ar-show-paragraph-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-show arg))

(defun ar-singlequote-paragraph-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-singlequote arg))

(defun ar-slash-paragraph-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-slash arg))

(defun ar-star-paragraph-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-star arg))

(defun ar-slashparen-paragraph-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-slashparen arg))

(defun ar-sort-paragraph-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-sort arg))

(defun ar-trim-paragraph-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-trim arg))

(defun ar-trim-left-paragraph-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-trim-left arg))

(defun ar-trim-right-paragraph-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-trim-right arg))

(defun ar-underscore-paragraph-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-underscore arg))

(defun ar-whitespace-paragraph-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of PARAGRAPH residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'paragraph 'region 'ar-th-whitespace arg))

(defun ar-phone-in-region-atpt (&optional arg)
  "Employ actions of  at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th arg))

(defun ar-greaterangle-phone-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-phone-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-lesserangle arg))

(defun ar-backslash-phone-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-backslash arg))

(defun ar-colon-phone-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-colon arg))

(defun ar-beg-phone-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-beg arg))

(defun ar-blok-phone-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-blok arg))

(defun ar-bounds-phone-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-bounds arg))

(defun ar-brace-phone-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-brace arg))

(defun ar-bracket-phone-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-bracket arg))

(defun ar-commatize-phone-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-commatize arg))

(defun ar-comment-phone-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-comment arg))

(defun ar-dollar-phone-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-phone-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-phone-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-phone-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-phone-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-phone-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-phone-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-end arg))

(defun ar-escape-phone-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-escape arg))

(defun ar-hide-phone-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-hide arg))

(defun ar-hide-show-phone-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-hide-show arg))

(defun ar-hyphen-phone-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-hyphen arg))

(defun ar-kill-phone-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-phone-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-phone-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-length arg))

(defun ar-parentize-phone-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-parentize arg))

(defun ar-quote-phone-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-quote arg))

(defun ar-separate-phone-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-separate arg))

(defun ar-show-phone-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-show arg))

(defun ar-singlequote-phone-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-singlequote arg))

(defun ar-slash-phone-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-slash arg))

(defun ar-star-phone-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-star arg))

(defun ar-slashparen-phone-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-slashparen arg))

(defun ar-sort-phone-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-sort arg))

(defun ar-trim-phone-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-trim arg))

(defun ar-trim-left-phone-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-trim-left arg))

(defun ar-trim-right-phone-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-trim-right arg))

(defun ar-underscore-phone-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-underscore arg))

(defun ar-whitespace-phone-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of PHONE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'phone 'region 'ar-th-whitespace arg))

(defun ar-region-in-region-atpt (&optional arg)
  "Employ actions of  at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th arg))

(defun ar-greaterangle-region-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-region-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-lesserangle arg))

(defun ar-backslash-region-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-backslash arg))

(defun ar-colon-region-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-colon arg))

(defun ar-beg-region-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-beg arg))

(defun ar-blok-region-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-blok arg))

(defun ar-bounds-region-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-bounds arg))

(defun ar-brace-region-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-brace arg))

(defun ar-bracket-region-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-bracket arg))

(defun ar-commatize-region-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-commatize arg))

(defun ar-comment-region-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-comment arg))

(defun ar-dollar-region-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-region-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-region-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-region-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-region-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-region-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-region-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-end arg))

(defun ar-escape-region-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-escape arg))

(defun ar-hide-region-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-hide arg))

(defun ar-hide-show-region-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-hide-show arg))

(defun ar-hyphen-region-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-hyphen arg))

(defun ar-kill-region-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-region-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-region-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-length arg))

(defun ar-parentize-region-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-parentize arg))

(defun ar-quote-region-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-quote arg))

(defun ar-separate-region-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-separate arg))

(defun ar-show-region-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-show arg))

(defun ar-singlequote-region-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-singlequote arg))

(defun ar-slash-region-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-slash arg))

(defun ar-star-region-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-star arg))

(defun ar-slashparen-region-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-slashparen arg))

(defun ar-sort-region-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-sort arg))

(defun ar-trim-region-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-trim arg))

(defun ar-trim-left-region-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-trim-left arg))

(defun ar-trim-right-region-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-trim-right arg))

(defun ar-underscore-region-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-underscore arg))

(defun ar-whitespace-region-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of REGION residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'region 'region 'ar-th-whitespace arg))

(defun ar-sentence-in-region-atpt (&optional arg)
  "Employ actions of  at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th arg))

(defun ar-greaterangle-sentence-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-sentence-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-lesserangle arg))

(defun ar-backslash-sentence-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-backslash arg))

(defun ar-colon-sentence-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-colon arg))

(defun ar-beg-sentence-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-beg arg))

(defun ar-blok-sentence-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-blok arg))

(defun ar-bounds-sentence-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-bounds arg))

(defun ar-brace-sentence-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-brace arg))

(defun ar-bracket-sentence-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-bracket arg))

(defun ar-commatize-sentence-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-commatize arg))

(defun ar-comment-sentence-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-comment arg))

(defun ar-dollar-sentence-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-sentence-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-sentence-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-sentence-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-sentence-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-sentence-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-sentence-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-end arg))

(defun ar-escape-sentence-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-escape arg))

(defun ar-hide-sentence-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-hide arg))

(defun ar-hide-show-sentence-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-hide-show arg))

(defun ar-hyphen-sentence-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-hyphen arg))

(defun ar-kill-sentence-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-sentence-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-sentence-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-length arg))

(defun ar-parentize-sentence-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-parentize arg))

(defun ar-quote-sentence-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-quote arg))

(defun ar-separate-sentence-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-separate arg))

(defun ar-show-sentence-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-show arg))

(defun ar-singlequote-sentence-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-singlequote arg))

(defun ar-slash-sentence-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-slash arg))

(defun ar-star-sentence-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-star arg))

(defun ar-slashparen-sentence-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-slashparen arg))

(defun ar-sort-sentence-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-sort arg))

(defun ar-trim-sentence-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-trim arg))

(defun ar-trim-left-sentence-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-trim-left arg))

(defun ar-trim-right-sentence-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-trim-right arg))

(defun ar-underscore-sentence-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-underscore arg))

(defun ar-whitespace-sentence-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of SENTENCE residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sentence 'region 'ar-th-whitespace arg))

(defun ar-sexp-in-region-atpt (&optional arg)
  "Employ actions of  at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th arg))

(defun ar-greaterangle-sexp-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-sexp-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-lesserangle arg))

(defun ar-backslash-sexp-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-backslash arg))

(defun ar-colon-sexp-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-colon arg))

(defun ar-beg-sexp-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-beg arg))

(defun ar-blok-sexp-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-blok arg))

(defun ar-bounds-sexp-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-bounds arg))

(defun ar-brace-sexp-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-brace arg))

(defun ar-bracket-sexp-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-bracket arg))

(defun ar-commatize-sexp-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-commatize arg))

(defun ar-comment-sexp-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-comment arg))

(defun ar-dollar-sexp-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-sexp-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-sexp-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-sexp-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-sexp-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-sexp-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-sexp-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-end arg))

(defun ar-escape-sexp-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-escape arg))

(defun ar-hide-sexp-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-hide arg))

(defun ar-hide-show-sexp-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-hide-show arg))

(defun ar-hyphen-sexp-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-hyphen arg))

(defun ar-kill-sexp-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-sexp-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-sexp-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-length arg))

(defun ar-parentize-sexp-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-parentize arg))

(defun ar-quote-sexp-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-quote arg))

(defun ar-separate-sexp-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-separate arg))

(defun ar-show-sexp-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-show arg))

(defun ar-singlequote-sexp-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-singlequote arg))

(defun ar-slash-sexp-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-slash arg))

(defun ar-star-sexp-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-star arg))

(defun ar-slashparen-sexp-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-slashparen arg))

(defun ar-sort-sexp-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-sort arg))

(defun ar-trim-sexp-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-trim arg))

(defun ar-trim-left-sexp-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-trim-left arg))

(defun ar-trim-right-sexp-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-trim-right arg))

(defun ar-underscore-sexp-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-underscore arg))

(defun ar-whitespace-sexp-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of SEXP residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'sexp 'region 'ar-th-whitespace arg))

(defun ar-shstruct-in-region-atpt (&optional arg)
  "Employ actions of  at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th arg))

(defun ar-greaterangle-shstruct-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-shstruct-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-lesserangle arg))

(defun ar-backslash-shstruct-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-backslash arg))

(defun ar-colon-shstruct-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-colon arg))

(defun ar-beg-shstruct-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-beg arg))

(defun ar-blok-shstruct-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-blok arg))

(defun ar-bounds-shstruct-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-bounds arg))

(defun ar-brace-shstruct-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-brace arg))

(defun ar-bracket-shstruct-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-bracket arg))

(defun ar-commatize-shstruct-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-commatize arg))

(defun ar-comment-shstruct-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-comment arg))

(defun ar-dollar-shstruct-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-shstruct-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-shstruct-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-shstruct-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-shstruct-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-shstruct-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-shstruct-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-end arg))

(defun ar-escape-shstruct-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-escape arg))

(defun ar-hide-shstruct-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-hide arg))

(defun ar-hide-show-shstruct-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-hide-show arg))

(defun ar-hyphen-shstruct-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-hyphen arg))

(defun ar-kill-shstruct-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-shstruct-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-shstruct-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-length arg))

(defun ar-parentize-shstruct-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-parentize arg))

(defun ar-quote-shstruct-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-quote arg))

(defun ar-separate-shstruct-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-separate arg))

(defun ar-show-shstruct-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-show arg))

(defun ar-singlequote-shstruct-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-singlequote arg))

(defun ar-slash-shstruct-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-slash arg))

(defun ar-star-shstruct-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-star arg))

(defun ar-slashparen-shstruct-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-slashparen arg))

(defun ar-sort-shstruct-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-sort arg))

(defun ar-trim-shstruct-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-trim arg))

(defun ar-trim-left-shstruct-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-trim-left arg))

(defun ar-trim-right-shstruct-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-trim-right arg))

(defun ar-underscore-shstruct-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-underscore arg))

(defun ar-whitespace-shstruct-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of SHSTRUCT residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'shstruct 'region 'ar-th-whitespace arg))

(defun ar-symbol-in-region-atpt (&optional arg)
  "Employ actions of  at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th arg))

(defun ar-greaterangle-symbol-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-symbol-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-lesserangle arg))

(defun ar-backslash-symbol-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-backslash arg))

(defun ar-colon-symbol-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-colon arg))

(defun ar-beg-symbol-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-beg arg))

(defun ar-blok-symbol-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-blok arg))

(defun ar-bounds-symbol-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-bounds arg))

(defun ar-brace-symbol-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-brace arg))

(defun ar-bracket-symbol-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-bracket arg))

(defun ar-commatize-symbol-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-commatize arg))

(defun ar-comment-symbol-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-comment arg))

(defun ar-dollar-symbol-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-symbol-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-symbol-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-symbol-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-symbol-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-symbol-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-symbol-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-end arg))

(defun ar-escape-symbol-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-escape arg))

(defun ar-hide-symbol-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-hide arg))

(defun ar-hide-show-symbol-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-hide-show arg))

(defun ar-hyphen-symbol-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-hyphen arg))

(defun ar-kill-symbol-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-symbol-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-symbol-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-length arg))

(defun ar-parentize-symbol-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-parentize arg))

(defun ar-quote-symbol-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-quote arg))

(defun ar-separate-symbol-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-separate arg))

(defun ar-show-symbol-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-show arg))

(defun ar-singlequote-symbol-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-singlequote arg))

(defun ar-slash-symbol-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-slash arg))

(defun ar-star-symbol-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-star arg))

(defun ar-slashparen-symbol-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-slashparen arg))

(defun ar-sort-symbol-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-sort arg))

(defun ar-trim-symbol-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-trim arg))

(defun ar-trim-left-symbol-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-trim-left arg))

(defun ar-trim-right-symbol-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-trim-right arg))

(defun ar-underscore-symbol-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-underscore arg))

(defun ar-whitespace-symbol-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of SYMBOL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'symbol 'region 'ar-th-whitespace arg))

(defun ar-url-in-region-atpt (&optional arg)
  "Employ actions of  at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th arg))

(defun ar-greaterangle-url-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-url-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-lesserangle arg))

(defun ar-backslash-url-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-backslash arg))

(defun ar-colon-url-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-colon arg))

(defun ar-beg-url-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-beg arg))

(defun ar-blok-url-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-blok arg))

(defun ar-bounds-url-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-bounds arg))

(defun ar-brace-url-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-brace arg))

(defun ar-bracket-url-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-bracket arg))

(defun ar-commatize-url-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-commatize arg))

(defun ar-comment-url-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-comment arg))

(defun ar-dollar-url-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-url-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-url-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-url-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-url-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-url-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-url-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-end arg))

(defun ar-escape-url-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-escape arg))

(defun ar-hide-url-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-hide arg))

(defun ar-hide-show-url-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-hide-show arg))

(defun ar-hyphen-url-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-hyphen arg))

(defun ar-kill-url-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-url-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-url-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-length arg))

(defun ar-parentize-url-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-parentize arg))

(defun ar-quote-url-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-quote arg))

(defun ar-separate-url-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-separate arg))

(defun ar-show-url-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-show arg))

(defun ar-singlequote-url-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-singlequote arg))

(defun ar-slash-url-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-slash arg))

(defun ar-star-url-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-star arg))

(defun ar-slashparen-url-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-slashparen arg))

(defun ar-sort-url-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-sort arg))

(defun ar-trim-url-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-trim arg))

(defun ar-trim-left-url-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-trim-left arg))

(defun ar-trim-right-url-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-trim-right arg))

(defun ar-underscore-url-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-underscore arg))

(defun ar-whitespace-url-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of URL residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'url 'region 'ar-th-whitespace arg))

(defun ar-word-in-region-atpt (&optional arg)
  "Employ actions of  at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th arg))

(defun ar-greaterangle-word-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-word-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-lesserangle arg))

(defun ar-backslash-word-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-backslash arg))

(defun ar-colon-word-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-colon arg))

(defun ar-beg-word-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-beg arg))

(defun ar-blok-word-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-blok arg))

(defun ar-bounds-word-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-bounds arg))

(defun ar-brace-word-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-brace arg))

(defun ar-bracket-word-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-bracket arg))

(defun ar-commatize-word-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-commatize arg))

(defun ar-comment-word-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-comment arg))

(defun ar-dollar-word-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-word-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-word-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-word-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-word-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-word-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-word-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-end arg))

(defun ar-escape-word-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-escape arg))

(defun ar-hide-word-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-hide arg))

(defun ar-hide-show-word-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-hide-show arg))

(defun ar-hyphen-word-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-hyphen arg))

(defun ar-kill-word-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-word-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-word-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-length arg))

(defun ar-parentize-word-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-parentize arg))

(defun ar-quote-word-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-quote arg))

(defun ar-separate-word-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-separate arg))

(defun ar-show-word-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-show arg))

(defun ar-singlequote-word-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-singlequote arg))

(defun ar-slash-word-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-slash arg))

(defun ar-star-word-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-star arg))

(defun ar-slashparen-word-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-slashparen arg))

(defun ar-sort-word-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-sort arg))

(defun ar-trim-word-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-trim arg))

(defun ar-trim-left-word-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-trim-left arg))

(defun ar-trim-right-word-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-trim-right arg))

(defun ar-underscore-word-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-underscore arg))

(defun ar-whitespace-word-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of WORD residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'word 'region 'ar-th-whitespace arg))

(defun ar-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of  at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th arg))

(defun ar-greaterangle-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of GREATERANGLE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-greaterangle arg))

(defun ar-lesserangle-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of LESSERANGLE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-lesserangle arg))

(defun ar-backslash-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of BACKSLASH- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-backslash arg))

(defun ar-colon-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of COLON- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-colon arg))

(defun ar-beg-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of BEG- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-beg arg))

(defun ar-blok-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of BLOK- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-blok arg))

(defun ar-bounds-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of BOUNDS- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-bounds arg))

(defun ar-brace-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of BRACE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-brace arg))

(defun ar-bracket-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of BRACKET- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-bracket arg))

(defun ar-commatize-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of COMMATIZE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-commatize arg))

(defun ar-comment-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of COMMENT- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-comment arg))

(defun ar-dollar-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of DOLLAR- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-dollar arg))

(defun ar-doublebackslash-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-doublebackslash arg))

(defun ar-doublebacktick-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKTICK- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-doublebacktick arg))

(defun ar-doublequote-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-doublequote arg))

(defun ar-doubleslash-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of DOUBLESLASH- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-doubleslash arg))

(defun ar-doublebackslashparen-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-doublebackslashparen arg))

(defun ar-end-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of END- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-end arg))

(defun ar-escape-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of ESCAPE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-escape arg))

(defun ar-hide-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of HIDE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-hide arg))

(defun ar-hide-show-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of HIDE-SHOW- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-hide-show arg))

(defun ar-hyphen-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of HYPHEN- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-hyphen arg))

(defun ar-kill-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of KILL- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-kill arg))

(defun ar-leftrightsinglequote-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-leftrightsinglequote arg))

(defun ar-length-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of LENGTH- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-length arg))

(defun ar-parentize-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of PARENTIZE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-parentize arg))

(defun ar-quote-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of QUOTE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-quote arg))

(defun ar-separate-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of SEPARATE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-separate arg))

(defun ar-show-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of SHOW- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-show arg))

(defun ar-singlequote-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-singlequote arg))

(defun ar-slash-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of SLASH- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-slash arg))

(defun ar-star-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of STAR- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-star arg))

(defun ar-slashparen-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of SLASHPAREN- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-slashparen arg))

(defun ar-sort-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of SORT- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-sort arg))

(defun ar-trim-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of TRIM- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-trim arg))

(defun ar-trim-left-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of TRIM-LEFT- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-trim-left arg))

(defun ar-trim-right-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of TRIM-RIGHT- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-trim-right arg))

(defun ar-underscore-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of UNDERSCORE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-underscore arg))

(defun ar-whitespace-wordalphaonly-in-region-atpt (&optional arg)
  "Employ actions of WHITESPACE- at things class of WORDALPHAONLY residing withing REGION. "
  (interactive "*p")
  (ar-thing-in-thing 'wordalphaonly 'region 'ar-th-whitespace arg))

(provide 'thing-rest-list-in-region-only)
;;;thing-rest-list-in-region-only.el ends here

