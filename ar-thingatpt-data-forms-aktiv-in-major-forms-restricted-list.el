;;; thing-data-forms-aktiv-in-major-forms-restricted-list.el --- thing-in-thing functions -*- lexical-binding: t; -*-
;; Built by ar-thing-in-thing-anlegen-intern ar-atpt-data-forms-aktiv ar-atpt-major-forms-restricted-list


;; Copyright (C) 2010-2026 Andreas Röhler, unless
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

(defun ar-beginendquote-in-buffer-atpt ()
  "Employ actions of  at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th))

(defun ar-greaterangle-beginendquote-in-buffer-atpt ()
  "Employ actions of GREATERANGLE- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-greaterangle))

(defun ar-lesserangle-beginendquote-in-buffer-atpt ()
  "Employ actions of LESSERANGLE- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-lesserangle))

(defun ar-backslash-beginendquote-in-buffer-atpt ()
  "Employ actions of BACKSLASH- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-backslash))

(defun ar-colon-beginendquote-in-buffer-atpt ()
  "Employ actions of COLON- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-colon))

(defun ar-beg-beginendquote-in-buffer-atpt ()
  "Employ actions of BEG- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-beg))

(defun ar-blok-beginendquote-in-buffer-atpt ()
  "Employ actions of BLOK- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-blok))

(defun ar-bounds-beginendquote-in-buffer-atpt ()
  "Employ actions of BOUNDS- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-bounds))

(defun ar-brace-beginendquote-in-buffer-atpt ()
  "Employ actions of BRACE- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-brace))

(defun ar-bracket-beginendquote-in-buffer-atpt ()
  "Employ actions of BRACKET- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-bracket))

(defun ar-commatize-beginendquote-in-buffer-atpt ()
  "Employ actions of COMMATIZE- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-commatize))

(defun ar-comment-beginendquote-in-buffer-atpt ()
  "Employ actions of COMMENT- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-comment))

(defun ar-dollar-beginendquote-in-buffer-atpt ()
  "Employ actions of DOLLAR- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-dollar))

(defun ar-doublebackslash-beginendquote-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-doublebackslash))

(defun ar-doublebacktick-beginendquote-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-doublebacktick))

(defun ar-doublequote-beginendquote-in-buffer-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-doublequote))

(defun ar-doubleslash-beginendquote-in-buffer-atpt ()
  "Employ actions of DOUBLESLASH- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-doubleslash))

(defun ar-doublebackslashparen-beginendquote-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-doublebackslashparen))

(defun ar-end-beginendquote-in-buffer-atpt ()
  "Employ actions of END- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-end))

(defun ar-escape-beginendquote-in-buffer-atpt ()
  "Employ actions of ESCAPE- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-escape))

(defun ar-hide-beginendquote-in-buffer-atpt ()
  "Employ actions of HIDE- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-hide))

(defun ar-hide-show-beginendquote-in-buffer-atpt ()
  "Employ actions of HIDE-SHOW- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-hide-show))

(defun ar-hyphen-beginendquote-in-buffer-atpt ()
  "Employ actions of HYPHEN- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-hyphen))

(defun ar-kill-beginendquote-in-buffer-atpt ()
  "Employ actions of KILL- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-kill))

(defun ar-curvedsinglequote-beginendquote-in-buffer-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-curvedsinglequote))

(defun ar-length-beginendquote-in-buffer-atpt ()
  "Employ actions of LENGTH- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-length))

(defun ar-parentize-beginendquote-in-buffer-atpt ()
  "Employ actions of PARENTIZE- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-parentize))

(defun ar-quote-beginendquote-in-buffer-atpt ()
  "Employ actions of QUOTE- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-quote))

(defun ar-separate-beginendquote-in-buffer-atpt ()
  "Employ actions of SEPARATE- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-separate))

(defun ar-show-beginendquote-in-buffer-atpt ()
  "Employ actions of SHOW- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-show))

(defun ar-singlequote-beginendquote-in-buffer-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-singlequote))

(defun ar-slash-beginendquote-in-buffer-atpt ()
  "Employ actions of SLASH- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-slash))

(defun ar-star-beginendquote-in-buffer-atpt ()
  "Employ actions of STAR- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-star))

(defun ar-slashparen-beginendquote-in-buffer-atpt ()
  "Employ actions of SLASHPAREN- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-slashparen))

(defun ar-sort-beginendquote-in-buffer-atpt ()
  "Employ actions of SORT- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-sort))

(defun ar-trim-beginendquote-in-buffer-atpt ()
  "Employ actions of TRIM- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-trim))

(defun ar-trim-left-beginendquote-in-buffer-atpt ()
  "Employ actions of TRIM-LEFT- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-trim-left))

(defun ar-trim-right-beginendquote-in-buffer-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-trim-right))

(defun ar-underscore-beginendquote-in-buffer-atpt ()
  "Employ actions of UNDERSCORE- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-underscore))

(defun ar-whitespace-beginendquote-in-buffer-atpt ()
  "Employ actions of WHITESPACE- at things class of BEGINENDQUOTE residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'buffer 'ar-th-whitespace))

(defun ar-beginendquote-in-page-atpt ()
  "Employ actions of  at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th))

(defun ar-greaterangle-beginendquote-in-page-atpt ()
  "Employ actions of GREATERANGLE- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-greaterangle))

(defun ar-lesserangle-beginendquote-in-page-atpt ()
  "Employ actions of LESSERANGLE- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-lesserangle))

(defun ar-backslash-beginendquote-in-page-atpt ()
  "Employ actions of BACKSLASH- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-backslash))

(defun ar-colon-beginendquote-in-page-atpt ()
  "Employ actions of COLON- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-colon))

(defun ar-beg-beginendquote-in-page-atpt ()
  "Employ actions of BEG- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-beg))

(defun ar-blok-beginendquote-in-page-atpt ()
  "Employ actions of BLOK- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-blok))

(defun ar-bounds-beginendquote-in-page-atpt ()
  "Employ actions of BOUNDS- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-bounds))

(defun ar-brace-beginendquote-in-page-atpt ()
  "Employ actions of BRACE- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-brace))

(defun ar-bracket-beginendquote-in-page-atpt ()
  "Employ actions of BRACKET- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-bracket))

(defun ar-commatize-beginendquote-in-page-atpt ()
  "Employ actions of COMMATIZE- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-commatize))

(defun ar-comment-beginendquote-in-page-atpt ()
  "Employ actions of COMMENT- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-comment))

(defun ar-dollar-beginendquote-in-page-atpt ()
  "Employ actions of DOLLAR- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-dollar))

(defun ar-doublebackslash-beginendquote-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-doublebackslash))

(defun ar-doublebacktick-beginendquote-in-page-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-doublebacktick))

(defun ar-doublequote-beginendquote-in-page-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-doublequote))

(defun ar-doubleslash-beginendquote-in-page-atpt ()
  "Employ actions of DOUBLESLASH- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-doubleslash))

(defun ar-doublebackslashparen-beginendquote-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-doublebackslashparen))

(defun ar-end-beginendquote-in-page-atpt ()
  "Employ actions of END- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-end))

(defun ar-escape-beginendquote-in-page-atpt ()
  "Employ actions of ESCAPE- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-escape))

(defun ar-hide-beginendquote-in-page-atpt ()
  "Employ actions of HIDE- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-hide))

(defun ar-hide-show-beginendquote-in-page-atpt ()
  "Employ actions of HIDE-SHOW- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-hide-show))

(defun ar-hyphen-beginendquote-in-page-atpt ()
  "Employ actions of HYPHEN- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-hyphen))

(defun ar-kill-beginendquote-in-page-atpt ()
  "Employ actions of KILL- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-kill))

(defun ar-curvedsinglequote-beginendquote-in-page-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-curvedsinglequote))

(defun ar-length-beginendquote-in-page-atpt ()
  "Employ actions of LENGTH- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-length))

(defun ar-parentize-beginendquote-in-page-atpt ()
  "Employ actions of PARENTIZE- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-parentize))

(defun ar-quote-beginendquote-in-page-atpt ()
  "Employ actions of QUOTE- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-quote))

(defun ar-separate-beginendquote-in-page-atpt ()
  "Employ actions of SEPARATE- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-separate))

(defun ar-show-beginendquote-in-page-atpt ()
  "Employ actions of SHOW- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-show))

(defun ar-singlequote-beginendquote-in-page-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-singlequote))

(defun ar-slash-beginendquote-in-page-atpt ()
  "Employ actions of SLASH- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-slash))

(defun ar-star-beginendquote-in-page-atpt ()
  "Employ actions of STAR- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-star))

(defun ar-slashparen-beginendquote-in-page-atpt ()
  "Employ actions of SLASHPAREN- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-slashparen))

(defun ar-sort-beginendquote-in-page-atpt ()
  "Employ actions of SORT- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-sort))

(defun ar-trim-beginendquote-in-page-atpt ()
  "Employ actions of TRIM- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-trim))

(defun ar-trim-left-beginendquote-in-page-atpt ()
  "Employ actions of TRIM-LEFT- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-trim-left))

(defun ar-trim-right-beginendquote-in-page-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-trim-right))

(defun ar-underscore-beginendquote-in-page-atpt ()
  "Employ actions of UNDERSCORE- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-underscore))

(defun ar-whitespace-beginendquote-in-page-atpt ()
  "Employ actions of WHITESPACE- at things class of BEGINENDQUOTE residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'page 'ar-th-whitespace))

(defun ar-beginendquote-in-paragraph-atpt ()
  "Employ actions of  at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th))

(defun ar-greaterangle-beginendquote-in-paragraph-atpt ()
  "Employ actions of GREATERANGLE- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-greaterangle))

(defun ar-lesserangle-beginendquote-in-paragraph-atpt ()
  "Employ actions of LESSERANGLE- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-lesserangle))

(defun ar-backslash-beginendquote-in-paragraph-atpt ()
  "Employ actions of BACKSLASH- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-backslash))

(defun ar-colon-beginendquote-in-paragraph-atpt ()
  "Employ actions of COLON- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-colon))

(defun ar-beg-beginendquote-in-paragraph-atpt ()
  "Employ actions of BEG- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-beg))

(defun ar-blok-beginendquote-in-paragraph-atpt ()
  "Employ actions of BLOK- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-blok))

(defun ar-bounds-beginendquote-in-paragraph-atpt ()
  "Employ actions of BOUNDS- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-bounds))

(defun ar-brace-beginendquote-in-paragraph-atpt ()
  "Employ actions of BRACE- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-brace))

(defun ar-bracket-beginendquote-in-paragraph-atpt ()
  "Employ actions of BRACKET- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-bracket))

(defun ar-commatize-beginendquote-in-paragraph-atpt ()
  "Employ actions of COMMATIZE- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-commatize))

(defun ar-comment-beginendquote-in-paragraph-atpt ()
  "Employ actions of COMMENT- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-comment))

(defun ar-dollar-beginendquote-in-paragraph-atpt ()
  "Employ actions of DOLLAR- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-dollar))

(defun ar-doublebackslash-beginendquote-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-doublebackslash))

(defun ar-doublebacktick-beginendquote-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-doublebacktick))

(defun ar-doublequote-beginendquote-in-paragraph-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-doublequote))

(defun ar-doubleslash-beginendquote-in-paragraph-atpt ()
  "Employ actions of DOUBLESLASH- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-doubleslash))

(defun ar-doublebackslashparen-beginendquote-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-doublebackslashparen))

(defun ar-end-beginendquote-in-paragraph-atpt ()
  "Employ actions of END- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-end))

(defun ar-escape-beginendquote-in-paragraph-atpt ()
  "Employ actions of ESCAPE- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-escape))

(defun ar-hide-beginendquote-in-paragraph-atpt ()
  "Employ actions of HIDE- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-hide))

(defun ar-hide-show-beginendquote-in-paragraph-atpt ()
  "Employ actions of HIDE-SHOW- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-hide-show))

(defun ar-hyphen-beginendquote-in-paragraph-atpt ()
  "Employ actions of HYPHEN- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-hyphen))

(defun ar-kill-beginendquote-in-paragraph-atpt ()
  "Employ actions of KILL- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-kill))

(defun ar-curvedsinglequote-beginendquote-in-paragraph-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-curvedsinglequote))

(defun ar-length-beginendquote-in-paragraph-atpt ()
  "Employ actions of LENGTH- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-length))

(defun ar-parentize-beginendquote-in-paragraph-atpt ()
  "Employ actions of PARENTIZE- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-parentize))

(defun ar-quote-beginendquote-in-paragraph-atpt ()
  "Employ actions of QUOTE- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-quote))

(defun ar-separate-beginendquote-in-paragraph-atpt ()
  "Employ actions of SEPARATE- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-separate))

(defun ar-show-beginendquote-in-paragraph-atpt ()
  "Employ actions of SHOW- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-show))

(defun ar-singlequote-beginendquote-in-paragraph-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-singlequote))

(defun ar-slash-beginendquote-in-paragraph-atpt ()
  "Employ actions of SLASH- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-slash))

(defun ar-star-beginendquote-in-paragraph-atpt ()
  "Employ actions of STAR- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-star))

(defun ar-slashparen-beginendquote-in-paragraph-atpt ()
  "Employ actions of SLASHPAREN- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-slashparen))

(defun ar-sort-beginendquote-in-paragraph-atpt ()
  "Employ actions of SORT- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-sort))

(defun ar-trim-beginendquote-in-paragraph-atpt ()
  "Employ actions of TRIM- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-trim))

(defun ar-trim-left-beginendquote-in-paragraph-atpt ()
  "Employ actions of TRIM-LEFT- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-trim-left))

(defun ar-trim-right-beginendquote-in-paragraph-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-trim-right))

(defun ar-underscore-beginendquote-in-paragraph-atpt ()
  "Employ actions of UNDERSCORE- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-underscore))

(defun ar-whitespace-beginendquote-in-paragraph-atpt ()
  "Employ actions of WHITESPACE- at things class of BEGINENDQUOTE residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'paragraph 'ar-th-whitespace))

(defun ar-beginendquote-in-region-atpt ()
  "Employ actions of  at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th))

(defun ar-greaterangle-beginendquote-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-greaterangle))

(defun ar-lesserangle-beginendquote-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-lesserangle))

(defun ar-backslash-beginendquote-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-backslash))

(defun ar-colon-beginendquote-in-region-atpt ()
  "Employ actions of COLON- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-colon))

(defun ar-beg-beginendquote-in-region-atpt ()
  "Employ actions of BEG- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-beg))

(defun ar-blok-beginendquote-in-region-atpt ()
  "Employ actions of BLOK- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-blok))

(defun ar-bounds-beginendquote-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-bounds))

(defun ar-brace-beginendquote-in-region-atpt ()
  "Employ actions of BRACE- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-brace))

(defun ar-bracket-beginendquote-in-region-atpt ()
  "Employ actions of BRACKET- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-bracket))

(defun ar-commatize-beginendquote-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-commatize))

(defun ar-comment-beginendquote-in-region-atpt ()
  "Employ actions of COMMENT- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-comment))

(defun ar-dollar-beginendquote-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-dollar))

(defun ar-doublebackslash-beginendquote-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-beginendquote-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-doublebacktick))

(defun ar-doublequote-beginendquote-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-doublequote))

(defun ar-doubleslash-beginendquote-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-beginendquote-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-doublebackslashparen))

(defun ar-end-beginendquote-in-region-atpt ()
  "Employ actions of END- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-end))

(defun ar-escape-beginendquote-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-escape))

(defun ar-hide-beginendquote-in-region-atpt ()
  "Employ actions of HIDE- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-hide))

(defun ar-hide-show-beginendquote-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-hide-show))

(defun ar-hyphen-beginendquote-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-hyphen))

(defun ar-kill-beginendquote-in-region-atpt ()
  "Employ actions of KILL- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-kill))

(defun ar-curvedsinglequote-beginendquote-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-curvedsinglequote))

(defun ar-length-beginendquote-in-region-atpt ()
  "Employ actions of LENGTH- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-length))

(defun ar-parentize-beginendquote-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-parentize))

(defun ar-quote-beginendquote-in-region-atpt ()
  "Employ actions of QUOTE- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-quote))

(defun ar-separate-beginendquote-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-separate))

(defun ar-show-beginendquote-in-region-atpt ()
  "Employ actions of SHOW- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-show))

(defun ar-singlequote-beginendquote-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-singlequote))

(defun ar-slash-beginendquote-in-region-atpt ()
  "Employ actions of SLASH- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-slash))

(defun ar-star-beginendquote-in-region-atpt ()
  "Employ actions of STAR- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-star))

(defun ar-slashparen-beginendquote-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-slashparen))

(defun ar-sort-beginendquote-in-region-atpt ()
  "Employ actions of SORT- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-sort))

(defun ar-trim-beginendquote-in-region-atpt ()
  "Employ actions of TRIM- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-trim))

(defun ar-trim-left-beginendquote-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-trim-left))

(defun ar-trim-right-beginendquote-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-trim-right))

(defun ar-underscore-beginendquote-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-underscore))

(defun ar-whitespace-beginendquote-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of BEGINENDQUOTE residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'beginendquote 'region 'ar-th-whitespace))

(defun ar-blok-in-buffer-atpt ()
  "Employ actions of  at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th))

(defun ar-greaterangle-blok-in-buffer-atpt ()
  "Employ actions of GREATERANGLE- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-greaterangle))

(defun ar-lesserangle-blok-in-buffer-atpt ()
  "Employ actions of LESSERANGLE- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-lesserangle))

(defun ar-backslash-blok-in-buffer-atpt ()
  "Employ actions of BACKSLASH- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-backslash))

(defun ar-colon-blok-in-buffer-atpt ()
  "Employ actions of COLON- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-colon))

(defun ar-beg-blok-in-buffer-atpt ()
  "Employ actions of BEG- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-beg))

(defun ar-blok-blok-in-buffer-atpt ()
  "Employ actions of BLOK- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-blok))

(defun ar-bounds-blok-in-buffer-atpt ()
  "Employ actions of BOUNDS- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-bounds))

(defun ar-brace-blok-in-buffer-atpt ()
  "Employ actions of BRACE- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-brace))

(defun ar-bracket-blok-in-buffer-atpt ()
  "Employ actions of BRACKET- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-bracket))

(defun ar-commatize-blok-in-buffer-atpt ()
  "Employ actions of COMMATIZE- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-commatize))

(defun ar-comment-blok-in-buffer-atpt ()
  "Employ actions of COMMENT- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-comment))

(defun ar-dollar-blok-in-buffer-atpt ()
  "Employ actions of DOLLAR- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-dollar))

(defun ar-doublebackslash-blok-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-doublebackslash))

(defun ar-doublebacktick-blok-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-doublebacktick))

(defun ar-doublequote-blok-in-buffer-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-doublequote))

(defun ar-doubleslash-blok-in-buffer-atpt ()
  "Employ actions of DOUBLESLASH- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-doubleslash))

(defun ar-doublebackslashparen-blok-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-doublebackslashparen))

(defun ar-end-blok-in-buffer-atpt ()
  "Employ actions of END- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-end))

(defun ar-escape-blok-in-buffer-atpt ()
  "Employ actions of ESCAPE- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-escape))

(defun ar-hide-blok-in-buffer-atpt ()
  "Employ actions of HIDE- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-hide))

(defun ar-hide-show-blok-in-buffer-atpt ()
  "Employ actions of HIDE-SHOW- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-hide-show))

(defun ar-hyphen-blok-in-buffer-atpt ()
  "Employ actions of HYPHEN- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-hyphen))

(defun ar-kill-blok-in-buffer-atpt ()
  "Employ actions of KILL- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-kill))

(defun ar-curvedsinglequote-blok-in-buffer-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-curvedsinglequote))

(defun ar-length-blok-in-buffer-atpt ()
  "Employ actions of LENGTH- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-length))

(defun ar-parentize-blok-in-buffer-atpt ()
  "Employ actions of PARENTIZE- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-parentize))

(defun ar-quote-blok-in-buffer-atpt ()
  "Employ actions of QUOTE- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-quote))

(defun ar-separate-blok-in-buffer-atpt ()
  "Employ actions of SEPARATE- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-separate))

(defun ar-show-blok-in-buffer-atpt ()
  "Employ actions of SHOW- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-show))

(defun ar-singlequote-blok-in-buffer-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-singlequote))

(defun ar-slash-blok-in-buffer-atpt ()
  "Employ actions of SLASH- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-slash))

(defun ar-star-blok-in-buffer-atpt ()
  "Employ actions of STAR- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-star))

(defun ar-slashparen-blok-in-buffer-atpt ()
  "Employ actions of SLASHPAREN- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-slashparen))

(defun ar-sort-blok-in-buffer-atpt ()
  "Employ actions of SORT- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-sort))

(defun ar-trim-blok-in-buffer-atpt ()
  "Employ actions of TRIM- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-trim))

(defun ar-trim-left-blok-in-buffer-atpt ()
  "Employ actions of TRIM-LEFT- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-trim-left))

(defun ar-trim-right-blok-in-buffer-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-trim-right))

(defun ar-underscore-blok-in-buffer-atpt ()
  "Employ actions of UNDERSCORE- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-underscore))

(defun ar-whitespace-blok-in-buffer-atpt ()
  "Employ actions of WHITESPACE- at things class of BLOK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'buffer 'ar-th-whitespace))

(defun ar-blok-in-page-atpt ()
  "Employ actions of  at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th))

(defun ar-greaterangle-blok-in-page-atpt ()
  "Employ actions of GREATERANGLE- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-greaterangle))

(defun ar-lesserangle-blok-in-page-atpt ()
  "Employ actions of LESSERANGLE- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-lesserangle))

(defun ar-backslash-blok-in-page-atpt ()
  "Employ actions of BACKSLASH- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-backslash))

(defun ar-colon-blok-in-page-atpt ()
  "Employ actions of COLON- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-colon))

(defun ar-beg-blok-in-page-atpt ()
  "Employ actions of BEG- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-beg))

(defun ar-blok-blok-in-page-atpt ()
  "Employ actions of BLOK- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-blok))

(defun ar-bounds-blok-in-page-atpt ()
  "Employ actions of BOUNDS- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-bounds))

(defun ar-brace-blok-in-page-atpt ()
  "Employ actions of BRACE- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-brace))

(defun ar-bracket-blok-in-page-atpt ()
  "Employ actions of BRACKET- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-bracket))

(defun ar-commatize-blok-in-page-atpt ()
  "Employ actions of COMMATIZE- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-commatize))

(defun ar-comment-blok-in-page-atpt ()
  "Employ actions of COMMENT- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-comment))

(defun ar-dollar-blok-in-page-atpt ()
  "Employ actions of DOLLAR- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-dollar))

(defun ar-doublebackslash-blok-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-doublebackslash))

(defun ar-doublebacktick-blok-in-page-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-doublebacktick))

(defun ar-doublequote-blok-in-page-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-doublequote))

(defun ar-doubleslash-blok-in-page-atpt ()
  "Employ actions of DOUBLESLASH- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-doubleslash))

(defun ar-doublebackslashparen-blok-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-doublebackslashparen))

(defun ar-end-blok-in-page-atpt ()
  "Employ actions of END- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-end))

(defun ar-escape-blok-in-page-atpt ()
  "Employ actions of ESCAPE- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-escape))

(defun ar-hide-blok-in-page-atpt ()
  "Employ actions of HIDE- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-hide))

(defun ar-hide-show-blok-in-page-atpt ()
  "Employ actions of HIDE-SHOW- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-hide-show))

(defun ar-hyphen-blok-in-page-atpt ()
  "Employ actions of HYPHEN- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-hyphen))

(defun ar-kill-blok-in-page-atpt ()
  "Employ actions of KILL- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-kill))

(defun ar-curvedsinglequote-blok-in-page-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-curvedsinglequote))

(defun ar-length-blok-in-page-atpt ()
  "Employ actions of LENGTH- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-length))

(defun ar-parentize-blok-in-page-atpt ()
  "Employ actions of PARENTIZE- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-parentize))

(defun ar-quote-blok-in-page-atpt ()
  "Employ actions of QUOTE- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-quote))

(defun ar-separate-blok-in-page-atpt ()
  "Employ actions of SEPARATE- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-separate))

(defun ar-show-blok-in-page-atpt ()
  "Employ actions of SHOW- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-show))

(defun ar-singlequote-blok-in-page-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-singlequote))

(defun ar-slash-blok-in-page-atpt ()
  "Employ actions of SLASH- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-slash))

(defun ar-star-blok-in-page-atpt ()
  "Employ actions of STAR- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-star))

(defun ar-slashparen-blok-in-page-atpt ()
  "Employ actions of SLASHPAREN- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-slashparen))

(defun ar-sort-blok-in-page-atpt ()
  "Employ actions of SORT- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-sort))

(defun ar-trim-blok-in-page-atpt ()
  "Employ actions of TRIM- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-trim))

(defun ar-trim-left-blok-in-page-atpt ()
  "Employ actions of TRIM-LEFT- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-trim-left))

(defun ar-trim-right-blok-in-page-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-trim-right))

(defun ar-underscore-blok-in-page-atpt ()
  "Employ actions of UNDERSCORE- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-underscore))

(defun ar-whitespace-blok-in-page-atpt ()
  "Employ actions of WHITESPACE- at things class of BLOK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'page 'ar-th-whitespace))

(defun ar-blok-in-paragraph-atpt ()
  "Employ actions of  at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th))

(defun ar-greaterangle-blok-in-paragraph-atpt ()
  "Employ actions of GREATERANGLE- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-greaterangle))

(defun ar-lesserangle-blok-in-paragraph-atpt ()
  "Employ actions of LESSERANGLE- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-lesserangle))

(defun ar-backslash-blok-in-paragraph-atpt ()
  "Employ actions of BACKSLASH- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-backslash))

(defun ar-colon-blok-in-paragraph-atpt ()
  "Employ actions of COLON- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-colon))

(defun ar-beg-blok-in-paragraph-atpt ()
  "Employ actions of BEG- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-beg))

(defun ar-blok-blok-in-paragraph-atpt ()
  "Employ actions of BLOK- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-blok))

(defun ar-bounds-blok-in-paragraph-atpt ()
  "Employ actions of BOUNDS- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-bounds))

(defun ar-brace-blok-in-paragraph-atpt ()
  "Employ actions of BRACE- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-brace))

(defun ar-bracket-blok-in-paragraph-atpt ()
  "Employ actions of BRACKET- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-bracket))

(defun ar-commatize-blok-in-paragraph-atpt ()
  "Employ actions of COMMATIZE- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-commatize))

(defun ar-comment-blok-in-paragraph-atpt ()
  "Employ actions of COMMENT- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-comment))

(defun ar-dollar-blok-in-paragraph-atpt ()
  "Employ actions of DOLLAR- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-dollar))

(defun ar-doublebackslash-blok-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-doublebackslash))

(defun ar-doublebacktick-blok-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-doublebacktick))

(defun ar-doublequote-blok-in-paragraph-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-doublequote))

(defun ar-doubleslash-blok-in-paragraph-atpt ()
  "Employ actions of DOUBLESLASH- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-doubleslash))

(defun ar-doublebackslashparen-blok-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-doublebackslashparen))

(defun ar-end-blok-in-paragraph-atpt ()
  "Employ actions of END- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-end))

(defun ar-escape-blok-in-paragraph-atpt ()
  "Employ actions of ESCAPE- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-escape))

(defun ar-hide-blok-in-paragraph-atpt ()
  "Employ actions of HIDE- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-hide))

(defun ar-hide-show-blok-in-paragraph-atpt ()
  "Employ actions of HIDE-SHOW- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-hide-show))

(defun ar-hyphen-blok-in-paragraph-atpt ()
  "Employ actions of HYPHEN- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-hyphen))

(defun ar-kill-blok-in-paragraph-atpt ()
  "Employ actions of KILL- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-kill))

(defun ar-curvedsinglequote-blok-in-paragraph-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-curvedsinglequote))

(defun ar-length-blok-in-paragraph-atpt ()
  "Employ actions of LENGTH- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-length))

(defun ar-parentize-blok-in-paragraph-atpt ()
  "Employ actions of PARENTIZE- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-parentize))

(defun ar-quote-blok-in-paragraph-atpt ()
  "Employ actions of QUOTE- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-quote))

(defun ar-separate-blok-in-paragraph-atpt ()
  "Employ actions of SEPARATE- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-separate))

(defun ar-show-blok-in-paragraph-atpt ()
  "Employ actions of SHOW- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-show))

(defun ar-singlequote-blok-in-paragraph-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-singlequote))

(defun ar-slash-blok-in-paragraph-atpt ()
  "Employ actions of SLASH- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-slash))

(defun ar-star-blok-in-paragraph-atpt ()
  "Employ actions of STAR- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-star))

(defun ar-slashparen-blok-in-paragraph-atpt ()
  "Employ actions of SLASHPAREN- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-slashparen))

(defun ar-sort-blok-in-paragraph-atpt ()
  "Employ actions of SORT- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-sort))

(defun ar-trim-blok-in-paragraph-atpt ()
  "Employ actions of TRIM- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-trim))

(defun ar-trim-left-blok-in-paragraph-atpt ()
  "Employ actions of TRIM-LEFT- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-trim-left))

(defun ar-trim-right-blok-in-paragraph-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-trim-right))

(defun ar-underscore-blok-in-paragraph-atpt ()
  "Employ actions of UNDERSCORE- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-underscore))

(defun ar-whitespace-blok-in-paragraph-atpt ()
  "Employ actions of WHITESPACE- at things class of BLOK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'paragraph 'ar-th-whitespace))

(defun ar-blok-in-region-atpt ()
  "Employ actions of  at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th))

(defun ar-greaterangle-blok-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-greaterangle))

(defun ar-lesserangle-blok-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-lesserangle))

(defun ar-backslash-blok-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-backslash))

(defun ar-colon-blok-in-region-atpt ()
  "Employ actions of COLON- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-colon))

(defun ar-beg-blok-in-region-atpt ()
  "Employ actions of BEG- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-beg))

(defun ar-blok-blok-in-region-atpt ()
  "Employ actions of BLOK- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-blok))

(defun ar-bounds-blok-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-bounds))

(defun ar-brace-blok-in-region-atpt ()
  "Employ actions of BRACE- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-brace))

(defun ar-bracket-blok-in-region-atpt ()
  "Employ actions of BRACKET- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-bracket))

(defun ar-commatize-blok-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-commatize))

(defun ar-comment-blok-in-region-atpt ()
  "Employ actions of COMMENT- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-comment))

(defun ar-dollar-blok-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-dollar))

(defun ar-doublebackslash-blok-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-blok-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-doublebacktick))

(defun ar-doublequote-blok-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-doublequote))

(defun ar-doubleslash-blok-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-blok-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-doublebackslashparen))

(defun ar-end-blok-in-region-atpt ()
  "Employ actions of END- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-end))

(defun ar-escape-blok-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-escape))

(defun ar-hide-blok-in-region-atpt ()
  "Employ actions of HIDE- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-hide))

(defun ar-hide-show-blok-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-hide-show))

(defun ar-hyphen-blok-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-hyphen))

(defun ar-kill-blok-in-region-atpt ()
  "Employ actions of KILL- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-kill))

(defun ar-curvedsinglequote-blok-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-curvedsinglequote))

(defun ar-length-blok-in-region-atpt ()
  "Employ actions of LENGTH- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-length))

(defun ar-parentize-blok-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-parentize))

(defun ar-quote-blok-in-region-atpt ()
  "Employ actions of QUOTE- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-quote))

(defun ar-separate-blok-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-separate))

(defun ar-show-blok-in-region-atpt ()
  "Employ actions of SHOW- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-show))

(defun ar-singlequote-blok-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-singlequote))

(defun ar-slash-blok-in-region-atpt ()
  "Employ actions of SLASH- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-slash))

(defun ar-star-blok-in-region-atpt ()
  "Employ actions of STAR- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-star))

(defun ar-slashparen-blok-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-slashparen))

(defun ar-sort-blok-in-region-atpt ()
  "Employ actions of SORT- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-sort))

(defun ar-trim-blok-in-region-atpt ()
  "Employ actions of TRIM- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-trim))

(defun ar-trim-left-blok-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-trim-left))

(defun ar-trim-right-blok-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-trim-right))

(defun ar-underscore-blok-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-underscore))

(defun ar-whitespace-blok-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of BLOK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'region 'ar-th-whitespace))

(defun ar-doublebackslash-in-buffer-atpt ()
  "Employ actions of  at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th))

(defun ar-greaterangle-doublebackslash-in-buffer-atpt ()
  "Employ actions of GREATERANGLE- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-greaterangle))

(defun ar-lesserangle-doublebackslash-in-buffer-atpt ()
  "Employ actions of LESSERANGLE- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-lesserangle))

(defun ar-backslash-doublebackslash-in-buffer-atpt ()
  "Employ actions of BACKSLASH- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-backslash))

(defun ar-colon-doublebackslash-in-buffer-atpt ()
  "Employ actions of COLON- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-colon))

(defun ar-beg-doublebackslash-in-buffer-atpt ()
  "Employ actions of BEG- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-beg))

(defun ar-blok-doublebackslash-in-buffer-atpt ()
  "Employ actions of BLOK- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-blok))

(defun ar-bounds-doublebackslash-in-buffer-atpt ()
  "Employ actions of BOUNDS- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-bounds))

(defun ar-brace-doublebackslash-in-buffer-atpt ()
  "Employ actions of BRACE- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-brace))

(defun ar-bracket-doublebackslash-in-buffer-atpt ()
  "Employ actions of BRACKET- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-bracket))

(defun ar-commatize-doublebackslash-in-buffer-atpt ()
  "Employ actions of COMMATIZE- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-commatize))

(defun ar-comment-doublebackslash-in-buffer-atpt ()
  "Employ actions of COMMENT- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-comment))

(defun ar-dollar-doublebackslash-in-buffer-atpt ()
  "Employ actions of DOLLAR- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-dollar))

(defun ar-doublebackslash-doublebackslash-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-doublebackslash))

(defun ar-doublebacktick-doublebackslash-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-doublebacktick))

(defun ar-doublequote-doublebackslash-in-buffer-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-doublequote))

(defun ar-doubleslash-doublebackslash-in-buffer-atpt ()
  "Employ actions of DOUBLESLASH- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-doubleslash))

(defun ar-doublebackslashparen-doublebackslash-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-doublebackslashparen))

(defun ar-end-doublebackslash-in-buffer-atpt ()
  "Employ actions of END- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-end))

(defun ar-escape-doublebackslash-in-buffer-atpt ()
  "Employ actions of ESCAPE- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-escape))

(defun ar-hide-doublebackslash-in-buffer-atpt ()
  "Employ actions of HIDE- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-hide))

(defun ar-hide-show-doublebackslash-in-buffer-atpt ()
  "Employ actions of HIDE-SHOW- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-hide-show))

(defun ar-hyphen-doublebackslash-in-buffer-atpt ()
  "Employ actions of HYPHEN- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-hyphen))

(defun ar-kill-doublebackslash-in-buffer-atpt ()
  "Employ actions of KILL- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-kill))

(defun ar-curvedsinglequote-doublebackslash-in-buffer-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-curvedsinglequote))

(defun ar-length-doublebackslash-in-buffer-atpt ()
  "Employ actions of LENGTH- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-length))

(defun ar-parentize-doublebackslash-in-buffer-atpt ()
  "Employ actions of PARENTIZE- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-parentize))

(defun ar-quote-doublebackslash-in-buffer-atpt ()
  "Employ actions of QUOTE- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-quote))

(defun ar-separate-doublebackslash-in-buffer-atpt ()
  "Employ actions of SEPARATE- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-separate))

(defun ar-show-doublebackslash-in-buffer-atpt ()
  "Employ actions of SHOW- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-show))

(defun ar-singlequote-doublebackslash-in-buffer-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-singlequote))

(defun ar-slash-doublebackslash-in-buffer-atpt ()
  "Employ actions of SLASH- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-slash))

(defun ar-star-doublebackslash-in-buffer-atpt ()
  "Employ actions of STAR- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-star))

(defun ar-slashparen-doublebackslash-in-buffer-atpt ()
  "Employ actions of SLASHPAREN- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-slashparen))

(defun ar-sort-doublebackslash-in-buffer-atpt ()
  "Employ actions of SORT- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-sort))

(defun ar-trim-doublebackslash-in-buffer-atpt ()
  "Employ actions of TRIM- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-trim))

(defun ar-trim-left-doublebackslash-in-buffer-atpt ()
  "Employ actions of TRIM-LEFT- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-trim-left))

(defun ar-trim-right-doublebackslash-in-buffer-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-trim-right))

(defun ar-underscore-doublebackslash-in-buffer-atpt ()
  "Employ actions of UNDERSCORE- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-underscore))

(defun ar-whitespace-doublebackslash-in-buffer-atpt ()
  "Employ actions of WHITESPACE- at things class of DOUBLEBACKSLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'buffer 'ar-th-whitespace))

(defun ar-doublebackslash-in-page-atpt ()
  "Employ actions of  at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th))

(defun ar-greaterangle-doublebackslash-in-page-atpt ()
  "Employ actions of GREATERANGLE- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-greaterangle))

(defun ar-lesserangle-doublebackslash-in-page-atpt ()
  "Employ actions of LESSERANGLE- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-lesserangle))

(defun ar-backslash-doublebackslash-in-page-atpt ()
  "Employ actions of BACKSLASH- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-backslash))

(defun ar-colon-doublebackslash-in-page-atpt ()
  "Employ actions of COLON- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-colon))

(defun ar-beg-doublebackslash-in-page-atpt ()
  "Employ actions of BEG- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-beg))

(defun ar-blok-doublebackslash-in-page-atpt ()
  "Employ actions of BLOK- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-blok))

(defun ar-bounds-doublebackslash-in-page-atpt ()
  "Employ actions of BOUNDS- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-bounds))

(defun ar-brace-doublebackslash-in-page-atpt ()
  "Employ actions of BRACE- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-brace))

(defun ar-bracket-doublebackslash-in-page-atpt ()
  "Employ actions of BRACKET- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-bracket))

(defun ar-commatize-doublebackslash-in-page-atpt ()
  "Employ actions of COMMATIZE- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-commatize))

(defun ar-comment-doublebackslash-in-page-atpt ()
  "Employ actions of COMMENT- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-comment))

(defun ar-dollar-doublebackslash-in-page-atpt ()
  "Employ actions of DOLLAR- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-dollar))

(defun ar-doublebackslash-doublebackslash-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-doublebackslash))

(defun ar-doublebacktick-doublebackslash-in-page-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-doublebacktick))

(defun ar-doublequote-doublebackslash-in-page-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-doublequote))

(defun ar-doubleslash-doublebackslash-in-page-atpt ()
  "Employ actions of DOUBLESLASH- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-doubleslash))

(defun ar-doublebackslashparen-doublebackslash-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-doublebackslashparen))

(defun ar-end-doublebackslash-in-page-atpt ()
  "Employ actions of END- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-end))

(defun ar-escape-doublebackslash-in-page-atpt ()
  "Employ actions of ESCAPE- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-escape))

(defun ar-hide-doublebackslash-in-page-atpt ()
  "Employ actions of HIDE- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-hide))

(defun ar-hide-show-doublebackslash-in-page-atpt ()
  "Employ actions of HIDE-SHOW- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-hide-show))

(defun ar-hyphen-doublebackslash-in-page-atpt ()
  "Employ actions of HYPHEN- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-hyphen))

(defun ar-kill-doublebackslash-in-page-atpt ()
  "Employ actions of KILL- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-kill))

(defun ar-curvedsinglequote-doublebackslash-in-page-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-curvedsinglequote))

(defun ar-length-doublebackslash-in-page-atpt ()
  "Employ actions of LENGTH- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-length))

(defun ar-parentize-doublebackslash-in-page-atpt ()
  "Employ actions of PARENTIZE- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-parentize))

(defun ar-quote-doublebackslash-in-page-atpt ()
  "Employ actions of QUOTE- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-quote))

(defun ar-separate-doublebackslash-in-page-atpt ()
  "Employ actions of SEPARATE- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-separate))

(defun ar-show-doublebackslash-in-page-atpt ()
  "Employ actions of SHOW- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-show))

(defun ar-singlequote-doublebackslash-in-page-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-singlequote))

(defun ar-slash-doublebackslash-in-page-atpt ()
  "Employ actions of SLASH- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-slash))

(defun ar-star-doublebackslash-in-page-atpt ()
  "Employ actions of STAR- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-star))

(defun ar-slashparen-doublebackslash-in-page-atpt ()
  "Employ actions of SLASHPAREN- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-slashparen))

(defun ar-sort-doublebackslash-in-page-atpt ()
  "Employ actions of SORT- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-sort))

(defun ar-trim-doublebackslash-in-page-atpt ()
  "Employ actions of TRIM- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-trim))

(defun ar-trim-left-doublebackslash-in-page-atpt ()
  "Employ actions of TRIM-LEFT- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-trim-left))

(defun ar-trim-right-doublebackslash-in-page-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-trim-right))

(defun ar-underscore-doublebackslash-in-page-atpt ()
  "Employ actions of UNDERSCORE- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-underscore))

(defun ar-whitespace-doublebackslash-in-page-atpt ()
  "Employ actions of WHITESPACE- at things class of DOUBLEBACKSLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'page 'ar-th-whitespace))

(defun ar-doublebackslash-in-paragraph-atpt ()
  "Employ actions of  at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th))

(defun ar-greaterangle-doublebackslash-in-paragraph-atpt ()
  "Employ actions of GREATERANGLE- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-greaterangle))

(defun ar-lesserangle-doublebackslash-in-paragraph-atpt ()
  "Employ actions of LESSERANGLE- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-lesserangle))

(defun ar-backslash-doublebackslash-in-paragraph-atpt ()
  "Employ actions of BACKSLASH- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-backslash))

(defun ar-colon-doublebackslash-in-paragraph-atpt ()
  "Employ actions of COLON- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-colon))

(defun ar-beg-doublebackslash-in-paragraph-atpt ()
  "Employ actions of BEG- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-beg))

(defun ar-blok-doublebackslash-in-paragraph-atpt ()
  "Employ actions of BLOK- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-blok))

(defun ar-bounds-doublebackslash-in-paragraph-atpt ()
  "Employ actions of BOUNDS- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-bounds))

(defun ar-brace-doublebackslash-in-paragraph-atpt ()
  "Employ actions of BRACE- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-brace))

(defun ar-bracket-doublebackslash-in-paragraph-atpt ()
  "Employ actions of BRACKET- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-bracket))

(defun ar-commatize-doublebackslash-in-paragraph-atpt ()
  "Employ actions of COMMATIZE- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-commatize))

(defun ar-comment-doublebackslash-in-paragraph-atpt ()
  "Employ actions of COMMENT- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-comment))

(defun ar-dollar-doublebackslash-in-paragraph-atpt ()
  "Employ actions of DOLLAR- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-dollar))

(defun ar-doublebackslash-doublebackslash-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-doublebackslash))

(defun ar-doublebacktick-doublebackslash-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-doublebacktick))

(defun ar-doublequote-doublebackslash-in-paragraph-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-doublequote))

(defun ar-doubleslash-doublebackslash-in-paragraph-atpt ()
  "Employ actions of DOUBLESLASH- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-doubleslash))

(defun ar-doublebackslashparen-doublebackslash-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-doublebackslashparen))

(defun ar-end-doublebackslash-in-paragraph-atpt ()
  "Employ actions of END- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-end))

(defun ar-escape-doublebackslash-in-paragraph-atpt ()
  "Employ actions of ESCAPE- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-escape))

(defun ar-hide-doublebackslash-in-paragraph-atpt ()
  "Employ actions of HIDE- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-hide))

(defun ar-hide-show-doublebackslash-in-paragraph-atpt ()
  "Employ actions of HIDE-SHOW- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-hide-show))

(defun ar-hyphen-doublebackslash-in-paragraph-atpt ()
  "Employ actions of HYPHEN- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-hyphen))

(defun ar-kill-doublebackslash-in-paragraph-atpt ()
  "Employ actions of KILL- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-kill))

(defun ar-curvedsinglequote-doublebackslash-in-paragraph-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-curvedsinglequote))

(defun ar-length-doublebackslash-in-paragraph-atpt ()
  "Employ actions of LENGTH- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-length))

(defun ar-parentize-doublebackslash-in-paragraph-atpt ()
  "Employ actions of PARENTIZE- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-parentize))

(defun ar-quote-doublebackslash-in-paragraph-atpt ()
  "Employ actions of QUOTE- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-quote))

(defun ar-separate-doublebackslash-in-paragraph-atpt ()
  "Employ actions of SEPARATE- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-separate))

(defun ar-show-doublebackslash-in-paragraph-atpt ()
  "Employ actions of SHOW- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-show))

(defun ar-singlequote-doublebackslash-in-paragraph-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-singlequote))

(defun ar-slash-doublebackslash-in-paragraph-atpt ()
  "Employ actions of SLASH- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-slash))

(defun ar-star-doublebackslash-in-paragraph-atpt ()
  "Employ actions of STAR- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-star))

(defun ar-slashparen-doublebackslash-in-paragraph-atpt ()
  "Employ actions of SLASHPAREN- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-slashparen))

(defun ar-sort-doublebackslash-in-paragraph-atpt ()
  "Employ actions of SORT- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-sort))

(defun ar-trim-doublebackslash-in-paragraph-atpt ()
  "Employ actions of TRIM- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-trim))

(defun ar-trim-left-doublebackslash-in-paragraph-atpt ()
  "Employ actions of TRIM-LEFT- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-trim-left))

(defun ar-trim-right-doublebackslash-in-paragraph-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-trim-right))

(defun ar-underscore-doublebackslash-in-paragraph-atpt ()
  "Employ actions of UNDERSCORE- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-underscore))

(defun ar-whitespace-doublebackslash-in-paragraph-atpt ()
  "Employ actions of WHITESPACE- at things class of DOUBLEBACKSLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'paragraph 'ar-th-whitespace))

(defun ar-doublebackslash-in-region-atpt ()
  "Employ actions of  at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th))

(defun ar-greaterangle-doublebackslash-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-greaterangle))

(defun ar-lesserangle-doublebackslash-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-lesserangle))

(defun ar-backslash-doublebackslash-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-backslash))

(defun ar-colon-doublebackslash-in-region-atpt ()
  "Employ actions of COLON- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-colon))

(defun ar-beg-doublebackslash-in-region-atpt ()
  "Employ actions of BEG- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-beg))

(defun ar-blok-doublebackslash-in-region-atpt ()
  "Employ actions of BLOK- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-blok))

(defun ar-bounds-doublebackslash-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-bounds))

(defun ar-brace-doublebackslash-in-region-atpt ()
  "Employ actions of BRACE- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-brace))

(defun ar-bracket-doublebackslash-in-region-atpt ()
  "Employ actions of BRACKET- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-bracket))

(defun ar-commatize-doublebackslash-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-commatize))

(defun ar-comment-doublebackslash-in-region-atpt ()
  "Employ actions of COMMENT- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-comment))

(defun ar-dollar-doublebackslash-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-dollar))

(defun ar-doublebackslash-doublebackslash-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-doublebackslash-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-doublebacktick))

(defun ar-doublequote-doublebackslash-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-doublequote))

(defun ar-doubleslash-doublebackslash-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-doublebackslash-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-doublebackslashparen))

(defun ar-end-doublebackslash-in-region-atpt ()
  "Employ actions of END- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-end))

(defun ar-escape-doublebackslash-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-escape))

(defun ar-hide-doublebackslash-in-region-atpt ()
  "Employ actions of HIDE- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-hide))

(defun ar-hide-show-doublebackslash-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-hide-show))

(defun ar-hyphen-doublebackslash-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-hyphen))

(defun ar-kill-doublebackslash-in-region-atpt ()
  "Employ actions of KILL- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-kill))

(defun ar-curvedsinglequote-doublebackslash-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-curvedsinglequote))

(defun ar-length-doublebackslash-in-region-atpt ()
  "Employ actions of LENGTH- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-length))

(defun ar-parentize-doublebackslash-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-parentize))

(defun ar-quote-doublebackslash-in-region-atpt ()
  "Employ actions of QUOTE- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-quote))

(defun ar-separate-doublebackslash-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-separate))

(defun ar-show-doublebackslash-in-region-atpt ()
  "Employ actions of SHOW- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-show))

(defun ar-singlequote-doublebackslash-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-singlequote))

(defun ar-slash-doublebackslash-in-region-atpt ()
  "Employ actions of SLASH- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-slash))

(defun ar-star-doublebackslash-in-region-atpt ()
  "Employ actions of STAR- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-star))

(defun ar-slashparen-doublebackslash-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-slashparen))

(defun ar-sort-doublebackslash-in-region-atpt ()
  "Employ actions of SORT- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-sort))

(defun ar-trim-doublebackslash-in-region-atpt ()
  "Employ actions of TRIM- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-trim))

(defun ar-trim-left-doublebackslash-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-trim-left))

(defun ar-trim-right-doublebackslash-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-trim-right))

(defun ar-underscore-doublebackslash-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-underscore))

(defun ar-whitespace-doublebackslash-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of DOUBLEBACKSLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslash 'region 'ar-th-whitespace))

(defun ar-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of  at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th))

(defun ar-greaterangle-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of GREATERANGLE- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-greaterangle))

(defun ar-lesserangle-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of LESSERANGLE- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-lesserangle))

(defun ar-backslash-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of BACKSLASH- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-backslash))

(defun ar-colon-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of COLON- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-colon))

(defun ar-beg-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of BEG- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-beg))

(defun ar-blok-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of BLOK- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-blok))

(defun ar-bounds-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of BOUNDS- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-bounds))

(defun ar-brace-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of BRACE- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-brace))

(defun ar-bracket-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of BRACKET- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-bracket))

(defun ar-commatize-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of COMMATIZE- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-commatize))

(defun ar-comment-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of COMMENT- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-comment))

(defun ar-dollar-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of DOLLAR- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-dollar))

(defun ar-doublebackslash-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-doublebackslash))

(defun ar-doublebacktick-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-doublebacktick))

(defun ar-doublequote-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-doublequote))

(defun ar-doubleslash-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of DOUBLESLASH- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-doubleslash))

(defun ar-doublebackslashparen-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-doublebackslashparen))

(defun ar-end-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of END- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-end))

(defun ar-escape-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of ESCAPE- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-escape))

(defun ar-hide-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of HIDE- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-hide))

(defun ar-hide-show-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of HIDE-SHOW- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-hide-show))

(defun ar-hyphen-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of HYPHEN- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-hyphen))

(defun ar-kill-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of KILL- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-kill))

(defun ar-curvedsinglequote-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-curvedsinglequote))

(defun ar-length-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of LENGTH- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-length))

(defun ar-parentize-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of PARENTIZE- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-parentize))

(defun ar-quote-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of QUOTE- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-quote))

(defun ar-separate-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of SEPARATE- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-separate))

(defun ar-show-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of SHOW- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-show))

(defun ar-singlequote-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-singlequote))

(defun ar-slash-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of SLASH- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-slash))

(defun ar-star-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of STAR- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-star))

(defun ar-slashparen-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of SLASHPAREN- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-slashparen))

(defun ar-sort-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of SORT- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-sort))

(defun ar-trim-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of TRIM- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-trim))

(defun ar-trim-left-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of TRIM-LEFT- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-trim-left))

(defun ar-trim-right-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-trim-right))

(defun ar-underscore-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of UNDERSCORE- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-underscore))

(defun ar-whitespace-doublebackslashparen-in-buffer-atpt ()
  "Employ actions of WHITESPACE- at things class of DOUBLEBACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'buffer 'ar-th-whitespace))

(defun ar-doublebackslashparen-in-page-atpt ()
  "Employ actions of  at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th))

(defun ar-greaterangle-doublebackslashparen-in-page-atpt ()
  "Employ actions of GREATERANGLE- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-greaterangle))

(defun ar-lesserangle-doublebackslashparen-in-page-atpt ()
  "Employ actions of LESSERANGLE- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-lesserangle))

(defun ar-backslash-doublebackslashparen-in-page-atpt ()
  "Employ actions of BACKSLASH- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-backslash))

(defun ar-colon-doublebackslashparen-in-page-atpt ()
  "Employ actions of COLON- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-colon))

(defun ar-beg-doublebackslashparen-in-page-atpt ()
  "Employ actions of BEG- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-beg))

(defun ar-blok-doublebackslashparen-in-page-atpt ()
  "Employ actions of BLOK- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-blok))

(defun ar-bounds-doublebackslashparen-in-page-atpt ()
  "Employ actions of BOUNDS- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-bounds))

(defun ar-brace-doublebackslashparen-in-page-atpt ()
  "Employ actions of BRACE- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-brace))

(defun ar-bracket-doublebackslashparen-in-page-atpt ()
  "Employ actions of BRACKET- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-bracket))

(defun ar-commatize-doublebackslashparen-in-page-atpt ()
  "Employ actions of COMMATIZE- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-commatize))

(defun ar-comment-doublebackslashparen-in-page-atpt ()
  "Employ actions of COMMENT- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-comment))

(defun ar-dollar-doublebackslashparen-in-page-atpt ()
  "Employ actions of DOLLAR- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-dollar))

(defun ar-doublebackslash-doublebackslashparen-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-doublebackslash))

(defun ar-doublebacktick-doublebackslashparen-in-page-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-doublebacktick))

(defun ar-doublequote-doublebackslashparen-in-page-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-doublequote))

(defun ar-doubleslash-doublebackslashparen-in-page-atpt ()
  "Employ actions of DOUBLESLASH- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-doubleslash))

(defun ar-doublebackslashparen-doublebackslashparen-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-doublebackslashparen))

(defun ar-end-doublebackslashparen-in-page-atpt ()
  "Employ actions of END- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-end))

(defun ar-escape-doublebackslashparen-in-page-atpt ()
  "Employ actions of ESCAPE- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-escape))

(defun ar-hide-doublebackslashparen-in-page-atpt ()
  "Employ actions of HIDE- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-hide))

(defun ar-hide-show-doublebackslashparen-in-page-atpt ()
  "Employ actions of HIDE-SHOW- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-hide-show))

(defun ar-hyphen-doublebackslashparen-in-page-atpt ()
  "Employ actions of HYPHEN- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-hyphen))

(defun ar-kill-doublebackslashparen-in-page-atpt ()
  "Employ actions of KILL- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-kill))

(defun ar-curvedsinglequote-doublebackslashparen-in-page-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-curvedsinglequote))

(defun ar-length-doublebackslashparen-in-page-atpt ()
  "Employ actions of LENGTH- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-length))

(defun ar-parentize-doublebackslashparen-in-page-atpt ()
  "Employ actions of PARENTIZE- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-parentize))

(defun ar-quote-doublebackslashparen-in-page-atpt ()
  "Employ actions of QUOTE- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-quote))

(defun ar-separate-doublebackslashparen-in-page-atpt ()
  "Employ actions of SEPARATE- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-separate))

(defun ar-show-doublebackslashparen-in-page-atpt ()
  "Employ actions of SHOW- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-show))

(defun ar-singlequote-doublebackslashparen-in-page-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-singlequote))

(defun ar-slash-doublebackslashparen-in-page-atpt ()
  "Employ actions of SLASH- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-slash))

(defun ar-star-doublebackslashparen-in-page-atpt ()
  "Employ actions of STAR- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-star))

(defun ar-slashparen-doublebackslashparen-in-page-atpt ()
  "Employ actions of SLASHPAREN- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-slashparen))

(defun ar-sort-doublebackslashparen-in-page-atpt ()
  "Employ actions of SORT- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-sort))

(defun ar-trim-doublebackslashparen-in-page-atpt ()
  "Employ actions of TRIM- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-trim))

(defun ar-trim-left-doublebackslashparen-in-page-atpt ()
  "Employ actions of TRIM-LEFT- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-trim-left))

(defun ar-trim-right-doublebackslashparen-in-page-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-trim-right))

(defun ar-underscore-doublebackslashparen-in-page-atpt ()
  "Employ actions of UNDERSCORE- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-underscore))

(defun ar-whitespace-doublebackslashparen-in-page-atpt ()
  "Employ actions of WHITESPACE- at things class of DOUBLEBACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'page 'ar-th-whitespace))

(defun ar-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of  at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th))

(defun ar-greaterangle-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of GREATERANGLE- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-greaterangle))

(defun ar-lesserangle-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of LESSERANGLE- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-lesserangle))

(defun ar-backslash-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of BACKSLASH- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-backslash))

(defun ar-colon-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of COLON- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-colon))

(defun ar-beg-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of BEG- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-beg))

(defun ar-blok-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of BLOK- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-blok))

(defun ar-bounds-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of BOUNDS- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-bounds))

(defun ar-brace-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of BRACE- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-brace))

(defun ar-bracket-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of BRACKET- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-bracket))

(defun ar-commatize-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of COMMATIZE- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-commatize))

(defun ar-comment-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of COMMENT- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-comment))

(defun ar-dollar-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of DOLLAR- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-dollar))

(defun ar-doublebackslash-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-doublebackslash))

(defun ar-doublebacktick-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-doublebacktick))

(defun ar-doublequote-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-doublequote))

(defun ar-doubleslash-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of DOUBLESLASH- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-doubleslash))

(defun ar-doublebackslashparen-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-doublebackslashparen))

(defun ar-end-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of END- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-end))

(defun ar-escape-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of ESCAPE- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-escape))

(defun ar-hide-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of HIDE- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-hide))

(defun ar-hide-show-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of HIDE-SHOW- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-hide-show))

(defun ar-hyphen-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of HYPHEN- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-hyphen))

(defun ar-kill-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of KILL- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-kill))

(defun ar-curvedsinglequote-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-curvedsinglequote))

(defun ar-length-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of LENGTH- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-length))

(defun ar-parentize-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of PARENTIZE- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-parentize))

(defun ar-quote-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of QUOTE- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-quote))

(defun ar-separate-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of SEPARATE- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-separate))

(defun ar-show-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of SHOW- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-show))

(defun ar-singlequote-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-singlequote))

(defun ar-slash-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of SLASH- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-slash))

(defun ar-star-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of STAR- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-star))

(defun ar-slashparen-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of SLASHPAREN- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-slashparen))

(defun ar-sort-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of SORT- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-sort))

(defun ar-trim-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of TRIM- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-trim))

(defun ar-trim-left-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of TRIM-LEFT- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-trim-left))

(defun ar-trim-right-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-trim-right))

(defun ar-underscore-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of UNDERSCORE- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-underscore))

(defun ar-whitespace-doublebackslashparen-in-paragraph-atpt ()
  "Employ actions of WHITESPACE- at things class of DOUBLEBACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'paragraph 'ar-th-whitespace))

(defun ar-doublebackslashparen-in-region-atpt ()
  "Employ actions of  at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th))

(defun ar-greaterangle-doublebackslashparen-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-greaterangle))

(defun ar-lesserangle-doublebackslashparen-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-lesserangle))

(defun ar-backslash-doublebackslashparen-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-backslash))

(defun ar-colon-doublebackslashparen-in-region-atpt ()
  "Employ actions of COLON- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-colon))

(defun ar-beg-doublebackslashparen-in-region-atpt ()
  "Employ actions of BEG- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-beg))

(defun ar-blok-doublebackslashparen-in-region-atpt ()
  "Employ actions of BLOK- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-blok))

(defun ar-bounds-doublebackslashparen-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-bounds))

(defun ar-brace-doublebackslashparen-in-region-atpt ()
  "Employ actions of BRACE- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-brace))

(defun ar-bracket-doublebackslashparen-in-region-atpt ()
  "Employ actions of BRACKET- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-bracket))

(defun ar-commatize-doublebackslashparen-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-commatize))

(defun ar-comment-doublebackslashparen-in-region-atpt ()
  "Employ actions of COMMENT- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-comment))

(defun ar-dollar-doublebackslashparen-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-dollar))

(defun ar-doublebackslash-doublebackslashparen-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-doublebackslashparen-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-doublebacktick))

(defun ar-doublequote-doublebackslashparen-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-doublequote))

(defun ar-doubleslash-doublebackslashparen-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-doublebackslashparen-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-doublebackslashparen))

(defun ar-end-doublebackslashparen-in-region-atpt ()
  "Employ actions of END- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-end))

(defun ar-escape-doublebackslashparen-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-escape))

(defun ar-hide-doublebackslashparen-in-region-atpt ()
  "Employ actions of HIDE- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-hide))

(defun ar-hide-show-doublebackslashparen-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-hide-show))

(defun ar-hyphen-doublebackslashparen-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-hyphen))

(defun ar-kill-doublebackslashparen-in-region-atpt ()
  "Employ actions of KILL- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-kill))

(defun ar-curvedsinglequote-doublebackslashparen-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-curvedsinglequote))

(defun ar-length-doublebackslashparen-in-region-atpt ()
  "Employ actions of LENGTH- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-length))

(defun ar-parentize-doublebackslashparen-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-parentize))

(defun ar-quote-doublebackslashparen-in-region-atpt ()
  "Employ actions of QUOTE- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-quote))

(defun ar-separate-doublebackslashparen-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-separate))

(defun ar-show-doublebackslashparen-in-region-atpt ()
  "Employ actions of SHOW- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-show))

(defun ar-singlequote-doublebackslashparen-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-singlequote))

(defun ar-slash-doublebackslashparen-in-region-atpt ()
  "Employ actions of SLASH- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-slash))

(defun ar-star-doublebackslashparen-in-region-atpt ()
  "Employ actions of STAR- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-star))

(defun ar-slashparen-doublebackslashparen-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-slashparen))

(defun ar-sort-doublebackslashparen-in-region-atpt ()
  "Employ actions of SORT- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-sort))

(defun ar-trim-doublebackslashparen-in-region-atpt ()
  "Employ actions of TRIM- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-trim))

(defun ar-trim-left-doublebackslashparen-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-trim-left))

(defun ar-trim-right-doublebackslashparen-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-trim-right))

(defun ar-underscore-doublebackslashparen-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-underscore))

(defun ar-whitespace-doublebackslashparen-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of DOUBLEBACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebackslashparen 'region 'ar-th-whitespace))

(defun ar-doublebacktick-in-buffer-atpt ()
  "Employ actions of  at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th))

(defun ar-greaterangle-doublebacktick-in-buffer-atpt ()
  "Employ actions of GREATERANGLE- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-greaterangle))

(defun ar-lesserangle-doublebacktick-in-buffer-atpt ()
  "Employ actions of LESSERANGLE- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-lesserangle))

(defun ar-backslash-doublebacktick-in-buffer-atpt ()
  "Employ actions of BACKSLASH- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-backslash))

(defun ar-colon-doublebacktick-in-buffer-atpt ()
  "Employ actions of COLON- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-colon))

(defun ar-beg-doublebacktick-in-buffer-atpt ()
  "Employ actions of BEG- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-beg))

(defun ar-blok-doublebacktick-in-buffer-atpt ()
  "Employ actions of BLOK- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-blok))

(defun ar-bounds-doublebacktick-in-buffer-atpt ()
  "Employ actions of BOUNDS- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-bounds))

(defun ar-brace-doublebacktick-in-buffer-atpt ()
  "Employ actions of BRACE- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-brace))

(defun ar-bracket-doublebacktick-in-buffer-atpt ()
  "Employ actions of BRACKET- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-bracket))

(defun ar-commatize-doublebacktick-in-buffer-atpt ()
  "Employ actions of COMMATIZE- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-commatize))

(defun ar-comment-doublebacktick-in-buffer-atpt ()
  "Employ actions of COMMENT- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-comment))

(defun ar-dollar-doublebacktick-in-buffer-atpt ()
  "Employ actions of DOLLAR- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-dollar))

(defun ar-doublebackslash-doublebacktick-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-doublebackslash))

(defun ar-doublebacktick-doublebacktick-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-doublebacktick))

(defun ar-doublequote-doublebacktick-in-buffer-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-doublequote))

(defun ar-doubleslash-doublebacktick-in-buffer-atpt ()
  "Employ actions of DOUBLESLASH- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-doubleslash))

(defun ar-doublebackslashparen-doublebacktick-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-doublebackslashparen))

(defun ar-end-doublebacktick-in-buffer-atpt ()
  "Employ actions of END- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-end))

(defun ar-escape-doublebacktick-in-buffer-atpt ()
  "Employ actions of ESCAPE- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-escape))

(defun ar-hide-doublebacktick-in-buffer-atpt ()
  "Employ actions of HIDE- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-hide))

(defun ar-hide-show-doublebacktick-in-buffer-atpt ()
  "Employ actions of HIDE-SHOW- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-hide-show))

(defun ar-hyphen-doublebacktick-in-buffer-atpt ()
  "Employ actions of HYPHEN- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-hyphen))

(defun ar-kill-doublebacktick-in-buffer-atpt ()
  "Employ actions of KILL- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-kill))

(defun ar-curvedsinglequote-doublebacktick-in-buffer-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-curvedsinglequote))

(defun ar-length-doublebacktick-in-buffer-atpt ()
  "Employ actions of LENGTH- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-length))

(defun ar-parentize-doublebacktick-in-buffer-atpt ()
  "Employ actions of PARENTIZE- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-parentize))

(defun ar-quote-doublebacktick-in-buffer-atpt ()
  "Employ actions of QUOTE- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-quote))

(defun ar-separate-doublebacktick-in-buffer-atpt ()
  "Employ actions of SEPARATE- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-separate))

(defun ar-show-doublebacktick-in-buffer-atpt ()
  "Employ actions of SHOW- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-show))

(defun ar-singlequote-doublebacktick-in-buffer-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-singlequote))

(defun ar-slash-doublebacktick-in-buffer-atpt ()
  "Employ actions of SLASH- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-slash))

(defun ar-star-doublebacktick-in-buffer-atpt ()
  "Employ actions of STAR- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-star))

(defun ar-slashparen-doublebacktick-in-buffer-atpt ()
  "Employ actions of SLASHPAREN- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-slashparen))

(defun ar-sort-doublebacktick-in-buffer-atpt ()
  "Employ actions of SORT- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-sort))

(defun ar-trim-doublebacktick-in-buffer-atpt ()
  "Employ actions of TRIM- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-trim))

(defun ar-trim-left-doublebacktick-in-buffer-atpt ()
  "Employ actions of TRIM-LEFT- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-trim-left))

(defun ar-trim-right-doublebacktick-in-buffer-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-trim-right))

(defun ar-underscore-doublebacktick-in-buffer-atpt ()
  "Employ actions of UNDERSCORE- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-underscore))

(defun ar-whitespace-doublebacktick-in-buffer-atpt ()
  "Employ actions of WHITESPACE- at things class of DOUBLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'buffer 'ar-th-whitespace))

(defun ar-doublebacktick-in-page-atpt ()
  "Employ actions of  at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th))

(defun ar-greaterangle-doublebacktick-in-page-atpt ()
  "Employ actions of GREATERANGLE- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-greaterangle))

(defun ar-lesserangle-doublebacktick-in-page-atpt ()
  "Employ actions of LESSERANGLE- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-lesserangle))

(defun ar-backslash-doublebacktick-in-page-atpt ()
  "Employ actions of BACKSLASH- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-backslash))

(defun ar-colon-doublebacktick-in-page-atpt ()
  "Employ actions of COLON- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-colon))

(defun ar-beg-doublebacktick-in-page-atpt ()
  "Employ actions of BEG- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-beg))

(defun ar-blok-doublebacktick-in-page-atpt ()
  "Employ actions of BLOK- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-blok))

(defun ar-bounds-doublebacktick-in-page-atpt ()
  "Employ actions of BOUNDS- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-bounds))

(defun ar-brace-doublebacktick-in-page-atpt ()
  "Employ actions of BRACE- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-brace))

(defun ar-bracket-doublebacktick-in-page-atpt ()
  "Employ actions of BRACKET- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-bracket))

(defun ar-commatize-doublebacktick-in-page-atpt ()
  "Employ actions of COMMATIZE- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-commatize))

(defun ar-comment-doublebacktick-in-page-atpt ()
  "Employ actions of COMMENT- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-comment))

(defun ar-dollar-doublebacktick-in-page-atpt ()
  "Employ actions of DOLLAR- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-dollar))

(defun ar-doublebackslash-doublebacktick-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-doublebackslash))

(defun ar-doublebacktick-doublebacktick-in-page-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-doublebacktick))

(defun ar-doublequote-doublebacktick-in-page-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-doublequote))

(defun ar-doubleslash-doublebacktick-in-page-atpt ()
  "Employ actions of DOUBLESLASH- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-doubleslash))

(defun ar-doublebackslashparen-doublebacktick-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-doublebackslashparen))

(defun ar-end-doublebacktick-in-page-atpt ()
  "Employ actions of END- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-end))

(defun ar-escape-doublebacktick-in-page-atpt ()
  "Employ actions of ESCAPE- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-escape))

(defun ar-hide-doublebacktick-in-page-atpt ()
  "Employ actions of HIDE- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-hide))

(defun ar-hide-show-doublebacktick-in-page-atpt ()
  "Employ actions of HIDE-SHOW- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-hide-show))

(defun ar-hyphen-doublebacktick-in-page-atpt ()
  "Employ actions of HYPHEN- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-hyphen))

(defun ar-kill-doublebacktick-in-page-atpt ()
  "Employ actions of KILL- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-kill))

(defun ar-curvedsinglequote-doublebacktick-in-page-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-curvedsinglequote))

(defun ar-length-doublebacktick-in-page-atpt ()
  "Employ actions of LENGTH- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-length))

(defun ar-parentize-doublebacktick-in-page-atpt ()
  "Employ actions of PARENTIZE- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-parentize))

(defun ar-quote-doublebacktick-in-page-atpt ()
  "Employ actions of QUOTE- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-quote))

(defun ar-separate-doublebacktick-in-page-atpt ()
  "Employ actions of SEPARATE- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-separate))

(defun ar-show-doublebacktick-in-page-atpt ()
  "Employ actions of SHOW- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-show))

(defun ar-singlequote-doublebacktick-in-page-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-singlequote))

(defun ar-slash-doublebacktick-in-page-atpt ()
  "Employ actions of SLASH- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-slash))

(defun ar-star-doublebacktick-in-page-atpt ()
  "Employ actions of STAR- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-star))

(defun ar-slashparen-doublebacktick-in-page-atpt ()
  "Employ actions of SLASHPAREN- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-slashparen))

(defun ar-sort-doublebacktick-in-page-atpt ()
  "Employ actions of SORT- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-sort))

(defun ar-trim-doublebacktick-in-page-atpt ()
  "Employ actions of TRIM- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-trim))

(defun ar-trim-left-doublebacktick-in-page-atpt ()
  "Employ actions of TRIM-LEFT- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-trim-left))

(defun ar-trim-right-doublebacktick-in-page-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-trim-right))

(defun ar-underscore-doublebacktick-in-page-atpt ()
  "Employ actions of UNDERSCORE- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-underscore))

(defun ar-whitespace-doublebacktick-in-page-atpt ()
  "Employ actions of WHITESPACE- at things class of DOUBLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'page 'ar-th-whitespace))

(defun ar-doublebacktick-in-paragraph-atpt ()
  "Employ actions of  at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th))

(defun ar-greaterangle-doublebacktick-in-paragraph-atpt ()
  "Employ actions of GREATERANGLE- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-greaterangle))

(defun ar-lesserangle-doublebacktick-in-paragraph-atpt ()
  "Employ actions of LESSERANGLE- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-lesserangle))

(defun ar-backslash-doublebacktick-in-paragraph-atpt ()
  "Employ actions of BACKSLASH- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-backslash))

(defun ar-colon-doublebacktick-in-paragraph-atpt ()
  "Employ actions of COLON- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-colon))

(defun ar-beg-doublebacktick-in-paragraph-atpt ()
  "Employ actions of BEG- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-beg))

(defun ar-blok-doublebacktick-in-paragraph-atpt ()
  "Employ actions of BLOK- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-blok))

(defun ar-bounds-doublebacktick-in-paragraph-atpt ()
  "Employ actions of BOUNDS- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-bounds))

(defun ar-brace-doublebacktick-in-paragraph-atpt ()
  "Employ actions of BRACE- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-brace))

(defun ar-bracket-doublebacktick-in-paragraph-atpt ()
  "Employ actions of BRACKET- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-bracket))

(defun ar-commatize-doublebacktick-in-paragraph-atpt ()
  "Employ actions of COMMATIZE- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-commatize))

(defun ar-comment-doublebacktick-in-paragraph-atpt ()
  "Employ actions of COMMENT- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-comment))

(defun ar-dollar-doublebacktick-in-paragraph-atpt ()
  "Employ actions of DOLLAR- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-dollar))

(defun ar-doublebackslash-doublebacktick-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-doublebackslash))

(defun ar-doublebacktick-doublebacktick-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-doublebacktick))

(defun ar-doublequote-doublebacktick-in-paragraph-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-doublequote))

(defun ar-doubleslash-doublebacktick-in-paragraph-atpt ()
  "Employ actions of DOUBLESLASH- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-doubleslash))

(defun ar-doublebackslashparen-doublebacktick-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-doublebackslashparen))

(defun ar-end-doublebacktick-in-paragraph-atpt ()
  "Employ actions of END- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-end))

(defun ar-escape-doublebacktick-in-paragraph-atpt ()
  "Employ actions of ESCAPE- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-escape))

(defun ar-hide-doublebacktick-in-paragraph-atpt ()
  "Employ actions of HIDE- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-hide))

(defun ar-hide-show-doublebacktick-in-paragraph-atpt ()
  "Employ actions of HIDE-SHOW- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-hide-show))

(defun ar-hyphen-doublebacktick-in-paragraph-atpt ()
  "Employ actions of HYPHEN- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-hyphen))

(defun ar-kill-doublebacktick-in-paragraph-atpt ()
  "Employ actions of KILL- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-kill))

(defun ar-curvedsinglequote-doublebacktick-in-paragraph-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-curvedsinglequote))

(defun ar-length-doublebacktick-in-paragraph-atpt ()
  "Employ actions of LENGTH- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-length))

(defun ar-parentize-doublebacktick-in-paragraph-atpt ()
  "Employ actions of PARENTIZE- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-parentize))

(defun ar-quote-doublebacktick-in-paragraph-atpt ()
  "Employ actions of QUOTE- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-quote))

(defun ar-separate-doublebacktick-in-paragraph-atpt ()
  "Employ actions of SEPARATE- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-separate))

(defun ar-show-doublebacktick-in-paragraph-atpt ()
  "Employ actions of SHOW- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-show))

(defun ar-singlequote-doublebacktick-in-paragraph-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-singlequote))

(defun ar-slash-doublebacktick-in-paragraph-atpt ()
  "Employ actions of SLASH- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-slash))

(defun ar-star-doublebacktick-in-paragraph-atpt ()
  "Employ actions of STAR- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-star))

(defun ar-slashparen-doublebacktick-in-paragraph-atpt ()
  "Employ actions of SLASHPAREN- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-slashparen))

(defun ar-sort-doublebacktick-in-paragraph-atpt ()
  "Employ actions of SORT- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-sort))

(defun ar-trim-doublebacktick-in-paragraph-atpt ()
  "Employ actions of TRIM- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-trim))

(defun ar-trim-left-doublebacktick-in-paragraph-atpt ()
  "Employ actions of TRIM-LEFT- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-trim-left))

(defun ar-trim-right-doublebacktick-in-paragraph-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-trim-right))

(defun ar-underscore-doublebacktick-in-paragraph-atpt ()
  "Employ actions of UNDERSCORE- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-underscore))

(defun ar-whitespace-doublebacktick-in-paragraph-atpt ()
  "Employ actions of WHITESPACE- at things class of DOUBLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'paragraph 'ar-th-whitespace))

(defun ar-doublebacktick-in-region-atpt ()
  "Employ actions of  at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th))

(defun ar-greaterangle-doublebacktick-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-greaterangle))

(defun ar-lesserangle-doublebacktick-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-lesserangle))

(defun ar-backslash-doublebacktick-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-backslash))

(defun ar-colon-doublebacktick-in-region-atpt ()
  "Employ actions of COLON- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-colon))

(defun ar-beg-doublebacktick-in-region-atpt ()
  "Employ actions of BEG- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-beg))

(defun ar-blok-doublebacktick-in-region-atpt ()
  "Employ actions of BLOK- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-blok))

(defun ar-bounds-doublebacktick-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-bounds))

(defun ar-brace-doublebacktick-in-region-atpt ()
  "Employ actions of BRACE- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-brace))

(defun ar-bracket-doublebacktick-in-region-atpt ()
  "Employ actions of BRACKET- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-bracket))

(defun ar-commatize-doublebacktick-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-commatize))

(defun ar-comment-doublebacktick-in-region-atpt ()
  "Employ actions of COMMENT- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-comment))

(defun ar-dollar-doublebacktick-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-dollar))

(defun ar-doublebackslash-doublebacktick-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-doublebacktick-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-doublebacktick))

(defun ar-doublequote-doublebacktick-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-doublequote))

(defun ar-doubleslash-doublebacktick-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-doublebacktick-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-doublebackslashparen))

(defun ar-end-doublebacktick-in-region-atpt ()
  "Employ actions of END- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-end))

(defun ar-escape-doublebacktick-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-escape))

(defun ar-hide-doublebacktick-in-region-atpt ()
  "Employ actions of HIDE- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-hide))

(defun ar-hide-show-doublebacktick-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-hide-show))

(defun ar-hyphen-doublebacktick-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-hyphen))

(defun ar-kill-doublebacktick-in-region-atpt ()
  "Employ actions of KILL- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-kill))

(defun ar-curvedsinglequote-doublebacktick-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-curvedsinglequote))

(defun ar-length-doublebacktick-in-region-atpt ()
  "Employ actions of LENGTH- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-length))

(defun ar-parentize-doublebacktick-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-parentize))

(defun ar-quote-doublebacktick-in-region-atpt ()
  "Employ actions of QUOTE- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-quote))

(defun ar-separate-doublebacktick-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-separate))

(defun ar-show-doublebacktick-in-region-atpt ()
  "Employ actions of SHOW- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-show))

(defun ar-singlequote-doublebacktick-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-singlequote))

(defun ar-slash-doublebacktick-in-region-atpt ()
  "Employ actions of SLASH- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-slash))

(defun ar-star-doublebacktick-in-region-atpt ()
  "Employ actions of STAR- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-star))

(defun ar-slashparen-doublebacktick-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-slashparen))

(defun ar-sort-doublebacktick-in-region-atpt ()
  "Employ actions of SORT- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-sort))

(defun ar-trim-doublebacktick-in-region-atpt ()
  "Employ actions of TRIM- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-trim))

(defun ar-trim-left-doublebacktick-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-trim-left))

(defun ar-trim-right-doublebacktick-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-trim-right))

(defun ar-underscore-doublebacktick-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-underscore))

(defun ar-whitespace-doublebacktick-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of DOUBLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doublebacktick 'region 'ar-th-whitespace))

(defun ar-triplebacktick-in-buffer-atpt ()
  "Employ actions of  at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th))

(defun ar-greaterangle-triplebacktick-in-buffer-atpt ()
  "Employ actions of GREATERANGLE- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-greaterangle))

(defun ar-lesserangle-triplebacktick-in-buffer-atpt ()
  "Employ actions of LESSERANGLE- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-lesserangle))

(defun ar-backslash-triplebacktick-in-buffer-atpt ()
  "Employ actions of BACKSLASH- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-backslash))

(defun ar-colon-triplebacktick-in-buffer-atpt ()
  "Employ actions of COLON- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-colon))

(defun ar-beg-triplebacktick-in-buffer-atpt ()
  "Employ actions of BEG- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-beg))

(defun ar-blok-triplebacktick-in-buffer-atpt ()
  "Employ actions of BLOK- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-blok))

(defun ar-bounds-triplebacktick-in-buffer-atpt ()
  "Employ actions of BOUNDS- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-bounds))

(defun ar-brace-triplebacktick-in-buffer-atpt ()
  "Employ actions of BRACE- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-brace))

(defun ar-bracket-triplebacktick-in-buffer-atpt ()
  "Employ actions of BRACKET- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-bracket))

(defun ar-commatize-triplebacktick-in-buffer-atpt ()
  "Employ actions of COMMATIZE- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-commatize))

(defun ar-comment-triplebacktick-in-buffer-atpt ()
  "Employ actions of COMMENT- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-comment))

(defun ar-dollar-triplebacktick-in-buffer-atpt ()
  "Employ actions of DOLLAR- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-dollar))

(defun ar-doublebackslash-triplebacktick-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-doublebackslash))

(defun ar-doublebacktick-triplebacktick-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-doublebacktick))

(defun ar-doublequote-triplebacktick-in-buffer-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-doublequote))

(defun ar-doubleslash-triplebacktick-in-buffer-atpt ()
  "Employ actions of DOUBLESLASH- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-doubleslash))

(defun ar-doublebackslashparen-triplebacktick-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-doublebackslashparen))

(defun ar-end-triplebacktick-in-buffer-atpt ()
  "Employ actions of END- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-end))

(defun ar-escape-triplebacktick-in-buffer-atpt ()
  "Employ actions of ESCAPE- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-escape))

(defun ar-hide-triplebacktick-in-buffer-atpt ()
  "Employ actions of HIDE- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-hide))

(defun ar-hide-show-triplebacktick-in-buffer-atpt ()
  "Employ actions of HIDE-SHOW- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-hide-show))

(defun ar-hyphen-triplebacktick-in-buffer-atpt ()
  "Employ actions of HYPHEN- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-hyphen))

(defun ar-kill-triplebacktick-in-buffer-atpt ()
  "Employ actions of KILL- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-kill))

(defun ar-curvedsinglequote-triplebacktick-in-buffer-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-curvedsinglequote))

(defun ar-length-triplebacktick-in-buffer-atpt ()
  "Employ actions of LENGTH- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-length))

(defun ar-parentize-triplebacktick-in-buffer-atpt ()
  "Employ actions of PARENTIZE- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-parentize))

(defun ar-quote-triplebacktick-in-buffer-atpt ()
  "Employ actions of QUOTE- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-quote))

(defun ar-separate-triplebacktick-in-buffer-atpt ()
  "Employ actions of SEPARATE- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-separate))

(defun ar-show-triplebacktick-in-buffer-atpt ()
  "Employ actions of SHOW- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-show))

(defun ar-singlequote-triplebacktick-in-buffer-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-singlequote))

(defun ar-slash-triplebacktick-in-buffer-atpt ()
  "Employ actions of SLASH- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-slash))

(defun ar-star-triplebacktick-in-buffer-atpt ()
  "Employ actions of STAR- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-star))

(defun ar-slashparen-triplebacktick-in-buffer-atpt ()
  "Employ actions of SLASHPAREN- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-slashparen))

(defun ar-sort-triplebacktick-in-buffer-atpt ()
  "Employ actions of SORT- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-sort))

(defun ar-trim-triplebacktick-in-buffer-atpt ()
  "Employ actions of TRIM- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-trim))

(defun ar-trim-left-triplebacktick-in-buffer-atpt ()
  "Employ actions of TRIM-LEFT- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-trim-left))

(defun ar-trim-right-triplebacktick-in-buffer-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-trim-right))

(defun ar-underscore-triplebacktick-in-buffer-atpt ()
  "Employ actions of UNDERSCORE- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-underscore))

(defun ar-whitespace-triplebacktick-in-buffer-atpt ()
  "Employ actions of WHITESPACE- at things class of TRIPLEBACKTICK residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'buffer 'ar-th-whitespace))

(defun ar-triplebacktick-in-page-atpt ()
  "Employ actions of  at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th))

(defun ar-greaterangle-triplebacktick-in-page-atpt ()
  "Employ actions of GREATERANGLE- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-greaterangle))

(defun ar-lesserangle-triplebacktick-in-page-atpt ()
  "Employ actions of LESSERANGLE- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-lesserangle))

(defun ar-backslash-triplebacktick-in-page-atpt ()
  "Employ actions of BACKSLASH- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-backslash))

(defun ar-colon-triplebacktick-in-page-atpt ()
  "Employ actions of COLON- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-colon))

(defun ar-beg-triplebacktick-in-page-atpt ()
  "Employ actions of BEG- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-beg))

(defun ar-blok-triplebacktick-in-page-atpt ()
  "Employ actions of BLOK- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-blok))

(defun ar-bounds-triplebacktick-in-page-atpt ()
  "Employ actions of BOUNDS- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-bounds))

(defun ar-brace-triplebacktick-in-page-atpt ()
  "Employ actions of BRACE- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-brace))

(defun ar-bracket-triplebacktick-in-page-atpt ()
  "Employ actions of BRACKET- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-bracket))

(defun ar-commatize-triplebacktick-in-page-atpt ()
  "Employ actions of COMMATIZE- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-commatize))

(defun ar-comment-triplebacktick-in-page-atpt ()
  "Employ actions of COMMENT- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-comment))

(defun ar-dollar-triplebacktick-in-page-atpt ()
  "Employ actions of DOLLAR- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-dollar))

(defun ar-doublebackslash-triplebacktick-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-doublebackslash))

(defun ar-doublebacktick-triplebacktick-in-page-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-doublebacktick))

(defun ar-doublequote-triplebacktick-in-page-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-doublequote))

(defun ar-doubleslash-triplebacktick-in-page-atpt ()
  "Employ actions of DOUBLESLASH- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-doubleslash))

(defun ar-doublebackslashparen-triplebacktick-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-doublebackslashparen))

(defun ar-end-triplebacktick-in-page-atpt ()
  "Employ actions of END- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-end))

(defun ar-escape-triplebacktick-in-page-atpt ()
  "Employ actions of ESCAPE- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-escape))

(defun ar-hide-triplebacktick-in-page-atpt ()
  "Employ actions of HIDE- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-hide))

(defun ar-hide-show-triplebacktick-in-page-atpt ()
  "Employ actions of HIDE-SHOW- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-hide-show))

(defun ar-hyphen-triplebacktick-in-page-atpt ()
  "Employ actions of HYPHEN- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-hyphen))

(defun ar-kill-triplebacktick-in-page-atpt ()
  "Employ actions of KILL- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-kill))

(defun ar-curvedsinglequote-triplebacktick-in-page-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-curvedsinglequote))

(defun ar-length-triplebacktick-in-page-atpt ()
  "Employ actions of LENGTH- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-length))

(defun ar-parentize-triplebacktick-in-page-atpt ()
  "Employ actions of PARENTIZE- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-parentize))

(defun ar-quote-triplebacktick-in-page-atpt ()
  "Employ actions of QUOTE- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-quote))

(defun ar-separate-triplebacktick-in-page-atpt ()
  "Employ actions of SEPARATE- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-separate))

(defun ar-show-triplebacktick-in-page-atpt ()
  "Employ actions of SHOW- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-show))

(defun ar-singlequote-triplebacktick-in-page-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-singlequote))

(defun ar-slash-triplebacktick-in-page-atpt ()
  "Employ actions of SLASH- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-slash))

(defun ar-star-triplebacktick-in-page-atpt ()
  "Employ actions of STAR- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-star))

(defun ar-slashparen-triplebacktick-in-page-atpt ()
  "Employ actions of SLASHPAREN- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-slashparen))

(defun ar-sort-triplebacktick-in-page-atpt ()
  "Employ actions of SORT- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-sort))

(defun ar-trim-triplebacktick-in-page-atpt ()
  "Employ actions of TRIM- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-trim))

(defun ar-trim-left-triplebacktick-in-page-atpt ()
  "Employ actions of TRIM-LEFT- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-trim-left))

(defun ar-trim-right-triplebacktick-in-page-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-trim-right))

(defun ar-underscore-triplebacktick-in-page-atpt ()
  "Employ actions of UNDERSCORE- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-underscore))

(defun ar-whitespace-triplebacktick-in-page-atpt ()
  "Employ actions of WHITESPACE- at things class of TRIPLEBACKTICK residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'page 'ar-th-whitespace))

(defun ar-triplebacktick-in-paragraph-atpt ()
  "Employ actions of  at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th))

(defun ar-greaterangle-triplebacktick-in-paragraph-atpt ()
  "Employ actions of GREATERANGLE- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-greaterangle))

(defun ar-lesserangle-triplebacktick-in-paragraph-atpt ()
  "Employ actions of LESSERANGLE- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-lesserangle))

(defun ar-backslash-triplebacktick-in-paragraph-atpt ()
  "Employ actions of BACKSLASH- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-backslash))

(defun ar-colon-triplebacktick-in-paragraph-atpt ()
  "Employ actions of COLON- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-colon))

(defun ar-beg-triplebacktick-in-paragraph-atpt ()
  "Employ actions of BEG- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-beg))

(defun ar-blok-triplebacktick-in-paragraph-atpt ()
  "Employ actions of BLOK- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-blok))

(defun ar-bounds-triplebacktick-in-paragraph-atpt ()
  "Employ actions of BOUNDS- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-bounds))

(defun ar-brace-triplebacktick-in-paragraph-atpt ()
  "Employ actions of BRACE- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-brace))

(defun ar-bracket-triplebacktick-in-paragraph-atpt ()
  "Employ actions of BRACKET- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-bracket))

(defun ar-commatize-triplebacktick-in-paragraph-atpt ()
  "Employ actions of COMMATIZE- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-commatize))

(defun ar-comment-triplebacktick-in-paragraph-atpt ()
  "Employ actions of COMMENT- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-comment))

(defun ar-dollar-triplebacktick-in-paragraph-atpt ()
  "Employ actions of DOLLAR- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-dollar))

(defun ar-doublebackslash-triplebacktick-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-doublebackslash))

(defun ar-doublebacktick-triplebacktick-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-doublebacktick))

(defun ar-doublequote-triplebacktick-in-paragraph-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-doublequote))

(defun ar-doubleslash-triplebacktick-in-paragraph-atpt ()
  "Employ actions of DOUBLESLASH- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-doubleslash))

(defun ar-doublebackslashparen-triplebacktick-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-doublebackslashparen))

(defun ar-end-triplebacktick-in-paragraph-atpt ()
  "Employ actions of END- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-end))

(defun ar-escape-triplebacktick-in-paragraph-atpt ()
  "Employ actions of ESCAPE- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-escape))

(defun ar-hide-triplebacktick-in-paragraph-atpt ()
  "Employ actions of HIDE- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-hide))

(defun ar-hide-show-triplebacktick-in-paragraph-atpt ()
  "Employ actions of HIDE-SHOW- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-hide-show))

(defun ar-hyphen-triplebacktick-in-paragraph-atpt ()
  "Employ actions of HYPHEN- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-hyphen))

(defun ar-kill-triplebacktick-in-paragraph-atpt ()
  "Employ actions of KILL- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-kill))

(defun ar-curvedsinglequote-triplebacktick-in-paragraph-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-curvedsinglequote))

(defun ar-length-triplebacktick-in-paragraph-atpt ()
  "Employ actions of LENGTH- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-length))

(defun ar-parentize-triplebacktick-in-paragraph-atpt ()
  "Employ actions of PARENTIZE- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-parentize))

(defun ar-quote-triplebacktick-in-paragraph-atpt ()
  "Employ actions of QUOTE- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-quote))

(defun ar-separate-triplebacktick-in-paragraph-atpt ()
  "Employ actions of SEPARATE- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-separate))

(defun ar-show-triplebacktick-in-paragraph-atpt ()
  "Employ actions of SHOW- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-show))

(defun ar-singlequote-triplebacktick-in-paragraph-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-singlequote))

(defun ar-slash-triplebacktick-in-paragraph-atpt ()
  "Employ actions of SLASH- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-slash))

(defun ar-star-triplebacktick-in-paragraph-atpt ()
  "Employ actions of STAR- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-star))

(defun ar-slashparen-triplebacktick-in-paragraph-atpt ()
  "Employ actions of SLASHPAREN- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-slashparen))

(defun ar-sort-triplebacktick-in-paragraph-atpt ()
  "Employ actions of SORT- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-sort))

(defun ar-trim-triplebacktick-in-paragraph-atpt ()
  "Employ actions of TRIM- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-trim))

(defun ar-trim-left-triplebacktick-in-paragraph-atpt ()
  "Employ actions of TRIM-LEFT- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-trim-left))

(defun ar-trim-right-triplebacktick-in-paragraph-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-trim-right))

(defun ar-underscore-triplebacktick-in-paragraph-atpt ()
  "Employ actions of UNDERSCORE- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-underscore))

(defun ar-whitespace-triplebacktick-in-paragraph-atpt ()
  "Employ actions of WHITESPACE- at things class of TRIPLEBACKTICK residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'paragraph 'ar-th-whitespace))

(defun ar-triplebacktick-in-region-atpt ()
  "Employ actions of  at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th))

(defun ar-greaterangle-triplebacktick-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-greaterangle))

(defun ar-lesserangle-triplebacktick-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-lesserangle))

(defun ar-backslash-triplebacktick-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-backslash))

(defun ar-colon-triplebacktick-in-region-atpt ()
  "Employ actions of COLON- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-colon))

(defun ar-beg-triplebacktick-in-region-atpt ()
  "Employ actions of BEG- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-beg))

(defun ar-blok-triplebacktick-in-region-atpt ()
  "Employ actions of BLOK- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-blok))

(defun ar-bounds-triplebacktick-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-bounds))

(defun ar-brace-triplebacktick-in-region-atpt ()
  "Employ actions of BRACE- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-brace))

(defun ar-bracket-triplebacktick-in-region-atpt ()
  "Employ actions of BRACKET- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-bracket))

(defun ar-commatize-triplebacktick-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-commatize))

(defun ar-comment-triplebacktick-in-region-atpt ()
  "Employ actions of COMMENT- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-comment))

(defun ar-dollar-triplebacktick-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-dollar))

(defun ar-doublebackslash-triplebacktick-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-triplebacktick-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-doublebacktick))

(defun ar-doublequote-triplebacktick-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-doublequote))

(defun ar-doubleslash-triplebacktick-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-triplebacktick-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-doublebackslashparen))

(defun ar-end-triplebacktick-in-region-atpt ()
  "Employ actions of END- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-end))

(defun ar-escape-triplebacktick-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-escape))

(defun ar-hide-triplebacktick-in-region-atpt ()
  "Employ actions of HIDE- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-hide))

(defun ar-hide-show-triplebacktick-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-hide-show))

(defun ar-hyphen-triplebacktick-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-hyphen))

(defun ar-kill-triplebacktick-in-region-atpt ()
  "Employ actions of KILL- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-kill))

(defun ar-curvedsinglequote-triplebacktick-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-curvedsinglequote))

(defun ar-length-triplebacktick-in-region-atpt ()
  "Employ actions of LENGTH- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-length))

(defun ar-parentize-triplebacktick-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-parentize))

(defun ar-quote-triplebacktick-in-region-atpt ()
  "Employ actions of QUOTE- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-quote))

(defun ar-separate-triplebacktick-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-separate))

(defun ar-show-triplebacktick-in-region-atpt ()
  "Employ actions of SHOW- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-show))

(defun ar-singlequote-triplebacktick-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-singlequote))

(defun ar-slash-triplebacktick-in-region-atpt ()
  "Employ actions of SLASH- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-slash))

(defun ar-star-triplebacktick-in-region-atpt ()
  "Employ actions of STAR- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-star))

(defun ar-slashparen-triplebacktick-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-slashparen))

(defun ar-sort-triplebacktick-in-region-atpt ()
  "Employ actions of SORT- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-sort))

(defun ar-trim-triplebacktick-in-region-atpt ()
  "Employ actions of TRIM- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-trim))

(defun ar-trim-left-triplebacktick-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-trim-left))

(defun ar-trim-right-triplebacktick-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-trim-right))

(defun ar-underscore-triplebacktick-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-underscore))

(defun ar-whitespace-triplebacktick-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of TRIPLEBACKTICK residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'triplebacktick 'region 'ar-th-whitespace))

(defun ar-doubleslash-in-buffer-atpt ()
  "Employ actions of  at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th))

(defun ar-greaterangle-doubleslash-in-buffer-atpt ()
  "Employ actions of GREATERANGLE- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-greaterangle))

(defun ar-lesserangle-doubleslash-in-buffer-atpt ()
  "Employ actions of LESSERANGLE- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-lesserangle))

(defun ar-backslash-doubleslash-in-buffer-atpt ()
  "Employ actions of BACKSLASH- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-backslash))

(defun ar-colon-doubleslash-in-buffer-atpt ()
  "Employ actions of COLON- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-colon))

(defun ar-beg-doubleslash-in-buffer-atpt ()
  "Employ actions of BEG- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-beg))

(defun ar-blok-doubleslash-in-buffer-atpt ()
  "Employ actions of BLOK- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-blok))

(defun ar-bounds-doubleslash-in-buffer-atpt ()
  "Employ actions of BOUNDS- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-bounds))

(defun ar-brace-doubleslash-in-buffer-atpt ()
  "Employ actions of BRACE- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-brace))

(defun ar-bracket-doubleslash-in-buffer-atpt ()
  "Employ actions of BRACKET- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-bracket))

(defun ar-commatize-doubleslash-in-buffer-atpt ()
  "Employ actions of COMMATIZE- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-commatize))

(defun ar-comment-doubleslash-in-buffer-atpt ()
  "Employ actions of COMMENT- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-comment))

(defun ar-dollar-doubleslash-in-buffer-atpt ()
  "Employ actions of DOLLAR- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-dollar))

(defun ar-doublebackslash-doubleslash-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-doublebackslash))

(defun ar-doublebacktick-doubleslash-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-doublebacktick))

(defun ar-doublequote-doubleslash-in-buffer-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-doublequote))

(defun ar-doubleslash-doubleslash-in-buffer-atpt ()
  "Employ actions of DOUBLESLASH- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-doubleslash))

(defun ar-doublebackslashparen-doubleslash-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-doublebackslashparen))

(defun ar-end-doubleslash-in-buffer-atpt ()
  "Employ actions of END- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-end))

(defun ar-escape-doubleslash-in-buffer-atpt ()
  "Employ actions of ESCAPE- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-escape))

(defun ar-hide-doubleslash-in-buffer-atpt ()
  "Employ actions of HIDE- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-hide))

(defun ar-hide-show-doubleslash-in-buffer-atpt ()
  "Employ actions of HIDE-SHOW- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-hide-show))

(defun ar-hyphen-doubleslash-in-buffer-atpt ()
  "Employ actions of HYPHEN- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-hyphen))

(defun ar-kill-doubleslash-in-buffer-atpt ()
  "Employ actions of KILL- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-kill))

(defun ar-curvedsinglequote-doubleslash-in-buffer-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-curvedsinglequote))

(defun ar-length-doubleslash-in-buffer-atpt ()
  "Employ actions of LENGTH- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-length))

(defun ar-parentize-doubleslash-in-buffer-atpt ()
  "Employ actions of PARENTIZE- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-parentize))

(defun ar-quote-doubleslash-in-buffer-atpt ()
  "Employ actions of QUOTE- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-quote))

(defun ar-separate-doubleslash-in-buffer-atpt ()
  "Employ actions of SEPARATE- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-separate))

(defun ar-show-doubleslash-in-buffer-atpt ()
  "Employ actions of SHOW- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-show))

(defun ar-singlequote-doubleslash-in-buffer-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-singlequote))

(defun ar-slash-doubleslash-in-buffer-atpt ()
  "Employ actions of SLASH- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-slash))

(defun ar-star-doubleslash-in-buffer-atpt ()
  "Employ actions of STAR- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-star))

(defun ar-slashparen-doubleslash-in-buffer-atpt ()
  "Employ actions of SLASHPAREN- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-slashparen))

(defun ar-sort-doubleslash-in-buffer-atpt ()
  "Employ actions of SORT- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-sort))

(defun ar-trim-doubleslash-in-buffer-atpt ()
  "Employ actions of TRIM- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-trim))

(defun ar-trim-left-doubleslash-in-buffer-atpt ()
  "Employ actions of TRIM-LEFT- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-trim-left))

(defun ar-trim-right-doubleslash-in-buffer-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-trim-right))

(defun ar-underscore-doubleslash-in-buffer-atpt ()
  "Employ actions of UNDERSCORE- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-underscore))

(defun ar-whitespace-doubleslash-in-buffer-atpt ()
  "Employ actions of WHITESPACE- at things class of DOUBLESLASH residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'buffer 'ar-th-whitespace))

(defun ar-doubleslash-in-page-atpt ()
  "Employ actions of  at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th))

(defun ar-greaterangle-doubleslash-in-page-atpt ()
  "Employ actions of GREATERANGLE- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-greaterangle))

(defun ar-lesserangle-doubleslash-in-page-atpt ()
  "Employ actions of LESSERANGLE- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-lesserangle))

(defun ar-backslash-doubleslash-in-page-atpt ()
  "Employ actions of BACKSLASH- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-backslash))

(defun ar-colon-doubleslash-in-page-atpt ()
  "Employ actions of COLON- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-colon))

(defun ar-beg-doubleslash-in-page-atpt ()
  "Employ actions of BEG- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-beg))

(defun ar-blok-doubleslash-in-page-atpt ()
  "Employ actions of BLOK- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-blok))

(defun ar-bounds-doubleslash-in-page-atpt ()
  "Employ actions of BOUNDS- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-bounds))

(defun ar-brace-doubleslash-in-page-atpt ()
  "Employ actions of BRACE- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-brace))

(defun ar-bracket-doubleslash-in-page-atpt ()
  "Employ actions of BRACKET- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-bracket))

(defun ar-commatize-doubleslash-in-page-atpt ()
  "Employ actions of COMMATIZE- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-commatize))

(defun ar-comment-doubleslash-in-page-atpt ()
  "Employ actions of COMMENT- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-comment))

(defun ar-dollar-doubleslash-in-page-atpt ()
  "Employ actions of DOLLAR- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-dollar))

(defun ar-doublebackslash-doubleslash-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-doublebackslash))

(defun ar-doublebacktick-doubleslash-in-page-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-doublebacktick))

(defun ar-doublequote-doubleslash-in-page-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-doublequote))

(defun ar-doubleslash-doubleslash-in-page-atpt ()
  "Employ actions of DOUBLESLASH- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-doubleslash))

(defun ar-doublebackslashparen-doubleslash-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-doublebackslashparen))

(defun ar-end-doubleslash-in-page-atpt ()
  "Employ actions of END- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-end))

(defun ar-escape-doubleslash-in-page-atpt ()
  "Employ actions of ESCAPE- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-escape))

(defun ar-hide-doubleslash-in-page-atpt ()
  "Employ actions of HIDE- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-hide))

(defun ar-hide-show-doubleslash-in-page-atpt ()
  "Employ actions of HIDE-SHOW- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-hide-show))

(defun ar-hyphen-doubleslash-in-page-atpt ()
  "Employ actions of HYPHEN- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-hyphen))

(defun ar-kill-doubleslash-in-page-atpt ()
  "Employ actions of KILL- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-kill))

(defun ar-curvedsinglequote-doubleslash-in-page-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-curvedsinglequote))

(defun ar-length-doubleslash-in-page-atpt ()
  "Employ actions of LENGTH- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-length))

(defun ar-parentize-doubleslash-in-page-atpt ()
  "Employ actions of PARENTIZE- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-parentize))

(defun ar-quote-doubleslash-in-page-atpt ()
  "Employ actions of QUOTE- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-quote))

(defun ar-separate-doubleslash-in-page-atpt ()
  "Employ actions of SEPARATE- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-separate))

(defun ar-show-doubleslash-in-page-atpt ()
  "Employ actions of SHOW- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-show))

(defun ar-singlequote-doubleslash-in-page-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-singlequote))

(defun ar-slash-doubleslash-in-page-atpt ()
  "Employ actions of SLASH- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-slash))

(defun ar-star-doubleslash-in-page-atpt ()
  "Employ actions of STAR- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-star))

(defun ar-slashparen-doubleslash-in-page-atpt ()
  "Employ actions of SLASHPAREN- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-slashparen))

(defun ar-sort-doubleslash-in-page-atpt ()
  "Employ actions of SORT- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-sort))

(defun ar-trim-doubleslash-in-page-atpt ()
  "Employ actions of TRIM- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-trim))

(defun ar-trim-left-doubleslash-in-page-atpt ()
  "Employ actions of TRIM-LEFT- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-trim-left))

(defun ar-trim-right-doubleslash-in-page-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-trim-right))

(defun ar-underscore-doubleslash-in-page-atpt ()
  "Employ actions of UNDERSCORE- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-underscore))

(defun ar-whitespace-doubleslash-in-page-atpt ()
  "Employ actions of WHITESPACE- at things class of DOUBLESLASH residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'page 'ar-th-whitespace))

(defun ar-doubleslash-in-paragraph-atpt ()
  "Employ actions of  at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th))

(defun ar-greaterangle-doubleslash-in-paragraph-atpt ()
  "Employ actions of GREATERANGLE- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-greaterangle))

(defun ar-lesserangle-doubleslash-in-paragraph-atpt ()
  "Employ actions of LESSERANGLE- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-lesserangle))

(defun ar-backslash-doubleslash-in-paragraph-atpt ()
  "Employ actions of BACKSLASH- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-backslash))

(defun ar-colon-doubleslash-in-paragraph-atpt ()
  "Employ actions of COLON- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-colon))

(defun ar-beg-doubleslash-in-paragraph-atpt ()
  "Employ actions of BEG- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-beg))

(defun ar-blok-doubleslash-in-paragraph-atpt ()
  "Employ actions of BLOK- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-blok))

(defun ar-bounds-doubleslash-in-paragraph-atpt ()
  "Employ actions of BOUNDS- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-bounds))

(defun ar-brace-doubleslash-in-paragraph-atpt ()
  "Employ actions of BRACE- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-brace))

(defun ar-bracket-doubleslash-in-paragraph-atpt ()
  "Employ actions of BRACKET- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-bracket))

(defun ar-commatize-doubleslash-in-paragraph-atpt ()
  "Employ actions of COMMATIZE- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-commatize))

(defun ar-comment-doubleslash-in-paragraph-atpt ()
  "Employ actions of COMMENT- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-comment))

(defun ar-dollar-doubleslash-in-paragraph-atpt ()
  "Employ actions of DOLLAR- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-dollar))

(defun ar-doublebackslash-doubleslash-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-doublebackslash))

(defun ar-doublebacktick-doubleslash-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-doublebacktick))

(defun ar-doublequote-doubleslash-in-paragraph-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-doublequote))

(defun ar-doubleslash-doubleslash-in-paragraph-atpt ()
  "Employ actions of DOUBLESLASH- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-doubleslash))

(defun ar-doublebackslashparen-doubleslash-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-doublebackslashparen))

(defun ar-end-doubleslash-in-paragraph-atpt ()
  "Employ actions of END- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-end))

(defun ar-escape-doubleslash-in-paragraph-atpt ()
  "Employ actions of ESCAPE- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-escape))

(defun ar-hide-doubleslash-in-paragraph-atpt ()
  "Employ actions of HIDE- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-hide))

(defun ar-hide-show-doubleslash-in-paragraph-atpt ()
  "Employ actions of HIDE-SHOW- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-hide-show))

(defun ar-hyphen-doubleslash-in-paragraph-atpt ()
  "Employ actions of HYPHEN- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-hyphen))

(defun ar-kill-doubleslash-in-paragraph-atpt ()
  "Employ actions of KILL- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-kill))

(defun ar-curvedsinglequote-doubleslash-in-paragraph-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-curvedsinglequote))

(defun ar-length-doubleslash-in-paragraph-atpt ()
  "Employ actions of LENGTH- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-length))

(defun ar-parentize-doubleslash-in-paragraph-atpt ()
  "Employ actions of PARENTIZE- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-parentize))

(defun ar-quote-doubleslash-in-paragraph-atpt ()
  "Employ actions of QUOTE- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-quote))

(defun ar-separate-doubleslash-in-paragraph-atpt ()
  "Employ actions of SEPARATE- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-separate))

(defun ar-show-doubleslash-in-paragraph-atpt ()
  "Employ actions of SHOW- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-show))

(defun ar-singlequote-doubleslash-in-paragraph-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-singlequote))

(defun ar-slash-doubleslash-in-paragraph-atpt ()
  "Employ actions of SLASH- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-slash))

(defun ar-star-doubleslash-in-paragraph-atpt ()
  "Employ actions of STAR- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-star))

(defun ar-slashparen-doubleslash-in-paragraph-atpt ()
  "Employ actions of SLASHPAREN- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-slashparen))

(defun ar-sort-doubleslash-in-paragraph-atpt ()
  "Employ actions of SORT- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-sort))

(defun ar-trim-doubleslash-in-paragraph-atpt ()
  "Employ actions of TRIM- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-trim))

(defun ar-trim-left-doubleslash-in-paragraph-atpt ()
  "Employ actions of TRIM-LEFT- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-trim-left))

(defun ar-trim-right-doubleslash-in-paragraph-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-trim-right))

(defun ar-underscore-doubleslash-in-paragraph-atpt ()
  "Employ actions of UNDERSCORE- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-underscore))

(defun ar-whitespace-doubleslash-in-paragraph-atpt ()
  "Employ actions of WHITESPACE- at things class of DOUBLESLASH residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'paragraph 'ar-th-whitespace))

(defun ar-doubleslash-in-region-atpt ()
  "Employ actions of  at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th))

(defun ar-greaterangle-doubleslash-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-greaterangle))

(defun ar-lesserangle-doubleslash-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-lesserangle))

(defun ar-backslash-doubleslash-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-backslash))

(defun ar-colon-doubleslash-in-region-atpt ()
  "Employ actions of COLON- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-colon))

(defun ar-beg-doubleslash-in-region-atpt ()
  "Employ actions of BEG- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-beg))

(defun ar-blok-doubleslash-in-region-atpt ()
  "Employ actions of BLOK- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-blok))

(defun ar-bounds-doubleslash-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-bounds))

(defun ar-brace-doubleslash-in-region-atpt ()
  "Employ actions of BRACE- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-brace))

(defun ar-bracket-doubleslash-in-region-atpt ()
  "Employ actions of BRACKET- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-bracket))

(defun ar-commatize-doubleslash-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-commatize))

(defun ar-comment-doubleslash-in-region-atpt ()
  "Employ actions of COMMENT- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-comment))

(defun ar-dollar-doubleslash-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-dollar))

(defun ar-doublebackslash-doubleslash-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-doubleslash-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-doublebacktick))

(defun ar-doublequote-doubleslash-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-doublequote))

(defun ar-doubleslash-doubleslash-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-doubleslash-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-doublebackslashparen))

(defun ar-end-doubleslash-in-region-atpt ()
  "Employ actions of END- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-end))

(defun ar-escape-doubleslash-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-escape))

(defun ar-hide-doubleslash-in-region-atpt ()
  "Employ actions of HIDE- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-hide))

(defun ar-hide-show-doubleslash-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-hide-show))

(defun ar-hyphen-doubleslash-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-hyphen))

(defun ar-kill-doubleslash-in-region-atpt ()
  "Employ actions of KILL- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-kill))

(defun ar-curvedsinglequote-doubleslash-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-curvedsinglequote))

(defun ar-length-doubleslash-in-region-atpt ()
  "Employ actions of LENGTH- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-length))

(defun ar-parentize-doubleslash-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-parentize))

(defun ar-quote-doubleslash-in-region-atpt ()
  "Employ actions of QUOTE- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-quote))

(defun ar-separate-doubleslash-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-separate))

(defun ar-show-doubleslash-in-region-atpt ()
  "Employ actions of SHOW- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-show))

(defun ar-singlequote-doubleslash-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-singlequote))

(defun ar-slash-doubleslash-in-region-atpt ()
  "Employ actions of SLASH- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-slash))

(defun ar-star-doubleslash-in-region-atpt ()
  "Employ actions of STAR- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-star))

(defun ar-slashparen-doubleslash-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-slashparen))

(defun ar-sort-doubleslash-in-region-atpt ()
  "Employ actions of SORT- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-sort))

(defun ar-trim-doubleslash-in-region-atpt ()
  "Employ actions of TRIM- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-trim))

(defun ar-trim-left-doubleslash-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-trim-left))

(defun ar-trim-right-doubleslash-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-trim-right))

(defun ar-underscore-doubleslash-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-underscore))

(defun ar-whitespace-doubleslash-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of DOUBLESLASH residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'region 'ar-th-whitespace))

(defun ar-backslashparen-in-buffer-atpt ()
  "Employ actions of  at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th))

(defun ar-greaterangle-backslashparen-in-buffer-atpt ()
  "Employ actions of GREATERANGLE- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-greaterangle))

(defun ar-lesserangle-backslashparen-in-buffer-atpt ()
  "Employ actions of LESSERANGLE- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-lesserangle))

(defun ar-backslash-backslashparen-in-buffer-atpt ()
  "Employ actions of BACKSLASH- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-backslash))

(defun ar-colon-backslashparen-in-buffer-atpt ()
  "Employ actions of COLON- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-colon))

(defun ar-beg-backslashparen-in-buffer-atpt ()
  "Employ actions of BEG- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-beg))

(defun ar-blok-backslashparen-in-buffer-atpt ()
  "Employ actions of BLOK- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-blok))

(defun ar-bounds-backslashparen-in-buffer-atpt ()
  "Employ actions of BOUNDS- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-bounds))

(defun ar-brace-backslashparen-in-buffer-atpt ()
  "Employ actions of BRACE- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-brace))

(defun ar-bracket-backslashparen-in-buffer-atpt ()
  "Employ actions of BRACKET- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-bracket))

(defun ar-commatize-backslashparen-in-buffer-atpt ()
  "Employ actions of COMMATIZE- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-commatize))

(defun ar-comment-backslashparen-in-buffer-atpt ()
  "Employ actions of COMMENT- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-comment))

(defun ar-dollar-backslashparen-in-buffer-atpt ()
  "Employ actions of DOLLAR- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-dollar))

(defun ar-doublebackslash-backslashparen-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-doublebackslash))

(defun ar-doublebacktick-backslashparen-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-doublebacktick))

(defun ar-doublequote-backslashparen-in-buffer-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-doublequote))

(defun ar-doubleslash-backslashparen-in-buffer-atpt ()
  "Employ actions of DOUBLESLASH- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-doubleslash))

(defun ar-doublebackslashparen-backslashparen-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-doublebackslashparen))

(defun ar-end-backslashparen-in-buffer-atpt ()
  "Employ actions of END- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-end))

(defun ar-escape-backslashparen-in-buffer-atpt ()
  "Employ actions of ESCAPE- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-escape))

(defun ar-hide-backslashparen-in-buffer-atpt ()
  "Employ actions of HIDE- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-hide))

(defun ar-hide-show-backslashparen-in-buffer-atpt ()
  "Employ actions of HIDE-SHOW- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-hide-show))

(defun ar-hyphen-backslashparen-in-buffer-atpt ()
  "Employ actions of HYPHEN- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-hyphen))

(defun ar-kill-backslashparen-in-buffer-atpt ()
  "Employ actions of KILL- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-kill))

(defun ar-curvedsinglequote-backslashparen-in-buffer-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-curvedsinglequote))

(defun ar-length-backslashparen-in-buffer-atpt ()
  "Employ actions of LENGTH- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-length))

(defun ar-parentize-backslashparen-in-buffer-atpt ()
  "Employ actions of PARENTIZE- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-parentize))

(defun ar-quote-backslashparen-in-buffer-atpt ()
  "Employ actions of QUOTE- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-quote))

(defun ar-separate-backslashparen-in-buffer-atpt ()
  "Employ actions of SEPARATE- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-separate))

(defun ar-show-backslashparen-in-buffer-atpt ()
  "Employ actions of SHOW- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-show))

(defun ar-singlequote-backslashparen-in-buffer-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-singlequote))

(defun ar-slash-backslashparen-in-buffer-atpt ()
  "Employ actions of SLASH- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-slash))

(defun ar-star-backslashparen-in-buffer-atpt ()
  "Employ actions of STAR- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-star))

(defun ar-slashparen-backslashparen-in-buffer-atpt ()
  "Employ actions of SLASHPAREN- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-slashparen))

(defun ar-sort-backslashparen-in-buffer-atpt ()
  "Employ actions of SORT- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-sort))

(defun ar-trim-backslashparen-in-buffer-atpt ()
  "Employ actions of TRIM- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-trim))

(defun ar-trim-left-backslashparen-in-buffer-atpt ()
  "Employ actions of TRIM-LEFT- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-trim-left))

(defun ar-trim-right-backslashparen-in-buffer-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-trim-right))

(defun ar-underscore-backslashparen-in-buffer-atpt ()
  "Employ actions of UNDERSCORE- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-underscore))

(defun ar-whitespace-backslashparen-in-buffer-atpt ()
  "Employ actions of WHITESPACE- at things class of BACKSLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'buffer 'ar-th-whitespace))

(defun ar-backslashparen-in-page-atpt ()
  "Employ actions of  at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th))

(defun ar-greaterangle-backslashparen-in-page-atpt ()
  "Employ actions of GREATERANGLE- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-greaterangle))

(defun ar-lesserangle-backslashparen-in-page-atpt ()
  "Employ actions of LESSERANGLE- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-lesserangle))

(defun ar-backslash-backslashparen-in-page-atpt ()
  "Employ actions of BACKSLASH- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-backslash))

(defun ar-colon-backslashparen-in-page-atpt ()
  "Employ actions of COLON- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-colon))

(defun ar-beg-backslashparen-in-page-atpt ()
  "Employ actions of BEG- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-beg))

(defun ar-blok-backslashparen-in-page-atpt ()
  "Employ actions of BLOK- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-blok))

(defun ar-bounds-backslashparen-in-page-atpt ()
  "Employ actions of BOUNDS- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-bounds))

(defun ar-brace-backslashparen-in-page-atpt ()
  "Employ actions of BRACE- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-brace))

(defun ar-bracket-backslashparen-in-page-atpt ()
  "Employ actions of BRACKET- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-bracket))

(defun ar-commatize-backslashparen-in-page-atpt ()
  "Employ actions of COMMATIZE- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-commatize))

(defun ar-comment-backslashparen-in-page-atpt ()
  "Employ actions of COMMENT- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-comment))

(defun ar-dollar-backslashparen-in-page-atpt ()
  "Employ actions of DOLLAR- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-dollar))

(defun ar-doublebackslash-backslashparen-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-doublebackslash))

(defun ar-doublebacktick-backslashparen-in-page-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-doublebacktick))

(defun ar-doublequote-backslashparen-in-page-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-doublequote))

(defun ar-doubleslash-backslashparen-in-page-atpt ()
  "Employ actions of DOUBLESLASH- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-doubleslash))

(defun ar-doublebackslashparen-backslashparen-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-doublebackslashparen))

(defun ar-end-backslashparen-in-page-atpt ()
  "Employ actions of END- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-end))

(defun ar-escape-backslashparen-in-page-atpt ()
  "Employ actions of ESCAPE- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-escape))

(defun ar-hide-backslashparen-in-page-atpt ()
  "Employ actions of HIDE- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-hide))

(defun ar-hide-show-backslashparen-in-page-atpt ()
  "Employ actions of HIDE-SHOW- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-hide-show))

(defun ar-hyphen-backslashparen-in-page-atpt ()
  "Employ actions of HYPHEN- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-hyphen))

(defun ar-kill-backslashparen-in-page-atpt ()
  "Employ actions of KILL- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-kill))

(defun ar-curvedsinglequote-backslashparen-in-page-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-curvedsinglequote))

(defun ar-length-backslashparen-in-page-atpt ()
  "Employ actions of LENGTH- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-length))

(defun ar-parentize-backslashparen-in-page-atpt ()
  "Employ actions of PARENTIZE- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-parentize))

(defun ar-quote-backslashparen-in-page-atpt ()
  "Employ actions of QUOTE- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-quote))

(defun ar-separate-backslashparen-in-page-atpt ()
  "Employ actions of SEPARATE- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-separate))

(defun ar-show-backslashparen-in-page-atpt ()
  "Employ actions of SHOW- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-show))

(defun ar-singlequote-backslashparen-in-page-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-singlequote))

(defun ar-slash-backslashparen-in-page-atpt ()
  "Employ actions of SLASH- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-slash))

(defun ar-star-backslashparen-in-page-atpt ()
  "Employ actions of STAR- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-star))

(defun ar-slashparen-backslashparen-in-page-atpt ()
  "Employ actions of SLASHPAREN- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-slashparen))

(defun ar-sort-backslashparen-in-page-atpt ()
  "Employ actions of SORT- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-sort))

(defun ar-trim-backslashparen-in-page-atpt ()
  "Employ actions of TRIM- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-trim))

(defun ar-trim-left-backslashparen-in-page-atpt ()
  "Employ actions of TRIM-LEFT- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-trim-left))

(defun ar-trim-right-backslashparen-in-page-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-trim-right))

(defun ar-underscore-backslashparen-in-page-atpt ()
  "Employ actions of UNDERSCORE- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-underscore))

(defun ar-whitespace-backslashparen-in-page-atpt ()
  "Employ actions of WHITESPACE- at things class of BACKSLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'page 'ar-th-whitespace))

(defun ar-backslashparen-in-paragraph-atpt ()
  "Employ actions of  at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th))

(defun ar-greaterangle-backslashparen-in-paragraph-atpt ()
  "Employ actions of GREATERANGLE- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-greaterangle))

(defun ar-lesserangle-backslashparen-in-paragraph-atpt ()
  "Employ actions of LESSERANGLE- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-lesserangle))

(defun ar-backslash-backslashparen-in-paragraph-atpt ()
  "Employ actions of BACKSLASH- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-backslash))

(defun ar-colon-backslashparen-in-paragraph-atpt ()
  "Employ actions of COLON- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-colon))

(defun ar-beg-backslashparen-in-paragraph-atpt ()
  "Employ actions of BEG- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-beg))

(defun ar-blok-backslashparen-in-paragraph-atpt ()
  "Employ actions of BLOK- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-blok))

(defun ar-bounds-backslashparen-in-paragraph-atpt ()
  "Employ actions of BOUNDS- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-bounds))

(defun ar-brace-backslashparen-in-paragraph-atpt ()
  "Employ actions of BRACE- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-brace))

(defun ar-bracket-backslashparen-in-paragraph-atpt ()
  "Employ actions of BRACKET- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-bracket))

(defun ar-commatize-backslashparen-in-paragraph-atpt ()
  "Employ actions of COMMATIZE- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-commatize))

(defun ar-comment-backslashparen-in-paragraph-atpt ()
  "Employ actions of COMMENT- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-comment))

(defun ar-dollar-backslashparen-in-paragraph-atpt ()
  "Employ actions of DOLLAR- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-dollar))

(defun ar-doublebackslash-backslashparen-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-doublebackslash))

(defun ar-doublebacktick-backslashparen-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-doublebacktick))

(defun ar-doublequote-backslashparen-in-paragraph-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-doublequote))

(defun ar-doubleslash-backslashparen-in-paragraph-atpt ()
  "Employ actions of DOUBLESLASH- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-doubleslash))

(defun ar-doublebackslashparen-backslashparen-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-doublebackslashparen))

(defun ar-end-backslashparen-in-paragraph-atpt ()
  "Employ actions of END- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-end))

(defun ar-escape-backslashparen-in-paragraph-atpt ()
  "Employ actions of ESCAPE- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-escape))

(defun ar-hide-backslashparen-in-paragraph-atpt ()
  "Employ actions of HIDE- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-hide))

(defun ar-hide-show-backslashparen-in-paragraph-atpt ()
  "Employ actions of HIDE-SHOW- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-hide-show))

(defun ar-hyphen-backslashparen-in-paragraph-atpt ()
  "Employ actions of HYPHEN- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-hyphen))

(defun ar-kill-backslashparen-in-paragraph-atpt ()
  "Employ actions of KILL- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-kill))

(defun ar-curvedsinglequote-backslashparen-in-paragraph-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-curvedsinglequote))

(defun ar-length-backslashparen-in-paragraph-atpt ()
  "Employ actions of LENGTH- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-length))

(defun ar-parentize-backslashparen-in-paragraph-atpt ()
  "Employ actions of PARENTIZE- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-parentize))

(defun ar-quote-backslashparen-in-paragraph-atpt ()
  "Employ actions of QUOTE- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-quote))

(defun ar-separate-backslashparen-in-paragraph-atpt ()
  "Employ actions of SEPARATE- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-separate))

(defun ar-show-backslashparen-in-paragraph-atpt ()
  "Employ actions of SHOW- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-show))

(defun ar-singlequote-backslashparen-in-paragraph-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-singlequote))

(defun ar-slash-backslashparen-in-paragraph-atpt ()
  "Employ actions of SLASH- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-slash))

(defun ar-star-backslashparen-in-paragraph-atpt ()
  "Employ actions of STAR- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-star))

(defun ar-slashparen-backslashparen-in-paragraph-atpt ()
  "Employ actions of SLASHPAREN- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-slashparen))

(defun ar-sort-backslashparen-in-paragraph-atpt ()
  "Employ actions of SORT- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-sort))

(defun ar-trim-backslashparen-in-paragraph-atpt ()
  "Employ actions of TRIM- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-trim))

(defun ar-trim-left-backslashparen-in-paragraph-atpt ()
  "Employ actions of TRIM-LEFT- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-trim-left))

(defun ar-trim-right-backslashparen-in-paragraph-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-trim-right))

(defun ar-underscore-backslashparen-in-paragraph-atpt ()
  "Employ actions of UNDERSCORE- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-underscore))

(defun ar-whitespace-backslashparen-in-paragraph-atpt ()
  "Employ actions of WHITESPACE- at things class of BACKSLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'paragraph 'ar-th-whitespace))

(defun ar-backslashparen-in-region-atpt ()
  "Employ actions of  at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th))

(defun ar-greaterangle-backslashparen-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-greaterangle))

(defun ar-lesserangle-backslashparen-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-lesserangle))

(defun ar-backslash-backslashparen-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-backslash))

(defun ar-colon-backslashparen-in-region-atpt ()
  "Employ actions of COLON- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-colon))

(defun ar-beg-backslashparen-in-region-atpt ()
  "Employ actions of BEG- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-beg))

(defun ar-blok-backslashparen-in-region-atpt ()
  "Employ actions of BLOK- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-blok))

(defun ar-bounds-backslashparen-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-bounds))

(defun ar-brace-backslashparen-in-region-atpt ()
  "Employ actions of BRACE- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-brace))

(defun ar-bracket-backslashparen-in-region-atpt ()
  "Employ actions of BRACKET- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-bracket))

(defun ar-commatize-backslashparen-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-commatize))

(defun ar-comment-backslashparen-in-region-atpt ()
  "Employ actions of COMMENT- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-comment))

(defun ar-dollar-backslashparen-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-dollar))

(defun ar-doublebackslash-backslashparen-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-backslashparen-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-doublebacktick))

(defun ar-doublequote-backslashparen-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-doublequote))

(defun ar-doubleslash-backslashparen-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-backslashparen-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-doublebackslashparen))

(defun ar-end-backslashparen-in-region-atpt ()
  "Employ actions of END- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-end))

(defun ar-escape-backslashparen-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-escape))

(defun ar-hide-backslashparen-in-region-atpt ()
  "Employ actions of HIDE- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-hide))

(defun ar-hide-show-backslashparen-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-hide-show))

(defun ar-hyphen-backslashparen-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-hyphen))

(defun ar-kill-backslashparen-in-region-atpt ()
  "Employ actions of KILL- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-kill))

(defun ar-curvedsinglequote-backslashparen-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-curvedsinglequote))

(defun ar-length-backslashparen-in-region-atpt ()
  "Employ actions of LENGTH- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-length))

(defun ar-parentize-backslashparen-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-parentize))

(defun ar-quote-backslashparen-in-region-atpt ()
  "Employ actions of QUOTE- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-quote))

(defun ar-separate-backslashparen-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-separate))

(defun ar-show-backslashparen-in-region-atpt ()
  "Employ actions of SHOW- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-show))

(defun ar-singlequote-backslashparen-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-singlequote))

(defun ar-slash-backslashparen-in-region-atpt ()
  "Employ actions of SLASH- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-slash))

(defun ar-star-backslashparen-in-region-atpt ()
  "Employ actions of STAR- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-star))

(defun ar-slashparen-backslashparen-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-slashparen))

(defun ar-sort-backslashparen-in-region-atpt ()
  "Employ actions of SORT- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-sort))

(defun ar-trim-backslashparen-in-region-atpt ()
  "Employ actions of TRIM- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-trim))

(defun ar-trim-left-backslashparen-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-trim-left))

(defun ar-trim-right-backslashparen-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-trim-right))

(defun ar-underscore-backslashparen-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-underscore))

(defun ar-whitespace-backslashparen-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of BACKSLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'backslashparen 'region 'ar-th-whitespace))

(defun ar-slashparen-in-buffer-atpt ()
  "Employ actions of  at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th))

(defun ar-greaterangle-slashparen-in-buffer-atpt ()
  "Employ actions of GREATERANGLE- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-greaterangle))

(defun ar-lesserangle-slashparen-in-buffer-atpt ()
  "Employ actions of LESSERANGLE- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-lesserangle))

(defun ar-backslash-slashparen-in-buffer-atpt ()
  "Employ actions of BACKSLASH- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-backslash))

(defun ar-colon-slashparen-in-buffer-atpt ()
  "Employ actions of COLON- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-colon))

(defun ar-beg-slashparen-in-buffer-atpt ()
  "Employ actions of BEG- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-beg))

(defun ar-blok-slashparen-in-buffer-atpt ()
  "Employ actions of BLOK- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-blok))

(defun ar-bounds-slashparen-in-buffer-atpt ()
  "Employ actions of BOUNDS- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-bounds))

(defun ar-brace-slashparen-in-buffer-atpt ()
  "Employ actions of BRACE- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-brace))

(defun ar-bracket-slashparen-in-buffer-atpt ()
  "Employ actions of BRACKET- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-bracket))

(defun ar-commatize-slashparen-in-buffer-atpt ()
  "Employ actions of COMMATIZE- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-commatize))

(defun ar-comment-slashparen-in-buffer-atpt ()
  "Employ actions of COMMENT- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-comment))

(defun ar-dollar-slashparen-in-buffer-atpt ()
  "Employ actions of DOLLAR- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-dollar))

(defun ar-doublebackslash-slashparen-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-doublebackslash))

(defun ar-doublebacktick-slashparen-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-doublebacktick))

(defun ar-doublequote-slashparen-in-buffer-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-doublequote))

(defun ar-doubleslash-slashparen-in-buffer-atpt ()
  "Employ actions of DOUBLESLASH- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-doubleslash))

(defun ar-doublebackslashparen-slashparen-in-buffer-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-doublebackslashparen))

(defun ar-end-slashparen-in-buffer-atpt ()
  "Employ actions of END- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-end))

(defun ar-escape-slashparen-in-buffer-atpt ()
  "Employ actions of ESCAPE- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-escape))

(defun ar-hide-slashparen-in-buffer-atpt ()
  "Employ actions of HIDE- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-hide))

(defun ar-hide-show-slashparen-in-buffer-atpt ()
  "Employ actions of HIDE-SHOW- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-hide-show))

(defun ar-hyphen-slashparen-in-buffer-atpt ()
  "Employ actions of HYPHEN- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-hyphen))

(defun ar-kill-slashparen-in-buffer-atpt ()
  "Employ actions of KILL- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-kill))

(defun ar-curvedsinglequote-slashparen-in-buffer-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-curvedsinglequote))

(defun ar-length-slashparen-in-buffer-atpt ()
  "Employ actions of LENGTH- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-length))

(defun ar-parentize-slashparen-in-buffer-atpt ()
  "Employ actions of PARENTIZE- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-parentize))

(defun ar-quote-slashparen-in-buffer-atpt ()
  "Employ actions of QUOTE- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-quote))

(defun ar-separate-slashparen-in-buffer-atpt ()
  "Employ actions of SEPARATE- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-separate))

(defun ar-show-slashparen-in-buffer-atpt ()
  "Employ actions of SHOW- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-show))

(defun ar-singlequote-slashparen-in-buffer-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-singlequote))

(defun ar-slash-slashparen-in-buffer-atpt ()
  "Employ actions of SLASH- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-slash))

(defun ar-star-slashparen-in-buffer-atpt ()
  "Employ actions of STAR- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-star))

(defun ar-slashparen-slashparen-in-buffer-atpt ()
  "Employ actions of SLASHPAREN- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-slashparen))

(defun ar-sort-slashparen-in-buffer-atpt ()
  "Employ actions of SORT- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-sort))

(defun ar-trim-slashparen-in-buffer-atpt ()
  "Employ actions of TRIM- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-trim))

(defun ar-trim-left-slashparen-in-buffer-atpt ()
  "Employ actions of TRIM-LEFT- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-trim-left))

(defun ar-trim-right-slashparen-in-buffer-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-trim-right))

(defun ar-underscore-slashparen-in-buffer-atpt ()
  "Employ actions of UNDERSCORE- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-underscore))

(defun ar-whitespace-slashparen-in-buffer-atpt ()
  "Employ actions of WHITESPACE- at things class of SLASHPAREN residing withing BUFFER. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'buffer 'ar-th-whitespace))

(defun ar-slashparen-in-page-atpt ()
  "Employ actions of  at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th))

(defun ar-greaterangle-slashparen-in-page-atpt ()
  "Employ actions of GREATERANGLE- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-greaterangle))

(defun ar-lesserangle-slashparen-in-page-atpt ()
  "Employ actions of LESSERANGLE- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-lesserangle))

(defun ar-backslash-slashparen-in-page-atpt ()
  "Employ actions of BACKSLASH- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-backslash))

(defun ar-colon-slashparen-in-page-atpt ()
  "Employ actions of COLON- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-colon))

(defun ar-beg-slashparen-in-page-atpt ()
  "Employ actions of BEG- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-beg))

(defun ar-blok-slashparen-in-page-atpt ()
  "Employ actions of BLOK- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-blok))

(defun ar-bounds-slashparen-in-page-atpt ()
  "Employ actions of BOUNDS- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-bounds))

(defun ar-brace-slashparen-in-page-atpt ()
  "Employ actions of BRACE- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-brace))

(defun ar-bracket-slashparen-in-page-atpt ()
  "Employ actions of BRACKET- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-bracket))

(defun ar-commatize-slashparen-in-page-atpt ()
  "Employ actions of COMMATIZE- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-commatize))

(defun ar-comment-slashparen-in-page-atpt ()
  "Employ actions of COMMENT- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-comment))

(defun ar-dollar-slashparen-in-page-atpt ()
  "Employ actions of DOLLAR- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-dollar))

(defun ar-doublebackslash-slashparen-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-doublebackslash))

(defun ar-doublebacktick-slashparen-in-page-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-doublebacktick))

(defun ar-doublequote-slashparen-in-page-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-doublequote))

(defun ar-doubleslash-slashparen-in-page-atpt ()
  "Employ actions of DOUBLESLASH- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-doubleslash))

(defun ar-doublebackslashparen-slashparen-in-page-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-doublebackslashparen))

(defun ar-end-slashparen-in-page-atpt ()
  "Employ actions of END- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-end))

(defun ar-escape-slashparen-in-page-atpt ()
  "Employ actions of ESCAPE- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-escape))

(defun ar-hide-slashparen-in-page-atpt ()
  "Employ actions of HIDE- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-hide))

(defun ar-hide-show-slashparen-in-page-atpt ()
  "Employ actions of HIDE-SHOW- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-hide-show))

(defun ar-hyphen-slashparen-in-page-atpt ()
  "Employ actions of HYPHEN- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-hyphen))

(defun ar-kill-slashparen-in-page-atpt ()
  "Employ actions of KILL- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-kill))

(defun ar-curvedsinglequote-slashparen-in-page-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-curvedsinglequote))

(defun ar-length-slashparen-in-page-atpt ()
  "Employ actions of LENGTH- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-length))

(defun ar-parentize-slashparen-in-page-atpt ()
  "Employ actions of PARENTIZE- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-parentize))

(defun ar-quote-slashparen-in-page-atpt ()
  "Employ actions of QUOTE- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-quote))

(defun ar-separate-slashparen-in-page-atpt ()
  "Employ actions of SEPARATE- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-separate))

(defun ar-show-slashparen-in-page-atpt ()
  "Employ actions of SHOW- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-show))

(defun ar-singlequote-slashparen-in-page-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-singlequote))

(defun ar-slash-slashparen-in-page-atpt ()
  "Employ actions of SLASH- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-slash))

(defun ar-star-slashparen-in-page-atpt ()
  "Employ actions of STAR- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-star))

(defun ar-slashparen-slashparen-in-page-atpt ()
  "Employ actions of SLASHPAREN- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-slashparen))

(defun ar-sort-slashparen-in-page-atpt ()
  "Employ actions of SORT- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-sort))

(defun ar-trim-slashparen-in-page-atpt ()
  "Employ actions of TRIM- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-trim))

(defun ar-trim-left-slashparen-in-page-atpt ()
  "Employ actions of TRIM-LEFT- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-trim-left))

(defun ar-trim-right-slashparen-in-page-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-trim-right))

(defun ar-underscore-slashparen-in-page-atpt ()
  "Employ actions of UNDERSCORE- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-underscore))

(defun ar-whitespace-slashparen-in-page-atpt ()
  "Employ actions of WHITESPACE- at things class of SLASHPAREN residing withing PAGE. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'page 'ar-th-whitespace))

(defun ar-slashparen-in-paragraph-atpt ()
  "Employ actions of  at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th))

(defun ar-greaterangle-slashparen-in-paragraph-atpt ()
  "Employ actions of GREATERANGLE- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-greaterangle))

(defun ar-lesserangle-slashparen-in-paragraph-atpt ()
  "Employ actions of LESSERANGLE- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-lesserangle))

(defun ar-backslash-slashparen-in-paragraph-atpt ()
  "Employ actions of BACKSLASH- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-backslash))

(defun ar-colon-slashparen-in-paragraph-atpt ()
  "Employ actions of COLON- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-colon))

(defun ar-beg-slashparen-in-paragraph-atpt ()
  "Employ actions of BEG- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-beg))

(defun ar-blok-slashparen-in-paragraph-atpt ()
  "Employ actions of BLOK- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-blok))

(defun ar-bounds-slashparen-in-paragraph-atpt ()
  "Employ actions of BOUNDS- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-bounds))

(defun ar-brace-slashparen-in-paragraph-atpt ()
  "Employ actions of BRACE- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-brace))

(defun ar-bracket-slashparen-in-paragraph-atpt ()
  "Employ actions of BRACKET- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-bracket))

(defun ar-commatize-slashparen-in-paragraph-atpt ()
  "Employ actions of COMMATIZE- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-commatize))

(defun ar-comment-slashparen-in-paragraph-atpt ()
  "Employ actions of COMMENT- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-comment))

(defun ar-dollar-slashparen-in-paragraph-atpt ()
  "Employ actions of DOLLAR- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-dollar))

(defun ar-doublebackslash-slashparen-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-doublebackslash))

(defun ar-doublebacktick-slashparen-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-doublebacktick))

(defun ar-doublequote-slashparen-in-paragraph-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-doublequote))

(defun ar-doubleslash-slashparen-in-paragraph-atpt ()
  "Employ actions of DOUBLESLASH- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-doubleslash))

(defun ar-doublebackslashparen-slashparen-in-paragraph-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-doublebackslashparen))

(defun ar-end-slashparen-in-paragraph-atpt ()
  "Employ actions of END- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-end))

(defun ar-escape-slashparen-in-paragraph-atpt ()
  "Employ actions of ESCAPE- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-escape))

(defun ar-hide-slashparen-in-paragraph-atpt ()
  "Employ actions of HIDE- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-hide))

(defun ar-hide-show-slashparen-in-paragraph-atpt ()
  "Employ actions of HIDE-SHOW- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-hide-show))

(defun ar-hyphen-slashparen-in-paragraph-atpt ()
  "Employ actions of HYPHEN- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-hyphen))

(defun ar-kill-slashparen-in-paragraph-atpt ()
  "Employ actions of KILL- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-kill))

(defun ar-curvedsinglequote-slashparen-in-paragraph-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-curvedsinglequote))

(defun ar-length-slashparen-in-paragraph-atpt ()
  "Employ actions of LENGTH- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-length))

(defun ar-parentize-slashparen-in-paragraph-atpt ()
  "Employ actions of PARENTIZE- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-parentize))

(defun ar-quote-slashparen-in-paragraph-atpt ()
  "Employ actions of QUOTE- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-quote))

(defun ar-separate-slashparen-in-paragraph-atpt ()
  "Employ actions of SEPARATE- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-separate))

(defun ar-show-slashparen-in-paragraph-atpt ()
  "Employ actions of SHOW- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-show))

(defun ar-singlequote-slashparen-in-paragraph-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-singlequote))

(defun ar-slash-slashparen-in-paragraph-atpt ()
  "Employ actions of SLASH- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-slash))

(defun ar-star-slashparen-in-paragraph-atpt ()
  "Employ actions of STAR- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-star))

(defun ar-slashparen-slashparen-in-paragraph-atpt ()
  "Employ actions of SLASHPAREN- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-slashparen))

(defun ar-sort-slashparen-in-paragraph-atpt ()
  "Employ actions of SORT- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-sort))

(defun ar-trim-slashparen-in-paragraph-atpt ()
  "Employ actions of TRIM- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-trim))

(defun ar-trim-left-slashparen-in-paragraph-atpt ()
  "Employ actions of TRIM-LEFT- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-trim-left))

(defun ar-trim-right-slashparen-in-paragraph-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-trim-right))

(defun ar-underscore-slashparen-in-paragraph-atpt ()
  "Employ actions of UNDERSCORE- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-underscore))

(defun ar-whitespace-slashparen-in-paragraph-atpt ()
  "Employ actions of WHITESPACE- at things class of SLASHPAREN residing withing PARAGRAPH. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'paragraph 'ar-th-whitespace))

(defun ar-slashparen-in-region-atpt ()
  "Employ actions of  at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th))

(defun ar-greaterangle-slashparen-in-region-atpt ()
  "Employ actions of GREATERANGLE- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-greaterangle))

(defun ar-lesserangle-slashparen-in-region-atpt ()
  "Employ actions of LESSERANGLE- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-lesserangle))

(defun ar-backslash-slashparen-in-region-atpt ()
  "Employ actions of BACKSLASH- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-backslash))

(defun ar-colon-slashparen-in-region-atpt ()
  "Employ actions of COLON- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-colon))

(defun ar-beg-slashparen-in-region-atpt ()
  "Employ actions of BEG- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-beg))

(defun ar-blok-slashparen-in-region-atpt ()
  "Employ actions of BLOK- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-blok))

(defun ar-bounds-slashparen-in-region-atpt ()
  "Employ actions of BOUNDS- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-bounds))

(defun ar-brace-slashparen-in-region-atpt ()
  "Employ actions of BRACE- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-brace))

(defun ar-bracket-slashparen-in-region-atpt ()
  "Employ actions of BRACKET- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-bracket))

(defun ar-commatize-slashparen-in-region-atpt ()
  "Employ actions of COMMATIZE- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-commatize))

(defun ar-comment-slashparen-in-region-atpt ()
  "Employ actions of COMMENT- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-comment))

(defun ar-dollar-slashparen-in-region-atpt ()
  "Employ actions of DOLLAR- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-dollar))

(defun ar-doublebackslash-slashparen-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASH- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-doublebackslash))

(defun ar-doublebacktick-slashparen-in-region-atpt ()
  "Employ actions of DOUBLEBACKTICK- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-doublebacktick))

(defun ar-doublequote-slashparen-in-region-atpt ()
  "Employ actions of DOUBLEQUOTE- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-doublequote))

(defun ar-doubleslash-slashparen-in-region-atpt ()
  "Employ actions of DOUBLESLASH- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-doubleslash))

(defun ar-doublebackslashparen-slashparen-in-region-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-doublebackslashparen))

(defun ar-end-slashparen-in-region-atpt ()
  "Employ actions of END- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-end))

(defun ar-escape-slashparen-in-region-atpt ()
  "Employ actions of ESCAPE- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-escape))

(defun ar-hide-slashparen-in-region-atpt ()
  "Employ actions of HIDE- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-hide))

(defun ar-hide-show-slashparen-in-region-atpt ()
  "Employ actions of HIDE-SHOW- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-hide-show))

(defun ar-hyphen-slashparen-in-region-atpt ()
  "Employ actions of HYPHEN- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-hyphen))

(defun ar-kill-slashparen-in-region-atpt ()
  "Employ actions of KILL- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-kill))

(defun ar-curvedsinglequote-slashparen-in-region-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-curvedsinglequote))

(defun ar-length-slashparen-in-region-atpt ()
  "Employ actions of LENGTH- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-length))

(defun ar-parentize-slashparen-in-region-atpt ()
  "Employ actions of PARENTIZE- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-parentize))

(defun ar-quote-slashparen-in-region-atpt ()
  "Employ actions of QUOTE- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-quote))

(defun ar-separate-slashparen-in-region-atpt ()
  "Employ actions of SEPARATE- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-separate))

(defun ar-show-slashparen-in-region-atpt ()
  "Employ actions of SHOW- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-show))

(defun ar-singlequote-slashparen-in-region-atpt ()
  "Employ actions of SINGLEQUOTE- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-singlequote))

(defun ar-slash-slashparen-in-region-atpt ()
  "Employ actions of SLASH- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-slash))

(defun ar-star-slashparen-in-region-atpt ()
  "Employ actions of STAR- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-star))

(defun ar-slashparen-slashparen-in-region-atpt ()
  "Employ actions of SLASHPAREN- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-slashparen))

(defun ar-sort-slashparen-in-region-atpt ()
  "Employ actions of SORT- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-sort))

(defun ar-trim-slashparen-in-region-atpt ()
  "Employ actions of TRIM- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-trim))

(defun ar-trim-left-slashparen-in-region-atpt ()
  "Employ actions of TRIM-LEFT- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-trim-left))

(defun ar-trim-right-slashparen-in-region-atpt ()
  "Employ actions of TRIM-RIGHT- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-trim-right))

(defun ar-underscore-slashparen-in-region-atpt ()
  "Employ actions of UNDERSCORE- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-underscore))

(defun ar-whitespace-slashparen-in-region-atpt ()
  "Employ actions of WHITESPACE- at things class of SLASHPAREN residing withing REGION. "
  (interactive "*")
  (ar-thing-in-thing 'slashparen 'region 'ar-th-whitespace))

(provide 'ar-thingatpt-data-forms-aktiv-in-major-forms-restricted-list)
;;;thing-data-forms-aktiv-in-major-forms-restricted-list.el ends here

