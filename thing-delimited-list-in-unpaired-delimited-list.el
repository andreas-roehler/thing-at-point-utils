;;; thing-delimited-list-in-unpaired-delimited-list.el --- thing-in-thing functions
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

;;;###autoload 
(defun ar-braced-in-backslashed-atpt ()
  "Employ actions of  at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-braced-in-backslashed-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-braced-in-backslashed-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-braced-in-backslashed-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-braced-in-backslashed-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-braced-in-backslashed-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-braced-in-backslashed-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-braced-in-backslashed-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-braced-in-backslashed-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-braced-in-backslashed-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-braced-in-backslashed-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-braced-in-backslashed-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-braced-in-backslashed-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-braced-in-backslashed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-braced-in-backslashed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-braced-in-backslashed-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-braced-in-backslashed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-braced-in-backslashed-atpt ()
  "Employ actions of END at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-braced-in-backslashed-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-braced-in-backslashed-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-braced-in-backslashed-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-braced-in-backslashed-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-braced-in-backslashed-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-braced-in-backslashed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-braced-in-backslashed-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-braced-in-backslashed-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-braced-in-backslashed-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-braced-in-backslashed-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-braced-in-backslashed-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-braced-in-backslashed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-braced-in-backslashed-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-braced-in-backslashed-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-braced-in-backslashed-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-braced-in-backslashed-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-braced-in-backslashed-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-braced-in-backslashed-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-braced-in-backslashed-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-braced-in-backslashed-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backslashed 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-braced-in-backticked-atpt ()
  "Employ actions of  at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-braced-in-backticked-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-braced-in-backticked-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-braced-in-backticked-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-braced-in-backticked-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-braced-in-backticked-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-braced-in-backticked-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-braced-in-backticked-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-braced-in-backticked-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-braced-in-backticked-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-braced-in-backticked-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-braced-in-backticked-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-braced-in-backticked-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-braced-in-backticked-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-braced-in-backticked-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-braced-in-backticked-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-braced-in-backticked-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-braced-in-backticked-atpt ()
  "Employ actions of END at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-braced-in-backticked-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-braced-in-backticked-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-braced-in-backticked-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-braced-in-backticked-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-braced-in-backticked-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-braced-in-backticked-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-braced-in-backticked-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-braced-in-backticked-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-braced-in-backticked-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-braced-in-backticked-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-braced-in-backticked-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-braced-in-backticked-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-braced-in-backticked-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-braced-in-backticked-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-braced-in-backticked-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-braced-in-backticked-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-braced-in-backticked-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-braced-in-backticked-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-braced-in-backticked-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-braced-in-backticked-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'backticked 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-braced-in-dollared-atpt ()
  "Employ actions of  at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-braced-in-dollared-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-braced-in-dollared-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-braced-in-dollared-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-braced-in-dollared-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-braced-in-dollared-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-braced-in-dollared-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-braced-in-dollared-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-braced-in-dollared-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-braced-in-dollared-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-braced-in-dollared-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-braced-in-dollared-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-braced-in-dollared-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-braced-in-dollared-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-braced-in-dollared-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-braced-in-dollared-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-braced-in-dollared-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-braced-in-dollared-atpt ()
  "Employ actions of END at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-braced-in-dollared-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-braced-in-dollared-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-braced-in-dollared-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-braced-in-dollared-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-braced-in-dollared-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-braced-in-dollared-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-braced-in-dollared-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-braced-in-dollared-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-braced-in-dollared-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-braced-in-dollared-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-braced-in-dollared-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-braced-in-dollared-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-braced-in-dollared-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-braced-in-dollared-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-braced-in-dollared-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-braced-in-dollared-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-braced-in-dollared-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-braced-in-dollared-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-braced-in-dollared-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-braced-in-dollared-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'dollared 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-braced-in-doublequoted-atpt ()
  "Employ actions of  at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-braced-in-doublequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-braced-in-doublequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-braced-in-doublequoted-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-braced-in-doublequoted-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-braced-in-doublequoted-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-braced-in-doublequoted-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-braced-in-doublequoted-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-braced-in-doublequoted-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-braced-in-doublequoted-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-braced-in-doublequoted-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-braced-in-doublequoted-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-braced-in-doublequoted-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-braced-in-doublequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-braced-in-doublequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-braced-in-doublequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-braced-in-doublequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-braced-in-doublequoted-atpt ()
  "Employ actions of END at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-braced-in-doublequoted-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-braced-in-doublequoted-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-braced-in-doublequoted-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-braced-in-doublequoted-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-braced-in-doublequoted-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-braced-in-doublequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-braced-in-doublequoted-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-braced-in-doublequoted-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-braced-in-doublequoted-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-braced-in-doublequoted-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-braced-in-doublequoted-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-braced-in-doublequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-braced-in-doublequoted-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-braced-in-doublequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-braced-in-doublequoted-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-braced-in-doublequoted-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-braced-in-doublequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-braced-in-doublequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-braced-in-doublequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-braced-in-doublequoted-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doublequoted 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-braced-in-equalized-atpt ()
  "Employ actions of  at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-braced-in-equalized-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-braced-in-equalized-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-braced-in-equalized-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-braced-in-equalized-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-braced-in-equalized-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-braced-in-equalized-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-braced-in-equalized-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-braced-in-equalized-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-braced-in-equalized-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-braced-in-equalized-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-braced-in-equalized-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-braced-in-equalized-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-braced-in-equalized-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-braced-in-equalized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-braced-in-equalized-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-braced-in-equalized-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-braced-in-equalized-atpt ()
  "Employ actions of END at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-braced-in-equalized-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-braced-in-equalized-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-braced-in-equalized-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-braced-in-equalized-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-braced-in-equalized-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-braced-in-equalized-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-braced-in-equalized-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-braced-in-equalized-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-braced-in-equalized-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-braced-in-equalized-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-braced-in-equalized-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-braced-in-equalized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-braced-in-equalized-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-braced-in-equalized-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-braced-in-equalized-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-braced-in-equalized-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-braced-in-equalized-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-braced-in-equalized-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-braced-in-equalized-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-braced-in-equalized-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'equalized 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-braced-in-hyphened-atpt ()
  "Employ actions of  at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-braced-in-hyphened-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-braced-in-hyphened-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-braced-in-hyphened-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-braced-in-hyphened-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-braced-in-hyphened-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-braced-in-hyphened-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-braced-in-hyphened-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-braced-in-hyphened-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-braced-in-hyphened-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-braced-in-hyphened-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-braced-in-hyphened-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-braced-in-hyphened-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-braced-in-hyphened-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-braced-in-hyphened-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-braced-in-hyphened-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-braced-in-hyphened-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-braced-in-hyphened-atpt ()
  "Employ actions of END at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-braced-in-hyphened-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-braced-in-hyphened-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-braced-in-hyphened-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-braced-in-hyphened-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-braced-in-hyphened-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-braced-in-hyphened-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-braced-in-hyphened-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-braced-in-hyphened-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-braced-in-hyphened-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-braced-in-hyphened-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-braced-in-hyphened-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-braced-in-hyphened-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-braced-in-hyphened-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-braced-in-hyphened-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-braced-in-hyphened-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-braced-in-hyphened-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-braced-in-hyphened-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-braced-in-hyphened-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-braced-in-hyphened-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-braced-in-hyphened-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'hyphened 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-braced-in-singlequoted-atpt ()
  "Employ actions of  at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-braced-in-singlequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-braced-in-singlequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-braced-in-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-braced-in-singlequoted-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-braced-in-singlequoted-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-braced-in-singlequoted-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-braced-in-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-braced-in-singlequoted-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-braced-in-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-braced-in-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-braced-in-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-braced-in-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-braced-in-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-braced-in-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-braced-in-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-braced-in-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-braced-in-singlequoted-atpt ()
  "Employ actions of END at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-braced-in-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-braced-in-singlequoted-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-braced-in-singlequoted-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-braced-in-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-braced-in-singlequoted-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-braced-in-singlequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-braced-in-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-braced-in-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-braced-in-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-braced-in-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-braced-in-singlequoted-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-braced-in-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-braced-in-singlequoted-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-braced-in-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-braced-in-singlequoted-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-braced-in-singlequoted-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-braced-in-singlequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-braced-in-singlequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-braced-in-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-braced-in-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'singlequoted 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-braced-in-slashed-atpt ()
  "Employ actions of  at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-braced-in-slashed-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-braced-in-slashed-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-braced-in-slashed-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-braced-in-slashed-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-braced-in-slashed-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-braced-in-slashed-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-braced-in-slashed-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-braced-in-slashed-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-braced-in-slashed-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-braced-in-slashed-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-braced-in-slashed-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-braced-in-slashed-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-braced-in-slashed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-braced-in-slashed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-braced-in-slashed-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-braced-in-slashed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-braced-in-slashed-atpt ()
  "Employ actions of END at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-braced-in-slashed-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-braced-in-slashed-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-braced-in-slashed-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-braced-in-slashed-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-braced-in-slashed-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-braced-in-slashed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-braced-in-slashed-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-braced-in-slashed-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-braced-in-slashed-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-braced-in-slashed-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-braced-in-slashed-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-braced-in-slashed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-braced-in-slashed-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-braced-in-slashed-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-braced-in-slashed-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-braced-in-slashed-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-braced-in-slashed-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-braced-in-slashed-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-braced-in-slashed-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-braced-in-slashed-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slashed 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-braced-in-underscored-atpt ()
  "Employ actions of  at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-braced-in-underscored-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-braced-in-underscored-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-braced-in-underscored-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-braced-in-underscored-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-braced-in-underscored-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-braced-in-underscored-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-braced-in-underscored-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-braced-in-underscored-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-braced-in-underscored-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-braced-in-underscored-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-braced-in-underscored-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-braced-in-underscored-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-braced-in-underscored-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-braced-in-underscored-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-braced-in-underscored-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-braced-in-underscored-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-braced-in-underscored-atpt ()
  "Employ actions of END at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-braced-in-underscored-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-braced-in-underscored-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-braced-in-underscored-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-braced-in-underscored-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-braced-in-underscored-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-braced-in-underscored-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-braced-in-underscored-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-braced-in-underscored-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-braced-in-underscored-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-braced-in-underscored-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-braced-in-underscored-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-braced-in-underscored-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-braced-in-underscored-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-braced-in-underscored-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-braced-in-underscored-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-braced-in-underscored-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-braced-in-underscored-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-braced-in-underscored-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-braced-in-underscored-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-braced-in-underscored-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'underscored 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-braced-in-whitespaced-atpt ()
  "Employ actions of  at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-braced-in-whitespaced-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-braced-in-whitespaced-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-braced-in-whitespaced-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-braced-in-whitespaced-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-braced-in-whitespaced-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-braced-in-whitespaced-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-braced-in-whitespaced-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-braced-in-whitespaced-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-braced-in-whitespaced-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-braced-in-whitespaced-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-braced-in-whitespaced-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-braced-in-whitespaced-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-braced-in-whitespaced-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-braced-in-whitespaced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-braced-in-whitespaced-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-braced-in-whitespaced-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-braced-in-whitespaced-atpt ()
  "Employ actions of END at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-braced-in-whitespaced-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-braced-in-whitespaced-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-braced-in-whitespaced-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-braced-in-whitespaced-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-braced-in-whitespaced-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-braced-in-whitespaced-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-braced-in-whitespaced-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-braced-in-whitespaced-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-braced-in-whitespaced-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-braced-in-whitespaced-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-braced-in-whitespaced-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-braced-in-whitespaced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-braced-in-whitespaced-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-braced-in-whitespaced-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-braced-in-whitespaced-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-braced-in-whitespaced-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-braced-in-whitespaced-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-braced-in-whitespaced-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-braced-in-whitespaced-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-braced-in-whitespaced-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'whitespaced 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-bracketed-in-backslashed-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-bracketed-in-backslashed-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-bracketed-in-backslashed-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-bracketed-in-backslashed-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-bracketed-in-backslashed-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-bracketed-in-backslashed-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-bracketed-in-backslashed-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-bracketed-in-backslashed-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-bracketed-in-backslashed-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-bracketed-in-backslashed-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-bracketed-in-backslashed-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-bracketed-in-backslashed-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-bracketed-in-backslashed-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-bracketed-in-backslashed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-bracketed-in-backslashed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-bracketed-in-backslashed-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-bracketed-in-backslashed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-bracketed-in-backslashed-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-bracketed-in-backslashed-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-bracketed-in-backslashed-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-bracketed-in-backslashed-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-bracketed-in-backslashed-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-bracketed-in-backslashed-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-bracketed-in-backslashed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-bracketed-in-backslashed-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-bracketed-in-backslashed-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-bracketed-in-backslashed-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-bracketed-in-backslashed-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-bracketed-in-backslashed-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-bracketed-in-backslashed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-bracketed-in-backslashed-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-bracketed-in-backslashed-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-bracketed-in-backslashed-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-bracketed-in-backslashed-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-bracketed-in-backslashed-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-bracketed-in-backslashed-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-bracketed-in-backslashed-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-bracketed-in-backslashed-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backslashed 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-bracketed-in-backticked-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-bracketed-in-backticked-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-bracketed-in-backticked-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-bracketed-in-backticked-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-bracketed-in-backticked-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-bracketed-in-backticked-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-bracketed-in-backticked-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-bracketed-in-backticked-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-bracketed-in-backticked-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-bracketed-in-backticked-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-bracketed-in-backticked-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-bracketed-in-backticked-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-bracketed-in-backticked-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-bracketed-in-backticked-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-bracketed-in-backticked-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-bracketed-in-backticked-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-bracketed-in-backticked-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-bracketed-in-backticked-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-bracketed-in-backticked-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-bracketed-in-backticked-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-bracketed-in-backticked-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-bracketed-in-backticked-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-bracketed-in-backticked-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-bracketed-in-backticked-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-bracketed-in-backticked-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-bracketed-in-backticked-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-bracketed-in-backticked-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-bracketed-in-backticked-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-bracketed-in-backticked-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-bracketed-in-backticked-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-bracketed-in-backticked-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-bracketed-in-backticked-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-bracketed-in-backticked-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-bracketed-in-backticked-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-bracketed-in-backticked-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-bracketed-in-backticked-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-bracketed-in-backticked-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-bracketed-in-backticked-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'backticked 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-bracketed-in-dollared-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-bracketed-in-dollared-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-bracketed-in-dollared-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-bracketed-in-dollared-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-bracketed-in-dollared-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-bracketed-in-dollared-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-bracketed-in-dollared-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-bracketed-in-dollared-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-bracketed-in-dollared-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-bracketed-in-dollared-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-bracketed-in-dollared-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-bracketed-in-dollared-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-bracketed-in-dollared-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-bracketed-in-dollared-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-bracketed-in-dollared-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-bracketed-in-dollared-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-bracketed-in-dollared-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-bracketed-in-dollared-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-bracketed-in-dollared-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-bracketed-in-dollared-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-bracketed-in-dollared-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-bracketed-in-dollared-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-bracketed-in-dollared-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-bracketed-in-dollared-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-bracketed-in-dollared-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-bracketed-in-dollared-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-bracketed-in-dollared-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-bracketed-in-dollared-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-bracketed-in-dollared-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-bracketed-in-dollared-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-bracketed-in-dollared-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-bracketed-in-dollared-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-bracketed-in-dollared-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-bracketed-in-dollared-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-bracketed-in-dollared-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-bracketed-in-dollared-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-bracketed-in-dollared-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-bracketed-in-dollared-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'dollared 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-bracketed-in-doublequoted-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-bracketed-in-doublequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-bracketed-in-doublequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-bracketed-in-doublequoted-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-bracketed-in-doublequoted-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-bracketed-in-doublequoted-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-bracketed-in-doublequoted-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-bracketed-in-doublequoted-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-bracketed-in-doublequoted-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-bracketed-in-doublequoted-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-bracketed-in-doublequoted-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-bracketed-in-doublequoted-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-bracketed-in-doublequoted-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-bracketed-in-doublequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-bracketed-in-doublequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-bracketed-in-doublequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-bracketed-in-doublequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-bracketed-in-doublequoted-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-bracketed-in-doublequoted-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-bracketed-in-doublequoted-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-bracketed-in-doublequoted-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-bracketed-in-doublequoted-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-bracketed-in-doublequoted-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-bracketed-in-doublequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-bracketed-in-doublequoted-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-bracketed-in-doublequoted-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-bracketed-in-doublequoted-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-bracketed-in-doublequoted-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-bracketed-in-doublequoted-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-bracketed-in-doublequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-bracketed-in-doublequoted-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-bracketed-in-doublequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-bracketed-in-doublequoted-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-bracketed-in-doublequoted-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-bracketed-in-doublequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-bracketed-in-doublequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-bracketed-in-doublequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-bracketed-in-doublequoted-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doublequoted 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-bracketed-in-equalized-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-bracketed-in-equalized-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-bracketed-in-equalized-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-bracketed-in-equalized-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-bracketed-in-equalized-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-bracketed-in-equalized-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-bracketed-in-equalized-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-bracketed-in-equalized-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-bracketed-in-equalized-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-bracketed-in-equalized-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-bracketed-in-equalized-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-bracketed-in-equalized-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-bracketed-in-equalized-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-bracketed-in-equalized-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-bracketed-in-equalized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-bracketed-in-equalized-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-bracketed-in-equalized-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-bracketed-in-equalized-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-bracketed-in-equalized-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-bracketed-in-equalized-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-bracketed-in-equalized-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-bracketed-in-equalized-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-bracketed-in-equalized-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-bracketed-in-equalized-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-bracketed-in-equalized-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-bracketed-in-equalized-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-bracketed-in-equalized-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-bracketed-in-equalized-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-bracketed-in-equalized-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-bracketed-in-equalized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-bracketed-in-equalized-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-bracketed-in-equalized-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-bracketed-in-equalized-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-bracketed-in-equalized-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-bracketed-in-equalized-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-bracketed-in-equalized-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-bracketed-in-equalized-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-bracketed-in-equalized-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'equalized 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-bracketed-in-hyphened-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-bracketed-in-hyphened-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-bracketed-in-hyphened-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-bracketed-in-hyphened-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-bracketed-in-hyphened-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-bracketed-in-hyphened-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-bracketed-in-hyphened-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-bracketed-in-hyphened-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-bracketed-in-hyphened-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-bracketed-in-hyphened-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-bracketed-in-hyphened-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-bracketed-in-hyphened-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-bracketed-in-hyphened-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-bracketed-in-hyphened-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-bracketed-in-hyphened-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-bracketed-in-hyphened-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-bracketed-in-hyphened-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-bracketed-in-hyphened-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-bracketed-in-hyphened-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-bracketed-in-hyphened-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-bracketed-in-hyphened-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-bracketed-in-hyphened-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-bracketed-in-hyphened-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-bracketed-in-hyphened-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-bracketed-in-hyphened-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-bracketed-in-hyphened-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-bracketed-in-hyphened-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-bracketed-in-hyphened-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-bracketed-in-hyphened-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-bracketed-in-hyphened-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-bracketed-in-hyphened-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-bracketed-in-hyphened-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-bracketed-in-hyphened-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-bracketed-in-hyphened-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-bracketed-in-hyphened-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-bracketed-in-hyphened-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-bracketed-in-hyphened-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-bracketed-in-hyphened-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'hyphened 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-bracketed-in-singlequoted-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-bracketed-in-singlequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-bracketed-in-singlequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-bracketed-in-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-bracketed-in-singlequoted-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-bracketed-in-singlequoted-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-bracketed-in-singlequoted-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-bracketed-in-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-bracketed-in-singlequoted-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-bracketed-in-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-bracketed-in-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-bracketed-in-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-bracketed-in-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-bracketed-in-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-bracketed-in-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-bracketed-in-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-bracketed-in-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-bracketed-in-singlequoted-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-bracketed-in-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-bracketed-in-singlequoted-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-bracketed-in-singlequoted-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-bracketed-in-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-bracketed-in-singlequoted-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-bracketed-in-singlequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-bracketed-in-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-bracketed-in-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-bracketed-in-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-bracketed-in-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-bracketed-in-singlequoted-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-bracketed-in-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-bracketed-in-singlequoted-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-bracketed-in-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-bracketed-in-singlequoted-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-bracketed-in-singlequoted-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-bracketed-in-singlequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-bracketed-in-singlequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-bracketed-in-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-bracketed-in-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'singlequoted 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-bracketed-in-slashed-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-bracketed-in-slashed-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-bracketed-in-slashed-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-bracketed-in-slashed-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-bracketed-in-slashed-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-bracketed-in-slashed-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-bracketed-in-slashed-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-bracketed-in-slashed-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-bracketed-in-slashed-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-bracketed-in-slashed-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-bracketed-in-slashed-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-bracketed-in-slashed-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-bracketed-in-slashed-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-bracketed-in-slashed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-bracketed-in-slashed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-bracketed-in-slashed-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-bracketed-in-slashed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-bracketed-in-slashed-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-bracketed-in-slashed-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-bracketed-in-slashed-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-bracketed-in-slashed-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-bracketed-in-slashed-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-bracketed-in-slashed-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-bracketed-in-slashed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-bracketed-in-slashed-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-bracketed-in-slashed-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-bracketed-in-slashed-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-bracketed-in-slashed-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-bracketed-in-slashed-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-bracketed-in-slashed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-bracketed-in-slashed-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-bracketed-in-slashed-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-bracketed-in-slashed-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-bracketed-in-slashed-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-bracketed-in-slashed-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-bracketed-in-slashed-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-bracketed-in-slashed-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-bracketed-in-slashed-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slashed 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-bracketed-in-underscored-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-bracketed-in-underscored-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-bracketed-in-underscored-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-bracketed-in-underscored-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-bracketed-in-underscored-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-bracketed-in-underscored-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-bracketed-in-underscored-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-bracketed-in-underscored-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-bracketed-in-underscored-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-bracketed-in-underscored-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-bracketed-in-underscored-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-bracketed-in-underscored-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-bracketed-in-underscored-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-bracketed-in-underscored-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-bracketed-in-underscored-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-bracketed-in-underscored-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-bracketed-in-underscored-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-bracketed-in-underscored-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-bracketed-in-underscored-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-bracketed-in-underscored-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-bracketed-in-underscored-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-bracketed-in-underscored-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-bracketed-in-underscored-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-bracketed-in-underscored-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-bracketed-in-underscored-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-bracketed-in-underscored-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-bracketed-in-underscored-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-bracketed-in-underscored-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-bracketed-in-underscored-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-bracketed-in-underscored-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-bracketed-in-underscored-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-bracketed-in-underscored-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-bracketed-in-underscored-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-bracketed-in-underscored-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-bracketed-in-underscored-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-bracketed-in-underscored-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-bracketed-in-underscored-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-bracketed-in-underscored-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'underscored 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-bracketed-in-whitespaced-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-bracketed-in-whitespaced-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-bracketed-in-whitespaced-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-bracketed-in-whitespaced-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-bracketed-in-whitespaced-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-bracketed-in-whitespaced-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-bracketed-in-whitespaced-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-bracketed-in-whitespaced-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-bracketed-in-whitespaced-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-bracketed-in-whitespaced-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-bracketed-in-whitespaced-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-bracketed-in-whitespaced-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-bracketed-in-whitespaced-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-bracketed-in-whitespaced-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-bracketed-in-whitespaced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-bracketed-in-whitespaced-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-bracketed-in-whitespaced-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-bracketed-in-whitespaced-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-bracketed-in-whitespaced-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-bracketed-in-whitespaced-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-bracketed-in-whitespaced-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-bracketed-in-whitespaced-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-bracketed-in-whitespaced-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-bracketed-in-whitespaced-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-bracketed-in-whitespaced-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-bracketed-in-whitespaced-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-bracketed-in-whitespaced-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-bracketed-in-whitespaced-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-bracketed-in-whitespaced-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-bracketed-in-whitespaced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-bracketed-in-whitespaced-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-bracketed-in-whitespaced-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-bracketed-in-whitespaced-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-bracketed-in-whitespaced-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-bracketed-in-whitespaced-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-bracketed-in-whitespaced-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-bracketed-in-whitespaced-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-bracketed-in-whitespaced-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'whitespaced 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-lesserangled-in-backslashed-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-lesserangled-in-backslashed-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-lesserangled-in-backslashed-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-lesserangled-in-backslashed-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-lesserangled-in-backslashed-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-lesserangled-in-backslashed-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-lesserangled-in-backslashed-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-lesserangled-in-backslashed-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-lesserangled-in-backslashed-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-lesserangled-in-backslashed-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-lesserangled-in-backslashed-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-lesserangled-in-backslashed-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-lesserangled-in-backslashed-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-lesserangled-in-backslashed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-lesserangled-in-backslashed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-lesserangled-in-backslashed-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-lesserangled-in-backslashed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-lesserangled-in-backslashed-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-lesserangled-in-backslashed-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-lesserangled-in-backslashed-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-lesserangled-in-backslashed-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-lesserangled-in-backslashed-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-lesserangled-in-backslashed-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-lesserangled-in-backslashed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-lesserangled-in-backslashed-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-lesserangled-in-backslashed-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-lesserangled-in-backslashed-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-lesserangled-in-backslashed-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-lesserangled-in-backslashed-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-lesserangled-in-backslashed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-lesserangled-in-backslashed-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-lesserangled-in-backslashed-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-lesserangled-in-backslashed-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-lesserangled-in-backslashed-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-lesserangled-in-backslashed-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-lesserangled-in-backslashed-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-lesserangled-in-backslashed-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-lesserangled-in-backslashed-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backslashed 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-lesserangled-in-backticked-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-lesserangled-in-backticked-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-lesserangled-in-backticked-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-lesserangled-in-backticked-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-lesserangled-in-backticked-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-lesserangled-in-backticked-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-lesserangled-in-backticked-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-lesserangled-in-backticked-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-lesserangled-in-backticked-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-lesserangled-in-backticked-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-lesserangled-in-backticked-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-lesserangled-in-backticked-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-lesserangled-in-backticked-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-lesserangled-in-backticked-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-lesserangled-in-backticked-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-lesserangled-in-backticked-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-lesserangled-in-backticked-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-lesserangled-in-backticked-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-lesserangled-in-backticked-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-lesserangled-in-backticked-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-lesserangled-in-backticked-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-lesserangled-in-backticked-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-lesserangled-in-backticked-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-lesserangled-in-backticked-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-lesserangled-in-backticked-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-lesserangled-in-backticked-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-lesserangled-in-backticked-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-lesserangled-in-backticked-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-lesserangled-in-backticked-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-lesserangled-in-backticked-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-lesserangled-in-backticked-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-lesserangled-in-backticked-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-lesserangled-in-backticked-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-lesserangled-in-backticked-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-lesserangled-in-backticked-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-lesserangled-in-backticked-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-lesserangled-in-backticked-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-lesserangled-in-backticked-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'backticked 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-lesserangled-in-dollared-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-lesserangled-in-dollared-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-lesserangled-in-dollared-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-lesserangled-in-dollared-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-lesserangled-in-dollared-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-lesserangled-in-dollared-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-lesserangled-in-dollared-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-lesserangled-in-dollared-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-lesserangled-in-dollared-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-lesserangled-in-dollared-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-lesserangled-in-dollared-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-lesserangled-in-dollared-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-lesserangled-in-dollared-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-lesserangled-in-dollared-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-lesserangled-in-dollared-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-lesserangled-in-dollared-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-lesserangled-in-dollared-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-lesserangled-in-dollared-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-lesserangled-in-dollared-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-lesserangled-in-dollared-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-lesserangled-in-dollared-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-lesserangled-in-dollared-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-lesserangled-in-dollared-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-lesserangled-in-dollared-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-lesserangled-in-dollared-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-lesserangled-in-dollared-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-lesserangled-in-dollared-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-lesserangled-in-dollared-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-lesserangled-in-dollared-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-lesserangled-in-dollared-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-lesserangled-in-dollared-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-lesserangled-in-dollared-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-lesserangled-in-dollared-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-lesserangled-in-dollared-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-lesserangled-in-dollared-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-lesserangled-in-dollared-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-lesserangled-in-dollared-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-lesserangled-in-dollared-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'dollared 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-lesserangled-in-doublequoted-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-lesserangled-in-doublequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-lesserangled-in-doublequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-lesserangled-in-doublequoted-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-lesserangled-in-doublequoted-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-lesserangled-in-doublequoted-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-lesserangled-in-doublequoted-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-lesserangled-in-doublequoted-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-lesserangled-in-doublequoted-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-lesserangled-in-doublequoted-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-lesserangled-in-doublequoted-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-lesserangled-in-doublequoted-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-lesserangled-in-doublequoted-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-lesserangled-in-doublequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-lesserangled-in-doublequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-lesserangled-in-doublequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-lesserangled-in-doublequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-lesserangled-in-doublequoted-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-lesserangled-in-doublequoted-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-lesserangled-in-doublequoted-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-lesserangled-in-doublequoted-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-lesserangled-in-doublequoted-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-lesserangled-in-doublequoted-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-lesserangled-in-doublequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-lesserangled-in-doublequoted-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-lesserangled-in-doublequoted-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-lesserangled-in-doublequoted-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-lesserangled-in-doublequoted-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-lesserangled-in-doublequoted-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-lesserangled-in-doublequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-lesserangled-in-doublequoted-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-lesserangled-in-doublequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-lesserangled-in-doublequoted-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-lesserangled-in-doublequoted-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-lesserangled-in-doublequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-lesserangled-in-doublequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-lesserangled-in-doublequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-lesserangled-in-doublequoted-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doublequoted 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-lesserangled-in-equalized-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-lesserangled-in-equalized-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-lesserangled-in-equalized-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-lesserangled-in-equalized-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-lesserangled-in-equalized-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-lesserangled-in-equalized-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-lesserangled-in-equalized-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-lesserangled-in-equalized-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-lesserangled-in-equalized-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-lesserangled-in-equalized-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-lesserangled-in-equalized-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-lesserangled-in-equalized-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-lesserangled-in-equalized-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-lesserangled-in-equalized-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-lesserangled-in-equalized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-lesserangled-in-equalized-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-lesserangled-in-equalized-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-lesserangled-in-equalized-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-lesserangled-in-equalized-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-lesserangled-in-equalized-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-lesserangled-in-equalized-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-lesserangled-in-equalized-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-lesserangled-in-equalized-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-lesserangled-in-equalized-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-lesserangled-in-equalized-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-lesserangled-in-equalized-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-lesserangled-in-equalized-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-lesserangled-in-equalized-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-lesserangled-in-equalized-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-lesserangled-in-equalized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-lesserangled-in-equalized-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-lesserangled-in-equalized-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-lesserangled-in-equalized-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-lesserangled-in-equalized-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-lesserangled-in-equalized-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-lesserangled-in-equalized-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-lesserangled-in-equalized-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-lesserangled-in-equalized-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'equalized 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-lesserangled-in-hyphened-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-lesserangled-in-hyphened-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-lesserangled-in-hyphened-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-lesserangled-in-hyphened-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-lesserangled-in-hyphened-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-lesserangled-in-hyphened-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-lesserangled-in-hyphened-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-lesserangled-in-hyphened-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-lesserangled-in-hyphened-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-lesserangled-in-hyphened-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-lesserangled-in-hyphened-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-lesserangled-in-hyphened-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-lesserangled-in-hyphened-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-lesserangled-in-hyphened-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-lesserangled-in-hyphened-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-lesserangled-in-hyphened-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-lesserangled-in-hyphened-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-lesserangled-in-hyphened-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-lesserangled-in-hyphened-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-lesserangled-in-hyphened-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-lesserangled-in-hyphened-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-lesserangled-in-hyphened-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-lesserangled-in-hyphened-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-lesserangled-in-hyphened-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-lesserangled-in-hyphened-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-lesserangled-in-hyphened-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-lesserangled-in-hyphened-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-lesserangled-in-hyphened-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-lesserangled-in-hyphened-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-lesserangled-in-hyphened-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-lesserangled-in-hyphened-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-lesserangled-in-hyphened-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-lesserangled-in-hyphened-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-lesserangled-in-hyphened-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-lesserangled-in-hyphened-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-lesserangled-in-hyphened-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-lesserangled-in-hyphened-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-lesserangled-in-hyphened-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'hyphened 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-lesserangled-in-singlequoted-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-lesserangled-in-singlequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-lesserangled-in-singlequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-lesserangled-in-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-lesserangled-in-singlequoted-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-lesserangled-in-singlequoted-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-lesserangled-in-singlequoted-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-lesserangled-in-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-lesserangled-in-singlequoted-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-lesserangled-in-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-lesserangled-in-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-lesserangled-in-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-lesserangled-in-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-lesserangled-in-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-lesserangled-in-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-lesserangled-in-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-lesserangled-in-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-lesserangled-in-singlequoted-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-lesserangled-in-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-lesserangled-in-singlequoted-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-lesserangled-in-singlequoted-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-lesserangled-in-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-lesserangled-in-singlequoted-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-lesserangled-in-singlequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-lesserangled-in-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-lesserangled-in-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-lesserangled-in-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-lesserangled-in-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-lesserangled-in-singlequoted-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-lesserangled-in-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-lesserangled-in-singlequoted-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-lesserangled-in-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-lesserangled-in-singlequoted-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-lesserangled-in-singlequoted-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-lesserangled-in-singlequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-lesserangled-in-singlequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-lesserangled-in-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-lesserangled-in-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'singlequoted 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-lesserangled-in-slashed-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-lesserangled-in-slashed-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-lesserangled-in-slashed-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-lesserangled-in-slashed-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-lesserangled-in-slashed-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-lesserangled-in-slashed-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-lesserangled-in-slashed-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-lesserangled-in-slashed-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-lesserangled-in-slashed-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-lesserangled-in-slashed-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-lesserangled-in-slashed-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-lesserangled-in-slashed-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-lesserangled-in-slashed-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-lesserangled-in-slashed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-lesserangled-in-slashed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-lesserangled-in-slashed-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-lesserangled-in-slashed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-lesserangled-in-slashed-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-lesserangled-in-slashed-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-lesserangled-in-slashed-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-lesserangled-in-slashed-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-lesserangled-in-slashed-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-lesserangled-in-slashed-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-lesserangled-in-slashed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-lesserangled-in-slashed-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-lesserangled-in-slashed-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-lesserangled-in-slashed-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-lesserangled-in-slashed-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-lesserangled-in-slashed-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-lesserangled-in-slashed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-lesserangled-in-slashed-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-lesserangled-in-slashed-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-lesserangled-in-slashed-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-lesserangled-in-slashed-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-lesserangled-in-slashed-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-lesserangled-in-slashed-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-lesserangled-in-slashed-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-lesserangled-in-slashed-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slashed 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-lesserangled-in-underscored-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-lesserangled-in-underscored-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-lesserangled-in-underscored-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-lesserangled-in-underscored-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-lesserangled-in-underscored-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-lesserangled-in-underscored-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-lesserangled-in-underscored-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-lesserangled-in-underscored-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-lesserangled-in-underscored-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-lesserangled-in-underscored-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-lesserangled-in-underscored-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-lesserangled-in-underscored-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-lesserangled-in-underscored-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-lesserangled-in-underscored-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-lesserangled-in-underscored-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-lesserangled-in-underscored-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-lesserangled-in-underscored-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-lesserangled-in-underscored-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-lesserangled-in-underscored-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-lesserangled-in-underscored-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-lesserangled-in-underscored-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-lesserangled-in-underscored-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-lesserangled-in-underscored-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-lesserangled-in-underscored-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-lesserangled-in-underscored-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-lesserangled-in-underscored-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-lesserangled-in-underscored-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-lesserangled-in-underscored-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-lesserangled-in-underscored-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-lesserangled-in-underscored-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-lesserangled-in-underscored-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-lesserangled-in-underscored-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-lesserangled-in-underscored-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-lesserangled-in-underscored-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-lesserangled-in-underscored-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-lesserangled-in-underscored-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-lesserangled-in-underscored-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-lesserangled-in-underscored-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'underscored 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-lesserangled-in-whitespaced-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-lesserangled-in-whitespaced-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-lesserangled-in-whitespaced-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-lesserangled-in-whitespaced-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-lesserangled-in-whitespaced-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-lesserangled-in-whitespaced-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-lesserangled-in-whitespaced-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-lesserangled-in-whitespaced-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-lesserangled-in-whitespaced-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-lesserangled-in-whitespaced-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-lesserangled-in-whitespaced-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-lesserangled-in-whitespaced-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-lesserangled-in-whitespaced-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-lesserangled-in-whitespaced-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-lesserangled-in-whitespaced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-lesserangled-in-whitespaced-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-lesserangled-in-whitespaced-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-lesserangled-in-whitespaced-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-lesserangled-in-whitespaced-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-lesserangled-in-whitespaced-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-lesserangled-in-whitespaced-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-lesserangled-in-whitespaced-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-lesserangled-in-whitespaced-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-lesserangled-in-whitespaced-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-lesserangled-in-whitespaced-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-lesserangled-in-whitespaced-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-lesserangled-in-whitespaced-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-lesserangled-in-whitespaced-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-lesserangled-in-whitespaced-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-lesserangled-in-whitespaced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-lesserangled-in-whitespaced-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-lesserangled-in-whitespaced-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-lesserangled-in-whitespaced-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-lesserangled-in-whitespaced-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-lesserangled-in-whitespaced-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-lesserangled-in-whitespaced-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-lesserangled-in-whitespaced-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-lesserangled-in-whitespaced-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'whitespaced 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-greaterangled-in-backslashed-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-greaterangled-in-backslashed-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-greaterangled-in-backslashed-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-greaterangled-in-backslashed-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-greaterangled-in-backslashed-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-greaterangled-in-backslashed-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-greaterangled-in-backslashed-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-greaterangled-in-backslashed-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-greaterangled-in-backslashed-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-greaterangled-in-backslashed-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-greaterangled-in-backslashed-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-greaterangled-in-backslashed-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-greaterangled-in-backslashed-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-greaterangled-in-backslashed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-greaterangled-in-backslashed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-greaterangled-in-backslashed-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-greaterangled-in-backslashed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-greaterangled-in-backslashed-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-greaterangled-in-backslashed-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-greaterangled-in-backslashed-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-greaterangled-in-backslashed-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-greaterangled-in-backslashed-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-greaterangled-in-backslashed-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-greaterangled-in-backslashed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-greaterangled-in-backslashed-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-greaterangled-in-backslashed-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-greaterangled-in-backslashed-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-greaterangled-in-backslashed-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-greaterangled-in-backslashed-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-greaterangled-in-backslashed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-greaterangled-in-backslashed-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-greaterangled-in-backslashed-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-greaterangled-in-backslashed-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-greaterangled-in-backslashed-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-greaterangled-in-backslashed-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-greaterangled-in-backslashed-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-greaterangled-in-backslashed-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-greaterangled-in-backslashed-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backslashed 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-greaterangled-in-backticked-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-greaterangled-in-backticked-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-greaterangled-in-backticked-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-greaterangled-in-backticked-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-greaterangled-in-backticked-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-greaterangled-in-backticked-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-greaterangled-in-backticked-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-greaterangled-in-backticked-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-greaterangled-in-backticked-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-greaterangled-in-backticked-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-greaterangled-in-backticked-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-greaterangled-in-backticked-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-greaterangled-in-backticked-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-greaterangled-in-backticked-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-greaterangled-in-backticked-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-greaterangled-in-backticked-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-greaterangled-in-backticked-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-greaterangled-in-backticked-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-greaterangled-in-backticked-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-greaterangled-in-backticked-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-greaterangled-in-backticked-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-greaterangled-in-backticked-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-greaterangled-in-backticked-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-greaterangled-in-backticked-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-greaterangled-in-backticked-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-greaterangled-in-backticked-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-greaterangled-in-backticked-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-greaterangled-in-backticked-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-greaterangled-in-backticked-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-greaterangled-in-backticked-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-greaterangled-in-backticked-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-greaterangled-in-backticked-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-greaterangled-in-backticked-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-greaterangled-in-backticked-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-greaterangled-in-backticked-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-greaterangled-in-backticked-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-greaterangled-in-backticked-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-greaterangled-in-backticked-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'backticked 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-greaterangled-in-dollared-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-greaterangled-in-dollared-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-greaterangled-in-dollared-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-greaterangled-in-dollared-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-greaterangled-in-dollared-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-greaterangled-in-dollared-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-greaterangled-in-dollared-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-greaterangled-in-dollared-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-greaterangled-in-dollared-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-greaterangled-in-dollared-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-greaterangled-in-dollared-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-greaterangled-in-dollared-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-greaterangled-in-dollared-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-greaterangled-in-dollared-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-greaterangled-in-dollared-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-greaterangled-in-dollared-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-greaterangled-in-dollared-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-greaterangled-in-dollared-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-greaterangled-in-dollared-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-greaterangled-in-dollared-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-greaterangled-in-dollared-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-greaterangled-in-dollared-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-greaterangled-in-dollared-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-greaterangled-in-dollared-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-greaterangled-in-dollared-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-greaterangled-in-dollared-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-greaterangled-in-dollared-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-greaterangled-in-dollared-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-greaterangled-in-dollared-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-greaterangled-in-dollared-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-greaterangled-in-dollared-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-greaterangled-in-dollared-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-greaterangled-in-dollared-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-greaterangled-in-dollared-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-greaterangled-in-dollared-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-greaterangled-in-dollared-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-greaterangled-in-dollared-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-greaterangled-in-dollared-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'dollared 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-greaterangled-in-doublequoted-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-greaterangled-in-doublequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-greaterangled-in-doublequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-greaterangled-in-doublequoted-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-greaterangled-in-doublequoted-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-greaterangled-in-doublequoted-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-greaterangled-in-doublequoted-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-greaterangled-in-doublequoted-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-greaterangled-in-doublequoted-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-greaterangled-in-doublequoted-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-greaterangled-in-doublequoted-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-greaterangled-in-doublequoted-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-greaterangled-in-doublequoted-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-greaterangled-in-doublequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-greaterangled-in-doublequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-greaterangled-in-doublequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-greaterangled-in-doublequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-greaterangled-in-doublequoted-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-greaterangled-in-doublequoted-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-greaterangled-in-doublequoted-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-greaterangled-in-doublequoted-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-greaterangled-in-doublequoted-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-greaterangled-in-doublequoted-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-greaterangled-in-doublequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-greaterangled-in-doublequoted-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-greaterangled-in-doublequoted-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-greaterangled-in-doublequoted-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-greaterangled-in-doublequoted-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-greaterangled-in-doublequoted-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-greaterangled-in-doublequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-greaterangled-in-doublequoted-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-greaterangled-in-doublequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-greaterangled-in-doublequoted-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-greaterangled-in-doublequoted-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-greaterangled-in-doublequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-greaterangled-in-doublequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-greaterangled-in-doublequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-greaterangled-in-doublequoted-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doublequoted 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-greaterangled-in-equalized-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-greaterangled-in-equalized-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-greaterangled-in-equalized-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-greaterangled-in-equalized-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-greaterangled-in-equalized-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-greaterangled-in-equalized-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-greaterangled-in-equalized-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-greaterangled-in-equalized-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-greaterangled-in-equalized-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-greaterangled-in-equalized-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-greaterangled-in-equalized-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-greaterangled-in-equalized-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-greaterangled-in-equalized-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-greaterangled-in-equalized-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-greaterangled-in-equalized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-greaterangled-in-equalized-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-greaterangled-in-equalized-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-greaterangled-in-equalized-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-greaterangled-in-equalized-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-greaterangled-in-equalized-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-greaterangled-in-equalized-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-greaterangled-in-equalized-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-greaterangled-in-equalized-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-greaterangled-in-equalized-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-greaterangled-in-equalized-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-greaterangled-in-equalized-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-greaterangled-in-equalized-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-greaterangled-in-equalized-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-greaterangled-in-equalized-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-greaterangled-in-equalized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-greaterangled-in-equalized-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-greaterangled-in-equalized-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-greaterangled-in-equalized-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-greaterangled-in-equalized-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-greaterangled-in-equalized-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-greaterangled-in-equalized-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-greaterangled-in-equalized-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-greaterangled-in-equalized-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'equalized 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-greaterangled-in-hyphened-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-greaterangled-in-hyphened-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-greaterangled-in-hyphened-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-greaterangled-in-hyphened-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-greaterangled-in-hyphened-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-greaterangled-in-hyphened-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-greaterangled-in-hyphened-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-greaterangled-in-hyphened-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-greaterangled-in-hyphened-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-greaterangled-in-hyphened-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-greaterangled-in-hyphened-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-greaterangled-in-hyphened-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-greaterangled-in-hyphened-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-greaterangled-in-hyphened-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-greaterangled-in-hyphened-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-greaterangled-in-hyphened-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-greaterangled-in-hyphened-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-greaterangled-in-hyphened-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-greaterangled-in-hyphened-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-greaterangled-in-hyphened-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-greaterangled-in-hyphened-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-greaterangled-in-hyphened-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-greaterangled-in-hyphened-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-greaterangled-in-hyphened-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-greaterangled-in-hyphened-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-greaterangled-in-hyphened-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-greaterangled-in-hyphened-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-greaterangled-in-hyphened-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-greaterangled-in-hyphened-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-greaterangled-in-hyphened-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-greaterangled-in-hyphened-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-greaterangled-in-hyphened-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-greaterangled-in-hyphened-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-greaterangled-in-hyphened-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-greaterangled-in-hyphened-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-greaterangled-in-hyphened-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-greaterangled-in-hyphened-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-greaterangled-in-hyphened-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'hyphened 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-greaterangled-in-singlequoted-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-greaterangled-in-singlequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-greaterangled-in-singlequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-greaterangled-in-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-greaterangled-in-singlequoted-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-greaterangled-in-singlequoted-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-greaterangled-in-singlequoted-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-greaterangled-in-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-greaterangled-in-singlequoted-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-greaterangled-in-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-greaterangled-in-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-greaterangled-in-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-greaterangled-in-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-greaterangled-in-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-greaterangled-in-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-greaterangled-in-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-greaterangled-in-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-greaterangled-in-singlequoted-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-greaterangled-in-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-greaterangled-in-singlequoted-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-greaterangled-in-singlequoted-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-greaterangled-in-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-greaterangled-in-singlequoted-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-greaterangled-in-singlequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-greaterangled-in-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-greaterangled-in-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-greaterangled-in-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-greaterangled-in-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-greaterangled-in-singlequoted-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-greaterangled-in-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-greaterangled-in-singlequoted-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-greaterangled-in-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-greaterangled-in-singlequoted-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-greaterangled-in-singlequoted-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-greaterangled-in-singlequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-greaterangled-in-singlequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-greaterangled-in-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-greaterangled-in-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'singlequoted 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-greaterangled-in-slashed-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-greaterangled-in-slashed-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-greaterangled-in-slashed-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-greaterangled-in-slashed-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-greaterangled-in-slashed-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-greaterangled-in-slashed-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-greaterangled-in-slashed-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-greaterangled-in-slashed-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-greaterangled-in-slashed-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-greaterangled-in-slashed-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-greaterangled-in-slashed-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-greaterangled-in-slashed-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-greaterangled-in-slashed-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-greaterangled-in-slashed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-greaterangled-in-slashed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-greaterangled-in-slashed-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-greaterangled-in-slashed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-greaterangled-in-slashed-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-greaterangled-in-slashed-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-greaterangled-in-slashed-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-greaterangled-in-slashed-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-greaterangled-in-slashed-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-greaterangled-in-slashed-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-greaterangled-in-slashed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-greaterangled-in-slashed-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-greaterangled-in-slashed-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-greaterangled-in-slashed-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-greaterangled-in-slashed-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-greaterangled-in-slashed-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-greaterangled-in-slashed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-greaterangled-in-slashed-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-greaterangled-in-slashed-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-greaterangled-in-slashed-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-greaterangled-in-slashed-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-greaterangled-in-slashed-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-greaterangled-in-slashed-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-greaterangled-in-slashed-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-greaterangled-in-slashed-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slashed 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-greaterangled-in-underscored-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-greaterangled-in-underscored-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-greaterangled-in-underscored-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-greaterangled-in-underscored-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-greaterangled-in-underscored-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-greaterangled-in-underscored-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-greaterangled-in-underscored-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-greaterangled-in-underscored-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-greaterangled-in-underscored-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-greaterangled-in-underscored-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-greaterangled-in-underscored-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-greaterangled-in-underscored-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-greaterangled-in-underscored-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-greaterangled-in-underscored-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-greaterangled-in-underscored-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-greaterangled-in-underscored-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-greaterangled-in-underscored-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-greaterangled-in-underscored-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-greaterangled-in-underscored-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-greaterangled-in-underscored-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-greaterangled-in-underscored-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-greaterangled-in-underscored-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-greaterangled-in-underscored-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-greaterangled-in-underscored-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-greaterangled-in-underscored-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-greaterangled-in-underscored-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-greaterangled-in-underscored-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-greaterangled-in-underscored-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-greaterangled-in-underscored-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-greaterangled-in-underscored-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-greaterangled-in-underscored-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-greaterangled-in-underscored-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-greaterangled-in-underscored-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-greaterangled-in-underscored-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-greaterangled-in-underscored-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-greaterangled-in-underscored-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-greaterangled-in-underscored-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-greaterangled-in-underscored-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'underscored 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-greaterangled-in-whitespaced-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-greaterangled-in-whitespaced-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-greaterangled-in-whitespaced-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-greaterangled-in-whitespaced-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-greaterangled-in-whitespaced-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-greaterangled-in-whitespaced-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-greaterangled-in-whitespaced-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-greaterangled-in-whitespaced-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-greaterangled-in-whitespaced-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-greaterangled-in-whitespaced-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-greaterangled-in-whitespaced-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-greaterangled-in-whitespaced-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-greaterangled-in-whitespaced-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-greaterangled-in-whitespaced-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-greaterangled-in-whitespaced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-greaterangled-in-whitespaced-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-greaterangled-in-whitespaced-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-greaterangled-in-whitespaced-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-greaterangled-in-whitespaced-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-greaterangled-in-whitespaced-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-greaterangled-in-whitespaced-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-greaterangled-in-whitespaced-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-greaterangled-in-whitespaced-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-greaterangled-in-whitespaced-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-greaterangled-in-whitespaced-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-greaterangled-in-whitespaced-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-greaterangled-in-whitespaced-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-greaterangled-in-whitespaced-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-greaterangled-in-whitespaced-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-greaterangled-in-whitespaced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-greaterangled-in-whitespaced-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-greaterangled-in-whitespaced-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-greaterangled-in-whitespaced-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-greaterangled-in-whitespaced-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-greaterangled-in-whitespaced-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-greaterangled-in-whitespaced-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-greaterangled-in-whitespaced-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-greaterangled-in-whitespaced-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'whitespaced 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-left-right-singlequoted-in-backslashed-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backslashed 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-left-right-singlequoted-in-backticked-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'backticked 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-left-right-singlequoted-in-dollared-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'dollared 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-left-right-singlequoted-in-doublequoted-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doublequoted 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-left-right-singlequoted-in-equalized-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'equalized 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-left-right-singlequoted-in-hyphened-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'hyphened 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-left-right-singlequoted-in-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'singlequoted 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-left-right-singlequoted-in-slashed-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slashed 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-left-right-singlequoted-in-underscored-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'underscored 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-left-right-singlequoted-in-whitespaced-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'whitespaced 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-parentized-in-backslashed-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-parentized-in-backslashed-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-parentized-in-backslashed-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-parentized-in-backslashed-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-parentized-in-backslashed-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-parentized-in-backslashed-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-parentized-in-backslashed-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-parentized-in-backslashed-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-parentized-in-backslashed-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-parentized-in-backslashed-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-parentized-in-backslashed-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-parentized-in-backslashed-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-parentized-in-backslashed-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-parentized-in-backslashed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-parentized-in-backslashed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-parentized-in-backslashed-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-parentized-in-backslashed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-parentized-in-backslashed-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-parentized-in-backslashed-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-parentized-in-backslashed-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-parentized-in-backslashed-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-parentized-in-backslashed-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-parentized-in-backslashed-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-parentized-in-backslashed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-parentized-in-backslashed-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-parentized-in-backslashed-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-parentized-in-backslashed-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-parentized-in-backslashed-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-parentized-in-backslashed-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-parentized-in-backslashed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-parentized-in-backslashed-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-parentized-in-backslashed-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-parentized-in-backslashed-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-parentized-in-backslashed-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-parentized-in-backslashed-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-parentized-in-backslashed-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-parentized-in-backslashed-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-parentized-in-backslashed-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing BACKSLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backslashed 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-parentized-in-backticked-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-parentized-in-backticked-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-parentized-in-backticked-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-parentized-in-backticked-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-parentized-in-backticked-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-parentized-in-backticked-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-parentized-in-backticked-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-parentized-in-backticked-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-parentized-in-backticked-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-parentized-in-backticked-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-parentized-in-backticked-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-parentized-in-backticked-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-parentized-in-backticked-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-parentized-in-backticked-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-parentized-in-backticked-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-parentized-in-backticked-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-parentized-in-backticked-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-parentized-in-backticked-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-parentized-in-backticked-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-parentized-in-backticked-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-parentized-in-backticked-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-parentized-in-backticked-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-parentized-in-backticked-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-parentized-in-backticked-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-parentized-in-backticked-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-parentized-in-backticked-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-parentized-in-backticked-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-parentized-in-backticked-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-parentized-in-backticked-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-parentized-in-backticked-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-parentized-in-backticked-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-parentized-in-backticked-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-parentized-in-backticked-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-parentized-in-backticked-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-parentized-in-backticked-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-parentized-in-backticked-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-parentized-in-backticked-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-parentized-in-backticked-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing BACKTICKED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'backticked 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-parentized-in-dollared-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-parentized-in-dollared-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-parentized-in-dollared-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-parentized-in-dollared-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-parentized-in-dollared-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-parentized-in-dollared-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-parentized-in-dollared-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-parentized-in-dollared-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-parentized-in-dollared-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-parentized-in-dollared-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-parentized-in-dollared-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-parentized-in-dollared-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-parentized-in-dollared-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-parentized-in-dollared-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-parentized-in-dollared-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-parentized-in-dollared-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-parentized-in-dollared-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-parentized-in-dollared-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-parentized-in-dollared-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-parentized-in-dollared-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-parentized-in-dollared-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-parentized-in-dollared-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-parentized-in-dollared-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-parentized-in-dollared-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-parentized-in-dollared-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-parentized-in-dollared-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-parentized-in-dollared-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-parentized-in-dollared-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-parentized-in-dollared-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-parentized-in-dollared-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-parentized-in-dollared-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-parentized-in-dollared-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-parentized-in-dollared-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-parentized-in-dollared-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-parentized-in-dollared-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-parentized-in-dollared-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-parentized-in-dollared-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-parentized-in-dollared-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing DOLLARED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'dollared 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-parentized-in-doublequoted-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-parentized-in-doublequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-parentized-in-doublequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-parentized-in-doublequoted-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-parentized-in-doublequoted-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-parentized-in-doublequoted-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-parentized-in-doublequoted-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-parentized-in-doublequoted-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-parentized-in-doublequoted-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-parentized-in-doublequoted-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-parentized-in-doublequoted-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-parentized-in-doublequoted-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-parentized-in-doublequoted-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-parentized-in-doublequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-parentized-in-doublequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-parentized-in-doublequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-parentized-in-doublequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-parentized-in-doublequoted-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-parentized-in-doublequoted-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-parentized-in-doublequoted-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-parentized-in-doublequoted-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-parentized-in-doublequoted-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-parentized-in-doublequoted-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-parentized-in-doublequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-parentized-in-doublequoted-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-parentized-in-doublequoted-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-parentized-in-doublequoted-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-parentized-in-doublequoted-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-parentized-in-doublequoted-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-parentized-in-doublequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-parentized-in-doublequoted-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-parentized-in-doublequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-parentized-in-doublequoted-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-parentized-in-doublequoted-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-parentized-in-doublequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-parentized-in-doublequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-parentized-in-doublequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-parentized-in-doublequoted-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing DOUBLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doublequoted 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-parentized-in-equalized-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-parentized-in-equalized-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-parentized-in-equalized-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-parentized-in-equalized-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-parentized-in-equalized-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-parentized-in-equalized-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-parentized-in-equalized-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-parentized-in-equalized-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-parentized-in-equalized-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-parentized-in-equalized-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-parentized-in-equalized-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-parentized-in-equalized-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-parentized-in-equalized-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-parentized-in-equalized-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-parentized-in-equalized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-parentized-in-equalized-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-parentized-in-equalized-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-parentized-in-equalized-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-parentized-in-equalized-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-parentized-in-equalized-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-parentized-in-equalized-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-parentized-in-equalized-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-parentized-in-equalized-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-parentized-in-equalized-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-parentized-in-equalized-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-parentized-in-equalized-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-parentized-in-equalized-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-parentized-in-equalized-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-parentized-in-equalized-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-parentized-in-equalized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-parentized-in-equalized-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-parentized-in-equalized-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-parentized-in-equalized-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-parentized-in-equalized-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-parentized-in-equalized-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-parentized-in-equalized-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-parentized-in-equalized-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-parentized-in-equalized-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing EQUALIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'equalized 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-parentized-in-hyphened-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-parentized-in-hyphened-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-parentized-in-hyphened-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-parentized-in-hyphened-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-parentized-in-hyphened-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-parentized-in-hyphened-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-parentized-in-hyphened-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-parentized-in-hyphened-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-parentized-in-hyphened-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-parentized-in-hyphened-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-parentized-in-hyphened-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-parentized-in-hyphened-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-parentized-in-hyphened-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-parentized-in-hyphened-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-parentized-in-hyphened-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-parentized-in-hyphened-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-parentized-in-hyphened-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-parentized-in-hyphened-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-parentized-in-hyphened-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-parentized-in-hyphened-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-parentized-in-hyphened-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-parentized-in-hyphened-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-parentized-in-hyphened-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-parentized-in-hyphened-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-parentized-in-hyphened-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-parentized-in-hyphened-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-parentized-in-hyphened-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-parentized-in-hyphened-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-parentized-in-hyphened-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-parentized-in-hyphened-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-parentized-in-hyphened-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-parentized-in-hyphened-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-parentized-in-hyphened-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-parentized-in-hyphened-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-parentized-in-hyphened-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-parentized-in-hyphened-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-parentized-in-hyphened-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-parentized-in-hyphened-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing HYPHENED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'hyphened 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-parentized-in-singlequoted-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-parentized-in-singlequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-parentized-in-singlequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-parentized-in-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-parentized-in-singlequoted-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-parentized-in-singlequoted-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-parentized-in-singlequoted-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-parentized-in-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-parentized-in-singlequoted-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-parentized-in-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-parentized-in-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-parentized-in-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-parentized-in-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-parentized-in-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-parentized-in-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-parentized-in-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-parentized-in-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-parentized-in-singlequoted-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-parentized-in-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-parentized-in-singlequoted-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-parentized-in-singlequoted-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-parentized-in-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-parentized-in-singlequoted-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-parentized-in-singlequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-parentized-in-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-parentized-in-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-parentized-in-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-parentized-in-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-parentized-in-singlequoted-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-parentized-in-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-parentized-in-singlequoted-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-parentized-in-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-parentized-in-singlequoted-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-parentized-in-singlequoted-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-parentized-in-singlequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-parentized-in-singlequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-parentized-in-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-parentized-in-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'singlequoted 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-parentized-in-slashed-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-parentized-in-slashed-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-parentized-in-slashed-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-parentized-in-slashed-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-parentized-in-slashed-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-parentized-in-slashed-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-parentized-in-slashed-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-parentized-in-slashed-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-parentized-in-slashed-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-parentized-in-slashed-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-parentized-in-slashed-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-parentized-in-slashed-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-parentized-in-slashed-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-parentized-in-slashed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-parentized-in-slashed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-parentized-in-slashed-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-parentized-in-slashed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-parentized-in-slashed-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-parentized-in-slashed-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-parentized-in-slashed-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-parentized-in-slashed-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-parentized-in-slashed-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-parentized-in-slashed-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-parentized-in-slashed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-parentized-in-slashed-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-parentized-in-slashed-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-parentized-in-slashed-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-parentized-in-slashed-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-parentized-in-slashed-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-parentized-in-slashed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-parentized-in-slashed-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-parentized-in-slashed-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-parentized-in-slashed-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-parentized-in-slashed-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-parentized-in-slashed-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-parentized-in-slashed-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-parentized-in-slashed-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-parentized-in-slashed-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing SLASHED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slashed 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-parentized-in-underscored-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-parentized-in-underscored-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-parentized-in-underscored-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-parentized-in-underscored-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-parentized-in-underscored-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-parentized-in-underscored-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-parentized-in-underscored-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-parentized-in-underscored-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-parentized-in-underscored-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-parentized-in-underscored-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-parentized-in-underscored-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-parentized-in-underscored-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-parentized-in-underscored-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-parentized-in-underscored-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-parentized-in-underscored-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-parentized-in-underscored-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-parentized-in-underscored-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-parentized-in-underscored-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-parentized-in-underscored-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-parentized-in-underscored-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-parentized-in-underscored-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-parentized-in-underscored-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-parentized-in-underscored-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-parentized-in-underscored-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-parentized-in-underscored-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-parentized-in-underscored-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-parentized-in-underscored-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-parentized-in-underscored-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-parentized-in-underscored-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-parentized-in-underscored-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-parentized-in-underscored-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-parentized-in-underscored-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-parentized-in-underscored-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-parentized-in-underscored-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-parentized-in-underscored-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-parentized-in-underscored-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-parentized-in-underscored-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-parentized-in-underscored-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing UNDERSCORED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'underscored 'ar-th-whitespace (interactive-p)))

;;;###autoload 
(defun ar-parentized-in-whitespaced-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th (interactive-p)))

;;;###autoload 
(defun ar-greaterangle-parentized-in-whitespaced-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-greaterangle (interactive-p)))

;;;###autoload 
(defun ar-lesserangle-parentized-in-whitespaced-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-lesserangle (interactive-p)))

;;;###autoload 
(defun ar-backslash-parentized-in-whitespaced-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-backslash (interactive-p)))

;;;###autoload 
(defun ar-backtick-parentized-in-whitespaced-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-backtick (interactive-p)))

;;;###autoload 
(defun ar-beg-parentized-in-whitespaced-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-beg (interactive-p)))

;;;###autoload 
(defun ar-blok-parentized-in-whitespaced-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-blok (interactive-p)))

;;;###autoload 
(defun ar-bounds-parentized-in-whitespaced-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-bounds (interactive-p)))

;;;###autoload 
(defun ar-brace-parentized-in-whitespaced-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-brace (interactive-p)))

;;;###autoload 
(defun ar-bracket-parentized-in-whitespaced-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-bracket (interactive-p)))

;;;###autoload 
(defun ar-commatize-parentized-in-whitespaced-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-commatize (interactive-p)))

;;;###autoload 
(defun ar-comment-parentized-in-whitespaced-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-comment (interactive-p)))

;;;###autoload 
(defun ar-dollar-parentized-in-whitespaced-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-dollar (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-parentized-in-whitespaced-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-double-backslash (interactive-p)))

;;;###autoload 
(defun ar-doublequote-parentized-in-whitespaced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-doublequote (interactive-p)))

;;;###autoload 
(defun ar-doubleslash-parentized-in-whitespaced-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-doubleslash (interactive-p)))

;;;###autoload 
(defun ar-double-backslash-paren-parentized-in-whitespaced-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-double-backslash-paren (interactive-p)))

;;;###autoload 
(defun ar-end-parentized-in-whitespaced-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-end (interactive-p)))

;;;###autoload 
(defun ar-escape-parentized-in-whitespaced-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-escape (interactive-p)))

;;;###autoload 
(defun ar-hide-parentized-in-whitespaced-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-hide (interactive-p)))

;;;###autoload 
(defun ar-hide-show-parentized-in-whitespaced-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-hide-show (interactive-p)))

;;;###autoload 
(defun ar-hyphen-parentized-in-whitespaced-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-hyphen (interactive-p)))

;;;###autoload 
(defun ar-kill-parentized-in-whitespaced-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-kill (interactive-p)))

;;;###autoload 
(defun ar-left-right-singlequote-parentized-in-whitespaced-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-left-right-singlequote (interactive-p)))

;;;###autoload 
(defun ar-length-parentized-in-whitespaced-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-length (interactive-p)))

;;;###autoload 
(defun ar-parentize-parentized-in-whitespaced-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-parentize (interactive-p)))

;;;###autoload 
(defun ar-quote-parentized-in-whitespaced-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-quote (interactive-p)))

;;;###autoload 
(defun ar-separate-parentized-in-whitespaced-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-separate (interactive-p)))

;;;###autoload 
(defun ar-show-parentized-in-whitespaced-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-show (interactive-p)))

;;;###autoload 
(defun ar-singlequote-parentized-in-whitespaced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-singlequote (interactive-p)))

;;;###autoload 
(defun ar-slash-parentized-in-whitespaced-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-slash (interactive-p)))

;;;###autoload 
(defun ar-slashparen-parentized-in-whitespaced-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-slashparen (interactive-p)))

;;;###autoload 
(defun ar-sort-parentized-in-whitespaced-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-sort (interactive-p)))

;;;###autoload 
(defun ar-trim-parentized-in-whitespaced-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-trim (interactive-p)))

;;;###autoload 
(defun ar-trim-left-parentized-in-whitespaced-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-trim-left (interactive-p)))

;;;###autoload 
(defun ar-trim-right-parentized-in-whitespaced-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-trim-right (interactive-p)))

;;;###autoload 
(defun ar-underscore-parentized-in-whitespaced-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-underscore (interactive-p)))

;;;###autoload 
(defun ar-whitespace-parentized-in-whitespaced-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing WHITESPACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'whitespaced 'ar-th-whitespace (interactive-p)))

(provide 'thing-delimited-list-in-unpaired-delimited-list)
;;;thing-delimited-list-in-unpaired-delimited-list.el ends here

