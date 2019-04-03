;;; thing-at-point-utils.el --- th-at-point edit functions -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2019 Andreas Röhler, unless
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

(require 'thingatpt-utils-core)

;; ar-thing-at-point-utils-aktiv-passiv ar-unpaired-delimit-aktiv ar-atpt-rest-list: start
(defun ar-colon-greateranglednested-atpt (&optional arg)
  "Colon GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-colon 'greateranglednested arg arg))

(defun ar-colon-lesseranglednested-atpt (&optional arg)
  "Colon LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-colon 'lesseranglednested arg arg))

(defun ar-colon-buffer-atpt (&optional arg)
  "Colon BUFFER at point."
  (interactive "*P")
  (ar-th-colon 'buffer arg arg))

(defun ar-colon-char-atpt (&optional arg)
  "Colon CHAR at point."
  (interactive "*P")
  (ar-th-colon 'char arg arg))

(defun ar-colon-comment-atpt (&optional arg)
  "Colon COMMENT at point."
  (interactive "*P")
  (ar-th-colon 'comment arg arg))

(defun ar-colon-csv-atpt (&optional arg)
  "Colon CSV at point."
  (interactive "*P")
  (ar-th-colon 'csv arg arg))

(defun ar-colon-date-atpt (&optional arg)
  "Colon DATE at point."
  (interactive "*P")
  (ar-th-colon 'date arg arg))

(defun ar-colon-delimited-atpt (&optional arg)
  "Colon DELIMITED at point."
  (interactive "*P")
  (ar-th-colon 'delimited arg arg))

(defun ar-colon-email-atpt (&optional arg)
  "Colon EMAIL at point."
  (interactive "*P")
  (ar-th-colon 'email arg arg))

(defun ar-colon-filename-atpt (&optional arg)
  "Colon FILENAME at point."
  (interactive "*P")
  (ar-th-colon 'filename arg arg))

(defun ar-colon-filenamenondirectory-atpt (&optional arg)
  "Colon FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-colon 'filenamenondirectory arg arg))

(defun ar-colon-float-atpt (&optional arg)
  "Colon FLOAT at point."
  (interactive "*P")
  (ar-th-colon 'float arg arg))

(defun ar-colon-function-atpt (&optional arg)
  "Colon FUNCTION at point."
  (interactive "*P")
  (ar-th-colon 'function arg arg))

(defun ar-colon-ip-atpt (&optional arg)
  "Colon IP at point."
  (interactive "*P")
  (ar-th-colon 'ip arg arg))

(defun ar-colon-isbn-atpt (&optional arg)
  "Colon ISBN at point."
  (interactive "*P")
  (ar-th-colon 'isbn arg arg))

(defun ar-colon-line-atpt (&optional arg)
  "Colon LINE at point."
  (interactive "*P")
  (ar-th-colon 'line arg arg))

(defun ar-colon-list-atpt (&optional arg)
  "Colon LIST at point."
  (interactive "*P")
  (ar-th-colon 'list arg arg))

(defun ar-colon-name-atpt (&optional arg)
  "Colon NAME at point."
  (interactive "*P")
  (ar-th-colon 'name arg arg))

(defun ar-colon-number-atpt (&optional arg)
  "Colon NUMBER at point."
  (interactive "*P")
  (ar-th-colon 'number arg arg))

(defun ar-colon-page-atpt (&optional arg)
  "Colon PAGE at point."
  (interactive "*P")
  (ar-th-colon 'page arg arg))

(defun ar-colon-paragraph-atpt (&optional arg)
  "Colon PARAGRAPH at point."
  (interactive "*P")
  (ar-th-colon 'paragraph arg arg))

(defun ar-colon-phone-atpt (&optional arg)
  "Colon PHONE at point."
  (interactive "*P")
  (ar-th-colon 'phone arg arg))

(defun ar-colon-region-atpt (&optional arg)
  "Colon REGION at point."
  (interactive "*P")
  (ar-th-colon 'region arg arg))

(defun ar-colon-sentence-atpt (&optional arg)
  "Colon SENTENCE at point."
  (interactive "*P")
  (ar-th-colon 'sentence arg arg))

(defun ar-colon-sexp-atpt (&optional arg)
  "Colon SEXP at point."
  (interactive "*P")
  (ar-th-colon 'sexp arg arg))

(defun ar-colon-shstruct-atpt (&optional arg)
  "Colon SHSTRUCT at point."
  (interactive "*P")
  (ar-th-colon 'shstruct arg arg))

(defun ar-colon-symbol-atpt (&optional arg)
  "Colon SYMBOL at point."
  (interactive "*P")
  (ar-th-colon 'symbol arg arg))

(defun ar-colon-url-atpt (&optional arg)
  "Colon URL at point."
  (interactive "*P")
  (ar-th-colon 'url arg arg))

(defun ar-colon-word-atpt (&optional arg)
  "Colon WORD at point."
  (interactive "*P")
  (ar-th-colon 'word arg arg))

(defun ar-colon-wordalphaonly-atpt (&optional arg)
  "Colon WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-colon 'wordalphaonly arg arg))

(defun ar-cross-greateranglednested-atpt (&optional arg)
  "Cross GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-cross 'greateranglednested arg arg))

(defun ar-cross-lesseranglednested-atpt (&optional arg)
  "Cross LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-cross 'lesseranglednested arg arg))

(defun ar-cross-buffer-atpt (&optional arg)
  "Cross BUFFER at point."
  (interactive "*P")
  (ar-th-cross 'buffer arg arg))

(defun ar-cross-char-atpt (&optional arg)
  "Cross CHAR at point."
  (interactive "*P")
  (ar-th-cross 'char arg arg))

(defun ar-cross-comment-atpt (&optional arg)
  "Cross COMMENT at point."
  (interactive "*P")
  (ar-th-cross 'comment arg arg))

(defun ar-cross-csv-atpt (&optional arg)
  "Cross CSV at point."
  (interactive "*P")
  (ar-th-cross 'csv arg arg))

(defun ar-cross-date-atpt (&optional arg)
  "Cross DATE at point."
  (interactive "*P")
  (ar-th-cross 'date arg arg))

(defun ar-cross-delimited-atpt (&optional arg)
  "Cross DELIMITED at point."
  (interactive "*P")
  (ar-th-cross 'delimited arg arg))

(defun ar-cross-email-atpt (&optional arg)
  "Cross EMAIL at point."
  (interactive "*P")
  (ar-th-cross 'email arg arg))

(defun ar-cross-filename-atpt (&optional arg)
  "Cross FILENAME at point."
  (interactive "*P")
  (ar-th-cross 'filename arg arg))

(defun ar-cross-filenamenondirectory-atpt (&optional arg)
  "Cross FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-cross 'filenamenondirectory arg arg))

(defun ar-cross-float-atpt (&optional arg)
  "Cross FLOAT at point."
  (interactive "*P")
  (ar-th-cross 'float arg arg))

(defun ar-cross-function-atpt (&optional arg)
  "Cross FUNCTION at point."
  (interactive "*P")
  (ar-th-cross 'function arg arg))

(defun ar-cross-ip-atpt (&optional arg)
  "Cross IP at point."
  (interactive "*P")
  (ar-th-cross 'ip arg arg))

(defun ar-cross-isbn-atpt (&optional arg)
  "Cross ISBN at point."
  (interactive "*P")
  (ar-th-cross 'isbn arg arg))

(defun ar-cross-line-atpt (&optional arg)
  "Cross LINE at point."
  (interactive "*P")
  (ar-th-cross 'line arg arg))

(defun ar-cross-list-atpt (&optional arg)
  "Cross LIST at point."
  (interactive "*P")
  (ar-th-cross 'list arg arg))

(defun ar-cross-name-atpt (&optional arg)
  "Cross NAME at point."
  (interactive "*P")
  (ar-th-cross 'name arg arg))

(defun ar-cross-number-atpt (&optional arg)
  "Cross NUMBER at point."
  (interactive "*P")
  (ar-th-cross 'number arg arg))

(defun ar-cross-page-atpt (&optional arg)
  "Cross PAGE at point."
  (interactive "*P")
  (ar-th-cross 'page arg arg))

(defun ar-cross-paragraph-atpt (&optional arg)
  "Cross PARAGRAPH at point."
  (interactive "*P")
  (ar-th-cross 'paragraph arg arg))

(defun ar-cross-phone-atpt (&optional arg)
  "Cross PHONE at point."
  (interactive "*P")
  (ar-th-cross 'phone arg arg))

(defun ar-cross-region-atpt (&optional arg)
  "Cross REGION at point."
  (interactive "*P")
  (ar-th-cross 'region arg arg))

(defun ar-cross-sentence-atpt (&optional arg)
  "Cross SENTENCE at point."
  (interactive "*P")
  (ar-th-cross 'sentence arg arg))

(defun ar-cross-sexp-atpt (&optional arg)
  "Cross SEXP at point."
  (interactive "*P")
  (ar-th-cross 'sexp arg arg))

(defun ar-cross-shstruct-atpt (&optional arg)
  "Cross SHSTRUCT at point."
  (interactive "*P")
  (ar-th-cross 'shstruct arg arg))

(defun ar-cross-symbol-atpt (&optional arg)
  "Cross SYMBOL at point."
  (interactive "*P")
  (ar-th-cross 'symbol arg arg))

(defun ar-cross-url-atpt (&optional arg)
  "Cross URL at point."
  (interactive "*P")
  (ar-th-cross 'url arg arg))

(defun ar-cross-word-atpt (&optional arg)
  "Cross WORD at point."
  (interactive "*P")
  (ar-th-cross 'word arg arg))

(defun ar-cross-wordalphaonly-atpt (&optional arg)
  "Cross WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-cross 'wordalphaonly arg arg))

(defun ar-doubleslash-greateranglednested-atpt (&optional arg)
  "Doubleslash GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-doubleslash 'greateranglednested arg arg))

(defun ar-doubleslash-lesseranglednested-atpt (&optional arg)
  "Doubleslash LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-doubleslash 'lesseranglednested arg arg))

(defun ar-doubleslash-buffer-atpt (&optional arg)
  "Doubleslash BUFFER at point."
  (interactive "*P")
  (ar-th-doubleslash 'buffer arg arg))

(defun ar-doubleslash-char-atpt (&optional arg)
  "Doubleslash CHAR at point."
  (interactive "*P")
  (ar-th-doubleslash 'char arg arg))

(defun ar-doubleslash-comment-atpt (&optional arg)
  "Doubleslash COMMENT at point."
  (interactive "*P")
  (ar-th-doubleslash 'comment arg arg))

(defun ar-doubleslash-csv-atpt (&optional arg)
  "Doubleslash CSV at point."
  (interactive "*P")
  (ar-th-doubleslash 'csv arg arg))

(defun ar-doubleslash-date-atpt (&optional arg)
  "Doubleslash DATE at point."
  (interactive "*P")
  (ar-th-doubleslash 'date arg arg))

(defun ar-doubleslash-delimited-atpt (&optional arg)
  "Doubleslash DELIMITED at point."
  (interactive "*P")
  (ar-th-doubleslash 'delimited arg arg))

(defun ar-doubleslash-email-atpt (&optional arg)
  "Doubleslash EMAIL at point."
  (interactive "*P")
  (ar-th-doubleslash 'email arg arg))

(defun ar-doubleslash-filename-atpt (&optional arg)
  "Doubleslash FILENAME at point."
  (interactive "*P")
  (ar-th-doubleslash 'filename arg arg))

(defun ar-doubleslash-filenamenondirectory-atpt (&optional arg)
  "Doubleslash FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-doubleslash 'filenamenondirectory arg arg))

(defun ar-doubleslash-float-atpt (&optional arg)
  "Doubleslash FLOAT at point."
  (interactive "*P")
  (ar-th-doubleslash 'float arg arg))

(defun ar-doubleslash-function-atpt (&optional arg)
  "Doubleslash FUNCTION at point."
  (interactive "*P")
  (ar-th-doubleslash 'function arg arg))

(defun ar-doubleslash-ip-atpt (&optional arg)
  "Doubleslash IP at point."
  (interactive "*P")
  (ar-th-doubleslash 'ip arg arg))

(defun ar-doubleslash-isbn-atpt (&optional arg)
  "Doubleslash ISBN at point."
  (interactive "*P")
  (ar-th-doubleslash 'isbn arg arg))

(defun ar-doubleslash-line-atpt (&optional arg)
  "Doubleslash LINE at point."
  (interactive "*P")
  (ar-th-doubleslash 'line arg arg))

(defun ar-doubleslash-list-atpt (&optional arg)
  "Doubleslash LIST at point."
  (interactive "*P")
  (ar-th-doubleslash 'list arg arg))

(defun ar-doubleslash-name-atpt (&optional arg)
  "Doubleslash NAME at point."
  (interactive "*P")
  (ar-th-doubleslash 'name arg arg))

(defun ar-doubleslash-number-atpt (&optional arg)
  "Doubleslash NUMBER at point."
  (interactive "*P")
  (ar-th-doubleslash 'number arg arg))

(defun ar-doubleslash-page-atpt (&optional arg)
  "Doubleslash PAGE at point."
  (interactive "*P")
  (ar-th-doubleslash 'page arg arg))

(defun ar-doubleslash-paragraph-atpt (&optional arg)
  "Doubleslash PARAGRAPH at point."
  (interactive "*P")
  (ar-th-doubleslash 'paragraph arg arg))

(defun ar-doubleslash-phone-atpt (&optional arg)
  "Doubleslash PHONE at point."
  (interactive "*P")
  (ar-th-doubleslash 'phone arg arg))

(defun ar-doubleslash-region-atpt (&optional arg)
  "Doubleslash REGION at point."
  (interactive "*P")
  (ar-th-doubleslash 'region arg arg))

(defun ar-doubleslash-sentence-atpt (&optional arg)
  "Doubleslash SENTENCE at point."
  (interactive "*P")
  (ar-th-doubleslash 'sentence arg arg))

(defun ar-doubleslash-sexp-atpt (&optional arg)
  "Doubleslash SEXP at point."
  (interactive "*P")
  (ar-th-doubleslash 'sexp arg arg))

(defun ar-doubleslash-shstruct-atpt (&optional arg)
  "Doubleslash SHSTRUCT at point."
  (interactive "*P")
  (ar-th-doubleslash 'shstruct arg arg))

(defun ar-doubleslash-symbol-atpt (&optional arg)
  "Doubleslash SYMBOL at point."
  (interactive "*P")
  (ar-th-doubleslash 'symbol arg arg))

(defun ar-doubleslash-url-atpt (&optional arg)
  "Doubleslash URL at point."
  (interactive "*P")
  (ar-th-doubleslash 'url arg arg))

(defun ar-doubleslash-word-atpt (&optional arg)
  "Doubleslash WORD at point."
  (interactive "*P")
  (ar-th-doubleslash 'word arg arg))

(defun ar-doubleslash-wordalphaonly-atpt (&optional arg)
  "Doubleslash WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-doubleslash 'wordalphaonly arg arg))

(defun ar-backslash-greateranglednested-atpt (&optional arg)
  "Backslash GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-backslash 'greateranglednested arg arg))

(defun ar-backslash-lesseranglednested-atpt (&optional arg)
  "Backslash LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-backslash 'lesseranglednested arg arg))

(defun ar-backslash-buffer-atpt (&optional arg)
  "Backslash BUFFER at point."
  (interactive "*P")
  (ar-th-backslash 'buffer arg arg))

(defun ar-backslash-char-atpt (&optional arg)
  "Backslash CHAR at point."
  (interactive "*P")
  (ar-th-backslash 'char arg arg))

(defun ar-backslash-comment-atpt (&optional arg)
  "Backslash COMMENT at point."
  (interactive "*P")
  (ar-th-backslash 'comment arg arg))

(defun ar-backslash-csv-atpt (&optional arg)
  "Backslash CSV at point."
  (interactive "*P")
  (ar-th-backslash 'csv arg arg))

(defun ar-backslash-date-atpt (&optional arg)
  "Backslash DATE at point."
  (interactive "*P")
  (ar-th-backslash 'date arg arg))

(defun ar-backslash-delimited-atpt (&optional arg)
  "Backslash DELIMITED at point."
  (interactive "*P")
  (ar-th-backslash 'delimited arg arg))

(defun ar-backslash-email-atpt (&optional arg)
  "Backslash EMAIL at point."
  (interactive "*P")
  (ar-th-backslash 'email arg arg))

(defun ar-backslash-filename-atpt (&optional arg)
  "Backslash FILENAME at point."
  (interactive "*P")
  (ar-th-backslash 'filename arg arg))

(defun ar-backslash-filenamenondirectory-atpt (&optional arg)
  "Backslash FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-backslash 'filenamenondirectory arg arg))

(defun ar-backslash-float-atpt (&optional arg)
  "Backslash FLOAT at point."
  (interactive "*P")
  (ar-th-backslash 'float arg arg))

(defun ar-backslash-function-atpt (&optional arg)
  "Backslash FUNCTION at point."
  (interactive "*P")
  (ar-th-backslash 'function arg arg))

(defun ar-backslash-ip-atpt (&optional arg)
  "Backslash IP at point."
  (interactive "*P")
  (ar-th-backslash 'ip arg arg))

(defun ar-backslash-isbn-atpt (&optional arg)
  "Backslash ISBN at point."
  (interactive "*P")
  (ar-th-backslash 'isbn arg arg))

(defun ar-backslash-line-atpt (&optional arg)
  "Backslash LINE at point."
  (interactive "*P")
  (ar-th-backslash 'line arg arg))

(defun ar-backslash-list-atpt (&optional arg)
  "Backslash LIST at point."
  (interactive "*P")
  (ar-th-backslash 'list arg arg))

(defun ar-backslash-name-atpt (&optional arg)
  "Backslash NAME at point."
  (interactive "*P")
  (ar-th-backslash 'name arg arg))

(defun ar-backslash-number-atpt (&optional arg)
  "Backslash NUMBER at point."
  (interactive "*P")
  (ar-th-backslash 'number arg arg))

(defun ar-backslash-page-atpt (&optional arg)
  "Backslash PAGE at point."
  (interactive "*P")
  (ar-th-backslash 'page arg arg))

(defun ar-backslash-paragraph-atpt (&optional arg)
  "Backslash PARAGRAPH at point."
  (interactive "*P")
  (ar-th-backslash 'paragraph arg arg))

(defun ar-backslash-phone-atpt (&optional arg)
  "Backslash PHONE at point."
  (interactive "*P")
  (ar-th-backslash 'phone arg arg))

(defun ar-backslash-region-atpt (&optional arg)
  "Backslash REGION at point."
  (interactive "*P")
  (ar-th-backslash 'region arg arg))

(defun ar-backslash-sentence-atpt (&optional arg)
  "Backslash SENTENCE at point."
  (interactive "*P")
  (ar-th-backslash 'sentence arg arg))

(defun ar-backslash-sexp-atpt (&optional arg)
  "Backslash SEXP at point."
  (interactive "*P")
  (ar-th-backslash 'sexp arg arg))

(defun ar-backslash-shstruct-atpt (&optional arg)
  "Backslash SHSTRUCT at point."
  (interactive "*P")
  (ar-th-backslash 'shstruct arg arg))

(defun ar-backslash-symbol-atpt (&optional arg)
  "Backslash SYMBOL at point."
  (interactive "*P")
  (ar-th-backslash 'symbol arg arg))

(defun ar-backslash-url-atpt (&optional arg)
  "Backslash URL at point."
  (interactive "*P")
  (ar-th-backslash 'url arg arg))

(defun ar-backslash-word-atpt (&optional arg)
  "Backslash WORD at point."
  (interactive "*P")
  (ar-th-backslash 'word arg arg))

(defun ar-backslash-wordalphaonly-atpt (&optional arg)
  "Backslash WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-backslash 'wordalphaonly arg arg))

(defun ar-backtick-greateranglednested-atpt (&optional arg)
  "Backtick GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-backtick 'greateranglednested arg arg))

(defun ar-backtick-lesseranglednested-atpt (&optional arg)
  "Backtick LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-backtick 'lesseranglednested arg arg))

(defun ar-backtick-buffer-atpt (&optional arg)
  "Backtick BUFFER at point."
  (interactive "*P")
  (ar-th-backtick 'buffer arg arg))

(defun ar-backtick-char-atpt (&optional arg)
  "Backtick CHAR at point."
  (interactive "*P")
  (ar-th-backtick 'char arg arg))

(defun ar-backtick-comment-atpt (&optional arg)
  "Backtick COMMENT at point."
  (interactive "*P")
  (ar-th-backtick 'comment arg arg))

(defun ar-backtick-csv-atpt (&optional arg)
  "Backtick CSV at point."
  (interactive "*P")
  (ar-th-backtick 'csv arg arg))

(defun ar-backtick-date-atpt (&optional arg)
  "Backtick DATE at point."
  (interactive "*P")
  (ar-th-backtick 'date arg arg))

(defun ar-backtick-delimited-atpt (&optional arg)
  "Backtick DELIMITED at point."
  (interactive "*P")
  (ar-th-backtick 'delimited arg arg))

(defun ar-backtick-email-atpt (&optional arg)
  "Backtick EMAIL at point."
  (interactive "*P")
  (ar-th-backtick 'email arg arg))

(defun ar-backtick-filename-atpt (&optional arg)
  "Backtick FILENAME at point."
  (interactive "*P")
  (ar-th-backtick 'filename arg arg))

(defun ar-backtick-filenamenondirectory-atpt (&optional arg)
  "Backtick FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-backtick 'filenamenondirectory arg arg))

(defun ar-backtick-float-atpt (&optional arg)
  "Backtick FLOAT at point."
  (interactive "*P")
  (ar-th-backtick 'float arg arg))

(defun ar-backtick-function-atpt (&optional arg)
  "Backtick FUNCTION at point."
  (interactive "*P")
  (ar-th-backtick 'function arg arg))

(defun ar-backtick-ip-atpt (&optional arg)
  "Backtick IP at point."
  (interactive "*P")
  (ar-th-backtick 'ip arg arg))

(defun ar-backtick-isbn-atpt (&optional arg)
  "Backtick ISBN at point."
  (interactive "*P")
  (ar-th-backtick 'isbn arg arg))

(defun ar-backtick-line-atpt (&optional arg)
  "Backtick LINE at point."
  (interactive "*P")
  (ar-th-backtick 'line arg arg))

(defun ar-backtick-list-atpt (&optional arg)
  "Backtick LIST at point."
  (interactive "*P")
  (ar-th-backtick 'list arg arg))

(defun ar-backtick-name-atpt (&optional arg)
  "Backtick NAME at point."
  (interactive "*P")
  (ar-th-backtick 'name arg arg))

(defun ar-backtick-number-atpt (&optional arg)
  "Backtick NUMBER at point."
  (interactive "*P")
  (ar-th-backtick 'number arg arg))

(defun ar-backtick-page-atpt (&optional arg)
  "Backtick PAGE at point."
  (interactive "*P")
  (ar-th-backtick 'page arg arg))

(defun ar-backtick-paragraph-atpt (&optional arg)
  "Backtick PARAGRAPH at point."
  (interactive "*P")
  (ar-th-backtick 'paragraph arg arg))

(defun ar-backtick-phone-atpt (&optional arg)
  "Backtick PHONE at point."
  (interactive "*P")
  (ar-th-backtick 'phone arg arg))

(defun ar-backtick-region-atpt (&optional arg)
  "Backtick REGION at point."
  (interactive "*P")
  (ar-th-backtick 'region arg arg))

(defun ar-backtick-sentence-atpt (&optional arg)
  "Backtick SENTENCE at point."
  (interactive "*P")
  (ar-th-backtick 'sentence arg arg))

(defun ar-backtick-sexp-atpt (&optional arg)
  "Backtick SEXP at point."
  (interactive "*P")
  (ar-th-backtick 'sexp arg arg))

(defun ar-backtick-shstruct-atpt (&optional arg)
  "Backtick SHSTRUCT at point."
  (interactive "*P")
  (ar-th-backtick 'shstruct arg arg))

(defun ar-backtick-symbol-atpt (&optional arg)
  "Backtick SYMBOL at point."
  (interactive "*P")
  (ar-th-backtick 'symbol arg arg))

(defun ar-backtick-url-atpt (&optional arg)
  "Backtick URL at point."
  (interactive "*P")
  (ar-th-backtick 'url arg arg))

(defun ar-backtick-word-atpt (&optional arg)
  "Backtick WORD at point."
  (interactive "*P")
  (ar-th-backtick 'word arg arg))

(defun ar-backtick-wordalphaonly-atpt (&optional arg)
  "Backtick WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-backtick 'wordalphaonly arg arg))

(defun ar-dollar-greateranglednested-atpt (&optional arg)
  "Dollar GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-dollar 'greateranglednested arg arg))

(defun ar-dollar-lesseranglednested-atpt (&optional arg)
  "Dollar LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-dollar 'lesseranglednested arg arg))

(defun ar-dollar-buffer-atpt (&optional arg)
  "Dollar BUFFER at point."
  (interactive "*P")
  (ar-th-dollar 'buffer arg arg))

(defun ar-dollar-char-atpt (&optional arg)
  "Dollar CHAR at point."
  (interactive "*P")
  (ar-th-dollar 'char arg arg))

(defun ar-dollar-comment-atpt (&optional arg)
  "Dollar COMMENT at point."
  (interactive "*P")
  (ar-th-dollar 'comment arg arg))

(defun ar-dollar-csv-atpt (&optional arg)
  "Dollar CSV at point."
  (interactive "*P")
  (ar-th-dollar 'csv arg arg))

(defun ar-dollar-date-atpt (&optional arg)
  "Dollar DATE at point."
  (interactive "*P")
  (ar-th-dollar 'date arg arg))

(defun ar-dollar-delimited-atpt (&optional arg)
  "Dollar DELIMITED at point."
  (interactive "*P")
  (ar-th-dollar 'delimited arg arg))

(defun ar-dollar-email-atpt (&optional arg)
  "Dollar EMAIL at point."
  (interactive "*P")
  (ar-th-dollar 'email arg arg))

(defun ar-dollar-filename-atpt (&optional arg)
  "Dollar FILENAME at point."
  (interactive "*P")
  (ar-th-dollar 'filename arg arg))

(defun ar-dollar-filenamenondirectory-atpt (&optional arg)
  "Dollar FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-dollar 'filenamenondirectory arg arg))

(defun ar-dollar-float-atpt (&optional arg)
  "Dollar FLOAT at point."
  (interactive "*P")
  (ar-th-dollar 'float arg arg))

(defun ar-dollar-function-atpt (&optional arg)
  "Dollar FUNCTION at point."
  (interactive "*P")
  (ar-th-dollar 'function arg arg))

(defun ar-dollar-ip-atpt (&optional arg)
  "Dollar IP at point."
  (interactive "*P")
  (ar-th-dollar 'ip arg arg))

(defun ar-dollar-isbn-atpt (&optional arg)
  "Dollar ISBN at point."
  (interactive "*P")
  (ar-th-dollar 'isbn arg arg))

(defun ar-dollar-line-atpt (&optional arg)
  "Dollar LINE at point."
  (interactive "*P")
  (ar-th-dollar 'line arg arg))

(defun ar-dollar-list-atpt (&optional arg)
  "Dollar LIST at point."
  (interactive "*P")
  (ar-th-dollar 'list arg arg))

(defun ar-dollar-name-atpt (&optional arg)
  "Dollar NAME at point."
  (interactive "*P")
  (ar-th-dollar 'name arg arg))

(defun ar-dollar-number-atpt (&optional arg)
  "Dollar NUMBER at point."
  (interactive "*P")
  (ar-th-dollar 'number arg arg))

(defun ar-dollar-page-atpt (&optional arg)
  "Dollar PAGE at point."
  (interactive "*P")
  (ar-th-dollar 'page arg arg))

(defun ar-dollar-paragraph-atpt (&optional arg)
  "Dollar PARAGRAPH at point."
  (interactive "*P")
  (ar-th-dollar 'paragraph arg arg))

(defun ar-dollar-phone-atpt (&optional arg)
  "Dollar PHONE at point."
  (interactive "*P")
  (ar-th-dollar 'phone arg arg))

(defun ar-dollar-region-atpt (&optional arg)
  "Dollar REGION at point."
  (interactive "*P")
  (ar-th-dollar 'region arg arg))

(defun ar-dollar-sentence-atpt (&optional arg)
  "Dollar SENTENCE at point."
  (interactive "*P")
  (ar-th-dollar 'sentence arg arg))

(defun ar-dollar-sexp-atpt (&optional arg)
  "Dollar SEXP at point."
  (interactive "*P")
  (ar-th-dollar 'sexp arg arg))

(defun ar-dollar-shstruct-atpt (&optional arg)
  "Dollar SHSTRUCT at point."
  (interactive "*P")
  (ar-th-dollar 'shstruct arg arg))

(defun ar-dollar-symbol-atpt (&optional arg)
  "Dollar SYMBOL at point."
  (interactive "*P")
  (ar-th-dollar 'symbol arg arg))

(defun ar-dollar-url-atpt (&optional arg)
  "Dollar URL at point."
  (interactive "*P")
  (ar-th-dollar 'url arg arg))

(defun ar-dollar-word-atpt (&optional arg)
  "Dollar WORD at point."
  (interactive "*P")
  (ar-th-dollar 'word arg arg))

(defun ar-dollar-wordalphaonly-atpt (&optional arg)
  "Dollar WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-dollar 'wordalphaonly arg arg))

(defun ar-doublebacktick-greateranglednested-atpt (&optional arg)
  "Doublebacktick GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-doublebacktick 'greateranglednested arg arg))

(defun ar-doublebacktick-lesseranglednested-atpt (&optional arg)
  "Doublebacktick LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-doublebacktick 'lesseranglednested arg arg))

(defun ar-doublebacktick-buffer-atpt (&optional arg)
  "Doublebacktick BUFFER at point."
  (interactive "*P")
  (ar-th-doublebacktick 'buffer arg arg))

(defun ar-doublebacktick-char-atpt (&optional arg)
  "Doublebacktick CHAR at point."
  (interactive "*P")
  (ar-th-doublebacktick 'char arg arg))

(defun ar-doublebacktick-comment-atpt (&optional arg)
  "Doublebacktick COMMENT at point."
  (interactive "*P")
  (ar-th-doublebacktick 'comment arg arg))

(defun ar-doublebacktick-csv-atpt (&optional arg)
  "Doublebacktick CSV at point."
  (interactive "*P")
  (ar-th-doublebacktick 'csv arg arg))

(defun ar-doublebacktick-date-atpt (&optional arg)
  "Doublebacktick DATE at point."
  (interactive "*P")
  (ar-th-doublebacktick 'date arg arg))

(defun ar-doublebacktick-delimited-atpt (&optional arg)
  "Doublebacktick DELIMITED at point."
  (interactive "*P")
  (ar-th-doublebacktick 'delimited arg arg))

(defun ar-doublebacktick-email-atpt (&optional arg)
  "Doublebacktick EMAIL at point."
  (interactive "*P")
  (ar-th-doublebacktick 'email arg arg))

(defun ar-doublebacktick-filename-atpt (&optional arg)
  "Doublebacktick FILENAME at point."
  (interactive "*P")
  (ar-th-doublebacktick 'filename arg arg))

(defun ar-doublebacktick-filenamenondirectory-atpt (&optional arg)
  "Doublebacktick FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-doublebacktick 'filenamenondirectory arg arg))

(defun ar-doublebacktick-float-atpt (&optional arg)
  "Doublebacktick FLOAT at point."
  (interactive "*P")
  (ar-th-doublebacktick 'float arg arg))

(defun ar-doublebacktick-function-atpt (&optional arg)
  "Doublebacktick FUNCTION at point."
  (interactive "*P")
  (ar-th-doublebacktick 'function arg arg))

(defun ar-doublebacktick-ip-atpt (&optional arg)
  "Doublebacktick IP at point."
  (interactive "*P")
  (ar-th-doublebacktick 'ip arg arg))

(defun ar-doublebacktick-isbn-atpt (&optional arg)
  "Doublebacktick ISBN at point."
  (interactive "*P")
  (ar-th-doublebacktick 'isbn arg arg))

(defun ar-doublebacktick-line-atpt (&optional arg)
  "Doublebacktick LINE at point."
  (interactive "*P")
  (ar-th-doublebacktick 'line arg arg))

(defun ar-doublebacktick-list-atpt (&optional arg)
  "Doublebacktick LIST at point."
  (interactive "*P")
  (ar-th-doublebacktick 'list arg arg))

(defun ar-doublebacktick-name-atpt (&optional arg)
  "Doublebacktick NAME at point."
  (interactive "*P")
  (ar-th-doublebacktick 'name arg arg))

(defun ar-doublebacktick-number-atpt (&optional arg)
  "Doublebacktick NUMBER at point."
  (interactive "*P")
  (ar-th-doublebacktick 'number arg arg))

(defun ar-doublebacktick-page-atpt (&optional arg)
  "Doublebacktick PAGE at point."
  (interactive "*P")
  (ar-th-doublebacktick 'page arg arg))

(defun ar-doublebacktick-paragraph-atpt (&optional arg)
  "Doublebacktick PARAGRAPH at point."
  (interactive "*P")
  (ar-th-doublebacktick 'paragraph arg arg))

(defun ar-doublebacktick-phone-atpt (&optional arg)
  "Doublebacktick PHONE at point."
  (interactive "*P")
  (ar-th-doublebacktick 'phone arg arg))

(defun ar-doublebacktick-region-atpt (&optional arg)
  "Doublebacktick REGION at point."
  (interactive "*P")
  (ar-th-doublebacktick 'region arg arg))

(defun ar-doublebacktick-sentence-atpt (&optional arg)
  "Doublebacktick SENTENCE at point."
  (interactive "*P")
  (ar-th-doublebacktick 'sentence arg arg))

(defun ar-doublebacktick-sexp-atpt (&optional arg)
  "Doublebacktick SEXP at point."
  (interactive "*P")
  (ar-th-doublebacktick 'sexp arg arg))

(defun ar-doublebacktick-shstruct-atpt (&optional arg)
  "Doublebacktick SHSTRUCT at point."
  (interactive "*P")
  (ar-th-doublebacktick 'shstruct arg arg))

(defun ar-doublebacktick-symbol-atpt (&optional arg)
  "Doublebacktick SYMBOL at point."
  (interactive "*P")
  (ar-th-doublebacktick 'symbol arg arg))

(defun ar-doublebacktick-url-atpt (&optional arg)
  "Doublebacktick URL at point."
  (interactive "*P")
  (ar-th-doublebacktick 'url arg arg))

(defun ar-doublebacktick-word-atpt (&optional arg)
  "Doublebacktick WORD at point."
  (interactive "*P")
  (ar-th-doublebacktick 'word arg arg))

(defun ar-doublebacktick-wordalphaonly-atpt (&optional arg)
  "Doublebacktick WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-doublebacktick 'wordalphaonly arg arg))

(defun ar-doublequote-greateranglednested-atpt (&optional arg)
  "Doublequote GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-doublequote 'greateranglednested arg arg))

(defun ar-doublequote-lesseranglednested-atpt (&optional arg)
  "Doublequote LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-doublequote 'lesseranglednested arg arg))

(defun ar-doublequote-buffer-atpt (&optional arg)
  "Doublequote BUFFER at point."
  (interactive "*P")
  (ar-th-doublequote 'buffer arg arg))

(defun ar-doublequote-char-atpt (&optional arg)
  "Doublequote CHAR at point."
  (interactive "*P")
  (ar-th-doublequote 'char arg arg))

(defun ar-doublequote-comment-atpt (&optional arg)
  "Doublequote COMMENT at point."
  (interactive "*P")
  (ar-th-doublequote 'comment arg arg))

(defun ar-doublequote-csv-atpt (&optional arg)
  "Doublequote CSV at point."
  (interactive "*P")
  (ar-th-doublequote 'csv arg arg))

(defun ar-doublequote-date-atpt (&optional arg)
  "Doublequote DATE at point."
  (interactive "*P")
  (ar-th-doublequote 'date arg arg))

(defun ar-doublequote-delimited-atpt (&optional arg)
  "Doublequote DELIMITED at point."
  (interactive "*P")
  (ar-th-doublequote 'delimited arg arg))

(defun ar-doublequote-email-atpt (&optional arg)
  "Doublequote EMAIL at point."
  (interactive "*P")
  (ar-th-doublequote 'email arg arg))

(defun ar-doublequote-filename-atpt (&optional arg)
  "Doublequote FILENAME at point."
  (interactive "*P")
  (ar-th-doublequote 'filename arg arg))

(defun ar-doublequote-filenamenondirectory-atpt (&optional arg)
  "Doublequote FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-doublequote 'filenamenondirectory arg arg))

(defun ar-doublequote-float-atpt (&optional arg)
  "Doublequote FLOAT at point."
  (interactive "*P")
  (ar-th-doublequote 'float arg arg))

(defun ar-doublequote-function-atpt (&optional arg)
  "Doublequote FUNCTION at point."
  (interactive "*P")
  (ar-th-doublequote 'function arg arg))

(defun ar-doublequote-ip-atpt (&optional arg)
  "Doublequote IP at point."
  (interactive "*P")
  (ar-th-doublequote 'ip arg arg))

(defun ar-doublequote-isbn-atpt (&optional arg)
  "Doublequote ISBN at point."
  (interactive "*P")
  (ar-th-doublequote 'isbn arg arg))

(defun ar-doublequote-line-atpt (&optional arg)
  "Doublequote LINE at point."
  (interactive "*P")
  (ar-th-doublequote 'line arg arg))

(defun ar-doublequote-list-atpt (&optional arg)
  "Doublequote LIST at point."
  (interactive "*P")
  (ar-th-doublequote 'list arg arg))

(defun ar-doublequote-name-atpt (&optional arg)
  "Doublequote NAME at point."
  (interactive "*P")
  (ar-th-doublequote 'name arg arg))

(defun ar-doublequote-number-atpt (&optional arg)
  "Doublequote NUMBER at point."
  (interactive "*P")
  (ar-th-doublequote 'number arg arg))

(defun ar-doublequote-page-atpt (&optional arg)
  "Doublequote PAGE at point."
  (interactive "*P")
  (ar-th-doublequote 'page arg arg))

(defun ar-doublequote-paragraph-atpt (&optional arg)
  "Doublequote PARAGRAPH at point."
  (interactive "*P")
  (ar-th-doublequote 'paragraph arg arg))

(defun ar-doublequote-phone-atpt (&optional arg)
  "Doublequote PHONE at point."
  (interactive "*P")
  (ar-th-doublequote 'phone arg arg))

(defun ar-doublequote-region-atpt (&optional arg)
  "Doublequote REGION at point."
  (interactive "*P")
  (ar-th-doublequote 'region arg arg))

(defun ar-doublequote-sentence-atpt (&optional arg)
  "Doublequote SENTENCE at point."
  (interactive "*P")
  (ar-th-doublequote 'sentence arg arg))

(defun ar-doublequote-sexp-atpt (&optional arg)
  "Doublequote SEXP at point."
  (interactive "*P")
  (ar-th-doublequote 'sexp arg arg))

(defun ar-doublequote-shstruct-atpt (&optional arg)
  "Doublequote SHSTRUCT at point."
  (interactive "*P")
  (ar-th-doublequote 'shstruct arg arg))

(defun ar-doublequote-symbol-atpt (&optional arg)
  "Doublequote SYMBOL at point."
  (interactive "*P")
  (ar-th-doublequote 'symbol arg arg))

(defun ar-doublequote-url-atpt (&optional arg)
  "Doublequote URL at point."
  (interactive "*P")
  (ar-th-doublequote 'url arg arg))

(defun ar-doublequote-word-atpt (&optional arg)
  "Doublequote WORD at point."
  (interactive "*P")
  (ar-th-doublequote 'word arg arg))

(defun ar-doublequote-wordalphaonly-atpt (&optional arg)
  "Doublequote WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-doublequote 'wordalphaonly arg arg))

(defun ar-equalize-greateranglednested-atpt (&optional arg)
  "Equalize GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-equalize 'greateranglednested arg arg))

(defun ar-equalize-lesseranglednested-atpt (&optional arg)
  "Equalize LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-equalize 'lesseranglednested arg arg))

(defun ar-equalize-buffer-atpt (&optional arg)
  "Equalize BUFFER at point."
  (interactive "*P")
  (ar-th-equalize 'buffer arg arg))

(defun ar-equalize-char-atpt (&optional arg)
  "Equalize CHAR at point."
  (interactive "*P")
  (ar-th-equalize 'char arg arg))

(defun ar-equalize-comment-atpt (&optional arg)
  "Equalize COMMENT at point."
  (interactive "*P")
  (ar-th-equalize 'comment arg arg))

(defun ar-equalize-csv-atpt (&optional arg)
  "Equalize CSV at point."
  (interactive "*P")
  (ar-th-equalize 'csv arg arg))

(defun ar-equalize-date-atpt (&optional arg)
  "Equalize DATE at point."
  (interactive "*P")
  (ar-th-equalize 'date arg arg))

(defun ar-equalize-delimited-atpt (&optional arg)
  "Equalize DELIMITED at point."
  (interactive "*P")
  (ar-th-equalize 'delimited arg arg))

(defun ar-equalize-email-atpt (&optional arg)
  "Equalize EMAIL at point."
  (interactive "*P")
  (ar-th-equalize 'email arg arg))

(defun ar-equalize-filename-atpt (&optional arg)
  "Equalize FILENAME at point."
  (interactive "*P")
  (ar-th-equalize 'filename arg arg))

(defun ar-equalize-filenamenondirectory-atpt (&optional arg)
  "Equalize FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-equalize 'filenamenondirectory arg arg))

(defun ar-equalize-float-atpt (&optional arg)
  "Equalize FLOAT at point."
  (interactive "*P")
  (ar-th-equalize 'float arg arg))

(defun ar-equalize-function-atpt (&optional arg)
  "Equalize FUNCTION at point."
  (interactive "*P")
  (ar-th-equalize 'function arg arg))

(defun ar-equalize-ip-atpt (&optional arg)
  "Equalize IP at point."
  (interactive "*P")
  (ar-th-equalize 'ip arg arg))

(defun ar-equalize-isbn-atpt (&optional arg)
  "Equalize ISBN at point."
  (interactive "*P")
  (ar-th-equalize 'isbn arg arg))

(defun ar-equalize-line-atpt (&optional arg)
  "Equalize LINE at point."
  (interactive "*P")
  (ar-th-equalize 'line arg arg))

(defun ar-equalize-list-atpt (&optional arg)
  "Equalize LIST at point."
  (interactive "*P")
  (ar-th-equalize 'list arg arg))

(defun ar-equalize-name-atpt (&optional arg)
  "Equalize NAME at point."
  (interactive "*P")
  (ar-th-equalize 'name arg arg))

(defun ar-equalize-number-atpt (&optional arg)
  "Equalize NUMBER at point."
  (interactive "*P")
  (ar-th-equalize 'number arg arg))

(defun ar-equalize-page-atpt (&optional arg)
  "Equalize PAGE at point."
  (interactive "*P")
  (ar-th-equalize 'page arg arg))

(defun ar-equalize-paragraph-atpt (&optional arg)
  "Equalize PARAGRAPH at point."
  (interactive "*P")
  (ar-th-equalize 'paragraph arg arg))

(defun ar-equalize-phone-atpt (&optional arg)
  "Equalize PHONE at point."
  (interactive "*P")
  (ar-th-equalize 'phone arg arg))

(defun ar-equalize-region-atpt (&optional arg)
  "Equalize REGION at point."
  (interactive "*P")
  (ar-th-equalize 'region arg arg))

(defun ar-equalize-sentence-atpt (&optional arg)
  "Equalize SENTENCE at point."
  (interactive "*P")
  (ar-th-equalize 'sentence arg arg))

(defun ar-equalize-sexp-atpt (&optional arg)
  "Equalize SEXP at point."
  (interactive "*P")
  (ar-th-equalize 'sexp arg arg))

(defun ar-equalize-shstruct-atpt (&optional arg)
  "Equalize SHSTRUCT at point."
  (interactive "*P")
  (ar-th-equalize 'shstruct arg arg))

(defun ar-equalize-symbol-atpt (&optional arg)
  "Equalize SYMBOL at point."
  (interactive "*P")
  (ar-th-equalize 'symbol arg arg))

(defun ar-equalize-url-atpt (&optional arg)
  "Equalize URL at point."
  (interactive "*P")
  (ar-th-equalize 'url arg arg))

(defun ar-equalize-word-atpt (&optional arg)
  "Equalize WORD at point."
  (interactive "*P")
  (ar-th-equalize 'word arg arg))

(defun ar-equalize-wordalphaonly-atpt (&optional arg)
  "Equalize WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-equalize 'wordalphaonly arg arg))

(defun ar-escape-greateranglednested-atpt (&optional arg)
  "Escape GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-escape 'greateranglednested arg arg))

(defun ar-escape-lesseranglednested-atpt (&optional arg)
  "Escape LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-escape 'lesseranglednested arg arg))

(defun ar-escape-buffer-atpt (&optional arg)
  "Escape BUFFER at point."
  (interactive "*P")
  (ar-th-escape 'buffer arg arg))

(defun ar-escape-char-atpt (&optional arg)
  "Escape CHAR at point."
  (interactive "*P")
  (ar-th-escape 'char arg arg))

(defun ar-escape-comment-atpt (&optional arg)
  "Escape COMMENT at point."
  (interactive "*P")
  (ar-th-escape 'comment arg arg))

(defun ar-escape-csv-atpt (&optional arg)
  "Escape CSV at point."
  (interactive "*P")
  (ar-th-escape 'csv arg arg))

(defun ar-escape-date-atpt (&optional arg)
  "Escape DATE at point."
  (interactive "*P")
  (ar-th-escape 'date arg arg))

(defun ar-escape-delimited-atpt (&optional arg)
  "Escape DELIMITED at point."
  (interactive "*P")
  (ar-th-escape 'delimited arg arg))

(defun ar-escape-email-atpt (&optional arg)
  "Escape EMAIL at point."
  (interactive "*P")
  (ar-th-escape 'email arg arg))

(defun ar-escape-filename-atpt (&optional arg)
  "Escape FILENAME at point."
  (interactive "*P")
  (ar-th-escape 'filename arg arg))

(defun ar-escape-filenamenondirectory-atpt (&optional arg)
  "Escape FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-escape 'filenamenondirectory arg arg))

(defun ar-escape-float-atpt (&optional arg)
  "Escape FLOAT at point."
  (interactive "*P")
  (ar-th-escape 'float arg arg))

(defun ar-escape-function-atpt (&optional arg)
  "Escape FUNCTION at point."
  (interactive "*P")
  (ar-th-escape 'function arg arg))

(defun ar-escape-ip-atpt (&optional arg)
  "Escape IP at point."
  (interactive "*P")
  (ar-th-escape 'ip arg arg))

(defun ar-escape-isbn-atpt (&optional arg)
  "Escape ISBN at point."
  (interactive "*P")
  (ar-th-escape 'isbn arg arg))

(defun ar-escape-line-atpt (&optional arg)
  "Escape LINE at point."
  (interactive "*P")
  (ar-th-escape 'line arg arg))

(defun ar-escape-list-atpt (&optional arg)
  "Escape LIST at point."
  (interactive "*P")
  (ar-th-escape 'list arg arg))

(defun ar-escape-name-atpt (&optional arg)
  "Escape NAME at point."
  (interactive "*P")
  (ar-th-escape 'name arg arg))

(defun ar-escape-number-atpt (&optional arg)
  "Escape NUMBER at point."
  (interactive "*P")
  (ar-th-escape 'number arg arg))

(defun ar-escape-page-atpt (&optional arg)
  "Escape PAGE at point."
  (interactive "*P")
  (ar-th-escape 'page arg arg))

(defun ar-escape-paragraph-atpt (&optional arg)
  "Escape PARAGRAPH at point."
  (interactive "*P")
  (ar-th-escape 'paragraph arg arg))

(defun ar-escape-phone-atpt (&optional arg)
  "Escape PHONE at point."
  (interactive "*P")
  (ar-th-escape 'phone arg arg))

(defun ar-escape-region-atpt (&optional arg)
  "Escape REGION at point."
  (interactive "*P")
  (ar-th-escape 'region arg arg))

(defun ar-escape-sentence-atpt (&optional arg)
  "Escape SENTENCE at point."
  (interactive "*P")
  (ar-th-escape 'sentence arg arg))

(defun ar-escape-sexp-atpt (&optional arg)
  "Escape SEXP at point."
  (interactive "*P")
  (ar-th-escape 'sexp arg arg))

(defun ar-escape-shstruct-atpt (&optional arg)
  "Escape SHSTRUCT at point."
  (interactive "*P")
  (ar-th-escape 'shstruct arg arg))

(defun ar-escape-symbol-atpt (&optional arg)
  "Escape SYMBOL at point."
  (interactive "*P")
  (ar-th-escape 'symbol arg arg))

(defun ar-escape-url-atpt (&optional arg)
  "Escape URL at point."
  (interactive "*P")
  (ar-th-escape 'url arg arg))

(defun ar-escape-word-atpt (&optional arg)
  "Escape WORD at point."
  (interactive "*P")
  (ar-th-escape 'word arg arg))

(defun ar-escape-wordalphaonly-atpt (&optional arg)
  "Escape WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-escape 'wordalphaonly arg arg))

(defun ar-hash-greateranglednested-atpt (&optional arg)
  "Hash GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-hash 'greateranglednested arg arg))

(defun ar-hash-lesseranglednested-atpt (&optional arg)
  "Hash LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-hash 'lesseranglednested arg arg))

(defun ar-hash-buffer-atpt (&optional arg)
  "Hash BUFFER at point."
  (interactive "*P")
  (ar-th-hash 'buffer arg arg))

(defun ar-hash-char-atpt (&optional arg)
  "Hash CHAR at point."
  (interactive "*P")
  (ar-th-hash 'char arg arg))

(defun ar-hash-comment-atpt (&optional arg)
  "Hash COMMENT at point."
  (interactive "*P")
  (ar-th-hash 'comment arg arg))

(defun ar-hash-csv-atpt (&optional arg)
  "Hash CSV at point."
  (interactive "*P")
  (ar-th-hash 'csv arg arg))

(defun ar-hash-date-atpt (&optional arg)
  "Hash DATE at point."
  (interactive "*P")
  (ar-th-hash 'date arg arg))

(defun ar-hash-delimited-atpt (&optional arg)
  "Hash DELIMITED at point."
  (interactive "*P")
  (ar-th-hash 'delimited arg arg))

(defun ar-hash-email-atpt (&optional arg)
  "Hash EMAIL at point."
  (interactive "*P")
  (ar-th-hash 'email arg arg))

(defun ar-hash-filename-atpt (&optional arg)
  "Hash FILENAME at point."
  (interactive "*P")
  (ar-th-hash 'filename arg arg))

(defun ar-hash-filenamenondirectory-atpt (&optional arg)
  "Hash FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-hash 'filenamenondirectory arg arg))

(defun ar-hash-float-atpt (&optional arg)
  "Hash FLOAT at point."
  (interactive "*P")
  (ar-th-hash 'float arg arg))

(defun ar-hash-function-atpt (&optional arg)
  "Hash FUNCTION at point."
  (interactive "*P")
  (ar-th-hash 'function arg arg))

(defun ar-hash-ip-atpt (&optional arg)
  "Hash IP at point."
  (interactive "*P")
  (ar-th-hash 'ip arg arg))

(defun ar-hash-isbn-atpt (&optional arg)
  "Hash ISBN at point."
  (interactive "*P")
  (ar-th-hash 'isbn arg arg))

(defun ar-hash-line-atpt (&optional arg)
  "Hash LINE at point."
  (interactive "*P")
  (ar-th-hash 'line arg arg))

(defun ar-hash-list-atpt (&optional arg)
  "Hash LIST at point."
  (interactive "*P")
  (ar-th-hash 'list arg arg))

(defun ar-hash-name-atpt (&optional arg)
  "Hash NAME at point."
  (interactive "*P")
  (ar-th-hash 'name arg arg))

(defun ar-hash-number-atpt (&optional arg)
  "Hash NUMBER at point."
  (interactive "*P")
  (ar-th-hash 'number arg arg))

(defun ar-hash-page-atpt (&optional arg)
  "Hash PAGE at point."
  (interactive "*P")
  (ar-th-hash 'page arg arg))

(defun ar-hash-paragraph-atpt (&optional arg)
  "Hash PARAGRAPH at point."
  (interactive "*P")
  (ar-th-hash 'paragraph arg arg))

(defun ar-hash-phone-atpt (&optional arg)
  "Hash PHONE at point."
  (interactive "*P")
  (ar-th-hash 'phone arg arg))

(defun ar-hash-region-atpt (&optional arg)
  "Hash REGION at point."
  (interactive "*P")
  (ar-th-hash 'region arg arg))

(defun ar-hash-sentence-atpt (&optional arg)
  "Hash SENTENCE at point."
  (interactive "*P")
  (ar-th-hash 'sentence arg arg))

(defun ar-hash-sexp-atpt (&optional arg)
  "Hash SEXP at point."
  (interactive "*P")
  (ar-th-hash 'sexp arg arg))

(defun ar-hash-shstruct-atpt (&optional arg)
  "Hash SHSTRUCT at point."
  (interactive "*P")
  (ar-th-hash 'shstruct arg arg))

(defun ar-hash-symbol-atpt (&optional arg)
  "Hash SYMBOL at point."
  (interactive "*P")
  (ar-th-hash 'symbol arg arg))

(defun ar-hash-url-atpt (&optional arg)
  "Hash URL at point."
  (interactive "*P")
  (ar-th-hash 'url arg arg))

(defun ar-hash-word-atpt (&optional arg)
  "Hash WORD at point."
  (interactive "*P")
  (ar-th-hash 'word arg arg))

(defun ar-hash-wordalphaonly-atpt (&optional arg)
  "Hash WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-hash 'wordalphaonly arg arg))

(defun ar-hyphen-greateranglednested-atpt (&optional arg)
  "Hyphen GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-hyphen 'greateranglednested arg arg))

(defun ar-hyphen-lesseranglednested-atpt (&optional arg)
  "Hyphen LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-hyphen 'lesseranglednested arg arg))

(defun ar-hyphen-buffer-atpt (&optional arg)
  "Hyphen BUFFER at point."
  (interactive "*P")
  (ar-th-hyphen 'buffer arg arg))

(defun ar-hyphen-char-atpt (&optional arg)
  "Hyphen CHAR at point."
  (interactive "*P")
  (ar-th-hyphen 'char arg arg))

(defun ar-hyphen-comment-atpt (&optional arg)
  "Hyphen COMMENT at point."
  (interactive "*P")
  (ar-th-hyphen 'comment arg arg))

(defun ar-hyphen-csv-atpt (&optional arg)
  "Hyphen CSV at point."
  (interactive "*P")
  (ar-th-hyphen 'csv arg arg))

(defun ar-hyphen-date-atpt (&optional arg)
  "Hyphen DATE at point."
  (interactive "*P")
  (ar-th-hyphen 'date arg arg))

(defun ar-hyphen-delimited-atpt (&optional arg)
  "Hyphen DELIMITED at point."
  (interactive "*P")
  (ar-th-hyphen 'delimited arg arg))

(defun ar-hyphen-email-atpt (&optional arg)
  "Hyphen EMAIL at point."
  (interactive "*P")
  (ar-th-hyphen 'email arg arg))

(defun ar-hyphen-filename-atpt (&optional arg)
  "Hyphen FILENAME at point."
  (interactive "*P")
  (ar-th-hyphen 'filename arg arg))

(defun ar-hyphen-filenamenondirectory-atpt (&optional arg)
  "Hyphen FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-hyphen 'filenamenondirectory arg arg))

(defun ar-hyphen-float-atpt (&optional arg)
  "Hyphen FLOAT at point."
  (interactive "*P")
  (ar-th-hyphen 'float arg arg))

(defun ar-hyphen-function-atpt (&optional arg)
  "Hyphen FUNCTION at point."
  (interactive "*P")
  (ar-th-hyphen 'function arg arg))

(defun ar-hyphen-ip-atpt (&optional arg)
  "Hyphen IP at point."
  (interactive "*P")
  (ar-th-hyphen 'ip arg arg))

(defun ar-hyphen-isbn-atpt (&optional arg)
  "Hyphen ISBN at point."
  (interactive "*P")
  (ar-th-hyphen 'isbn arg arg))

(defun ar-hyphen-line-atpt (&optional arg)
  "Hyphen LINE at point."
  (interactive "*P")
  (ar-th-hyphen 'line arg arg))

(defun ar-hyphen-list-atpt (&optional arg)
  "Hyphen LIST at point."
  (interactive "*P")
  (ar-th-hyphen 'list arg arg))

(defun ar-hyphen-name-atpt (&optional arg)
  "Hyphen NAME at point."
  (interactive "*P")
  (ar-th-hyphen 'name arg arg))

(defun ar-hyphen-number-atpt (&optional arg)
  "Hyphen NUMBER at point."
  (interactive "*P")
  (ar-th-hyphen 'number arg arg))

(defun ar-hyphen-page-atpt (&optional arg)
  "Hyphen PAGE at point."
  (interactive "*P")
  (ar-th-hyphen 'page arg arg))

(defun ar-hyphen-paragraph-atpt (&optional arg)
  "Hyphen PARAGRAPH at point."
  (interactive "*P")
  (ar-th-hyphen 'paragraph arg arg))

(defun ar-hyphen-phone-atpt (&optional arg)
  "Hyphen PHONE at point."
  (interactive "*P")
  (ar-th-hyphen 'phone arg arg))

(defun ar-hyphen-region-atpt (&optional arg)
  "Hyphen REGION at point."
  (interactive "*P")
  (ar-th-hyphen 'region arg arg))

(defun ar-hyphen-sentence-atpt (&optional arg)
  "Hyphen SENTENCE at point."
  (interactive "*P")
  (ar-th-hyphen 'sentence arg arg))

(defun ar-hyphen-sexp-atpt (&optional arg)
  "Hyphen SEXP at point."
  (interactive "*P")
  (ar-th-hyphen 'sexp arg arg))

(defun ar-hyphen-shstruct-atpt (&optional arg)
  "Hyphen SHSTRUCT at point."
  (interactive "*P")
  (ar-th-hyphen 'shstruct arg arg))

(defun ar-hyphen-symbol-atpt (&optional arg)
  "Hyphen SYMBOL at point."
  (interactive "*P")
  (ar-th-hyphen 'symbol arg arg))

(defun ar-hyphen-url-atpt (&optional arg)
  "Hyphen URL at point."
  (interactive "*P")
  (ar-th-hyphen 'url arg arg))

(defun ar-hyphen-word-atpt (&optional arg)
  "Hyphen WORD at point."
  (interactive "*P")
  (ar-th-hyphen 'word arg arg))

(defun ar-hyphen-wordalphaonly-atpt (&optional arg)
  "Hyphen WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-hyphen 'wordalphaonly arg arg))

(defun ar-pipe-greateranglednested-atpt (&optional arg)
  "Pipe GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-pipe 'greateranglednested arg arg))

(defun ar-pipe-lesseranglednested-atpt (&optional arg)
  "Pipe LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-pipe 'lesseranglednested arg arg))

(defun ar-pipe-buffer-atpt (&optional arg)
  "Pipe BUFFER at point."
  (interactive "*P")
  (ar-th-pipe 'buffer arg arg))

(defun ar-pipe-char-atpt (&optional arg)
  "Pipe CHAR at point."
  (interactive "*P")
  (ar-th-pipe 'char arg arg))

(defun ar-pipe-comment-atpt (&optional arg)
  "Pipe COMMENT at point."
  (interactive "*P")
  (ar-th-pipe 'comment arg arg))

(defun ar-pipe-csv-atpt (&optional arg)
  "Pipe CSV at point."
  (interactive "*P")
  (ar-th-pipe 'csv arg arg))

(defun ar-pipe-date-atpt (&optional arg)
  "Pipe DATE at point."
  (interactive "*P")
  (ar-th-pipe 'date arg arg))

(defun ar-pipe-delimited-atpt (&optional arg)
  "Pipe DELIMITED at point."
  (interactive "*P")
  (ar-th-pipe 'delimited arg arg))

(defun ar-pipe-email-atpt (&optional arg)
  "Pipe EMAIL at point."
  (interactive "*P")
  (ar-th-pipe 'email arg arg))

(defun ar-pipe-filename-atpt (&optional arg)
  "Pipe FILENAME at point."
  (interactive "*P")
  (ar-th-pipe 'filename arg arg))

(defun ar-pipe-filenamenondirectory-atpt (&optional arg)
  "Pipe FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-pipe 'filenamenondirectory arg arg))

(defun ar-pipe-float-atpt (&optional arg)
  "Pipe FLOAT at point."
  (interactive "*P")
  (ar-th-pipe 'float arg arg))

(defun ar-pipe-function-atpt (&optional arg)
  "Pipe FUNCTION at point."
  (interactive "*P")
  (ar-th-pipe 'function arg arg))

(defun ar-pipe-ip-atpt (&optional arg)
  "Pipe IP at point."
  (interactive "*P")
  (ar-th-pipe 'ip arg arg))

(defun ar-pipe-isbn-atpt (&optional arg)
  "Pipe ISBN at point."
  (interactive "*P")
  (ar-th-pipe 'isbn arg arg))

(defun ar-pipe-line-atpt (&optional arg)
  "Pipe LINE at point."
  (interactive "*P")
  (ar-th-pipe 'line arg arg))

(defun ar-pipe-list-atpt (&optional arg)
  "Pipe LIST at point."
  (interactive "*P")
  (ar-th-pipe 'list arg arg))

(defun ar-pipe-name-atpt (&optional arg)
  "Pipe NAME at point."
  (interactive "*P")
  (ar-th-pipe 'name arg arg))

(defun ar-pipe-number-atpt (&optional arg)
  "Pipe NUMBER at point."
  (interactive "*P")
  (ar-th-pipe 'number arg arg))

(defun ar-pipe-page-atpt (&optional arg)
  "Pipe PAGE at point."
  (interactive "*P")
  (ar-th-pipe 'page arg arg))

(defun ar-pipe-paragraph-atpt (&optional arg)
  "Pipe PARAGRAPH at point."
  (interactive "*P")
  (ar-th-pipe 'paragraph arg arg))

(defun ar-pipe-phone-atpt (&optional arg)
  "Pipe PHONE at point."
  (interactive "*P")
  (ar-th-pipe 'phone arg arg))

(defun ar-pipe-region-atpt (&optional arg)
  "Pipe REGION at point."
  (interactive "*P")
  (ar-th-pipe 'region arg arg))

(defun ar-pipe-sentence-atpt (&optional arg)
  "Pipe SENTENCE at point."
  (interactive "*P")
  (ar-th-pipe 'sentence arg arg))

(defun ar-pipe-sexp-atpt (&optional arg)
  "Pipe SEXP at point."
  (interactive "*P")
  (ar-th-pipe 'sexp arg arg))

(defun ar-pipe-shstruct-atpt (&optional arg)
  "Pipe SHSTRUCT at point."
  (interactive "*P")
  (ar-th-pipe 'shstruct arg arg))

(defun ar-pipe-symbol-atpt (&optional arg)
  "Pipe SYMBOL at point."
  (interactive "*P")
  (ar-th-pipe 'symbol arg arg))

(defun ar-pipe-url-atpt (&optional arg)
  "Pipe URL at point."
  (interactive "*P")
  (ar-th-pipe 'url arg arg))

(defun ar-pipe-word-atpt (&optional arg)
  "Pipe WORD at point."
  (interactive "*P")
  (ar-th-pipe 'word arg arg))

(defun ar-pipe-wordalphaonly-atpt (&optional arg)
  "Pipe WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-pipe 'wordalphaonly arg arg))

(defun ar-singlequote-greateranglednested-atpt (&optional arg)
  "Singlequote GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-singlequote 'greateranglednested arg arg))

(defun ar-singlequote-lesseranglednested-atpt (&optional arg)
  "Singlequote LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-singlequote 'lesseranglednested arg arg))

(defun ar-singlequote-buffer-atpt (&optional arg)
  "Singlequote BUFFER at point."
  (interactive "*P")
  (ar-th-singlequote 'buffer arg arg))

(defun ar-singlequote-char-atpt (&optional arg)
  "Singlequote CHAR at point."
  (interactive "*P")
  (ar-th-singlequote 'char arg arg))

(defun ar-singlequote-comment-atpt (&optional arg)
  "Singlequote COMMENT at point."
  (interactive "*P")
  (ar-th-singlequote 'comment arg arg))

(defun ar-singlequote-csv-atpt (&optional arg)
  "Singlequote CSV at point."
  (interactive "*P")
  (ar-th-singlequote 'csv arg arg))

(defun ar-singlequote-date-atpt (&optional arg)
  "Singlequote DATE at point."
  (interactive "*P")
  (ar-th-singlequote 'date arg arg))

(defun ar-singlequote-delimited-atpt (&optional arg)
  "Singlequote DELIMITED at point."
  (interactive "*P")
  (ar-th-singlequote 'delimited arg arg))

(defun ar-singlequote-email-atpt (&optional arg)
  "Singlequote EMAIL at point."
  (interactive "*P")
  (ar-th-singlequote 'email arg arg))

(defun ar-singlequote-filename-atpt (&optional arg)
  "Singlequote FILENAME at point."
  (interactive "*P")
  (ar-th-singlequote 'filename arg arg))

(defun ar-singlequote-filenamenondirectory-atpt (&optional arg)
  "Singlequote FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-singlequote 'filenamenondirectory arg arg))

(defun ar-singlequote-float-atpt (&optional arg)
  "Singlequote FLOAT at point."
  (interactive "*P")
  (ar-th-singlequote 'float arg arg))

(defun ar-singlequote-function-atpt (&optional arg)
  "Singlequote FUNCTION at point."
  (interactive "*P")
  (ar-th-singlequote 'function arg arg))

(defun ar-singlequote-ip-atpt (&optional arg)
  "Singlequote IP at point."
  (interactive "*P")
  (ar-th-singlequote 'ip arg arg))

(defun ar-singlequote-isbn-atpt (&optional arg)
  "Singlequote ISBN at point."
  (interactive "*P")
  (ar-th-singlequote 'isbn arg arg))

(defun ar-singlequote-line-atpt (&optional arg)
  "Singlequote LINE at point."
  (interactive "*P")
  (ar-th-singlequote 'line arg arg))

(defun ar-singlequote-list-atpt (&optional arg)
  "Singlequote LIST at point."
  (interactive "*P")
  (ar-th-singlequote 'list arg arg))

(defun ar-singlequote-name-atpt (&optional arg)
  "Singlequote NAME at point."
  (interactive "*P")
  (ar-th-singlequote 'name arg arg))

(defun ar-singlequote-number-atpt (&optional arg)
  "Singlequote NUMBER at point."
  (interactive "*P")
  (ar-th-singlequote 'number arg arg))

(defun ar-singlequote-page-atpt (&optional arg)
  "Singlequote PAGE at point."
  (interactive "*P")
  (ar-th-singlequote 'page arg arg))

(defun ar-singlequote-paragraph-atpt (&optional arg)
  "Singlequote PARAGRAPH at point."
  (interactive "*P")
  (ar-th-singlequote 'paragraph arg arg))

(defun ar-singlequote-phone-atpt (&optional arg)
  "Singlequote PHONE at point."
  (interactive "*P")
  (ar-th-singlequote 'phone arg arg))

(defun ar-singlequote-region-atpt (&optional arg)
  "Singlequote REGION at point."
  (interactive "*P")
  (ar-th-singlequote 'region arg arg))

(defun ar-singlequote-sentence-atpt (&optional arg)
  "Singlequote SENTENCE at point."
  (interactive "*P")
  (ar-th-singlequote 'sentence arg arg))

(defun ar-singlequote-sexp-atpt (&optional arg)
  "Singlequote SEXP at point."
  (interactive "*P")
  (ar-th-singlequote 'sexp arg arg))

(defun ar-singlequote-shstruct-atpt (&optional arg)
  "Singlequote SHSTRUCT at point."
  (interactive "*P")
  (ar-th-singlequote 'shstruct arg arg))

(defun ar-singlequote-symbol-atpt (&optional arg)
  "Singlequote SYMBOL at point."
  (interactive "*P")
  (ar-th-singlequote 'symbol arg arg))

(defun ar-singlequote-url-atpt (&optional arg)
  "Singlequote URL at point."
  (interactive "*P")
  (ar-th-singlequote 'url arg arg))

(defun ar-singlequote-word-atpt (&optional arg)
  "Singlequote WORD at point."
  (interactive "*P")
  (ar-th-singlequote 'word arg arg))

(defun ar-singlequote-wordalphaonly-atpt (&optional arg)
  "Singlequote WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-singlequote 'wordalphaonly arg arg))

(defun ar-slash-greateranglednested-atpt (&optional arg)
  "Slash GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-slash 'greateranglednested arg arg))

(defun ar-slash-lesseranglednested-atpt (&optional arg)
  "Slash LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-slash 'lesseranglednested arg arg))

(defun ar-slash-buffer-atpt (&optional arg)
  "Slash BUFFER at point."
  (interactive "*P")
  (ar-th-slash 'buffer arg arg))

(defun ar-slash-char-atpt (&optional arg)
  "Slash CHAR at point."
  (interactive "*P")
  (ar-th-slash 'char arg arg))

(defun ar-slash-comment-atpt (&optional arg)
  "Slash COMMENT at point."
  (interactive "*P")
  (ar-th-slash 'comment arg arg))

(defun ar-slash-csv-atpt (&optional arg)
  "Slash CSV at point."
  (interactive "*P")
  (ar-th-slash 'csv arg arg))

(defun ar-slash-date-atpt (&optional arg)
  "Slash DATE at point."
  (interactive "*P")
  (ar-th-slash 'date arg arg))

(defun ar-slash-delimited-atpt (&optional arg)
  "Slash DELIMITED at point."
  (interactive "*P")
  (ar-th-slash 'delimited arg arg))

(defun ar-slash-email-atpt (&optional arg)
  "Slash EMAIL at point."
  (interactive "*P")
  (ar-th-slash 'email arg arg))

(defun ar-slash-filename-atpt (&optional arg)
  "Slash FILENAME at point."
  (interactive "*P")
  (ar-th-slash 'filename arg arg))

(defun ar-slash-filenamenondirectory-atpt (&optional arg)
  "Slash FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-slash 'filenamenondirectory arg arg))

(defun ar-slash-float-atpt (&optional arg)
  "Slash FLOAT at point."
  (interactive "*P")
  (ar-th-slash 'float arg arg))

(defun ar-slash-function-atpt (&optional arg)
  "Slash FUNCTION at point."
  (interactive "*P")
  (ar-th-slash 'function arg arg))

(defun ar-slash-ip-atpt (&optional arg)
  "Slash IP at point."
  (interactive "*P")
  (ar-th-slash 'ip arg arg))

(defun ar-slash-isbn-atpt (&optional arg)
  "Slash ISBN at point."
  (interactive "*P")
  (ar-th-slash 'isbn arg arg))

(defun ar-slash-line-atpt (&optional arg)
  "Slash LINE at point."
  (interactive "*P")
  (ar-th-slash 'line arg arg))

(defun ar-slash-list-atpt (&optional arg)
  "Slash LIST at point."
  (interactive "*P")
  (ar-th-slash 'list arg arg))

(defun ar-slash-name-atpt (&optional arg)
  "Slash NAME at point."
  (interactive "*P")
  (ar-th-slash 'name arg arg))

(defun ar-slash-number-atpt (&optional arg)
  "Slash NUMBER at point."
  (interactive "*P")
  (ar-th-slash 'number arg arg))

(defun ar-slash-page-atpt (&optional arg)
  "Slash PAGE at point."
  (interactive "*P")
  (ar-th-slash 'page arg arg))

(defun ar-slash-paragraph-atpt (&optional arg)
  "Slash PARAGRAPH at point."
  (interactive "*P")
  (ar-th-slash 'paragraph arg arg))

(defun ar-slash-phone-atpt (&optional arg)
  "Slash PHONE at point."
  (interactive "*P")
  (ar-th-slash 'phone arg arg))

(defun ar-slash-region-atpt (&optional arg)
  "Slash REGION at point."
  (interactive "*P")
  (ar-th-slash 'region arg arg))

(defun ar-slash-sentence-atpt (&optional arg)
  "Slash SENTENCE at point."
  (interactive "*P")
  (ar-th-slash 'sentence arg arg))

(defun ar-slash-sexp-atpt (&optional arg)
  "Slash SEXP at point."
  (interactive "*P")
  (ar-th-slash 'sexp arg arg))

(defun ar-slash-shstruct-atpt (&optional arg)
  "Slash SHSTRUCT at point."
  (interactive "*P")
  (ar-th-slash 'shstruct arg arg))

(defun ar-slash-symbol-atpt (&optional arg)
  "Slash SYMBOL at point."
  (interactive "*P")
  (ar-th-slash 'symbol arg arg))

(defun ar-slash-url-atpt (&optional arg)
  "Slash URL at point."
  (interactive "*P")
  (ar-th-slash 'url arg arg))

(defun ar-slash-word-atpt (&optional arg)
  "Slash WORD at point."
  (interactive "*P")
  (ar-th-slash 'word arg arg))

(defun ar-slash-wordalphaonly-atpt (&optional arg)
  "Slash WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-slash 'wordalphaonly arg arg))

(defun ar-star-greateranglednested-atpt (&optional arg)
  "Star GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-star 'greateranglednested arg arg))

(defun ar-star-lesseranglednested-atpt (&optional arg)
  "Star LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-star 'lesseranglednested arg arg))

(defun ar-star-buffer-atpt (&optional arg)
  "Star BUFFER at point."
  (interactive "*P")
  (ar-th-star 'buffer arg arg))

(defun ar-star-char-atpt (&optional arg)
  "Star CHAR at point."
  (interactive "*P")
  (ar-th-star 'char arg arg))

(defun ar-star-comment-atpt (&optional arg)
  "Star COMMENT at point."
  (interactive "*P")
  (ar-th-star 'comment arg arg))

(defun ar-star-csv-atpt (&optional arg)
  "Star CSV at point."
  (interactive "*P")
  (ar-th-star 'csv arg arg))

(defun ar-star-date-atpt (&optional arg)
  "Star DATE at point."
  (interactive "*P")
  (ar-th-star 'date arg arg))

(defun ar-star-delimited-atpt (&optional arg)
  "Star DELIMITED at point."
  (interactive "*P")
  (ar-th-star 'delimited arg arg))

(defun ar-star-email-atpt (&optional arg)
  "Star EMAIL at point."
  (interactive "*P")
  (ar-th-star 'email arg arg))

(defun ar-star-filename-atpt (&optional arg)
  "Star FILENAME at point."
  (interactive "*P")
  (ar-th-star 'filename arg arg))

(defun ar-star-filenamenondirectory-atpt (&optional arg)
  "Star FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-star 'filenamenondirectory arg arg))

(defun ar-star-float-atpt (&optional arg)
  "Star FLOAT at point."
  (interactive "*P")
  (ar-th-star 'float arg arg))

(defun ar-star-function-atpt (&optional arg)
  "Star FUNCTION at point."
  (interactive "*P")
  (ar-th-star 'function arg arg))

(defun ar-star-ip-atpt (&optional arg)
  "Star IP at point."
  (interactive "*P")
  (ar-th-star 'ip arg arg))

(defun ar-star-isbn-atpt (&optional arg)
  "Star ISBN at point."
  (interactive "*P")
  (ar-th-star 'isbn arg arg))

(defun ar-star-line-atpt (&optional arg)
  "Star LINE at point."
  (interactive "*P")
  (ar-th-star 'line arg arg))

(defun ar-star-list-atpt (&optional arg)
  "Star LIST at point."
  (interactive "*P")
  (ar-th-star 'list arg arg))

(defun ar-star-name-atpt (&optional arg)
  "Star NAME at point."
  (interactive "*P")
  (ar-th-star 'name arg arg))

(defun ar-star-number-atpt (&optional arg)
  "Star NUMBER at point."
  (interactive "*P")
  (ar-th-star 'number arg arg))

(defun ar-star-page-atpt (&optional arg)
  "Star PAGE at point."
  (interactive "*P")
  (ar-th-star 'page arg arg))

(defun ar-star-paragraph-atpt (&optional arg)
  "Star PARAGRAPH at point."
  (interactive "*P")
  (ar-th-star 'paragraph arg arg))

(defun ar-star-phone-atpt (&optional arg)
  "Star PHONE at point."
  (interactive "*P")
  (ar-th-star 'phone arg arg))

(defun ar-star-region-atpt (&optional arg)
  "Star REGION at point."
  (interactive "*P")
  (ar-th-star 'region arg arg))

(defun ar-star-sentence-atpt (&optional arg)
  "Star SENTENCE at point."
  (interactive "*P")
  (ar-th-star 'sentence arg arg))

(defun ar-star-sexp-atpt (&optional arg)
  "Star SEXP at point."
  (interactive "*P")
  (ar-th-star 'sexp arg arg))

(defun ar-star-shstruct-atpt (&optional arg)
  "Star SHSTRUCT at point."
  (interactive "*P")
  (ar-th-star 'shstruct arg arg))

(defun ar-star-symbol-atpt (&optional arg)
  "Star SYMBOL at point."
  (interactive "*P")
  (ar-th-star 'symbol arg arg))

(defun ar-star-url-atpt (&optional arg)
  "Star URL at point."
  (interactive "*P")
  (ar-th-star 'url arg arg))

(defun ar-star-word-atpt (&optional arg)
  "Star WORD at point."
  (interactive "*P")
  (ar-th-star 'word arg arg))

(defun ar-star-wordalphaonly-atpt (&optional arg)
  "Star WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-star 'wordalphaonly arg arg))

(defun ar-tild-greateranglednested-atpt (&optional arg)
  "Tild GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-tild 'greateranglednested arg arg))

(defun ar-tild-lesseranglednested-atpt (&optional arg)
  "Tild LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-tild 'lesseranglednested arg arg))

(defun ar-tild-buffer-atpt (&optional arg)
  "Tild BUFFER at point."
  (interactive "*P")
  (ar-th-tild 'buffer arg arg))

(defun ar-tild-char-atpt (&optional arg)
  "Tild CHAR at point."
  (interactive "*P")
  (ar-th-tild 'char arg arg))

(defun ar-tild-comment-atpt (&optional arg)
  "Tild COMMENT at point."
  (interactive "*P")
  (ar-th-tild 'comment arg arg))

(defun ar-tild-csv-atpt (&optional arg)
  "Tild CSV at point."
  (interactive "*P")
  (ar-th-tild 'csv arg arg))

(defun ar-tild-date-atpt (&optional arg)
  "Tild DATE at point."
  (interactive "*P")
  (ar-th-tild 'date arg arg))

(defun ar-tild-delimited-atpt (&optional arg)
  "Tild DELIMITED at point."
  (interactive "*P")
  (ar-th-tild 'delimited arg arg))

(defun ar-tild-email-atpt (&optional arg)
  "Tild EMAIL at point."
  (interactive "*P")
  (ar-th-tild 'email arg arg))

(defun ar-tild-filename-atpt (&optional arg)
  "Tild FILENAME at point."
  (interactive "*P")
  (ar-th-tild 'filename arg arg))

(defun ar-tild-filenamenondirectory-atpt (&optional arg)
  "Tild FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-tild 'filenamenondirectory arg arg))

(defun ar-tild-float-atpt (&optional arg)
  "Tild FLOAT at point."
  (interactive "*P")
  (ar-th-tild 'float arg arg))

(defun ar-tild-function-atpt (&optional arg)
  "Tild FUNCTION at point."
  (interactive "*P")
  (ar-th-tild 'function arg arg))

(defun ar-tild-ip-atpt (&optional arg)
  "Tild IP at point."
  (interactive "*P")
  (ar-th-tild 'ip arg arg))

(defun ar-tild-isbn-atpt (&optional arg)
  "Tild ISBN at point."
  (interactive "*P")
  (ar-th-tild 'isbn arg arg))

(defun ar-tild-line-atpt (&optional arg)
  "Tild LINE at point."
  (interactive "*P")
  (ar-th-tild 'line arg arg))

(defun ar-tild-list-atpt (&optional arg)
  "Tild LIST at point."
  (interactive "*P")
  (ar-th-tild 'list arg arg))

(defun ar-tild-name-atpt (&optional arg)
  "Tild NAME at point."
  (interactive "*P")
  (ar-th-tild 'name arg arg))

(defun ar-tild-number-atpt (&optional arg)
  "Tild NUMBER at point."
  (interactive "*P")
  (ar-th-tild 'number arg arg))

(defun ar-tild-page-atpt (&optional arg)
  "Tild PAGE at point."
  (interactive "*P")
  (ar-th-tild 'page arg arg))

(defun ar-tild-paragraph-atpt (&optional arg)
  "Tild PARAGRAPH at point."
  (interactive "*P")
  (ar-th-tild 'paragraph arg arg))

(defun ar-tild-phone-atpt (&optional arg)
  "Tild PHONE at point."
  (interactive "*P")
  (ar-th-tild 'phone arg arg))

(defun ar-tild-region-atpt (&optional arg)
  "Tild REGION at point."
  (interactive "*P")
  (ar-th-tild 'region arg arg))

(defun ar-tild-sentence-atpt (&optional arg)
  "Tild SENTENCE at point."
  (interactive "*P")
  (ar-th-tild 'sentence arg arg))

(defun ar-tild-sexp-atpt (&optional arg)
  "Tild SEXP at point."
  (interactive "*P")
  (ar-th-tild 'sexp arg arg))

(defun ar-tild-shstruct-atpt (&optional arg)
  "Tild SHSTRUCT at point."
  (interactive "*P")
  (ar-th-tild 'shstruct arg arg))

(defun ar-tild-symbol-atpt (&optional arg)
  "Tild SYMBOL at point."
  (interactive "*P")
  (ar-th-tild 'symbol arg arg))

(defun ar-tild-url-atpt (&optional arg)
  "Tild URL at point."
  (interactive "*P")
  (ar-th-tild 'url arg arg))

(defun ar-tild-word-atpt (&optional arg)
  "Tild WORD at point."
  (interactive "*P")
  (ar-th-tild 'word arg arg))

(defun ar-tild-wordalphaonly-atpt (&optional arg)
  "Tild WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-tild 'wordalphaonly arg arg))

(defun ar-triplebacktick-greateranglednested-atpt (&optional arg)
  "Triplebacktick GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-triplebacktick 'greateranglednested arg arg))

(defun ar-triplebacktick-lesseranglednested-atpt (&optional arg)
  "Triplebacktick LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-triplebacktick 'lesseranglednested arg arg))

(defun ar-triplebacktick-buffer-atpt (&optional arg)
  "Triplebacktick BUFFER at point."
  (interactive "*P")
  (ar-th-triplebacktick 'buffer arg arg))

(defun ar-triplebacktick-char-atpt (&optional arg)
  "Triplebacktick CHAR at point."
  (interactive "*P")
  (ar-th-triplebacktick 'char arg arg))

(defun ar-triplebacktick-comment-atpt (&optional arg)
  "Triplebacktick COMMENT at point."
  (interactive "*P")
  (ar-th-triplebacktick 'comment arg arg))

(defun ar-triplebacktick-csv-atpt (&optional arg)
  "Triplebacktick CSV at point."
  (interactive "*P")
  (ar-th-triplebacktick 'csv arg arg))

(defun ar-triplebacktick-date-atpt (&optional arg)
  "Triplebacktick DATE at point."
  (interactive "*P")
  (ar-th-triplebacktick 'date arg arg))

(defun ar-triplebacktick-delimited-atpt (&optional arg)
  "Triplebacktick DELIMITED at point."
  (interactive "*P")
  (ar-th-triplebacktick 'delimited arg arg))

(defun ar-triplebacktick-email-atpt (&optional arg)
  "Triplebacktick EMAIL at point."
  (interactive "*P")
  (ar-th-triplebacktick 'email arg arg))

(defun ar-triplebacktick-filename-atpt (&optional arg)
  "Triplebacktick FILENAME at point."
  (interactive "*P")
  (ar-th-triplebacktick 'filename arg arg))

(defun ar-triplebacktick-filenamenondirectory-atpt (&optional arg)
  "Triplebacktick FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-triplebacktick 'filenamenondirectory arg arg))

(defun ar-triplebacktick-float-atpt (&optional arg)
  "Triplebacktick FLOAT at point."
  (interactive "*P")
  (ar-th-triplebacktick 'float arg arg))

(defun ar-triplebacktick-function-atpt (&optional arg)
  "Triplebacktick FUNCTION at point."
  (interactive "*P")
  (ar-th-triplebacktick 'function arg arg))

(defun ar-triplebacktick-ip-atpt (&optional arg)
  "Triplebacktick IP at point."
  (interactive "*P")
  (ar-th-triplebacktick 'ip arg arg))

(defun ar-triplebacktick-isbn-atpt (&optional arg)
  "Triplebacktick ISBN at point."
  (interactive "*P")
  (ar-th-triplebacktick 'isbn arg arg))

(defun ar-triplebacktick-line-atpt (&optional arg)
  "Triplebacktick LINE at point."
  (interactive "*P")
  (ar-th-triplebacktick 'line arg arg))

(defun ar-triplebacktick-list-atpt (&optional arg)
  "Triplebacktick LIST at point."
  (interactive "*P")
  (ar-th-triplebacktick 'list arg arg))

(defun ar-triplebacktick-name-atpt (&optional arg)
  "Triplebacktick NAME at point."
  (interactive "*P")
  (ar-th-triplebacktick 'name arg arg))

(defun ar-triplebacktick-number-atpt (&optional arg)
  "Triplebacktick NUMBER at point."
  (interactive "*P")
  (ar-th-triplebacktick 'number arg arg))

(defun ar-triplebacktick-page-atpt (&optional arg)
  "Triplebacktick PAGE at point."
  (interactive "*P")
  (ar-th-triplebacktick 'page arg arg))

(defun ar-triplebacktick-paragraph-atpt (&optional arg)
  "Triplebacktick PARAGRAPH at point."
  (interactive "*P")
  (ar-th-triplebacktick 'paragraph arg arg))

(defun ar-triplebacktick-phone-atpt (&optional arg)
  "Triplebacktick PHONE at point."
  (interactive "*P")
  (ar-th-triplebacktick 'phone arg arg))

(defun ar-triplebacktick-region-atpt (&optional arg)
  "Triplebacktick REGION at point."
  (interactive "*P")
  (ar-th-triplebacktick 'region arg arg))

(defun ar-triplebacktick-sentence-atpt (&optional arg)
  "Triplebacktick SENTENCE at point."
  (interactive "*P")
  (ar-th-triplebacktick 'sentence arg arg))

(defun ar-triplebacktick-sexp-atpt (&optional arg)
  "Triplebacktick SEXP at point."
  (interactive "*P")
  (ar-th-triplebacktick 'sexp arg arg))

(defun ar-triplebacktick-shstruct-atpt (&optional arg)
  "Triplebacktick SHSTRUCT at point."
  (interactive "*P")
  (ar-th-triplebacktick 'shstruct arg arg))

(defun ar-triplebacktick-symbol-atpt (&optional arg)
  "Triplebacktick SYMBOL at point."
  (interactive "*P")
  (ar-th-triplebacktick 'symbol arg arg))

(defun ar-triplebacktick-url-atpt (&optional arg)
  "Triplebacktick URL at point."
  (interactive "*P")
  (ar-th-triplebacktick 'url arg arg))

(defun ar-triplebacktick-word-atpt (&optional arg)
  "Triplebacktick WORD at point."
  (interactive "*P")
  (ar-th-triplebacktick 'word arg arg))

(defun ar-triplebacktick-wordalphaonly-atpt (&optional arg)
  "Triplebacktick WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-triplebacktick 'wordalphaonly arg arg))

(defun ar-underscore-greateranglednested-atpt (&optional arg)
  "Underscore GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-underscore 'greateranglednested arg arg))

(defun ar-underscore-lesseranglednested-atpt (&optional arg)
  "Underscore LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-underscore 'lesseranglednested arg arg))

(defun ar-underscore-buffer-atpt (&optional arg)
  "Underscore BUFFER at point."
  (interactive "*P")
  (ar-th-underscore 'buffer arg arg))

(defun ar-underscore-char-atpt (&optional arg)
  "Underscore CHAR at point."
  (interactive "*P")
  (ar-th-underscore 'char arg arg))

(defun ar-underscore-comment-atpt (&optional arg)
  "Underscore COMMENT at point."
  (interactive "*P")
  (ar-th-underscore 'comment arg arg))

(defun ar-underscore-csv-atpt (&optional arg)
  "Underscore CSV at point."
  (interactive "*P")
  (ar-th-underscore 'csv arg arg))

(defun ar-underscore-date-atpt (&optional arg)
  "Underscore DATE at point."
  (interactive "*P")
  (ar-th-underscore 'date arg arg))

(defun ar-underscore-delimited-atpt (&optional arg)
  "Underscore DELIMITED at point."
  (interactive "*P")
  (ar-th-underscore 'delimited arg arg))

(defun ar-underscore-email-atpt (&optional arg)
  "Underscore EMAIL at point."
  (interactive "*P")
  (ar-th-underscore 'email arg arg))

(defun ar-underscore-filename-atpt (&optional arg)
  "Underscore FILENAME at point."
  (interactive "*P")
  (ar-th-underscore 'filename arg arg))

(defun ar-underscore-filenamenondirectory-atpt (&optional arg)
  "Underscore FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-underscore 'filenamenondirectory arg arg))

(defun ar-underscore-float-atpt (&optional arg)
  "Underscore FLOAT at point."
  (interactive "*P")
  (ar-th-underscore 'float arg arg))

(defun ar-underscore-function-atpt (&optional arg)
  "Underscore FUNCTION at point."
  (interactive "*P")
  (ar-th-underscore 'function arg arg))

(defun ar-underscore-ip-atpt (&optional arg)
  "Underscore IP at point."
  (interactive "*P")
  (ar-th-underscore 'ip arg arg))

(defun ar-underscore-isbn-atpt (&optional arg)
  "Underscore ISBN at point."
  (interactive "*P")
  (ar-th-underscore 'isbn arg arg))

(defun ar-underscore-line-atpt (&optional arg)
  "Underscore LINE at point."
  (interactive "*P")
  (ar-th-underscore 'line arg arg))

(defun ar-underscore-list-atpt (&optional arg)
  "Underscore LIST at point."
  (interactive "*P")
  (ar-th-underscore 'list arg arg))

(defun ar-underscore-name-atpt (&optional arg)
  "Underscore NAME at point."
  (interactive "*P")
  (ar-th-underscore 'name arg arg))

(defun ar-underscore-number-atpt (&optional arg)
  "Underscore NUMBER at point."
  (interactive "*P")
  (ar-th-underscore 'number arg arg))

(defun ar-underscore-page-atpt (&optional arg)
  "Underscore PAGE at point."
  (interactive "*P")
  (ar-th-underscore 'page arg arg))

(defun ar-underscore-paragraph-atpt (&optional arg)
  "Underscore PARAGRAPH at point."
  (interactive "*P")
  (ar-th-underscore 'paragraph arg arg))

(defun ar-underscore-phone-atpt (&optional arg)
  "Underscore PHONE at point."
  (interactive "*P")
  (ar-th-underscore 'phone arg arg))

(defun ar-underscore-region-atpt (&optional arg)
  "Underscore REGION at point."
  (interactive "*P")
  (ar-th-underscore 'region arg arg))

(defun ar-underscore-sentence-atpt (&optional arg)
  "Underscore SENTENCE at point."
  (interactive "*P")
  (ar-th-underscore 'sentence arg arg))

(defun ar-underscore-sexp-atpt (&optional arg)
  "Underscore SEXP at point."
  (interactive "*P")
  (ar-th-underscore 'sexp arg arg))

(defun ar-underscore-shstruct-atpt (&optional arg)
  "Underscore SHSTRUCT at point."
  (interactive "*P")
  (ar-th-underscore 'shstruct arg arg))

(defun ar-underscore-symbol-atpt (&optional arg)
  "Underscore SYMBOL at point."
  (interactive "*P")
  (ar-th-underscore 'symbol arg arg))

(defun ar-underscore-url-atpt (&optional arg)
  "Underscore URL at point."
  (interactive "*P")
  (ar-th-underscore 'url arg arg))

(defun ar-underscore-word-atpt (&optional arg)
  "Underscore WORD at point."
  (interactive "*P")
  (ar-th-underscore 'word arg arg))

(defun ar-underscore-wordalphaonly-atpt (&optional arg)
  "Underscore WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-underscore 'wordalphaonly arg arg))

(defun ar-whitespace-greateranglednested-atpt (&optional arg)
  "Whitespace GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-whitespace 'greateranglednested arg arg))

(defun ar-whitespace-lesseranglednested-atpt (&optional arg)
  "Whitespace LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-whitespace 'lesseranglednested arg arg))

(defun ar-whitespace-buffer-atpt (&optional arg)
  "Whitespace BUFFER at point."
  (interactive "*P")
  (ar-th-whitespace 'buffer arg arg))

(defun ar-whitespace-char-atpt (&optional arg)
  "Whitespace CHAR at point."
  (interactive "*P")
  (ar-th-whitespace 'char arg arg))

(defun ar-whitespace-comment-atpt (&optional arg)
  "Whitespace COMMENT at point."
  (interactive "*P")
  (ar-th-whitespace 'comment arg arg))

(defun ar-whitespace-csv-atpt (&optional arg)
  "Whitespace CSV at point."
  (interactive "*P")
  (ar-th-whitespace 'csv arg arg))

(defun ar-whitespace-date-atpt (&optional arg)
  "Whitespace DATE at point."
  (interactive "*P")
  (ar-th-whitespace 'date arg arg))

(defun ar-whitespace-delimited-atpt (&optional arg)
  "Whitespace DELIMITED at point."
  (interactive "*P")
  (ar-th-whitespace 'delimited arg arg))

(defun ar-whitespace-email-atpt (&optional arg)
  "Whitespace EMAIL at point."
  (interactive "*P")
  (ar-th-whitespace 'email arg arg))

(defun ar-whitespace-filename-atpt (&optional arg)
  "Whitespace FILENAME at point."
  (interactive "*P")
  (ar-th-whitespace 'filename arg arg))

(defun ar-whitespace-filenamenondirectory-atpt (&optional arg)
  "Whitespace FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-whitespace 'filenamenondirectory arg arg))

(defun ar-whitespace-float-atpt (&optional arg)
  "Whitespace FLOAT at point."
  (interactive "*P")
  (ar-th-whitespace 'float arg arg))

(defun ar-whitespace-function-atpt (&optional arg)
  "Whitespace FUNCTION at point."
  (interactive "*P")
  (ar-th-whitespace 'function arg arg))

(defun ar-whitespace-ip-atpt (&optional arg)
  "Whitespace IP at point."
  (interactive "*P")
  (ar-th-whitespace 'ip arg arg))

(defun ar-whitespace-isbn-atpt (&optional arg)
  "Whitespace ISBN at point."
  (interactive "*P")
  (ar-th-whitespace 'isbn arg arg))

(defun ar-whitespace-line-atpt (&optional arg)
  "Whitespace LINE at point."
  (interactive "*P")
  (ar-th-whitespace 'line arg arg))

(defun ar-whitespace-list-atpt (&optional arg)
  "Whitespace LIST at point."
  (interactive "*P")
  (ar-th-whitespace 'list arg arg))

(defun ar-whitespace-name-atpt (&optional arg)
  "Whitespace NAME at point."
  (interactive "*P")
  (ar-th-whitespace 'name arg arg))

(defun ar-whitespace-number-atpt (&optional arg)
  "Whitespace NUMBER at point."
  (interactive "*P")
  (ar-th-whitespace 'number arg arg))

(defun ar-whitespace-page-atpt (&optional arg)
  "Whitespace PAGE at point."
  (interactive "*P")
  (ar-th-whitespace 'page arg arg))

(defun ar-whitespace-paragraph-atpt (&optional arg)
  "Whitespace PARAGRAPH at point."
  (interactive "*P")
  (ar-th-whitespace 'paragraph arg arg))

(defun ar-whitespace-phone-atpt (&optional arg)
  "Whitespace PHONE at point."
  (interactive "*P")
  (ar-th-whitespace 'phone arg arg))

(defun ar-whitespace-region-atpt (&optional arg)
  "Whitespace REGION at point."
  (interactive "*P")
  (ar-th-whitespace 'region arg arg))

(defun ar-whitespace-sentence-atpt (&optional arg)
  "Whitespace SENTENCE at point."
  (interactive "*P")
  (ar-th-whitespace 'sentence arg arg))

(defun ar-whitespace-sexp-atpt (&optional arg)
  "Whitespace SEXP at point."
  (interactive "*P")
  (ar-th-whitespace 'sexp arg arg))

(defun ar-whitespace-shstruct-atpt (&optional arg)
  "Whitespace SHSTRUCT at point."
  (interactive "*P")
  (ar-th-whitespace 'shstruct arg arg))

(defun ar-whitespace-symbol-atpt (&optional arg)
  "Whitespace SYMBOL at point."
  (interactive "*P")
  (ar-th-whitespace 'symbol arg arg))

(defun ar-whitespace-url-atpt (&optional arg)
  "Whitespace URL at point."
  (interactive "*P")
  (ar-th-whitespace 'url arg arg))

(defun ar-whitespace-word-atpt (&optional arg)
  "Whitespace WORD at point."
  (interactive "*P")
  (ar-th-whitespace 'word arg arg))

(defun ar-whitespace-wordalphaonly-atpt (&optional arg)
  "Whitespace WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-whitespace 'wordalphaonly arg arg))

;; ar-thing-at-point-utils-aktiv-passiv ar-unpaired-delimit-aktiv ar-atpt-rest-list: end
;; ar-thing-at-point-utils-pair-rest ar-paired-delimit-aktiv-raw ar-atpt-rest-list: start
(defun ar-brace-greateranglednested-atpt (&optional arg)
  "Brace GREATERANGLEDNESTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'greateranglednested "{" "}" arg arg)))

(defun ar-brace-lesseranglednested-atpt (&optional arg)
  "Brace LESSERANGLEDNESTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'lesseranglednested "{" "}" arg arg)))

(defun ar-brace-buffer-atpt (&optional arg)
  "Brace BUFFER at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'buffer "{" "}" arg arg)))

(defun ar-brace-char-atpt (&optional arg)
  "Brace CHAR at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'char "{" "}" arg arg)))

(defun ar-brace-comment-atpt (&optional arg)
  "Brace COMMENT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'comment "{" "}" arg arg)))

(defun ar-brace-csv-atpt (&optional arg)
  "Brace CSV at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'csv "{" "}" arg arg)))

(defun ar-brace-date-atpt (&optional arg)
  "Brace DATE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'date "{" "}" arg arg)))

(defun ar-brace-delimited-atpt (&optional arg)
  "Brace DELIMITED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'delimited "{" "}" arg arg)))

(defun ar-brace-email-atpt (&optional arg)
  "Brace EMAIL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'email "{" "}" arg arg)))

(defun ar-brace-filename-atpt (&optional arg)
  "Brace FILENAME at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'filename "{" "}" arg arg)))

(defun ar-brace-filenamenondirectory-atpt (&optional arg)
  "Brace FILENAMENONDIRECTORY at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'filenamenondirectory "{" "}" arg arg)))

(defun ar-brace-float-atpt (&optional arg)
  "Brace FLOAT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'float "{" "}" arg arg)))

(defun ar-brace-function-atpt (&optional arg)
  "Brace FUNCTION at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'function "{" "}" arg arg)))

(defun ar-brace-ip-atpt (&optional arg)
  "Brace IP at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'ip "{" "}" arg arg)))

(defun ar-brace-isbn-atpt (&optional arg)
  "Brace ISBN at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'isbn "{" "}" arg arg)))

(defun ar-brace-line-atpt (&optional arg)
  "Brace LINE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'line "{" "}" arg arg)))

(defun ar-brace-list-atpt (&optional arg)
  "Brace LIST at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'list "{" "}" arg arg)))

(defun ar-brace-name-atpt (&optional arg)
  "Brace NAME at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'name "{" "}" arg arg)))

(defun ar-brace-number-atpt (&optional arg)
  "Brace NUMBER at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'number "{" "}" arg arg)))

(defun ar-brace-page-atpt (&optional arg)
  "Brace PAGE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'page "{" "}" arg arg)))

(defun ar-brace-paragraph-atpt (&optional arg)
  "Brace PARAGRAPH at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'paragraph "{" "}" arg arg)))

(defun ar-brace-phone-atpt (&optional arg)
  "Brace PHONE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'phone "{" "}" arg arg)))

(defun ar-brace-region-atpt (&optional arg)
  "Brace REGION at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'region "{" "}" arg arg)))

(defun ar-brace-sentence-atpt (&optional arg)
  "Brace SENTENCE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'sentence "{" "}" arg arg)))

(defun ar-brace-sexp-atpt (&optional arg)
  "Brace SEXP at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'sexp "{" "}" arg arg)))

(defun ar-brace-shstruct-atpt (&optional arg)
  "Brace SHSTRUCT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'shstruct "{" "}" arg arg)))

(defun ar-brace-symbol-atpt (&optional arg)
  "Brace SYMBOL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'symbol "{" "}" arg arg)))

(defun ar-brace-url-atpt (&optional arg)
  "Brace URL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'url "{" "}" arg arg)))

(defun ar-brace-word-atpt (&optional arg)
  "Brace WORD at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'word "{" "}" arg arg)))

(defun ar-brace-wordalphaonly-atpt (&optional arg)
  "Brace WORDALPHAONLY at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'wordalphaonly "{" "}" arg arg)))

(defun ar-bracket-greateranglednested-atpt (&optional arg)
  "Bracket GREATERANGLEDNESTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'greateranglednested "[" "]" arg arg)))

(defun ar-bracket-lesseranglednested-atpt (&optional arg)
  "Bracket LESSERANGLEDNESTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'lesseranglednested "[" "]" arg arg)))

(defun ar-bracket-buffer-atpt (&optional arg)
  "Bracket BUFFER at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'buffer "[" "]" arg arg)))

(defun ar-bracket-char-atpt (&optional arg)
  "Bracket CHAR at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'char "[" "]" arg arg)))

(defun ar-bracket-comment-atpt (&optional arg)
  "Bracket COMMENT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'comment "[" "]" arg arg)))

(defun ar-bracket-csv-atpt (&optional arg)
  "Bracket CSV at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'csv "[" "]" arg arg)))

(defun ar-bracket-date-atpt (&optional arg)
  "Bracket DATE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'date "[" "]" arg arg)))

(defun ar-bracket-delimited-atpt (&optional arg)
  "Bracket DELIMITED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'delimited "[" "]" arg arg)))

(defun ar-bracket-email-atpt (&optional arg)
  "Bracket EMAIL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'email "[" "]" arg arg)))

(defun ar-bracket-filename-atpt (&optional arg)
  "Bracket FILENAME at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'filename "[" "]" arg arg)))

(defun ar-bracket-filenamenondirectory-atpt (&optional arg)
  "Bracket FILENAMENONDIRECTORY at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'filenamenondirectory "[" "]" arg arg)))

(defun ar-bracket-float-atpt (&optional arg)
  "Bracket FLOAT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'float "[" "]" arg arg)))

(defun ar-bracket-function-atpt (&optional arg)
  "Bracket FUNCTION at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'function "[" "]" arg arg)))

(defun ar-bracket-ip-atpt (&optional arg)
  "Bracket IP at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'ip "[" "]" arg arg)))

(defun ar-bracket-isbn-atpt (&optional arg)
  "Bracket ISBN at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'isbn "[" "]" arg arg)))

(defun ar-bracket-line-atpt (&optional arg)
  "Bracket LINE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'line "[" "]" arg arg)))

(defun ar-bracket-list-atpt (&optional arg)
  "Bracket LIST at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'list "[" "]" arg arg)))

(defun ar-bracket-name-atpt (&optional arg)
  "Bracket NAME at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'name "[" "]" arg arg)))

(defun ar-bracket-number-atpt (&optional arg)
  "Bracket NUMBER at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'number "[" "]" arg arg)))

(defun ar-bracket-page-atpt (&optional arg)
  "Bracket PAGE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'page "[" "]" arg arg)))

(defun ar-bracket-paragraph-atpt (&optional arg)
  "Bracket PARAGRAPH at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'paragraph "[" "]" arg arg)))

(defun ar-bracket-phone-atpt (&optional arg)
  "Bracket PHONE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'phone "[" "]" arg arg)))

(defun ar-bracket-region-atpt (&optional arg)
  "Bracket REGION at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'region "[" "]" arg arg)))

(defun ar-bracket-sentence-atpt (&optional arg)
  "Bracket SENTENCE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'sentence "[" "]" arg arg)))

(defun ar-bracket-sexp-atpt (&optional arg)
  "Bracket SEXP at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'sexp "[" "]" arg arg)))

(defun ar-bracket-shstruct-atpt (&optional arg)
  "Bracket SHSTRUCT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'shstruct "[" "]" arg arg)))

(defun ar-bracket-symbol-atpt (&optional arg)
  "Bracket SYMBOL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'symbol "[" "]" arg arg)))

(defun ar-bracket-url-atpt (&optional arg)
  "Bracket URL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'url "[" "]" arg arg)))

(defun ar-bracket-word-atpt (&optional arg)
  "Bracket WORD at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'word "[" "]" arg arg)))

(defun ar-bracket-wordalphaonly-atpt (&optional arg)
  "Bracket WORDALPHAONLY at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'wordalphaonly "[" "]" arg arg)))

(defun ar-lesserangle-greateranglednested-atpt (&optional arg)
  "Lesserangle GREATERANGLEDNESTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'greateranglednested "<" ">" arg arg)))

(defun ar-lesserangle-lesseranglednested-atpt (&optional arg)
  "Lesserangle LESSERANGLEDNESTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'lesseranglednested "<" ">" arg arg)))

(defun ar-lesserangle-buffer-atpt (&optional arg)
  "Lesserangle BUFFER at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'buffer "<" ">" arg arg)))

(defun ar-lesserangle-char-atpt (&optional arg)
  "Lesserangle CHAR at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'char "<" ">" arg arg)))

(defun ar-lesserangle-comment-atpt (&optional arg)
  "Lesserangle COMMENT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'comment "<" ">" arg arg)))

(defun ar-lesserangle-csv-atpt (&optional arg)
  "Lesserangle CSV at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'csv "<" ">" arg arg)))

(defun ar-lesserangle-date-atpt (&optional arg)
  "Lesserangle DATE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'date "<" ">" arg arg)))

(defun ar-lesserangle-delimited-atpt (&optional arg)
  "Lesserangle DELIMITED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'delimited "<" ">" arg arg)))

(defun ar-lesserangle-email-atpt (&optional arg)
  "Lesserangle EMAIL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'email "<" ">" arg arg)))

(defun ar-lesserangle-filename-atpt (&optional arg)
  "Lesserangle FILENAME at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'filename "<" ">" arg arg)))

(defun ar-lesserangle-filenamenondirectory-atpt (&optional arg)
  "Lesserangle FILENAMENONDIRECTORY at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'filenamenondirectory "<" ">" arg arg)))

(defun ar-lesserangle-float-atpt (&optional arg)
  "Lesserangle FLOAT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'float "<" ">" arg arg)))

(defun ar-lesserangle-function-atpt (&optional arg)
  "Lesserangle FUNCTION at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'function "<" ">" arg arg)))

(defun ar-lesserangle-ip-atpt (&optional arg)
  "Lesserangle IP at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'ip "<" ">" arg arg)))

(defun ar-lesserangle-isbn-atpt (&optional arg)
  "Lesserangle ISBN at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'isbn "<" ">" arg arg)))

(defun ar-lesserangle-line-atpt (&optional arg)
  "Lesserangle LINE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'line "<" ">" arg arg)))

(defun ar-lesserangle-list-atpt (&optional arg)
  "Lesserangle LIST at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'list "<" ">" arg arg)))

(defun ar-lesserangle-name-atpt (&optional arg)
  "Lesserangle NAME at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'name "<" ">" arg arg)))

(defun ar-lesserangle-number-atpt (&optional arg)
  "Lesserangle NUMBER at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'number "<" ">" arg arg)))

(defun ar-lesserangle-page-atpt (&optional arg)
  "Lesserangle PAGE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'page "<" ">" arg arg)))

(defun ar-lesserangle-paragraph-atpt (&optional arg)
  "Lesserangle PARAGRAPH at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'paragraph "<" ">" arg arg)))

(defun ar-lesserangle-phone-atpt (&optional arg)
  "Lesserangle PHONE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'phone "<" ">" arg arg)))

(defun ar-lesserangle-region-atpt (&optional arg)
  "Lesserangle REGION at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'region "<" ">" arg arg)))

(defun ar-lesserangle-sentence-atpt (&optional arg)
  "Lesserangle SENTENCE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'sentence "<" ">" arg arg)))

(defun ar-lesserangle-sexp-atpt (&optional arg)
  "Lesserangle SEXP at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'sexp "<" ">" arg arg)))

(defun ar-lesserangle-shstruct-atpt (&optional arg)
  "Lesserangle SHSTRUCT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'shstruct "<" ">" arg arg)))

(defun ar-lesserangle-symbol-atpt (&optional arg)
  "Lesserangle SYMBOL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'symbol "<" ">" arg arg)))

(defun ar-lesserangle-url-atpt (&optional arg)
  "Lesserangle URL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'url "<" ">" arg arg)))

(defun ar-lesserangle-word-atpt (&optional arg)
  "Lesserangle WORD at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'word "<" ">" arg arg)))

(defun ar-lesserangle-wordalphaonly-atpt (&optional arg)
  "Lesserangle WORDALPHAONLY at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'wordalphaonly "<" ">" arg arg)))

(defun ar-greaterangle-greateranglednested-atpt (&optional arg)
  "Greaterangle GREATERANGLEDNESTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'greateranglednested ">" "<" arg arg)))

(defun ar-greaterangle-lesseranglednested-atpt (&optional arg)
  "Greaterangle LESSERANGLEDNESTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'lesseranglednested ">" "<" arg arg)))

(defun ar-greaterangle-buffer-atpt (&optional arg)
  "Greaterangle BUFFER at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'buffer ">" "<" arg arg)))

(defun ar-greaterangle-char-atpt (&optional arg)
  "Greaterangle CHAR at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'char ">" "<" arg arg)))

(defun ar-greaterangle-comment-atpt (&optional arg)
  "Greaterangle COMMENT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'comment ">" "<" arg arg)))

(defun ar-greaterangle-csv-atpt (&optional arg)
  "Greaterangle CSV at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'csv ">" "<" arg arg)))

(defun ar-greaterangle-date-atpt (&optional arg)
  "Greaterangle DATE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'date ">" "<" arg arg)))

(defun ar-greaterangle-delimited-atpt (&optional arg)
  "Greaterangle DELIMITED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'delimited ">" "<" arg arg)))

(defun ar-greaterangle-email-atpt (&optional arg)
  "Greaterangle EMAIL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'email ">" "<" arg arg)))

(defun ar-greaterangle-filename-atpt (&optional arg)
  "Greaterangle FILENAME at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'filename ">" "<" arg arg)))

(defun ar-greaterangle-filenamenondirectory-atpt (&optional arg)
  "Greaterangle FILENAMENONDIRECTORY at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'filenamenondirectory ">" "<" arg arg)))

(defun ar-greaterangle-float-atpt (&optional arg)
  "Greaterangle FLOAT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'float ">" "<" arg arg)))

(defun ar-greaterangle-function-atpt (&optional arg)
  "Greaterangle FUNCTION at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'function ">" "<" arg arg)))

(defun ar-greaterangle-ip-atpt (&optional arg)
  "Greaterangle IP at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'ip ">" "<" arg arg)))

(defun ar-greaterangle-isbn-atpt (&optional arg)
  "Greaterangle ISBN at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'isbn ">" "<" arg arg)))

(defun ar-greaterangle-line-atpt (&optional arg)
  "Greaterangle LINE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'line ">" "<" arg arg)))

(defun ar-greaterangle-list-atpt (&optional arg)
  "Greaterangle LIST at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'list ">" "<" arg arg)))

(defun ar-greaterangle-name-atpt (&optional arg)
  "Greaterangle NAME at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'name ">" "<" arg arg)))

(defun ar-greaterangle-number-atpt (&optional arg)
  "Greaterangle NUMBER at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'number ">" "<" arg arg)))

(defun ar-greaterangle-page-atpt (&optional arg)
  "Greaterangle PAGE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'page ">" "<" arg arg)))

(defun ar-greaterangle-paragraph-atpt (&optional arg)
  "Greaterangle PARAGRAPH at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'paragraph ">" "<" arg arg)))

(defun ar-greaterangle-phone-atpt (&optional arg)
  "Greaterangle PHONE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'phone ">" "<" arg arg)))

(defun ar-greaterangle-region-atpt (&optional arg)
  "Greaterangle REGION at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'region ">" "<" arg arg)))

(defun ar-greaterangle-sentence-atpt (&optional arg)
  "Greaterangle SENTENCE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'sentence ">" "<" arg arg)))

(defun ar-greaterangle-sexp-atpt (&optional arg)
  "Greaterangle SEXP at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'sexp ">" "<" arg arg)))

(defun ar-greaterangle-shstruct-atpt (&optional arg)
  "Greaterangle SHSTRUCT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'shstruct ">" "<" arg arg)))

(defun ar-greaterangle-symbol-atpt (&optional arg)
  "Greaterangle SYMBOL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'symbol ">" "<" arg arg)))

(defun ar-greaterangle-url-atpt (&optional arg)
  "Greaterangle URL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'url ">" "<" arg arg)))

(defun ar-greaterangle-word-atpt (&optional arg)
  "Greaterangle WORD at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'word ">" "<" arg arg)))

(defun ar-greaterangle-wordalphaonly-atpt (&optional arg)
  "Greaterangle WORDALPHAONLY at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'wordalphaonly ">" "<" arg arg)))

(defun ar-curvedsinglequote-greateranglednested-atpt (&optional arg)
  "Curvedsinglequote GREATERANGLEDNESTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'greateranglednested "‘" "’" arg arg)))

(defun ar-curvedsinglequote-lesseranglednested-atpt (&optional arg)
  "Curvedsinglequote LESSERANGLEDNESTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'lesseranglednested "‘" "’" arg arg)))

(defun ar-curvedsinglequote-buffer-atpt (&optional arg)
  "Curvedsinglequote BUFFER at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'buffer "‘" "’" arg arg)))

(defun ar-curvedsinglequote-char-atpt (&optional arg)
  "Curvedsinglequote CHAR at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'char "‘" "’" arg arg)))

(defun ar-curvedsinglequote-comment-atpt (&optional arg)
  "Curvedsinglequote COMMENT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'comment "‘" "’" arg arg)))

(defun ar-curvedsinglequote-csv-atpt (&optional arg)
  "Curvedsinglequote CSV at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'csv "‘" "’" arg arg)))

(defun ar-curvedsinglequote-date-atpt (&optional arg)
  "Curvedsinglequote DATE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'date "‘" "’" arg arg)))

(defun ar-curvedsinglequote-delimited-atpt (&optional arg)
  "Curvedsinglequote DELIMITED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'delimited "‘" "’" arg arg)))

(defun ar-curvedsinglequote-email-atpt (&optional arg)
  "Curvedsinglequote EMAIL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'email "‘" "’" arg arg)))

(defun ar-curvedsinglequote-filename-atpt (&optional arg)
  "Curvedsinglequote FILENAME at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'filename "‘" "’" arg arg)))

(defun ar-curvedsinglequote-filenamenondirectory-atpt (&optional arg)
  "Curvedsinglequote FILENAMENONDIRECTORY at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'filenamenondirectory "‘" "’" arg arg)))

(defun ar-curvedsinglequote-float-atpt (&optional arg)
  "Curvedsinglequote FLOAT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'float "‘" "’" arg arg)))

(defun ar-curvedsinglequote-function-atpt (&optional arg)
  "Curvedsinglequote FUNCTION at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'function "‘" "’" arg arg)))

(defun ar-curvedsinglequote-ip-atpt (&optional arg)
  "Curvedsinglequote IP at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'ip "‘" "’" arg arg)))

(defun ar-curvedsinglequote-isbn-atpt (&optional arg)
  "Curvedsinglequote ISBN at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'isbn "‘" "’" arg arg)))

(defun ar-curvedsinglequote-line-atpt (&optional arg)
  "Curvedsinglequote LINE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'line "‘" "’" arg arg)))

(defun ar-curvedsinglequote-list-atpt (&optional arg)
  "Curvedsinglequote LIST at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'list "‘" "’" arg arg)))

(defun ar-curvedsinglequote-name-atpt (&optional arg)
  "Curvedsinglequote NAME at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'name "‘" "’" arg arg)))

(defun ar-curvedsinglequote-number-atpt (&optional arg)
  "Curvedsinglequote NUMBER at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'number "‘" "’" arg arg)))

(defun ar-curvedsinglequote-page-atpt (&optional arg)
  "Curvedsinglequote PAGE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'page "‘" "’" arg arg)))

(defun ar-curvedsinglequote-paragraph-atpt (&optional arg)
  "Curvedsinglequote PARAGRAPH at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'paragraph "‘" "’" arg arg)))

(defun ar-curvedsinglequote-phone-atpt (&optional arg)
  "Curvedsinglequote PHONE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'phone "‘" "’" arg arg)))

(defun ar-curvedsinglequote-region-atpt (&optional arg)
  "Curvedsinglequote REGION at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'region "‘" "’" arg arg)))

(defun ar-curvedsinglequote-sentence-atpt (&optional arg)
  "Curvedsinglequote SENTENCE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'sentence "‘" "’" arg arg)))

(defun ar-curvedsinglequote-sexp-atpt (&optional arg)
  "Curvedsinglequote SEXP at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'sexp "‘" "’" arg arg)))

(defun ar-curvedsinglequote-shstruct-atpt (&optional arg)
  "Curvedsinglequote SHSTRUCT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'shstruct "‘" "’" arg arg)))

(defun ar-curvedsinglequote-symbol-atpt (&optional arg)
  "Curvedsinglequote SYMBOL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'symbol "‘" "’" arg arg)))

(defun ar-curvedsinglequote-url-atpt (&optional arg)
  "Curvedsinglequote URL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'url "‘" "’" arg arg)))

(defun ar-curvedsinglequote-word-atpt (&optional arg)
  "Curvedsinglequote WORD at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'word "‘" "’" arg arg)))

(defun ar-curvedsinglequote-wordalphaonly-atpt (&optional arg)
  "Curvedsinglequote WORDALPHAONLY at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'wordalphaonly "‘" "’" arg arg)))

(defun ar-curveddoublequote-greateranglednested-atpt (&optional arg)
  "Curveddoublequote GREATERANGLEDNESTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'greateranglednested "“" "”" arg arg)))

(defun ar-curveddoublequote-lesseranglednested-atpt (&optional arg)
  "Curveddoublequote LESSERANGLEDNESTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'lesseranglednested "“" "”" arg arg)))

(defun ar-curveddoublequote-buffer-atpt (&optional arg)
  "Curveddoublequote BUFFER at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'buffer "“" "”" arg arg)))

(defun ar-curveddoublequote-char-atpt (&optional arg)
  "Curveddoublequote CHAR at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'char "“" "”" arg arg)))

(defun ar-curveddoublequote-comment-atpt (&optional arg)
  "Curveddoublequote COMMENT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'comment "“" "”" arg arg)))

(defun ar-curveddoublequote-csv-atpt (&optional arg)
  "Curveddoublequote CSV at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'csv "“" "”" arg arg)))

(defun ar-curveddoublequote-date-atpt (&optional arg)
  "Curveddoublequote DATE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'date "“" "”" arg arg)))

(defun ar-curveddoublequote-delimited-atpt (&optional arg)
  "Curveddoublequote DELIMITED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'delimited "“" "”" arg arg)))

(defun ar-curveddoublequote-email-atpt (&optional arg)
  "Curveddoublequote EMAIL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'email "“" "”" arg arg)))

(defun ar-curveddoublequote-filename-atpt (&optional arg)
  "Curveddoublequote FILENAME at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'filename "“" "”" arg arg)))

(defun ar-curveddoublequote-filenamenondirectory-atpt (&optional arg)
  "Curveddoublequote FILENAMENONDIRECTORY at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'filenamenondirectory "“" "”" arg arg)))

(defun ar-curveddoublequote-float-atpt (&optional arg)
  "Curveddoublequote FLOAT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'float "“" "”" arg arg)))

(defun ar-curveddoublequote-function-atpt (&optional arg)
  "Curveddoublequote FUNCTION at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'function "“" "”" arg arg)))

(defun ar-curveddoublequote-ip-atpt (&optional arg)
  "Curveddoublequote IP at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'ip "“" "”" arg arg)))

(defun ar-curveddoublequote-isbn-atpt (&optional arg)
  "Curveddoublequote ISBN at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'isbn "“" "”" arg arg)))

(defun ar-curveddoublequote-line-atpt (&optional arg)
  "Curveddoublequote LINE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'line "“" "”" arg arg)))

(defun ar-curveddoublequote-list-atpt (&optional arg)
  "Curveddoublequote LIST at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'list "“" "”" arg arg)))

(defun ar-curveddoublequote-name-atpt (&optional arg)
  "Curveddoublequote NAME at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'name "“" "”" arg arg)))

(defun ar-curveddoublequote-number-atpt (&optional arg)
  "Curveddoublequote NUMBER at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'number "“" "”" arg arg)))

(defun ar-curveddoublequote-page-atpt (&optional arg)
  "Curveddoublequote PAGE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'page "“" "”" arg arg)))

(defun ar-curveddoublequote-paragraph-atpt (&optional arg)
  "Curveddoublequote PARAGRAPH at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'paragraph "“" "”" arg arg)))

(defun ar-curveddoublequote-phone-atpt (&optional arg)
  "Curveddoublequote PHONE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'phone "“" "”" arg arg)))

(defun ar-curveddoublequote-region-atpt (&optional arg)
  "Curveddoublequote REGION at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'region "“" "”" arg arg)))

(defun ar-curveddoublequote-sentence-atpt (&optional arg)
  "Curveddoublequote SENTENCE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'sentence "“" "”" arg arg)))

(defun ar-curveddoublequote-sexp-atpt (&optional arg)
  "Curveddoublequote SEXP at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'sexp "“" "”" arg arg)))

(defun ar-curveddoublequote-shstruct-atpt (&optional arg)
  "Curveddoublequote SHSTRUCT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'shstruct "“" "”" arg arg)))

(defun ar-curveddoublequote-symbol-atpt (&optional arg)
  "Curveddoublequote SYMBOL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'symbol "“" "”" arg arg)))

(defun ar-curveddoublequote-url-atpt (&optional arg)
  "Curveddoublequote URL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'url "“" "”" arg arg)))

(defun ar-curveddoublequote-word-atpt (&optional arg)
  "Curveddoublequote WORD at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'word "“" "”" arg arg)))

(defun ar-curveddoublequote-wordalphaonly-atpt (&optional arg)
  "Curveddoublequote WORDALPHAONLY at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'wordalphaonly "“" "”" arg arg)))

(defun ar-parentize-greateranglednested-atpt (&optional arg)
  "Parentize GREATERANGLEDNESTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'greateranglednested "(" ")" arg arg)))

(defun ar-parentize-lesseranglednested-atpt (&optional arg)
  "Parentize LESSERANGLEDNESTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'lesseranglednested "(" ")" arg arg)))

(defun ar-parentize-buffer-atpt (&optional arg)
  "Parentize BUFFER at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'buffer "(" ")" arg arg)))

(defun ar-parentize-char-atpt (&optional arg)
  "Parentize CHAR at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'char "(" ")" arg arg)))

(defun ar-parentize-comment-atpt (&optional arg)
  "Parentize COMMENT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'comment "(" ")" arg arg)))

(defun ar-parentize-csv-atpt (&optional arg)
  "Parentize CSV at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'csv "(" ")" arg arg)))

(defun ar-parentize-date-atpt (&optional arg)
  "Parentize DATE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'date "(" ")" arg arg)))

(defun ar-parentize-delimited-atpt (&optional arg)
  "Parentize DELIMITED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'delimited "(" ")" arg arg)))

(defun ar-parentize-email-atpt (&optional arg)
  "Parentize EMAIL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'email "(" ")" arg arg)))

(defun ar-parentize-filename-atpt (&optional arg)
  "Parentize FILENAME at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'filename "(" ")" arg arg)))

(defun ar-parentize-filenamenondirectory-atpt (&optional arg)
  "Parentize FILENAMENONDIRECTORY at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'filenamenondirectory "(" ")" arg arg)))

(defun ar-parentize-float-atpt (&optional arg)
  "Parentize FLOAT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'float "(" ")" arg arg)))

(defun ar-parentize-function-atpt (&optional arg)
  "Parentize FUNCTION at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'function "(" ")" arg arg)))

(defun ar-parentize-ip-atpt (&optional arg)
  "Parentize IP at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'ip "(" ")" arg arg)))

(defun ar-parentize-isbn-atpt (&optional arg)
  "Parentize ISBN at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'isbn "(" ")" arg arg)))

(defun ar-parentize-line-atpt (&optional arg)
  "Parentize LINE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'line "(" ")" arg arg)))

(defun ar-parentize-list-atpt (&optional arg)
  "Parentize LIST at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'list "(" ")" arg arg)))

(defun ar-parentize-name-atpt (&optional arg)
  "Parentize NAME at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'name "(" ")" arg arg)))

(defun ar-parentize-number-atpt (&optional arg)
  "Parentize NUMBER at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'number "(" ")" arg arg)))

(defun ar-parentize-page-atpt (&optional arg)
  "Parentize PAGE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'page "(" ")" arg arg)))

(defun ar-parentize-paragraph-atpt (&optional arg)
  "Parentize PARAGRAPH at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'paragraph "(" ")" arg arg)))

(defun ar-parentize-phone-atpt (&optional arg)
  "Parentize PHONE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'phone "(" ")" arg arg)))

(defun ar-parentize-region-atpt (&optional arg)
  "Parentize REGION at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'region "(" ")" arg arg)))

(defun ar-parentize-sentence-atpt (&optional arg)
  "Parentize SENTENCE at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'sentence "(" ")" arg arg)))

(defun ar-parentize-sexp-atpt (&optional arg)
  "Parentize SEXP at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'sexp "(" ")" arg arg)))

(defun ar-parentize-shstruct-atpt (&optional arg)
  "Parentize SHSTRUCT at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'shstruct "(" ")" arg arg)))

(defun ar-parentize-symbol-atpt (&optional arg)
  "Parentize SYMBOL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'symbol "(" ")" arg arg)))

(defun ar-parentize-url-atpt (&optional arg)
  "Parentize URL at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'url "(" ")" arg arg)))

(defun ar-parentize-word-atpt (&optional arg)
  "Parentize WORD at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'word "(" ")" arg arg)))

(defun ar-parentize-wordalphaonly-atpt (&optional arg)
  "Parentize WORDALPHAONLY at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'wordalphaonly "(" ")" arg arg)))

;; ar-thing-at-point-utils-pair-rest ar-paired-delimit-aktiv-raw ar-atpt-rest-list: end
;; ar-thing-at-point-utils-pair-rest ar-paired-delimit-aktiv-raw ar-unpaired-delimited-passiv: start
(defun ar-brace-backslashed-atpt (&optional arg)
  "Brace BACKSLASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'backslashed "{" "}" arg arg)))

(defun ar-brace-backticked-atpt (&optional arg)
  "Brace BACKTICKED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'backticked "{" "}" arg arg)))

(defun ar-brace-coloned-atpt (&optional arg)
  "Brace COLONED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'coloned "{" "}" arg arg)))

(defun ar-brace-crossed-atpt (&optional arg)
  "Brace CROSSED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'crossed "{" "}" arg arg)))

(defun ar-brace-dollared-atpt (&optional arg)
  "Brace DOLLARED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'dollared "{" "}" arg arg)))

(defun ar-brace-doublequoted-atpt (&optional arg)
  "Brace DOUBLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'doublequoted "{" "}" arg arg)))

(defun ar-brace-equalized-atpt (&optional arg)
  "Brace EQUALIZED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'equalized "{" "}" arg arg)))

(defun ar-brace-hashed-atpt (&optional arg)
  "Brace HASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'hashed "{" "}" arg arg)))

(defun ar-brace-hyphened-atpt (&optional arg)
  "Brace HYPHENED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'hyphened "{" "}" arg arg)))

(defun ar-brace-piped-atpt (&optional arg)
  "Brace PIPED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'piped "{" "}" arg arg)))

(defun ar-brace-singlequoted-atpt (&optional arg)
  "Brace SINGLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'singlequoted "{" "}" arg arg)))

(defun ar-brace-slashed-atpt (&optional arg)
  "Brace SLASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'slashed "{" "}" arg arg)))

(defun ar-brace-stared-atpt (&optional arg)
  "Brace STARED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'stared "{" "}" arg arg)))

(defun ar-brace-tilded-atpt (&optional arg)
  "Brace TILDED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'tilded "{" "}" arg arg)))

(defun ar-brace-underscored-atpt (&optional arg)
  "Brace UNDERSCORED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'underscored "{" "}" arg arg)))

(defun ar-brace-whitespaced-atpt (&optional arg)
  "Brace WHITESPACED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'whitespaced "{" "}" arg arg)))

(defun ar-bracket-backslashed-atpt (&optional arg)
  "Bracket BACKSLASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'backslashed "[" "]" arg arg)))

(defun ar-bracket-backticked-atpt (&optional arg)
  "Bracket BACKTICKED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'backticked "[" "]" arg arg)))

(defun ar-bracket-coloned-atpt (&optional arg)
  "Bracket COLONED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'coloned "[" "]" arg arg)))

(defun ar-bracket-crossed-atpt (&optional arg)
  "Bracket CROSSED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'crossed "[" "]" arg arg)))

(defun ar-bracket-dollared-atpt (&optional arg)
  "Bracket DOLLARED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'dollared "[" "]" arg arg)))

(defun ar-bracket-doublequoted-atpt (&optional arg)
  "Bracket DOUBLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'doublequoted "[" "]" arg arg)))

(defun ar-bracket-equalized-atpt (&optional arg)
  "Bracket EQUALIZED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'equalized "[" "]" arg arg)))

(defun ar-bracket-hashed-atpt (&optional arg)
  "Bracket HASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'hashed "[" "]" arg arg)))

(defun ar-bracket-hyphened-atpt (&optional arg)
  "Bracket HYPHENED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'hyphened "[" "]" arg arg)))

(defun ar-bracket-piped-atpt (&optional arg)
  "Bracket PIPED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'piped "[" "]" arg arg)))

(defun ar-bracket-singlequoted-atpt (&optional arg)
  "Bracket SINGLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'singlequoted "[" "]" arg arg)))

(defun ar-bracket-slashed-atpt (&optional arg)
  "Bracket SLASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'slashed "[" "]" arg arg)))

(defun ar-bracket-stared-atpt (&optional arg)
  "Bracket STARED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'stared "[" "]" arg arg)))

(defun ar-bracket-tilded-atpt (&optional arg)
  "Bracket TILDED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'tilded "[" "]" arg arg)))

(defun ar-bracket-underscored-atpt (&optional arg)
  "Bracket UNDERSCORED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'underscored "[" "]" arg arg)))

(defun ar-bracket-whitespaced-atpt (&optional arg)
  "Bracket WHITESPACED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'whitespaced "[" "]" arg arg)))

(defun ar-lesserangle-backslashed-atpt (&optional arg)
  "Lesserangle BACKSLASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'backslashed "<" ">" arg arg)))

(defun ar-lesserangle-backticked-atpt (&optional arg)
  "Lesserangle BACKTICKED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'backticked "<" ">" arg arg)))

(defun ar-lesserangle-coloned-atpt (&optional arg)
  "Lesserangle COLONED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'coloned "<" ">" arg arg)))

(defun ar-lesserangle-crossed-atpt (&optional arg)
  "Lesserangle CROSSED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'crossed "<" ">" arg arg)))

(defun ar-lesserangle-dollared-atpt (&optional arg)
  "Lesserangle DOLLARED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'dollared "<" ">" arg arg)))

(defun ar-lesserangle-doublequoted-atpt (&optional arg)
  "Lesserangle DOUBLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'doublequoted "<" ">" arg arg)))

(defun ar-lesserangle-equalized-atpt (&optional arg)
  "Lesserangle EQUALIZED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'equalized "<" ">" arg arg)))

(defun ar-lesserangle-hashed-atpt (&optional arg)
  "Lesserangle HASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'hashed "<" ">" arg arg)))

(defun ar-lesserangle-hyphened-atpt (&optional arg)
  "Lesserangle HYPHENED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'hyphened "<" ">" arg arg)))

(defun ar-lesserangle-piped-atpt (&optional arg)
  "Lesserangle PIPED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'piped "<" ">" arg arg)))

(defun ar-lesserangle-singlequoted-atpt (&optional arg)
  "Lesserangle SINGLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'singlequoted "<" ">" arg arg)))

(defun ar-lesserangle-slashed-atpt (&optional arg)
  "Lesserangle SLASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'slashed "<" ">" arg arg)))

(defun ar-lesserangle-stared-atpt (&optional arg)
  "Lesserangle STARED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'stared "<" ">" arg arg)))

(defun ar-lesserangle-tilded-atpt (&optional arg)
  "Lesserangle TILDED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'tilded "<" ">" arg arg)))

(defun ar-lesserangle-underscored-atpt (&optional arg)
  "Lesserangle UNDERSCORED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'underscored "<" ">" arg arg)))

(defun ar-lesserangle-whitespaced-atpt (&optional arg)
  "Lesserangle WHITESPACED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'whitespaced "<" ">" arg arg)))

(defun ar-greaterangle-backslashed-atpt (&optional arg)
  "Greaterangle BACKSLASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'backslashed ">" "<" arg arg)))

(defun ar-greaterangle-backticked-atpt (&optional arg)
  "Greaterangle BACKTICKED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'backticked ">" "<" arg arg)))

(defun ar-greaterangle-coloned-atpt (&optional arg)
  "Greaterangle COLONED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'coloned ">" "<" arg arg)))

(defun ar-greaterangle-crossed-atpt (&optional arg)
  "Greaterangle CROSSED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'crossed ">" "<" arg arg)))

(defun ar-greaterangle-dollared-atpt (&optional arg)
  "Greaterangle DOLLARED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'dollared ">" "<" arg arg)))

(defun ar-greaterangle-doublequoted-atpt (&optional arg)
  "Greaterangle DOUBLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'doublequoted ">" "<" arg arg)))

(defun ar-greaterangle-equalized-atpt (&optional arg)
  "Greaterangle EQUALIZED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'equalized ">" "<" arg arg)))

(defun ar-greaterangle-hashed-atpt (&optional arg)
  "Greaterangle HASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'hashed ">" "<" arg arg)))

(defun ar-greaterangle-hyphened-atpt (&optional arg)
  "Greaterangle HYPHENED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'hyphened ">" "<" arg arg)))

(defun ar-greaterangle-piped-atpt (&optional arg)
  "Greaterangle PIPED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'piped ">" "<" arg arg)))

(defun ar-greaterangle-singlequoted-atpt (&optional arg)
  "Greaterangle SINGLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'singlequoted ">" "<" arg arg)))

(defun ar-greaterangle-slashed-atpt (&optional arg)
  "Greaterangle SLASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'slashed ">" "<" arg arg)))

(defun ar-greaterangle-stared-atpt (&optional arg)
  "Greaterangle STARED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'stared ">" "<" arg arg)))

(defun ar-greaterangle-tilded-atpt (&optional arg)
  "Greaterangle TILDED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'tilded ">" "<" arg arg)))

(defun ar-greaterangle-underscored-atpt (&optional arg)
  "Greaterangle UNDERSCORED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'underscored ">" "<" arg arg)))

(defun ar-greaterangle-whitespaced-atpt (&optional arg)
  "Greaterangle WHITESPACED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'whitespaced ">" "<" arg arg)))

(defun ar-curvedsinglequote-backslashed-atpt (&optional arg)
  "Curvedsinglequote BACKSLASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'backslashed "‘" "’" arg arg)))

(defun ar-curvedsinglequote-backticked-atpt (&optional arg)
  "Curvedsinglequote BACKTICKED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'backticked "‘" "’" arg arg)))

(defun ar-curvedsinglequote-coloned-atpt (&optional arg)
  "Curvedsinglequote COLONED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'coloned "‘" "’" arg arg)))

(defun ar-curvedsinglequote-crossed-atpt (&optional arg)
  "Curvedsinglequote CROSSED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'crossed "‘" "’" arg arg)))

(defun ar-curvedsinglequote-dollared-atpt (&optional arg)
  "Curvedsinglequote DOLLARED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'dollared "‘" "’" arg arg)))

(defun ar-curvedsinglequote-doublequoted-atpt (&optional arg)
  "Curvedsinglequote DOUBLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'doublequoted "‘" "’" arg arg)))

(defun ar-curvedsinglequote-equalized-atpt (&optional arg)
  "Curvedsinglequote EQUALIZED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'equalized "‘" "’" arg arg)))

(defun ar-curvedsinglequote-hashed-atpt (&optional arg)
  "Curvedsinglequote HASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'hashed "‘" "’" arg arg)))

(defun ar-curvedsinglequote-hyphened-atpt (&optional arg)
  "Curvedsinglequote HYPHENED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'hyphened "‘" "’" arg arg)))

(defun ar-curvedsinglequote-piped-atpt (&optional arg)
  "Curvedsinglequote PIPED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'piped "‘" "’" arg arg)))

(defun ar-curvedsinglequote-singlequoted-atpt (&optional arg)
  "Curvedsinglequote SINGLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'singlequoted "‘" "’" arg arg)))

(defun ar-curvedsinglequote-slashed-atpt (&optional arg)
  "Curvedsinglequote SLASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'slashed "‘" "’" arg arg)))

(defun ar-curvedsinglequote-stared-atpt (&optional arg)
  "Curvedsinglequote STARED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'stared "‘" "’" arg arg)))

(defun ar-curvedsinglequote-tilded-atpt (&optional arg)
  "Curvedsinglequote TILDED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'tilded "‘" "’" arg arg)))

(defun ar-curvedsinglequote-underscored-atpt (&optional arg)
  "Curvedsinglequote UNDERSCORED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'underscored "‘" "’" arg arg)))

(defun ar-curvedsinglequote-whitespaced-atpt (&optional arg)
  "Curvedsinglequote WHITESPACED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'whitespaced "‘" "’" arg arg)))

(defun ar-curveddoublequote-backslashed-atpt (&optional arg)
  "Curveddoublequote BACKSLASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'backslashed "“" "”" arg arg)))

(defun ar-curveddoublequote-backticked-atpt (&optional arg)
  "Curveddoublequote BACKTICKED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'backticked "“" "”" arg arg)))

(defun ar-curveddoublequote-coloned-atpt (&optional arg)
  "Curveddoublequote COLONED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'coloned "“" "”" arg arg)))

(defun ar-curveddoublequote-crossed-atpt (&optional arg)
  "Curveddoublequote CROSSED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'crossed "“" "”" arg arg)))

(defun ar-curveddoublequote-dollared-atpt (&optional arg)
  "Curveddoublequote DOLLARED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'dollared "“" "”" arg arg)))

(defun ar-curveddoublequote-doublequoted-atpt (&optional arg)
  "Curveddoublequote DOUBLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'doublequoted "“" "”" arg arg)))

(defun ar-curveddoublequote-equalized-atpt (&optional arg)
  "Curveddoublequote EQUALIZED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'equalized "“" "”" arg arg)))

(defun ar-curveddoublequote-hashed-atpt (&optional arg)
  "Curveddoublequote HASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'hashed "“" "”" arg arg)))

(defun ar-curveddoublequote-hyphened-atpt (&optional arg)
  "Curveddoublequote HYPHENED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'hyphened "“" "”" arg arg)))

(defun ar-curveddoublequote-piped-atpt (&optional arg)
  "Curveddoublequote PIPED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'piped "“" "”" arg arg)))

(defun ar-curveddoublequote-singlequoted-atpt (&optional arg)
  "Curveddoublequote SINGLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'singlequoted "“" "”" arg arg)))

(defun ar-curveddoublequote-slashed-atpt (&optional arg)
  "Curveddoublequote SLASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'slashed "“" "”" arg arg)))

(defun ar-curveddoublequote-stared-atpt (&optional arg)
  "Curveddoublequote STARED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'stared "“" "”" arg arg)))

(defun ar-curveddoublequote-tilded-atpt (&optional arg)
  "Curveddoublequote TILDED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'tilded "“" "”" arg arg)))

(defun ar-curveddoublequote-underscored-atpt (&optional arg)
  "Curveddoublequote UNDERSCORED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'underscored "“" "”" arg arg)))

(defun ar-curveddoublequote-whitespaced-atpt (&optional arg)
  "Curveddoublequote WHITESPACED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'whitespaced "“" "”" arg arg)))

(defun ar-parentize-backslashed-atpt (&optional arg)
  "Parentize BACKSLASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'backslashed "(" ")" arg arg)))

(defun ar-parentize-backticked-atpt (&optional arg)
  "Parentize BACKTICKED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'backticked "(" ")" arg arg)))

(defun ar-parentize-coloned-atpt (&optional arg)
  "Parentize COLONED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'coloned "(" ")" arg arg)))

(defun ar-parentize-crossed-atpt (&optional arg)
  "Parentize CROSSED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'crossed "(" ")" arg arg)))

(defun ar-parentize-dollared-atpt (&optional arg)
  "Parentize DOLLARED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'dollared "(" ")" arg arg)))

(defun ar-parentize-doublequoted-atpt (&optional arg)
  "Parentize DOUBLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'doublequoted "(" ")" arg arg)))

(defun ar-parentize-equalized-atpt (&optional arg)
  "Parentize EQUALIZED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'equalized "(" ")" arg arg)))

(defun ar-parentize-hashed-atpt (&optional arg)
  "Parentize HASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'hashed "(" ")" arg arg)))

(defun ar-parentize-hyphened-atpt (&optional arg)
  "Parentize HYPHENED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'hyphened "(" ")" arg arg)))

(defun ar-parentize-piped-atpt (&optional arg)
  "Parentize PIPED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'piped "(" ")" arg arg)))

(defun ar-parentize-singlequoted-atpt (&optional arg)
  "Parentize SINGLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'singlequoted "(" ")" arg arg)))

(defun ar-parentize-slashed-atpt (&optional arg)
  "Parentize SLASHED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'slashed "(" ")" arg arg)))

(defun ar-parentize-stared-atpt (&optional arg)
  "Parentize STARED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'stared "(" ")" arg arg)))

(defun ar-parentize-tilded-atpt (&optional arg)
  "Parentize TILDED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'tilded "(" ")" arg arg)))

(defun ar-parentize-underscored-atpt (&optional arg)
  "Parentize UNDERSCORED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'underscored "(" ")" arg arg)))

(defun ar-parentize-whitespaced-atpt (&optional arg)
  "Parentize WHITESPACED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'whitespaced "(" ")" arg arg)))

;; ar-thing-at-point-utils-pair-rest ar-paired-delimit-aktiv-raw ar-unpaired-delimited-passiv: end
;; ar-thing-at-point-utils-activ-passiv ar-paired-delimited-passiv-raw: start
(defun ar-brace-braced-atpt (&optional arg)
  "Brace BRACED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'braced "{" "}")))

(defun ar-brace-bracketed-atpt (&optional arg)
  "Brace BRACKETED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'bracketed "{" "}")))

(defun ar-brace-lesserangled-atpt (&optional arg)
  "Brace LESSERANGLED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'lesserangled "{" "}")))

(defun ar-brace-greaterangled-atpt (&optional arg)
  "Brace GREATERANGLED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'greaterangled "{" "}")))

(defun ar-brace-curvedsinglequoted-atpt (&optional arg)
  "Brace CURVEDSINGLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'curvedsinglequoted "{" "}")))

(defun ar-brace-curveddoublequoted-atpt (&optional arg)
  "Brace CURVEDDOUBLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'curveddoublequoted "{" "}")))

(defun ar-brace-parentized-atpt (&optional arg)
  "Brace PARENTIZED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'parentized "{" "}")))

(defun ar-bracket-braced-atpt (&optional arg)
  "Bracket BRACED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'braced "[" "]")))

(defun ar-bracket-bracketed-atpt (&optional arg)
  "Bracket BRACKETED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'bracketed "[" "]")))

(defun ar-bracket-lesserangled-atpt (&optional arg)
  "Bracket LESSERANGLED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'lesserangled "[" "]")))

(defun ar-bracket-greaterangled-atpt (&optional arg)
  "Bracket GREATERANGLED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'greaterangled "[" "]")))

(defun ar-bracket-curvedsinglequoted-atpt (&optional arg)
  "Bracket CURVEDSINGLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'curvedsinglequoted "[" "]")))

(defun ar-bracket-curveddoublequoted-atpt (&optional arg)
  "Bracket CURVEDDOUBLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'curveddoublequoted "[" "]")))

(defun ar-bracket-parentized-atpt (&optional arg)
  "Bracket PARENTIZED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'parentized "[" "]")))

(defun ar-lesserangle-braced-atpt (&optional arg)
  "Lesserangle BRACED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'braced "<" ">")))

(defun ar-lesserangle-bracketed-atpt (&optional arg)
  "Lesserangle BRACKETED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'bracketed "<" ">")))

(defun ar-lesserangle-lesserangled-atpt (&optional arg)
  "Lesserangle LESSERANGLED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'lesserangled "<" ">")))

(defun ar-lesserangle-greaterangled-atpt (&optional arg)
  "Lesserangle GREATERANGLED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'greaterangled "<" ">")))

(defun ar-lesserangle-curvedsinglequoted-atpt (&optional arg)
  "Lesserangle CURVEDSINGLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'curvedsinglequoted "<" ">")))

(defun ar-lesserangle-curveddoublequoted-atpt (&optional arg)
  "Lesserangle CURVEDDOUBLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'curveddoublequoted "<" ">")))

(defun ar-lesserangle-parentized-atpt (&optional arg)
  "Lesserangle PARENTIZED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'parentized "<" ">")))

(defun ar-greaterangle-braced-atpt (&optional arg)
  "Greaterangle BRACED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'braced ">" "<")))

(defun ar-greaterangle-bracketed-atpt (&optional arg)
  "Greaterangle BRACKETED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'bracketed ">" "<")))

(defun ar-greaterangle-lesserangled-atpt (&optional arg)
  "Greaterangle LESSERANGLED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'lesserangled ">" "<")))

(defun ar-greaterangle-greaterangled-atpt (&optional arg)
  "Greaterangle GREATERANGLED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'greaterangled ">" "<")))

(defun ar-greaterangle-curvedsinglequoted-atpt (&optional arg)
  "Greaterangle CURVEDSINGLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'curvedsinglequoted ">" "<")))

(defun ar-greaterangle-curveddoublequoted-atpt (&optional arg)
  "Greaterangle CURVEDDOUBLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'curveddoublequoted ">" "<")))

(defun ar-greaterangle-parentized-atpt (&optional arg)
  "Greaterangle PARENTIZED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'parentized ">" "<")))

(defun ar-curvedsinglequote-braced-atpt (&optional arg)
  "Curvedsinglequote BRACED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'braced "‘" "’")))

(defun ar-curvedsinglequote-bracketed-atpt (&optional arg)
  "Curvedsinglequote BRACKETED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'bracketed "‘" "’")))

(defun ar-curvedsinglequote-lesserangled-atpt (&optional arg)
  "Curvedsinglequote LESSERANGLED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'lesserangled "‘" "’")))

(defun ar-curvedsinglequote-greaterangled-atpt (&optional arg)
  "Curvedsinglequote GREATERANGLED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'greaterangled "‘" "’")))

(defun ar-curvedsinglequote-curvedsinglequoted-atpt (&optional arg)
  "Curvedsinglequote CURVEDSINGLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'curvedsinglequoted "‘" "’")))

(defun ar-curvedsinglequote-curveddoublequoted-atpt (&optional arg)
  "Curvedsinglequote CURVEDDOUBLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'curveddoublequoted "‘" "’")))

(defun ar-curvedsinglequote-parentized-atpt (&optional arg)
  "Curvedsinglequote PARENTIZED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'parentized "‘" "’")))

(defun ar-curveddoublequote-braced-atpt (&optional arg)
  "Curveddoublequote BRACED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'braced "“" "”")))

(defun ar-curveddoublequote-bracketed-atpt (&optional arg)
  "Curveddoublequote BRACKETED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'bracketed "“" "”")))

(defun ar-curveddoublequote-lesserangled-atpt (&optional arg)
  "Curveddoublequote LESSERANGLED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'lesserangled "“" "”")))

(defun ar-curveddoublequote-greaterangled-atpt (&optional arg)
  "Curveddoublequote GREATERANGLED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'greaterangled "“" "”")))

(defun ar-curveddoublequote-curvedsinglequoted-atpt (&optional arg)
  "Curveddoublequote CURVEDSINGLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'curvedsinglequoted "“" "”")))

(defun ar-curveddoublequote-curveddoublequoted-atpt (&optional arg)
  "Curveddoublequote CURVEDDOUBLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'curveddoublequoted "“" "”")))

(defun ar-curveddoublequote-parentized-atpt (&optional arg)
  "Curveddoublequote PARENTIZED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'parentized "“" "”")))

(defun ar-parentize-braced-atpt (&optional arg)
  "Parentize BRACED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'braced "(" ")")))

(defun ar-parentize-bracketed-atpt (&optional arg)
  "Parentize BRACKETED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'bracketed "(" ")")))

(defun ar-parentize-lesserangled-atpt (&optional arg)
  "Parentize LESSERANGLED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'lesserangled "(" ")")))

(defun ar-parentize-greaterangled-atpt (&optional arg)
  "Parentize GREATERANGLED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'greaterangled "(" ")")))

(defun ar-parentize-curvedsinglequoted-atpt (&optional arg)
  "Parentize CURVEDSINGLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'curvedsinglequoted "(" ")")))

(defun ar-parentize-curveddoublequoted-atpt (&optional arg)
  "Parentize CURVEDDOUBLEQUOTED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'curveddoublequoted "(" ")")))

(defun ar-parentize-parentized-atpt (&optional arg)
  "Parentize PARENTIZED at point ARG times."
  (interactive "*P")
  (dotimes (i (or arg 1))
    (ar-th-delimit--intern 'parentized "(" ")")))

;; ar-thing-at-point-utils-activ-passiv ar-paired-delimited-passiv-raw: end
;; ar-thing-at-point-utils-nodelim-core ar-atpt-classes: start

(defun ar-alnum-atpt (&optional arg) 
  "Returns alnum at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'alnum arg))

(defun ar-bounds-of-alnum-atpt (&optional arg)
  "Returns a list, borders of alnum if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'alnum arg))

(defun ar-alnum-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position ALNUM at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'alnum arg))

(defun ar-alnum-end-position-atpt (&optional arg)
  "Returns a number, end position of ALNUM at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'alnum arg))

(defun ar-alnum-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class ALNUM at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'alnum arg))

(defun ar-alnum-end-atpt (&optional arg)
  "Goto end of symbol or char-class ALNUM at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'alnum arg))

(defun ar-in-alnum-p-atpt (&optional arg)
  "Returns bounds of ALNUM at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'alnum arg))

(defun ar-length-of-alnum-atpt (&optional arg)
  "Returns beginning of symbol or char-class ALNUM at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'alnum arg))

(defun ar-copy-alnum-atpt (&optional arg)
  "Returns a copy of ALNUM at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'alnum arg))

(defun ar-delete-alnum-atpt (&optional arg)
  "Deletes alnum at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'alnum arg))

(defun ar-kill-alnum-atpt (&optional arg)
  "Deletes alnum at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'alnum arg))

(defun ar-forward-alnum-atpt (&optional arg)
  "Forward alnum at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'alnum arg))

(defun ar-backward-alnum-atpt (&optional arg)
  "Backward alnum at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'alnum arg))

(defun ar-triplequotedq-alnum-atpt (&optional arg)
  "Put triple doublequotes around alnum at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'alnum arg))

(defun ar-triplequotesq-alnum-atpt (&optional arg)
  "Put triple singlequotes around alnum at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'alnum arg))

(defun ar-triplebacktick-alnum-atpt (&optional arg)
  "Put triple backticks around alnum at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'alnum arg))

(defun ar-delete-alnum-in-region (beg end)
  "Deletes ALNUM at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'alnum beg end))

(defun ar-alpha-atpt (&optional arg) 
  "Returns alpha at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'alpha arg))

(defun ar-bounds-of-alpha-atpt (&optional arg)
  "Returns a list, borders of alpha if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'alpha arg))

(defun ar-alpha-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position ALPHA at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'alpha arg))

(defun ar-alpha-end-position-atpt (&optional arg)
  "Returns a number, end position of ALPHA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'alpha arg))

(defun ar-alpha-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class ALPHA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'alpha arg))

(defun ar-alpha-end-atpt (&optional arg)
  "Goto end of symbol or char-class ALPHA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'alpha arg))

(defun ar-in-alpha-p-atpt (&optional arg)
  "Returns bounds of ALPHA at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'alpha arg))

(defun ar-length-of-alpha-atpt (&optional arg)
  "Returns beginning of symbol or char-class ALPHA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'alpha arg))

(defun ar-copy-alpha-atpt (&optional arg)
  "Returns a copy of ALPHA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'alpha arg))

(defun ar-delete-alpha-atpt (&optional arg)
  "Deletes alpha at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'alpha arg))

(defun ar-kill-alpha-atpt (&optional arg)
  "Deletes alpha at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'alpha arg))

(defun ar-forward-alpha-atpt (&optional arg)
  "Forward alpha at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'alpha arg))

(defun ar-backward-alpha-atpt (&optional arg)
  "Backward alpha at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'alpha arg))

(defun ar-triplequotedq-alpha-atpt (&optional arg)
  "Put triple doublequotes around alpha at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'alpha arg))

(defun ar-triplequotesq-alpha-atpt (&optional arg)
  "Put triple singlequotes around alpha at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'alpha arg))

(defun ar-triplebacktick-alpha-atpt (&optional arg)
  "Put triple backticks around alpha at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'alpha arg))

(defun ar-delete-alpha-in-region (beg end)
  "Deletes ALPHA at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'alpha beg end))

(defun ar-ascii-atpt (&optional arg) 
  "Returns ascii at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'ascii arg))

(defun ar-bounds-of-ascii-atpt (&optional arg)
  "Returns a list, borders of ascii if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'ascii arg))

(defun ar-ascii-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position ASCII at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'ascii arg))

(defun ar-ascii-end-position-atpt (&optional arg)
  "Returns a number, end position of ASCII at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'ascii arg))

(defun ar-ascii-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class ASCII at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'ascii arg))

(defun ar-ascii-end-atpt (&optional arg)
  "Goto end of symbol or char-class ASCII at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'ascii arg))

(defun ar-in-ascii-p-atpt (&optional arg)
  "Returns bounds of ASCII at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'ascii arg))

(defun ar-length-of-ascii-atpt (&optional arg)
  "Returns beginning of symbol or char-class ASCII at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'ascii arg))

(defun ar-copy-ascii-atpt (&optional arg)
  "Returns a copy of ASCII at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'ascii arg))

(defun ar-delete-ascii-atpt (&optional arg)
  "Deletes ascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'ascii arg))

(defun ar-kill-ascii-atpt (&optional arg)
  "Deletes ascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'ascii arg))

(defun ar-forward-ascii-atpt (&optional arg)
  "Forward ascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'ascii arg))

(defun ar-backward-ascii-atpt (&optional arg)
  "Backward ascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'ascii arg))

(defun ar-triplequotedq-ascii-atpt (&optional arg)
  "Put triple doublequotes around ascii at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'ascii arg))

(defun ar-triplequotesq-ascii-atpt (&optional arg)
  "Put triple singlequotes around ascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'ascii arg))

(defun ar-triplebacktick-ascii-atpt (&optional arg)
  "Put triple backticks around ascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'ascii arg))

(defun ar-delete-ascii-in-region (beg end)
  "Deletes ASCII at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'ascii beg end))

(defun ar-blank-atpt (&optional arg) 
  "Returns blank at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'blank arg))

(defun ar-bounds-of-blank-atpt (&optional arg)
  "Returns a list, borders of blank if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'blank arg))

(defun ar-blank-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position BLANK at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'blank arg))

(defun ar-blank-end-position-atpt (&optional arg)
  "Returns a number, end position of BLANK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'blank arg))

(defun ar-blank-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class BLANK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'blank arg))

(defun ar-blank-end-atpt (&optional arg)
  "Goto end of symbol or char-class BLANK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'blank arg))

(defun ar-in-blank-p-atpt (&optional arg)
  "Returns bounds of BLANK at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'blank arg))

(defun ar-length-of-blank-atpt (&optional arg)
  "Returns beginning of symbol or char-class BLANK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'blank arg))

(defun ar-copy-blank-atpt (&optional arg)
  "Returns a copy of BLANK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'blank arg))

(defun ar-delete-blank-atpt (&optional arg)
  "Deletes blank at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'blank arg))

(defun ar-kill-blank-atpt (&optional arg)
  "Deletes blank at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'blank arg))

(defun ar-forward-blank-atpt (&optional arg)
  "Forward blank at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'blank arg))

(defun ar-backward-blank-atpt (&optional arg)
  "Backward blank at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'blank arg))

(defun ar-triplequotedq-blank-atpt (&optional arg)
  "Put triple doublequotes around blank at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'blank arg))

(defun ar-triplequotesq-blank-atpt (&optional arg)
  "Put triple singlequotes around blank at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'blank arg))

(defun ar-triplebacktick-blank-atpt (&optional arg)
  "Put triple backticks around blank at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'blank arg))

(defun ar-delete-blank-in-region (beg end)
  "Deletes BLANK at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'blank beg end))

(defun ar-cntrl-atpt (&optional arg) 
  "Returns cntrl at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'cntrl arg))

(defun ar-bounds-of-cntrl-atpt (&optional arg)
  "Returns a list, borders of cntrl if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'cntrl arg))

(defun ar-cntrl-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position CNTRL at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'cntrl arg))

(defun ar-cntrl-end-position-atpt (&optional arg)
  "Returns a number, end position of CNTRL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'cntrl arg))

(defun ar-cntrl-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class CNTRL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'cntrl arg))

(defun ar-cntrl-end-atpt (&optional arg)
  "Goto end of symbol or char-class CNTRL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'cntrl arg))

(defun ar-in-cntrl-p-atpt (&optional arg)
  "Returns bounds of CNTRL at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'cntrl arg))

(defun ar-length-of-cntrl-atpt (&optional arg)
  "Returns beginning of symbol or char-class CNTRL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'cntrl arg))

(defun ar-copy-cntrl-atpt (&optional arg)
  "Returns a copy of CNTRL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'cntrl arg))

(defun ar-delete-cntrl-atpt (&optional arg)
  "Deletes cntrl at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'cntrl arg))

(defun ar-kill-cntrl-atpt (&optional arg)
  "Deletes cntrl at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'cntrl arg))

(defun ar-forward-cntrl-atpt (&optional arg)
  "Forward cntrl at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'cntrl arg))

(defun ar-backward-cntrl-atpt (&optional arg)
  "Backward cntrl at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'cntrl arg))

(defun ar-triplequotedq-cntrl-atpt (&optional arg)
  "Put triple doublequotes around cntrl at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'cntrl arg))

(defun ar-triplequotesq-cntrl-atpt (&optional arg)
  "Put triple singlequotes around cntrl at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'cntrl arg))

(defun ar-triplebacktick-cntrl-atpt (&optional arg)
  "Put triple backticks around cntrl at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'cntrl arg))

(defun ar-delete-cntrl-in-region (beg end)
  "Deletes CNTRL at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'cntrl beg end))

(defun ar-digit-atpt (&optional arg) 
  "Returns digit at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'digit arg))

(defun ar-bounds-of-digit-atpt (&optional arg)
  "Returns a list, borders of digit if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'digit arg))

(defun ar-digit-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position DIGIT at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'digit arg))

(defun ar-digit-end-position-atpt (&optional arg)
  "Returns a number, end position of DIGIT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'digit arg))

(defun ar-digit-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class DIGIT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'digit arg))

(defun ar-digit-end-atpt (&optional arg)
  "Goto end of symbol or char-class DIGIT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'digit arg))

(defun ar-in-digit-p-atpt (&optional arg)
  "Returns bounds of DIGIT at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'digit arg))

(defun ar-length-of-digit-atpt (&optional arg)
  "Returns beginning of symbol or char-class DIGIT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'digit arg))

(defun ar-copy-digit-atpt (&optional arg)
  "Returns a copy of DIGIT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'digit arg))

(defun ar-delete-digit-atpt (&optional arg)
  "Deletes digit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'digit arg))

(defun ar-kill-digit-atpt (&optional arg)
  "Deletes digit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'digit arg))

(defun ar-forward-digit-atpt (&optional arg)
  "Forward digit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'digit arg))

(defun ar-backward-digit-atpt (&optional arg)
  "Backward digit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'digit arg))

(defun ar-triplequotedq-digit-atpt (&optional arg)
  "Put triple doublequotes around digit at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'digit arg))

(defun ar-triplequotesq-digit-atpt (&optional arg)
  "Put triple singlequotes around digit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'digit arg))

(defun ar-triplebacktick-digit-atpt (&optional arg)
  "Put triple backticks around digit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'digit arg))

(defun ar-delete-digit-in-region (beg end)
  "Deletes DIGIT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'digit beg end))

(defun ar-graph-atpt (&optional arg) 
  "Returns graph at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'graph arg))

(defun ar-bounds-of-graph-atpt (&optional arg)
  "Returns a list, borders of graph if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'graph arg))

(defun ar-graph-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position GRAPH at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'graph arg))

(defun ar-graph-end-position-atpt (&optional arg)
  "Returns a number, end position of GRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'graph arg))

(defun ar-graph-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class GRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'graph arg))

(defun ar-graph-end-atpt (&optional arg)
  "Goto end of symbol or char-class GRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'graph arg))

(defun ar-in-graph-p-atpt (&optional arg)
  "Returns bounds of GRAPH at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'graph arg))

(defun ar-length-of-graph-atpt (&optional arg)
  "Returns beginning of symbol or char-class GRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'graph arg))

(defun ar-copy-graph-atpt (&optional arg)
  "Returns a copy of GRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'graph arg))

(defun ar-delete-graph-atpt (&optional arg)
  "Deletes graph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'graph arg))

(defun ar-kill-graph-atpt (&optional arg)
  "Deletes graph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'graph arg))

(defun ar-forward-graph-atpt (&optional arg)
  "Forward graph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'graph arg))

(defun ar-backward-graph-atpt (&optional arg)
  "Backward graph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'graph arg))

(defun ar-triplequotedq-graph-atpt (&optional arg)
  "Put triple doublequotes around graph at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'graph arg))

(defun ar-triplequotesq-graph-atpt (&optional arg)
  "Put triple singlequotes around graph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'graph arg))

(defun ar-triplebacktick-graph-atpt (&optional arg)
  "Put triple backticks around graph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'graph arg))

(defun ar-delete-graph-in-region (beg end)
  "Deletes GRAPH at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'graph beg end))

(defun ar-lower-atpt (&optional arg) 
  "Returns lower at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'lower arg))

(defun ar-bounds-of-lower-atpt (&optional arg)
  "Returns a list, borders of lower if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'lower arg))

(defun ar-lower-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position LOWER at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'lower arg))

(defun ar-lower-end-position-atpt (&optional arg)
  "Returns a number, end position of LOWER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'lower arg))

(defun ar-lower-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class LOWER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'lower arg))

(defun ar-lower-end-atpt (&optional arg)
  "Goto end of symbol or char-class LOWER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'lower arg))

(defun ar-in-lower-p-atpt (&optional arg)
  "Returns bounds of LOWER at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'lower arg))

(defun ar-length-of-lower-atpt (&optional arg)
  "Returns beginning of symbol or char-class LOWER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'lower arg))

(defun ar-copy-lower-atpt (&optional arg)
  "Returns a copy of LOWER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'lower arg))

(defun ar-delete-lower-atpt (&optional arg)
  "Deletes lower at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'lower arg))

(defun ar-kill-lower-atpt (&optional arg)
  "Deletes lower at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'lower arg))

(defun ar-forward-lower-atpt (&optional arg)
  "Forward lower at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'lower arg))

(defun ar-backward-lower-atpt (&optional arg)
  "Backward lower at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'lower arg))

(defun ar-triplequotedq-lower-atpt (&optional arg)
  "Put triple doublequotes around lower at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'lower arg))

(defun ar-triplequotesq-lower-atpt (&optional arg)
  "Put triple singlequotes around lower at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'lower arg))

(defun ar-triplebacktick-lower-atpt (&optional arg)
  "Put triple backticks around lower at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'lower arg))

(defun ar-delete-lower-in-region (beg end)
  "Deletes LOWER at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'lower beg end))

(defun ar-nonascii-atpt (&optional arg) 
  "Returns nonascii at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'nonascii arg))

(defun ar-bounds-of-nonascii-atpt (&optional arg)
  "Returns a list, borders of nonascii if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'nonascii arg))

(defun ar-nonascii-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position NONASCII at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'nonascii arg))

(defun ar-nonascii-end-position-atpt (&optional arg)
  "Returns a number, end position of NONASCII at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'nonascii arg))

(defun ar-nonascii-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class NONASCII at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'nonascii arg))

(defun ar-nonascii-end-atpt (&optional arg)
  "Goto end of symbol or char-class NONASCII at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'nonascii arg))

(defun ar-in-nonascii-p-atpt (&optional arg)
  "Returns bounds of NONASCII at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'nonascii arg))

(defun ar-length-of-nonascii-atpt (&optional arg)
  "Returns beginning of symbol or char-class NONASCII at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'nonascii arg))

(defun ar-copy-nonascii-atpt (&optional arg)
  "Returns a copy of NONASCII at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'nonascii arg))

(defun ar-delete-nonascii-atpt (&optional arg)
  "Deletes nonascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'nonascii arg))

(defun ar-kill-nonascii-atpt (&optional arg)
  "Deletes nonascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'nonascii arg))

(defun ar-forward-nonascii-atpt (&optional arg)
  "Forward nonascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'nonascii arg))

(defun ar-backward-nonascii-atpt (&optional arg)
  "Backward nonascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'nonascii arg))

(defun ar-triplequotedq-nonascii-atpt (&optional arg)
  "Put triple doublequotes around nonascii at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'nonascii arg))

(defun ar-triplequotesq-nonascii-atpt (&optional arg)
  "Put triple singlequotes around nonascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'nonascii arg))

(defun ar-triplebacktick-nonascii-atpt (&optional arg)
  "Put triple backticks around nonascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'nonascii arg))

(defun ar-delete-nonascii-in-region (beg end)
  "Deletes NONASCII at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'nonascii beg end))

(defun ar-print-atpt (&optional arg) 
  "Returns print at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'print arg))

(defun ar-bounds-of-print-atpt (&optional arg)
  "Returns a list, borders of print if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'print arg))

(defun ar-print-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position PRINT at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'print arg))

(defun ar-print-end-position-atpt (&optional arg)
  "Returns a number, end position of PRINT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'print arg))

(defun ar-print-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class PRINT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'print arg))

(defun ar-print-end-atpt (&optional arg)
  "Goto end of symbol or char-class PRINT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'print arg))

(defun ar-in-print-p-atpt (&optional arg)
  "Returns bounds of PRINT at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'print arg))

(defun ar-length-of-print-atpt (&optional arg)
  "Returns beginning of symbol or char-class PRINT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'print arg))

(defun ar-copy-print-atpt (&optional arg)
  "Returns a copy of PRINT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'print arg))

(defun ar-delete-print-atpt (&optional arg)
  "Deletes print at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'print arg))

(defun ar-kill-print-atpt (&optional arg)
  "Deletes print at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'print arg))

(defun ar-forward-print-atpt (&optional arg)
  "Forward print at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'print arg))

(defun ar-backward-print-atpt (&optional arg)
  "Backward print at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'print arg))

(defun ar-triplequotedq-print-atpt (&optional arg)
  "Put triple doublequotes around print at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'print arg))

(defun ar-triplequotesq-print-atpt (&optional arg)
  "Put triple singlequotes around print at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'print arg))

(defun ar-triplebacktick-print-atpt (&optional arg)
  "Put triple backticks around print at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'print arg))

(defun ar-delete-print-in-region (beg end)
  "Deletes PRINT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'print beg end))

(defun ar-punct-atpt (&optional arg) 
  "Returns punct at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'punct arg))

(defun ar-bounds-of-punct-atpt (&optional arg)
  "Returns a list, borders of punct if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'punct arg))

(defun ar-punct-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position PUNCT at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'punct arg))

(defun ar-punct-end-position-atpt (&optional arg)
  "Returns a number, end position of PUNCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'punct arg))

(defun ar-punct-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class PUNCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'punct arg))

(defun ar-punct-end-atpt (&optional arg)
  "Goto end of symbol or char-class PUNCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'punct arg))

(defun ar-in-punct-p-atpt (&optional arg)
  "Returns bounds of PUNCT at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'punct arg))

(defun ar-length-of-punct-atpt (&optional arg)
  "Returns beginning of symbol or char-class PUNCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'punct arg))

(defun ar-copy-punct-atpt (&optional arg)
  "Returns a copy of PUNCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'punct arg))

(defun ar-delete-punct-atpt (&optional arg)
  "Deletes punct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'punct arg))

(defun ar-kill-punct-atpt (&optional arg)
  "Deletes punct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'punct arg))

(defun ar-forward-punct-atpt (&optional arg)
  "Forward punct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'punct arg))

(defun ar-backward-punct-atpt (&optional arg)
  "Backward punct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'punct arg))

(defun ar-triplequotedq-punct-atpt (&optional arg)
  "Put triple doublequotes around punct at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'punct arg))

(defun ar-triplequotesq-punct-atpt (&optional arg)
  "Put triple singlequotes around punct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'punct arg))

(defun ar-triplebacktick-punct-atpt (&optional arg)
  "Put triple backticks around punct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'punct arg))

(defun ar-delete-punct-in-region (beg end)
  "Deletes PUNCT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'punct beg end))

(defun ar-space-atpt (&optional arg) 
  "Returns space at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'space arg))

(defun ar-bounds-of-space-atpt (&optional arg)
  "Returns a list, borders of space if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'space arg))

(defun ar-space-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position SPACE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'space arg))

(defun ar-space-end-position-atpt (&optional arg)
  "Returns a number, end position of SPACE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'space arg))

(defun ar-space-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class SPACE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'space arg))

(defun ar-space-end-atpt (&optional arg)
  "Goto end of symbol or char-class SPACE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'space arg))

(defun ar-in-space-p-atpt (&optional arg)
  "Returns bounds of SPACE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'space arg))

(defun ar-length-of-space-atpt (&optional arg)
  "Returns beginning of symbol or char-class SPACE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'space arg))

(defun ar-copy-space-atpt (&optional arg)
  "Returns a copy of SPACE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'space arg))

(defun ar-delete-space-atpt (&optional arg)
  "Deletes space at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'space arg))

(defun ar-kill-space-atpt (&optional arg)
  "Deletes space at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'space arg))

(defun ar-forward-space-atpt (&optional arg)
  "Forward space at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'space arg))

(defun ar-backward-space-atpt (&optional arg)
  "Backward space at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'space arg))

(defun ar-triplequotedq-space-atpt (&optional arg)
  "Put triple doublequotes around space at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'space arg))

(defun ar-triplequotesq-space-atpt (&optional arg)
  "Put triple singlequotes around space at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'space arg))

(defun ar-triplebacktick-space-atpt (&optional arg)
  "Put triple backticks around space at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'space arg))

(defun ar-delete-space-in-region (beg end)
  "Deletes SPACE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'space beg end))

(defun ar-upper-atpt (&optional arg) 
  "Returns upper at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'upper arg))

(defun ar-bounds-of-upper-atpt (&optional arg)
  "Returns a list, borders of upper if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'upper arg))

(defun ar-upper-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position UPPER at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'upper arg))

(defun ar-upper-end-position-atpt (&optional arg)
  "Returns a number, end position of UPPER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'upper arg))

(defun ar-upper-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class UPPER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'upper arg))

(defun ar-upper-end-atpt (&optional arg)
  "Goto end of symbol or char-class UPPER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'upper arg))

(defun ar-in-upper-p-atpt (&optional arg)
  "Returns bounds of UPPER at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'upper arg))

(defun ar-length-of-upper-atpt (&optional arg)
  "Returns beginning of symbol or char-class UPPER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'upper arg))

(defun ar-copy-upper-atpt (&optional arg)
  "Returns a copy of UPPER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'upper arg))

(defun ar-delete-upper-atpt (&optional arg)
  "Deletes upper at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'upper arg))

(defun ar-kill-upper-atpt (&optional arg)
  "Deletes upper at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'upper arg))

(defun ar-forward-upper-atpt (&optional arg)
  "Forward upper at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'upper arg))

(defun ar-backward-upper-atpt (&optional arg)
  "Backward upper at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'upper arg))

(defun ar-triplequotedq-upper-atpt (&optional arg)
  "Put triple doublequotes around upper at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'upper arg))

(defun ar-triplequotesq-upper-atpt (&optional arg)
  "Put triple singlequotes around upper at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'upper arg))

(defun ar-triplebacktick-upper-atpt (&optional arg)
  "Put triple backticks around upper at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'upper arg))

(defun ar-delete-upper-in-region (beg end)
  "Deletes UPPER at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'upper beg end))

(defun ar-xdigit-atpt (&optional arg) 
  "Returns xdigit at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'xdigit arg))

(defun ar-bounds-of-xdigit-atpt (&optional arg)
  "Returns a list, borders of xdigit if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'xdigit arg))

(defun ar-xdigit-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position XDIGIT at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'xdigit arg))

(defun ar-xdigit-end-position-atpt (&optional arg)
  "Returns a number, end position of XDIGIT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'xdigit arg))

(defun ar-xdigit-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class XDIGIT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'xdigit arg))

(defun ar-xdigit-end-atpt (&optional arg)
  "Goto end of symbol or char-class XDIGIT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'xdigit arg))

(defun ar-in-xdigit-p-atpt (&optional arg)
  "Returns bounds of XDIGIT at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'xdigit arg))

(defun ar-length-of-xdigit-atpt (&optional arg)
  "Returns beginning of symbol or char-class XDIGIT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'xdigit arg))

(defun ar-copy-xdigit-atpt (&optional arg)
  "Returns a copy of XDIGIT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'xdigit arg))

(defun ar-delete-xdigit-atpt (&optional arg)
  "Deletes xdigit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'xdigit arg))

(defun ar-kill-xdigit-atpt (&optional arg)
  "Deletes xdigit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'xdigit arg))

(defun ar-forward-xdigit-atpt (&optional arg)
  "Forward xdigit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'xdigit arg))

(defun ar-backward-xdigit-atpt (&optional arg)
  "Backward xdigit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'xdigit arg))

(defun ar-triplequotedq-xdigit-atpt (&optional arg)
  "Put triple doublequotes around xdigit at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'xdigit arg))

(defun ar-triplequotesq-xdigit-atpt (&optional arg)
  "Put triple singlequotes around xdigit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'xdigit arg))

(defun ar-triplebacktick-xdigit-atpt (&optional arg)
  "Put triple backticks around xdigit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'xdigit arg))

(defun ar-delete-xdigit-in-region (beg end)
  "Deletes XDIGIT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'xdigit beg end))
;; ar-thing-at-point-utils-nodelim-core ar-atpt-classes: end
;; ar-thing-at-point-utils-nodelim-core ar-atpt-rest-list: start

(defun ar-greateranglednested-atpt (&optional arg) 
  "Returns greateranglednested at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'greateranglednested arg))

(defun ar-bounds-of-greateranglednested-atpt (&optional arg)
  "Returns a list, borders of greateranglednested if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'greateranglednested arg))

(defun ar-greateranglednested-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position GREATERANGLEDNESTED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'greateranglednested arg))

(defun ar-greateranglednested-end-position-atpt (&optional arg)
  "Returns a number, end position of GREATERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'greateranglednested arg))

(defun ar-greateranglednested-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class GREATERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'greateranglednested arg))

(defun ar-greateranglednested-end-atpt (&optional arg)
  "Goto end of symbol or char-class GREATERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'greateranglednested arg))

(defun ar-in-greateranglednested-p-atpt (&optional arg)
  "Returns bounds of GREATERANGLEDNESTED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'greateranglednested arg))

(defun ar-length-of-greateranglednested-atpt (&optional arg)
  "Returns beginning of symbol or char-class GREATERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'greateranglednested arg))

(defun ar-copy-greateranglednested-atpt (&optional arg)
  "Returns a copy of GREATERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'greateranglednested arg))

(defun ar-delete-greateranglednested-atpt (&optional arg)
  "Deletes greateranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'greateranglednested arg))

(defun ar-kill-greateranglednested-atpt (&optional arg)
  "Deletes greateranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'greateranglednested arg))

(defun ar-forward-greateranglednested-atpt (&optional arg)
  "Forward greateranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'greateranglednested arg))

(defun ar-backward-greateranglednested-atpt (&optional arg)
  "Backward greateranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'greateranglednested arg))

(defun ar-triplequotedq-greateranglednested-atpt (&optional arg)
  "Put triple doublequotes around greateranglednested at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'greateranglednested arg))

(defun ar-triplequotesq-greateranglednested-atpt (&optional arg)
  "Put triple singlequotes around greateranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'greateranglednested arg))

(defun ar-triplebacktick-greateranglednested-atpt (&optional arg)
  "Put triple backticks around greateranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'greateranglednested arg))

(defun ar-delete-greateranglednested-in-region (beg end)
  "Deletes GREATERANGLEDNESTED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'greateranglednested beg end))

(defun ar-lesseranglednested-atpt (&optional arg) 
  "Returns lesseranglednested at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'lesseranglednested arg))

(defun ar-bounds-of-lesseranglednested-atpt (&optional arg)
  "Returns a list, borders of lesseranglednested if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'lesseranglednested arg))

(defun ar-lesseranglednested-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position LESSERANGLEDNESTED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'lesseranglednested arg))

(defun ar-lesseranglednested-end-position-atpt (&optional arg)
  "Returns a number, end position of LESSERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'lesseranglednested arg))

(defun ar-lesseranglednested-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class LESSERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'lesseranglednested arg))

(defun ar-lesseranglednested-end-atpt (&optional arg)
  "Goto end of symbol or char-class LESSERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'lesseranglednested arg))

(defun ar-in-lesseranglednested-p-atpt (&optional arg)
  "Returns bounds of LESSERANGLEDNESTED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'lesseranglednested arg))

(defun ar-length-of-lesseranglednested-atpt (&optional arg)
  "Returns beginning of symbol or char-class LESSERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'lesseranglednested arg))

(defun ar-copy-lesseranglednested-atpt (&optional arg)
  "Returns a copy of LESSERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'lesseranglednested arg))

(defun ar-delete-lesseranglednested-atpt (&optional arg)
  "Deletes lesseranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'lesseranglednested arg))

(defun ar-kill-lesseranglednested-atpt (&optional arg)
  "Deletes lesseranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'lesseranglednested arg))

(defun ar-forward-lesseranglednested-atpt (&optional arg)
  "Forward lesseranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'lesseranglednested arg))

(defun ar-backward-lesseranglednested-atpt (&optional arg)
  "Backward lesseranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'lesseranglednested arg))

(defun ar-triplequotedq-lesseranglednested-atpt (&optional arg)
  "Put triple doublequotes around lesseranglednested at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'lesseranglednested arg))

(defun ar-triplequotesq-lesseranglednested-atpt (&optional arg)
  "Put triple singlequotes around lesseranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'lesseranglednested arg))

(defun ar-triplebacktick-lesseranglednested-atpt (&optional arg)
  "Put triple backticks around lesseranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'lesseranglednested arg))

(defun ar-delete-lesseranglednested-in-region (beg end)
  "Deletes LESSERANGLEDNESTED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'lesseranglednested beg end))

(defun ar-buffer-atpt (&optional arg) 
  "Returns buffer at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'buffer arg))

(defun ar-bounds-of-buffer-atpt (&optional arg)
  "Returns a list, borders of buffer if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'buffer arg))

(defun ar-buffer-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position BUFFER at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'buffer arg))

(defun ar-buffer-end-position-atpt (&optional arg)
  "Returns a number, end position of BUFFER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'buffer arg))

(defun ar-buffer-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class BUFFER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'buffer arg))

(defun ar-buffer-end-atpt (&optional arg)
  "Goto end of symbol or char-class BUFFER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'buffer arg))

(defun ar-in-buffer-p-atpt (&optional arg)
  "Returns bounds of BUFFER at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'buffer arg))

(defun ar-length-of-buffer-atpt (&optional arg)
  "Returns beginning of symbol or char-class BUFFER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'buffer arg))

(defun ar-copy-buffer-atpt (&optional arg)
  "Returns a copy of BUFFER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'buffer arg))

(defun ar-delete-buffer-atpt (&optional arg)
  "Deletes buffer at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'buffer arg))

(defun ar-kill-buffer-atpt (&optional arg)
  "Deletes buffer at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'buffer arg))

(defun ar-forward-buffer-atpt (&optional arg)
  "Forward buffer at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'buffer arg))

(defun ar-backward-buffer-atpt (&optional arg)
  "Backward buffer at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'buffer arg))

(defun ar-triplequotedq-buffer-atpt (&optional arg)
  "Put triple doublequotes around buffer at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'buffer arg))

(defun ar-triplequotesq-buffer-atpt (&optional arg)
  "Put triple singlequotes around buffer at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'buffer arg))

(defun ar-triplebacktick-buffer-atpt (&optional arg)
  "Put triple backticks around buffer at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'buffer arg))

(defun ar-delete-buffer-in-region (beg end)
  "Deletes BUFFER at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'buffer beg end))

(defun ar-char-atpt (&optional arg) 
  "Returns char at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'char arg))

(defun ar-bounds-of-char-atpt (&optional arg)
  "Returns a list, borders of char if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'char arg))

(defun ar-char-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position CHAR at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'char arg))

(defun ar-char-end-position-atpt (&optional arg)
  "Returns a number, end position of CHAR at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'char arg))

(defun ar-char-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class CHAR at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'char arg))

(defun ar-char-end-atpt (&optional arg)
  "Goto end of symbol or char-class CHAR at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'char arg))

(defun ar-in-char-p-atpt (&optional arg)
  "Returns bounds of CHAR at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'char arg))

(defun ar-length-of-char-atpt (&optional arg)
  "Returns beginning of symbol or char-class CHAR at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'char arg))

(defun ar-copy-char-atpt (&optional arg)
  "Returns a copy of CHAR at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'char arg))

(defun ar-delete-char-atpt (&optional arg)
  "Deletes char at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'char arg))

(defun ar-kill-char-atpt (&optional arg)
  "Deletes char at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'char arg))

(defun ar-forward-char-atpt (&optional arg)
  "Forward char at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'char arg))

(defun ar-backward-char-atpt (&optional arg)
  "Backward char at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'char arg))

(defun ar-triplequotedq-char-atpt (&optional arg)
  "Put triple doublequotes around char at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'char arg))

(defun ar-triplequotesq-char-atpt (&optional arg)
  "Put triple singlequotes around char at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'char arg))

(defun ar-triplebacktick-char-atpt (&optional arg)
  "Put triple backticks around char at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'char arg))

(defun ar-delete-char-in-region (beg end)
  "Deletes CHAR at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'char beg end))

(defun ar-comment-atpt (&optional arg) 
  "Returns comment at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'comment arg))

(defun ar-bounds-of-comment-atpt (&optional arg)
  "Returns a list, borders of comment if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'comment arg))

(defun ar-comment-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position COMMENT at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'comment arg))

(defun ar-comment-end-position-atpt (&optional arg)
  "Returns a number, end position of COMMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'comment arg))

(defun ar-comment-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class COMMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'comment arg))

(defun ar-comment-end-atpt (&optional arg)
  "Goto end of symbol or char-class COMMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'comment arg))

(defun ar-in-comment-p-atpt (&optional arg)
  "Returns bounds of COMMENT at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'comment arg))

(defun ar-length-of-comment-atpt (&optional arg)
  "Returns beginning of symbol or char-class COMMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'comment arg))

(defun ar-copy-comment-atpt (&optional arg)
  "Returns a copy of COMMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'comment arg))

(defun ar-delete-comment-atpt (&optional arg)
  "Deletes comment at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'comment arg))

(defun ar-kill-comment-atpt (&optional arg)
  "Deletes comment at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'comment arg))

(defun ar-forward-comment-atpt (&optional arg)
  "Forward comment at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'comment arg))

(defun ar-backward-comment-atpt (&optional arg)
  "Backward comment at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'comment arg))

(defun ar-triplequotedq-comment-atpt (&optional arg)
  "Put triple doublequotes around comment at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'comment arg))

(defun ar-triplequotesq-comment-atpt (&optional arg)
  "Put triple singlequotes around comment at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'comment arg))

(defun ar-triplebacktick-comment-atpt (&optional arg)
  "Put triple backticks around comment at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'comment arg))

(defun ar-delete-comment-in-region (beg end)
  "Deletes COMMENT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'comment beg end))

(defun ar-csv-atpt (&optional arg) 
  "Returns csv at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'csv arg))

(defun ar-bounds-of-csv-atpt (&optional arg)
  "Returns a list, borders of csv if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'csv arg))

(defun ar-csv-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position CSV at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'csv arg))

(defun ar-csv-end-position-atpt (&optional arg)
  "Returns a number, end position of CSV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'csv arg))

(defun ar-csv-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class CSV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'csv arg))

(defun ar-csv-end-atpt (&optional arg)
  "Goto end of symbol or char-class CSV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'csv arg))

(defun ar-in-csv-p-atpt (&optional arg)
  "Returns bounds of CSV at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'csv arg))

(defun ar-length-of-csv-atpt (&optional arg)
  "Returns beginning of symbol or char-class CSV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'csv arg))

(defun ar-copy-csv-atpt (&optional arg)
  "Returns a copy of CSV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'csv arg))

(defun ar-delete-csv-atpt (&optional arg)
  "Deletes csv at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'csv arg))

(defun ar-kill-csv-atpt (&optional arg)
  "Deletes csv at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'csv arg))

(defun ar-forward-csv-atpt (&optional arg)
  "Forward csv at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'csv arg))

(defun ar-backward-csv-atpt (&optional arg)
  "Backward csv at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'csv arg))

(defun ar-triplequotedq-csv-atpt (&optional arg)
  "Put triple doublequotes around csv at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'csv arg))

(defun ar-triplequotesq-csv-atpt (&optional arg)
  "Put triple singlequotes around csv at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'csv arg))

(defun ar-triplebacktick-csv-atpt (&optional arg)
  "Put triple backticks around csv at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'csv arg))

(defun ar-delete-csv-in-region (beg end)
  "Deletes CSV at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'csv beg end))

(defun ar-date-atpt (&optional arg) 
  "Returns date at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'date arg))

(defun ar-bounds-of-date-atpt (&optional arg)
  "Returns a list, borders of date if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'date arg))

(defun ar-date-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position DATE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'date arg))

(defun ar-date-end-position-atpt (&optional arg)
  "Returns a number, end position of DATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'date arg))

(defun ar-date-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class DATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'date arg))

(defun ar-date-end-atpt (&optional arg)
  "Goto end of symbol or char-class DATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'date arg))

(defun ar-in-date-p-atpt (&optional arg)
  "Returns bounds of DATE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'date arg))

(defun ar-length-of-date-atpt (&optional arg)
  "Returns beginning of symbol or char-class DATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'date arg))

(defun ar-copy-date-atpt (&optional arg)
  "Returns a copy of DATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'date arg))

(defun ar-delete-date-atpt (&optional arg)
  "Deletes date at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'date arg))

(defun ar-kill-date-atpt (&optional arg)
  "Deletes date at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'date arg))

(defun ar-forward-date-atpt (&optional arg)
  "Forward date at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'date arg))

(defun ar-backward-date-atpt (&optional arg)
  "Backward date at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'date arg))

(defun ar-triplequotedq-date-atpt (&optional arg)
  "Put triple doublequotes around date at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'date arg))

(defun ar-triplequotesq-date-atpt (&optional arg)
  "Put triple singlequotes around date at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'date arg))

(defun ar-triplebacktick-date-atpt (&optional arg)
  "Put triple backticks around date at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'date arg))

(defun ar-delete-date-in-region (beg end)
  "Deletes DATE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'date beg end))

(defun ar-delimited-atpt (&optional arg) 
  "Returns delimited at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'delimited arg))

(defun ar-bounds-of-delimited-atpt (&optional arg)
  "Returns a list, borders of delimited if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'delimited arg))

(defun ar-delimited-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position DELIMITED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'delimited arg))

(defun ar-delimited-end-position-atpt (&optional arg)
  "Returns a number, end position of DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'delimited arg))

(defun ar-delimited-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'delimited arg))

(defun ar-delimited-end-atpt (&optional arg)
  "Goto end of symbol or char-class DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'delimited arg))

(defun ar-in-delimited-p-atpt (&optional arg)
  "Returns bounds of DELIMITED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'delimited arg))

(defun ar-length-of-delimited-atpt (&optional arg)
  "Returns beginning of symbol or char-class DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'delimited arg))

(defun ar-copy-delimited-atpt (&optional arg)
  "Returns a copy of DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'delimited arg))

(defun ar-delete-delimited-atpt (&optional arg)
  "Deletes delimited at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'delimited arg))

(defun ar-kill-delimited-atpt (&optional arg)
  "Deletes delimited at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'delimited arg))

(defun ar-forward-delimited-atpt (&optional arg)
  "Forward delimited at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'delimited arg))

(defun ar-backward-delimited-atpt (&optional arg)
  "Backward delimited at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'delimited arg))

(defun ar-triplequotedq-delimited-atpt (&optional arg)
  "Put triple doublequotes around delimited at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'delimited arg))

(defun ar-triplequotesq-delimited-atpt (&optional arg)
  "Put triple singlequotes around delimited at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'delimited arg))

(defun ar-triplebacktick-delimited-atpt (&optional arg)
  "Put triple backticks around delimited at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'delimited arg))

(defun ar-delete-delimited-in-region (beg end)
  "Deletes DELIMITED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'delimited beg end))

(defun ar-email-atpt (&optional arg) 
  "Returns email at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'email arg))

(defun ar-bounds-of-email-atpt (&optional arg)
  "Returns a list, borders of email if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'email arg))

(defun ar-email-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position EMAIL at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'email arg))

(defun ar-email-end-position-atpt (&optional arg)
  "Returns a number, end position of EMAIL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'email arg))

(defun ar-email-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class EMAIL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'email arg))

(defun ar-email-end-atpt (&optional arg)
  "Goto end of symbol or char-class EMAIL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'email arg))

(defun ar-in-email-p-atpt (&optional arg)
  "Returns bounds of EMAIL at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'email arg))

(defun ar-length-of-email-atpt (&optional arg)
  "Returns beginning of symbol or char-class EMAIL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'email arg))

(defun ar-copy-email-atpt (&optional arg)
  "Returns a copy of EMAIL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'email arg))

(defun ar-delete-email-atpt (&optional arg)
  "Deletes email at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'email arg))

(defun ar-kill-email-atpt (&optional arg)
  "Deletes email at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'email arg))

(defun ar-forward-email-atpt (&optional arg)
  "Forward email at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'email arg))

(defun ar-backward-email-atpt (&optional arg)
  "Backward email at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'email arg))

(defun ar-triplequotedq-email-atpt (&optional arg)
  "Put triple doublequotes around email at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'email arg))

(defun ar-triplequotesq-email-atpt (&optional arg)
  "Put triple singlequotes around email at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'email arg))

(defun ar-triplebacktick-email-atpt (&optional arg)
  "Put triple backticks around email at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'email arg))

(defun ar-delete-email-in-region (beg end)
  "Deletes EMAIL at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'email beg end))

(defun ar-filename-atpt (&optional arg) 
  "Returns filename at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'filename arg))

(defun ar-bounds-of-filename-atpt (&optional arg)
  "Returns a list, borders of filename if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'filename arg))

(defun ar-filename-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position FILENAME at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'filename arg))

(defun ar-filename-end-position-atpt (&optional arg)
  "Returns a number, end position of FILENAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'filename arg))

(defun ar-filename-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class FILENAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'filename arg))

(defun ar-filename-end-atpt (&optional arg)
  "Goto end of symbol or char-class FILENAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'filename arg))

(defun ar-in-filename-p-atpt (&optional arg)
  "Returns bounds of FILENAME at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'filename arg))

(defun ar-length-of-filename-atpt (&optional arg)
  "Returns beginning of symbol or char-class FILENAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'filename arg))

(defun ar-copy-filename-atpt (&optional arg)
  "Returns a copy of FILENAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'filename arg))

(defun ar-delete-filename-atpt (&optional arg)
  "Deletes filename at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'filename arg))

(defun ar-kill-filename-atpt (&optional arg)
  "Deletes filename at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'filename arg))

(defun ar-forward-filename-atpt (&optional arg)
  "Forward filename at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'filename arg))

(defun ar-backward-filename-atpt (&optional arg)
  "Backward filename at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'filename arg))

(defun ar-triplequotedq-filename-atpt (&optional arg)
  "Put triple doublequotes around filename at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'filename arg))

(defun ar-triplequotesq-filename-atpt (&optional arg)
  "Put triple singlequotes around filename at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'filename arg))

(defun ar-triplebacktick-filename-atpt (&optional arg)
  "Put triple backticks around filename at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'filename arg))

(defun ar-delete-filename-in-region (beg end)
  "Deletes FILENAME at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'filename beg end))

(defun ar-filenamenondirectory-atpt (&optional arg) 
  "Returns filenamenondirectory at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'filenamenondirectory arg))

(defun ar-bounds-of-filenamenondirectory-atpt (&optional arg)
  "Returns a list, borders of filenamenondirectory if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'filenamenondirectory arg))

(defun ar-filenamenondirectory-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position FILENAMENONDIRECTORY at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'filenamenondirectory arg))

(defun ar-filenamenondirectory-end-position-atpt (&optional arg)
  "Returns a number, end position of FILENAMENONDIRECTORY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'filenamenondirectory arg))

(defun ar-filenamenondirectory-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class FILENAMENONDIRECTORY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'filenamenondirectory arg))

(defun ar-filenamenondirectory-end-atpt (&optional arg)
  "Goto end of symbol or char-class FILENAMENONDIRECTORY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'filenamenondirectory arg))

(defun ar-in-filenamenondirectory-p-atpt (&optional arg)
  "Returns bounds of FILENAMENONDIRECTORY at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'filenamenondirectory arg))

(defun ar-length-of-filenamenondirectory-atpt (&optional arg)
  "Returns beginning of symbol or char-class FILENAMENONDIRECTORY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'filenamenondirectory arg))

(defun ar-copy-filenamenondirectory-atpt (&optional arg)
  "Returns a copy of FILENAMENONDIRECTORY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'filenamenondirectory arg))

(defun ar-delete-filenamenondirectory-atpt (&optional arg)
  "Deletes filenamenondirectory at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'filenamenondirectory arg))

(defun ar-kill-filenamenondirectory-atpt (&optional arg)
  "Deletes filenamenondirectory at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'filenamenondirectory arg))

(defun ar-forward-filenamenondirectory-atpt (&optional arg)
  "Forward filenamenondirectory at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'filenamenondirectory arg))

(defun ar-backward-filenamenondirectory-atpt (&optional arg)
  "Backward filenamenondirectory at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'filenamenondirectory arg))

(defun ar-triplequotedq-filenamenondirectory-atpt (&optional arg)
  "Put triple doublequotes around filenamenondirectory at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'filenamenondirectory arg))

(defun ar-triplequotesq-filenamenondirectory-atpt (&optional arg)
  "Put triple singlequotes around filenamenondirectory at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'filenamenondirectory arg))

(defun ar-triplebacktick-filenamenondirectory-atpt (&optional arg)
  "Put triple backticks around filenamenondirectory at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'filenamenondirectory arg))

(defun ar-delete-filenamenondirectory-in-region (beg end)
  "Deletes FILENAMENONDIRECTORY at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'filenamenondirectory beg end))

(defun ar-float-atpt (&optional arg) 
  "Returns float at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'float arg))

(defun ar-bounds-of-float-atpt (&optional arg)
  "Returns a list, borders of float if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'float arg))

(defun ar-float-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position FLOAT at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'float arg))

(defun ar-float-end-position-atpt (&optional arg)
  "Returns a number, end position of FLOAT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'float arg))

(defun ar-float-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class FLOAT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'float arg))

(defun ar-float-end-atpt (&optional arg)
  "Goto end of symbol or char-class FLOAT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'float arg))

(defun ar-in-float-p-atpt (&optional arg)
  "Returns bounds of FLOAT at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'float arg))

(defun ar-length-of-float-atpt (&optional arg)
  "Returns beginning of symbol or char-class FLOAT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'float arg))

(defun ar-copy-float-atpt (&optional arg)
  "Returns a copy of FLOAT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'float arg))

(defun ar-delete-float-atpt (&optional arg)
  "Deletes float at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'float arg))

(defun ar-kill-float-atpt (&optional arg)
  "Deletes float at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'float arg))

(defun ar-forward-float-atpt (&optional arg)
  "Forward float at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'float arg))

(defun ar-backward-float-atpt (&optional arg)
  "Backward float at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'float arg))

(defun ar-triplequotedq-float-atpt (&optional arg)
  "Put triple doublequotes around float at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'float arg))

(defun ar-triplequotesq-float-atpt (&optional arg)
  "Put triple singlequotes around float at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'float arg))

(defun ar-triplebacktick-float-atpt (&optional arg)
  "Put triple backticks around float at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'float arg))

(defun ar-delete-float-in-region (beg end)
  "Deletes FLOAT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'float beg end))

(defun ar-function-atpt (&optional arg) 
  "Returns function at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'function arg))

(defun ar-bounds-of-function-atpt (&optional arg)
  "Returns a list, borders of function if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'function arg))

(defun ar-function-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position FUNCTION at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'function arg))

(defun ar-function-end-position-atpt (&optional arg)
  "Returns a number, end position of FUNCTION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'function arg))

(defun ar-function-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class FUNCTION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'function arg))

(defun ar-function-end-atpt (&optional arg)
  "Goto end of symbol or char-class FUNCTION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'function arg))

(defun ar-in-function-p-atpt (&optional arg)
  "Returns bounds of FUNCTION at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'function arg))

(defun ar-length-of-function-atpt (&optional arg)
  "Returns beginning of symbol or char-class FUNCTION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'function arg))

(defun ar-copy-function-atpt (&optional arg)
  "Returns a copy of FUNCTION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'function arg))

(defun ar-delete-function-atpt (&optional arg)
  "Deletes function at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'function arg))

(defun ar-kill-function-atpt (&optional arg)
  "Deletes function at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'function arg))

(defun ar-forward-function-atpt (&optional arg)
  "Forward function at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'function arg))

(defun ar-backward-function-atpt (&optional arg)
  "Backward function at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'function arg))

(defun ar-triplequotedq-function-atpt (&optional arg)
  "Put triple doublequotes around function at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'function arg))

(defun ar-triplequotesq-function-atpt (&optional arg)
  "Put triple singlequotes around function at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'function arg))

(defun ar-triplebacktick-function-atpt (&optional arg)
  "Put triple backticks around function at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'function arg))

(defun ar-delete-function-in-region (beg end)
  "Deletes FUNCTION at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'function beg end))

(defun ar-ip-atpt (&optional arg) 
  "Returns ip at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'ip arg))

(defun ar-bounds-of-ip-atpt (&optional arg)
  "Returns a list, borders of ip if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'ip arg))

(defun ar-ip-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position IP at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'ip arg))

(defun ar-ip-end-position-atpt (&optional arg)
  "Returns a number, end position of IP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'ip arg))

(defun ar-ip-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class IP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'ip arg))

(defun ar-ip-end-atpt (&optional arg)
  "Goto end of symbol or char-class IP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'ip arg))

(defun ar-in-ip-p-atpt (&optional arg)
  "Returns bounds of IP at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'ip arg))

(defun ar-length-of-ip-atpt (&optional arg)
  "Returns beginning of symbol or char-class IP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'ip arg))

(defun ar-copy-ip-atpt (&optional arg)
  "Returns a copy of IP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'ip arg))

(defun ar-delete-ip-atpt (&optional arg)
  "Deletes ip at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'ip arg))

(defun ar-kill-ip-atpt (&optional arg)
  "Deletes ip at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'ip arg))

(defun ar-forward-ip-atpt (&optional arg)
  "Forward ip at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'ip arg))

(defun ar-backward-ip-atpt (&optional arg)
  "Backward ip at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'ip arg))

(defun ar-triplequotedq-ip-atpt (&optional arg)
  "Put triple doublequotes around ip at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'ip arg))

(defun ar-triplequotesq-ip-atpt (&optional arg)
  "Put triple singlequotes around ip at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'ip arg))

(defun ar-triplebacktick-ip-atpt (&optional arg)
  "Put triple backticks around ip at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'ip arg))

(defun ar-delete-ip-in-region (beg end)
  "Deletes IP at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'ip beg end))

(defun ar-isbn-atpt (&optional arg) 
  "Returns isbn at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'isbn arg))

(defun ar-bounds-of-isbn-atpt (&optional arg)
  "Returns a list, borders of isbn if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'isbn arg))

(defun ar-isbn-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position ISBN at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'isbn arg))

(defun ar-isbn-end-position-atpt (&optional arg)
  "Returns a number, end position of ISBN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'isbn arg))

(defun ar-isbn-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class ISBN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'isbn arg))

(defun ar-isbn-end-atpt (&optional arg)
  "Goto end of symbol or char-class ISBN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'isbn arg))

(defun ar-in-isbn-p-atpt (&optional arg)
  "Returns bounds of ISBN at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'isbn arg))

(defun ar-length-of-isbn-atpt (&optional arg)
  "Returns beginning of symbol or char-class ISBN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'isbn arg))

(defun ar-copy-isbn-atpt (&optional arg)
  "Returns a copy of ISBN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'isbn arg))

(defun ar-delete-isbn-atpt (&optional arg)
  "Deletes isbn at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'isbn arg))

(defun ar-kill-isbn-atpt (&optional arg)
  "Deletes isbn at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'isbn arg))

(defun ar-forward-isbn-atpt (&optional arg)
  "Forward isbn at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'isbn arg))

(defun ar-backward-isbn-atpt (&optional arg)
  "Backward isbn at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'isbn arg))

(defun ar-triplequotedq-isbn-atpt (&optional arg)
  "Put triple doublequotes around isbn at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'isbn arg))

(defun ar-triplequotesq-isbn-atpt (&optional arg)
  "Put triple singlequotes around isbn at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'isbn arg))

(defun ar-triplebacktick-isbn-atpt (&optional arg)
  "Put triple backticks around isbn at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'isbn arg))

(defun ar-delete-isbn-in-region (beg end)
  "Deletes ISBN at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'isbn beg end))

(defun ar-line-atpt (&optional arg) 
  "Returns line at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'line arg))

(defun ar-bounds-of-line-atpt (&optional arg)
  "Returns a list, borders of line if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'line arg))

(defun ar-line-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position LINE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'line arg))

(defun ar-line-end-position-atpt (&optional arg)
  "Returns a number, end position of LINE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'line arg))

(defun ar-line-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class LINE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'line arg))

(defun ar-line-end-atpt (&optional arg)
  "Goto end of symbol or char-class LINE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'line arg))

(defun ar-in-line-p-atpt (&optional arg)
  "Returns bounds of LINE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'line arg))

(defun ar-length-of-line-atpt (&optional arg)
  "Returns beginning of symbol or char-class LINE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'line arg))

(defun ar-copy-line-atpt (&optional arg)
  "Returns a copy of LINE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'line arg))

(defun ar-delete-line-atpt (&optional arg)
  "Deletes line at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'line arg))

(defun ar-kill-line-atpt (&optional arg)
  "Deletes line at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'line arg))

(defun ar-forward-line-atpt (&optional arg)
  "Forward line at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'line arg))

(defun ar-backward-line-atpt (&optional arg)
  "Backward line at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'line arg))

(defun ar-triplequotedq-line-atpt (&optional arg)
  "Put triple doublequotes around line at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'line arg))

(defun ar-triplequotesq-line-atpt (&optional arg)
  "Put triple singlequotes around line at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'line arg))

(defun ar-triplebacktick-line-atpt (&optional arg)
  "Put triple backticks around line at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'line arg))

(defun ar-delete-line-in-region (beg end)
  "Deletes LINE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'line beg end))

(defun ar-list-atpt (&optional arg) 
  "Returns list at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'list arg))

(defun ar-bounds-of-list-atpt (&optional arg)
  "Returns a list, borders of list if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'list arg))

(defun ar-list-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position LIST at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'list arg))

(defun ar-list-end-position-atpt (&optional arg)
  "Returns a number, end position of LIST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'list arg))

(defun ar-list-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class LIST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'list arg))

(defun ar-list-end-atpt (&optional arg)
  "Goto end of symbol or char-class LIST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'list arg))

(defun ar-in-list-p-atpt (&optional arg)
  "Returns bounds of LIST at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'list arg))

(defun ar-length-of-list-atpt (&optional arg)
  "Returns beginning of symbol or char-class LIST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'list arg))

(defun ar-copy-list-atpt (&optional arg)
  "Returns a copy of LIST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'list arg))

(defun ar-delete-list-atpt (&optional arg)
  "Deletes list at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'list arg))

(defun ar-kill-list-atpt (&optional arg)
  "Deletes list at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'list arg))

(defun ar-forward-list-atpt (&optional arg)
  "Forward list at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'list arg))

(defun ar-backward-list-atpt (&optional arg)
  "Backward list at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'list arg))

(defun ar-triplequotedq-list-atpt (&optional arg)
  "Put triple doublequotes around list at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'list arg))

(defun ar-triplequotesq-list-atpt (&optional arg)
  "Put triple singlequotes around list at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'list arg))

(defun ar-triplebacktick-list-atpt (&optional arg)
  "Put triple backticks around list at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'list arg))

(defun ar-delete-list-in-region (beg end)
  "Deletes LIST at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'list beg end))

(defun ar-name-atpt (&optional arg) 
  "Returns name at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'name arg))

(defun ar-bounds-of-name-atpt (&optional arg)
  "Returns a list, borders of name if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'name arg))

(defun ar-name-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position NAME at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'name arg))

(defun ar-name-end-position-atpt (&optional arg)
  "Returns a number, end position of NAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'name arg))

(defun ar-name-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class NAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'name arg))

(defun ar-name-end-atpt (&optional arg)
  "Goto end of symbol or char-class NAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'name arg))

(defun ar-in-name-p-atpt (&optional arg)
  "Returns bounds of NAME at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'name arg))

(defun ar-length-of-name-atpt (&optional arg)
  "Returns beginning of symbol or char-class NAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'name arg))

(defun ar-copy-name-atpt (&optional arg)
  "Returns a copy of NAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'name arg))

(defun ar-delete-name-atpt (&optional arg)
  "Deletes name at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'name arg))

(defun ar-kill-name-atpt (&optional arg)
  "Deletes name at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'name arg))

(defun ar-forward-name-atpt (&optional arg)
  "Forward name at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'name arg))

(defun ar-backward-name-atpt (&optional arg)
  "Backward name at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'name arg))

(defun ar-triplequotedq-name-atpt (&optional arg)
  "Put triple doublequotes around name at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'name arg))

(defun ar-triplequotesq-name-atpt (&optional arg)
  "Put triple singlequotes around name at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'name arg))

(defun ar-triplebacktick-name-atpt (&optional arg)
  "Put triple backticks around name at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'name arg))

(defun ar-delete-name-in-region (beg end)
  "Deletes NAME at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'name beg end))

(defun ar-number-atpt (&optional arg) 
  "Returns number at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'number arg))

(defun ar-bounds-of-number-atpt (&optional arg)
  "Returns a list, borders of number if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'number arg))

(defun ar-number-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position NUMBER at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'number arg))

(defun ar-number-end-position-atpt (&optional arg)
  "Returns a number, end position of NUMBER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'number arg))

(defun ar-number-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class NUMBER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'number arg))

(defun ar-number-end-atpt (&optional arg)
  "Goto end of symbol or char-class NUMBER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'number arg))

(defun ar-in-number-p-atpt (&optional arg)
  "Returns bounds of NUMBER at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'number arg))

(defun ar-length-of-number-atpt (&optional arg)
  "Returns beginning of symbol or char-class NUMBER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'number arg))

(defun ar-copy-number-atpt (&optional arg)
  "Returns a copy of NUMBER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'number arg))

(defun ar-delete-number-atpt (&optional arg)
  "Deletes number at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'number arg))

(defun ar-kill-number-atpt (&optional arg)
  "Deletes number at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'number arg))

(defun ar-forward-number-atpt (&optional arg)
  "Forward number at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'number arg))

(defun ar-backward-number-atpt (&optional arg)
  "Backward number at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'number arg))

(defun ar-triplequotedq-number-atpt (&optional arg)
  "Put triple doublequotes around number at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'number arg))

(defun ar-triplequotesq-number-atpt (&optional arg)
  "Put triple singlequotes around number at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'number arg))

(defun ar-triplebacktick-number-atpt (&optional arg)
  "Put triple backticks around number at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'number arg))

(defun ar-delete-number-in-region (beg end)
  "Deletes NUMBER at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'number beg end))

(defun ar-page-atpt (&optional arg) 
  "Returns page at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'page arg))

(defun ar-bounds-of-page-atpt (&optional arg)
  "Returns a list, borders of page if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'page arg))

(defun ar-page-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position PAGE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'page arg))

(defun ar-page-end-position-atpt (&optional arg)
  "Returns a number, end position of PAGE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'page arg))

(defun ar-page-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class PAGE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'page arg))

(defun ar-page-end-atpt (&optional arg)
  "Goto end of symbol or char-class PAGE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'page arg))

(defun ar-in-page-p-atpt (&optional arg)
  "Returns bounds of PAGE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'page arg))

(defun ar-length-of-page-atpt (&optional arg)
  "Returns beginning of symbol or char-class PAGE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'page arg))

(defun ar-copy-page-atpt (&optional arg)
  "Returns a copy of PAGE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'page arg))

(defun ar-delete-page-atpt (&optional arg)
  "Deletes page at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'page arg))

(defun ar-kill-page-atpt (&optional arg)
  "Deletes page at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'page arg))

(defun ar-forward-page-atpt (&optional arg)
  "Forward page at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'page arg))

(defun ar-backward-page-atpt (&optional arg)
  "Backward page at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'page arg))

(defun ar-triplequotedq-page-atpt (&optional arg)
  "Put triple doublequotes around page at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'page arg))

(defun ar-triplequotesq-page-atpt (&optional arg)
  "Put triple singlequotes around page at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'page arg))

(defun ar-triplebacktick-page-atpt (&optional arg)
  "Put triple backticks around page at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'page arg))

(defun ar-delete-page-in-region (beg end)
  "Deletes PAGE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'page beg end))

(defun ar-paragraph-atpt (&optional arg) 
  "Returns paragraph at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'paragraph arg))

(defun ar-bounds-of-paragraph-atpt (&optional arg)
  "Returns a list, borders of paragraph if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'paragraph arg))

(defun ar-paragraph-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position PARAGRAPH at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'paragraph arg))

(defun ar-paragraph-end-position-atpt (&optional arg)
  "Returns a number, end position of PARAGRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'paragraph arg))

(defun ar-paragraph-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class PARAGRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'paragraph arg))

(defun ar-paragraph-end-atpt (&optional arg)
  "Goto end of symbol or char-class PARAGRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'paragraph arg))

(defun ar-in-paragraph-p-atpt (&optional arg)
  "Returns bounds of PARAGRAPH at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'paragraph arg))

(defun ar-length-of-paragraph-atpt (&optional arg)
  "Returns beginning of symbol or char-class PARAGRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'paragraph arg))

(defun ar-copy-paragraph-atpt (&optional arg)
  "Returns a copy of PARAGRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'paragraph arg))

(defun ar-delete-paragraph-atpt (&optional arg)
  "Deletes paragraph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'paragraph arg))

(defun ar-kill-paragraph-atpt (&optional arg)
  "Deletes paragraph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'paragraph arg))

(defun ar-forward-paragraph-atpt (&optional arg)
  "Forward paragraph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'paragraph arg))

(defun ar-backward-paragraph-atpt (&optional arg)
  "Backward paragraph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'paragraph arg))

(defun ar-triplequotedq-paragraph-atpt (&optional arg)
  "Put triple doublequotes around paragraph at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'paragraph arg))

(defun ar-triplequotesq-paragraph-atpt (&optional arg)
  "Put triple singlequotes around paragraph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'paragraph arg))

(defun ar-triplebacktick-paragraph-atpt (&optional arg)
  "Put triple backticks around paragraph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'paragraph arg))

(defun ar-delete-paragraph-in-region (beg end)
  "Deletes PARAGRAPH at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'paragraph beg end))

(defun ar-phone-atpt (&optional arg) 
  "Returns phone at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'phone arg))

(defun ar-bounds-of-phone-atpt (&optional arg)
  "Returns a list, borders of phone if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'phone arg))

(defun ar-phone-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position PHONE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'phone arg))

(defun ar-phone-end-position-atpt (&optional arg)
  "Returns a number, end position of PHONE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'phone arg))

(defun ar-phone-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class PHONE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'phone arg))

(defun ar-phone-end-atpt (&optional arg)
  "Goto end of symbol or char-class PHONE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'phone arg))

(defun ar-in-phone-p-atpt (&optional arg)
  "Returns bounds of PHONE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'phone arg))

(defun ar-length-of-phone-atpt (&optional arg)
  "Returns beginning of symbol or char-class PHONE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'phone arg))

(defun ar-copy-phone-atpt (&optional arg)
  "Returns a copy of PHONE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'phone arg))

(defun ar-delete-phone-atpt (&optional arg)
  "Deletes phone at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'phone arg))

(defun ar-kill-phone-atpt (&optional arg)
  "Deletes phone at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'phone arg))

(defun ar-forward-phone-atpt (&optional arg)
  "Forward phone at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'phone arg))

(defun ar-backward-phone-atpt (&optional arg)
  "Backward phone at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'phone arg))

(defun ar-triplequotedq-phone-atpt (&optional arg)
  "Put triple doublequotes around phone at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'phone arg))

(defun ar-triplequotesq-phone-atpt (&optional arg)
  "Put triple singlequotes around phone at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'phone arg))

(defun ar-triplebacktick-phone-atpt (&optional arg)
  "Put triple backticks around phone at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'phone arg))

(defun ar-delete-phone-in-region (beg end)
  "Deletes PHONE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'phone beg end))

(defun ar-region-atpt (&optional arg) 
  "Returns region at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'region arg))

(defun ar-bounds-of-region-atpt (&optional arg)
  "Returns a list, borders of region if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'region arg))

(defun ar-region-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position REGION at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'region arg))

(defun ar-region-end-position-atpt (&optional arg)
  "Returns a number, end position of REGION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'region arg))

(defun ar-region-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class REGION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'region arg))

(defun ar-region-end-atpt (&optional arg)
  "Goto end of symbol or char-class REGION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'region arg))

(defun ar-in-region-p-atpt (&optional arg)
  "Returns bounds of REGION at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'region arg))

(defun ar-length-of-region-atpt (&optional arg)
  "Returns beginning of symbol or char-class REGION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'region arg))

(defun ar-copy-region-atpt (&optional arg)
  "Returns a copy of REGION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'region arg))

(defun ar-delete-region-atpt (&optional arg)
  "Deletes region at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'region arg))

(defun ar-kill-region-atpt (&optional arg)
  "Deletes region at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'region arg))

(defun ar-forward-region-atpt (&optional arg)
  "Forward region at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'region arg))

(defun ar-backward-region-atpt (&optional arg)
  "Backward region at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'region arg))

(defun ar-triplequotedq-region-atpt (&optional arg)
  "Put triple doublequotes around region at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'region arg))

(defun ar-triplequotesq-region-atpt (&optional arg)
  "Put triple singlequotes around region at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'region arg))

(defun ar-triplebacktick-region-atpt (&optional arg)
  "Put triple backticks around region at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'region arg))

(defun ar-delete-region-in-region (beg end)
  "Deletes REGION at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'region beg end))

(defun ar-sentence-atpt (&optional arg) 
  "Returns sentence at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'sentence arg))

(defun ar-bounds-of-sentence-atpt (&optional arg)
  "Returns a list, borders of sentence if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'sentence arg))

(defun ar-sentence-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position SENTENCE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'sentence arg))

(defun ar-sentence-end-position-atpt (&optional arg)
  "Returns a number, end position of SENTENCE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'sentence arg))

(defun ar-sentence-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class SENTENCE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'sentence arg))

(defun ar-sentence-end-atpt (&optional arg)
  "Goto end of symbol or char-class SENTENCE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'sentence arg))

(defun ar-in-sentence-p-atpt (&optional arg)
  "Returns bounds of SENTENCE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'sentence arg))

(defun ar-length-of-sentence-atpt (&optional arg)
  "Returns beginning of symbol or char-class SENTENCE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'sentence arg))

(defun ar-copy-sentence-atpt (&optional arg)
  "Returns a copy of SENTENCE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'sentence arg))

(defun ar-delete-sentence-atpt (&optional arg)
  "Deletes sentence at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'sentence arg))

(defun ar-kill-sentence-atpt (&optional arg)
  "Deletes sentence at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'sentence arg))

(defun ar-forward-sentence-atpt (&optional arg)
  "Forward sentence at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'sentence arg))

(defun ar-backward-sentence-atpt (&optional arg)
  "Backward sentence at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'sentence arg))

(defun ar-triplequotedq-sentence-atpt (&optional arg)
  "Put triple doublequotes around sentence at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'sentence arg))

(defun ar-triplequotesq-sentence-atpt (&optional arg)
  "Put triple singlequotes around sentence at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'sentence arg))

(defun ar-triplebacktick-sentence-atpt (&optional arg)
  "Put triple backticks around sentence at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'sentence arg))

(defun ar-delete-sentence-in-region (beg end)
  "Deletes SENTENCE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'sentence beg end))

(defun ar-sexp-atpt (&optional arg) 
  "Returns sexp at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'sexp arg))

(defun ar-bounds-of-sexp-atpt (&optional arg)
  "Returns a list, borders of sexp if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'sexp arg))

(defun ar-sexp-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position SEXP at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'sexp arg))

(defun ar-sexp-end-position-atpt (&optional arg)
  "Returns a number, end position of SEXP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'sexp arg))

(defun ar-sexp-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class SEXP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'sexp arg))

(defun ar-sexp-end-atpt (&optional arg)
  "Goto end of symbol or char-class SEXP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'sexp arg))

(defun ar-in-sexp-p-atpt (&optional arg)
  "Returns bounds of SEXP at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'sexp arg))

(defun ar-length-of-sexp-atpt (&optional arg)
  "Returns beginning of symbol or char-class SEXP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'sexp arg))

(defun ar-copy-sexp-atpt (&optional arg)
  "Returns a copy of SEXP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'sexp arg))

(defun ar-delete-sexp-atpt (&optional arg)
  "Deletes sexp at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'sexp arg))

(defun ar-kill-sexp-atpt (&optional arg)
  "Deletes sexp at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'sexp arg))

(defun ar-forward-sexp-atpt (&optional arg)
  "Forward sexp at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'sexp arg))

(defun ar-backward-sexp-atpt (&optional arg)
  "Backward sexp at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'sexp arg))

(defun ar-triplequotedq-sexp-atpt (&optional arg)
  "Put triple doublequotes around sexp at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'sexp arg))

(defun ar-triplequotesq-sexp-atpt (&optional arg)
  "Put triple singlequotes around sexp at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'sexp arg))

(defun ar-triplebacktick-sexp-atpt (&optional arg)
  "Put triple backticks around sexp at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'sexp arg))

(defun ar-delete-sexp-in-region (beg end)
  "Deletes SEXP at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'sexp beg end))

(defun ar-shstruct-atpt (&optional arg) 
  "Returns shstruct at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'shstruct arg))

(defun ar-bounds-of-shstruct-atpt (&optional arg)
  "Returns a list, borders of shstruct if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'shstruct arg))

(defun ar-shstruct-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position SHSTRUCT at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'shstruct arg))

(defun ar-shstruct-end-position-atpt (&optional arg)
  "Returns a number, end position of SHSTRUCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'shstruct arg))

(defun ar-shstruct-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class SHSTRUCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'shstruct arg))

(defun ar-shstruct-end-atpt (&optional arg)
  "Goto end of symbol or char-class SHSTRUCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'shstruct arg))

(defun ar-in-shstruct-p-atpt (&optional arg)
  "Returns bounds of SHSTRUCT at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'shstruct arg))

(defun ar-length-of-shstruct-atpt (&optional arg)
  "Returns beginning of symbol or char-class SHSTRUCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'shstruct arg))

(defun ar-copy-shstruct-atpt (&optional arg)
  "Returns a copy of SHSTRUCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'shstruct arg))

(defun ar-delete-shstruct-atpt (&optional arg)
  "Deletes shstruct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'shstruct arg))

(defun ar-kill-shstruct-atpt (&optional arg)
  "Deletes shstruct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'shstruct arg))

(defun ar-forward-shstruct-atpt (&optional arg)
  "Forward shstruct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'shstruct arg))

(defun ar-backward-shstruct-atpt (&optional arg)
  "Backward shstruct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'shstruct arg))

(defun ar-triplequotedq-shstruct-atpt (&optional arg)
  "Put triple doublequotes around shstruct at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'shstruct arg))

(defun ar-triplequotesq-shstruct-atpt (&optional arg)
  "Put triple singlequotes around shstruct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'shstruct arg))

(defun ar-triplebacktick-shstruct-atpt (&optional arg)
  "Put triple backticks around shstruct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'shstruct arg))

(defun ar-delete-shstruct-in-region (beg end)
  "Deletes SHSTRUCT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'shstruct beg end))

(defun ar-symbol-atpt (&optional arg) 
  "Returns symbol at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'symbol arg))

(defun ar-bounds-of-symbol-atpt (&optional arg)
  "Returns a list, borders of symbol if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'symbol arg))

(defun ar-symbol-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position SYMBOL at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'symbol arg))

(defun ar-symbol-end-position-atpt (&optional arg)
  "Returns a number, end position of SYMBOL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'symbol arg))

(defun ar-symbol-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class SYMBOL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'symbol arg))

(defun ar-symbol-end-atpt (&optional arg)
  "Goto end of symbol or char-class SYMBOL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'symbol arg))

(defun ar-in-symbol-p-atpt (&optional arg)
  "Returns bounds of SYMBOL at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'symbol arg))

(defun ar-length-of-symbol-atpt (&optional arg)
  "Returns beginning of symbol or char-class SYMBOL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'symbol arg))

(defun ar-copy-symbol-atpt (&optional arg)
  "Returns a copy of SYMBOL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'symbol arg))

(defun ar-delete-symbol-atpt (&optional arg)
  "Deletes symbol at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'symbol arg))

(defun ar-kill-symbol-atpt (&optional arg)
  "Deletes symbol at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'symbol arg))

(defun ar-forward-symbol-atpt (&optional arg)
  "Forward symbol at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'symbol arg))

(defun ar-backward-symbol-atpt (&optional arg)
  "Backward symbol at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'symbol arg))

(defun ar-triplequotedq-symbol-atpt (&optional arg)
  "Put triple doublequotes around symbol at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'symbol arg))

(defun ar-triplequotesq-symbol-atpt (&optional arg)
  "Put triple singlequotes around symbol at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'symbol arg))

(defun ar-triplebacktick-symbol-atpt (&optional arg)
  "Put triple backticks around symbol at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'symbol arg))

(defun ar-delete-symbol-in-region (beg end)
  "Deletes SYMBOL at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'symbol beg end))

(defun ar-url-atpt (&optional arg) 
  "Returns url at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'url arg))

(defun ar-bounds-of-url-atpt (&optional arg)
  "Returns a list, borders of url if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'url arg))

(defun ar-url-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position URL at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'url arg))

(defun ar-url-end-position-atpt (&optional arg)
  "Returns a number, end position of URL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'url arg))

(defun ar-url-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class URL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'url arg))

(defun ar-url-end-atpt (&optional arg)
  "Goto end of symbol or char-class URL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'url arg))

(defun ar-in-url-p-atpt (&optional arg)
  "Returns bounds of URL at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'url arg))

(defun ar-length-of-url-atpt (&optional arg)
  "Returns beginning of symbol or char-class URL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'url arg))

(defun ar-copy-url-atpt (&optional arg)
  "Returns a copy of URL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'url arg))

(defun ar-delete-url-atpt (&optional arg)
  "Deletes url at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'url arg))

(defun ar-kill-url-atpt (&optional arg)
  "Deletes url at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'url arg))

(defun ar-forward-url-atpt (&optional arg)
  "Forward url at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'url arg))

(defun ar-backward-url-atpt (&optional arg)
  "Backward url at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'url arg))

(defun ar-triplequotedq-url-atpt (&optional arg)
  "Put triple doublequotes around url at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'url arg))

(defun ar-triplequotesq-url-atpt (&optional arg)
  "Put triple singlequotes around url at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'url arg))

(defun ar-triplebacktick-url-atpt (&optional arg)
  "Put triple backticks around url at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'url arg))

(defun ar-delete-url-in-region (beg end)
  "Deletes URL at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'url beg end))

(defun ar-word-atpt (&optional arg) 
  "Returns word at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'word arg))

(defun ar-bounds-of-word-atpt (&optional arg)
  "Returns a list, borders of word if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'word arg))

(defun ar-word-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position WORD at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'word arg))

(defun ar-word-end-position-atpt (&optional arg)
  "Returns a number, end position of WORD at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'word arg))

(defun ar-word-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class WORD at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'word arg))

(defun ar-word-end-atpt (&optional arg)
  "Goto end of symbol or char-class WORD at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'word arg))

(defun ar-in-word-p-atpt (&optional arg)
  "Returns bounds of WORD at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'word arg))

(defun ar-length-of-word-atpt (&optional arg)
  "Returns beginning of symbol or char-class WORD at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'word arg))

(defun ar-copy-word-atpt (&optional arg)
  "Returns a copy of WORD at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'word arg))

(defun ar-delete-word-atpt (&optional arg)
  "Deletes word at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'word arg))

(defun ar-kill-word-atpt (&optional arg)
  "Deletes word at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'word arg))

(defun ar-forward-word-atpt (&optional arg)
  "Forward word at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'word arg))

(defun ar-backward-word-atpt (&optional arg)
  "Backward word at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'word arg))

(defun ar-triplequotedq-word-atpt (&optional arg)
  "Put triple doublequotes around word at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'word arg))

(defun ar-triplequotesq-word-atpt (&optional arg)
  "Put triple singlequotes around word at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'word arg))

(defun ar-triplebacktick-word-atpt (&optional arg)
  "Put triple backticks around word at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'word arg))

(defun ar-delete-word-in-region (beg end)
  "Deletes WORD at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'word beg end))

(defun ar-wordalphaonly-atpt (&optional arg) 
  "Returns wordalphaonly at point as string if any, nil otherwise. 

With \\[universal-argument] strip delimiters if suitable"
  (interactive "P")
  (ar-th 'wordalphaonly arg))

(defun ar-bounds-of-wordalphaonly-atpt (&optional arg)
  "Returns a list, borders of wordalphaonly if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'wordalphaonly arg))

(defun ar-wordalphaonly-beginning-position-atpt (&optional arg)
  "Returns a number, beginning position WORDALPHAONLY at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'wordalphaonly arg))

(defun ar-wordalphaonly-end-position-atpt (&optional arg)
  "Returns a number, end position of WORDALPHAONLY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'wordalphaonly arg))

(defun ar-wordalphaonly-beginning-atpt (&optional arg)
  "Goto beginning of symbol or char-class WORDALPHAONLY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'wordalphaonly arg))

(defun ar-wordalphaonly-end-atpt (&optional arg)
  "Goto end of symbol or char-class WORDALPHAONLY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'wordalphaonly arg))

(defun ar-in-wordalphaonly-p-atpt (&optional arg)
  "Returns bounds of WORDALPHAONLY at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'wordalphaonly arg))

(defun ar-length-of-wordalphaonly-atpt (&optional arg)
  "Returns beginning of symbol or char-class WORDALPHAONLY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'wordalphaonly arg))

(defun ar-copy-wordalphaonly-atpt (&optional arg)
  "Returns a copy of WORDALPHAONLY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'wordalphaonly arg))

(defun ar-delete-wordalphaonly-atpt (&optional arg)
  "Deletes wordalphaonly at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'wordalphaonly arg))

(defun ar-kill-wordalphaonly-atpt (&optional arg)
  "Deletes wordalphaonly at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'wordalphaonly arg))

(defun ar-forward-wordalphaonly-atpt (&optional arg)
  "Forward wordalphaonly at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-forward 'wordalphaonly arg))

(defun ar-backward-wordalphaonly-atpt (&optional arg)
  "Backward wordalphaonly at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backward 'wordalphaonly arg))

(defun ar-triplequotedq-wordalphaonly-atpt (&optional arg)
  "Put triple doublequotes around wordalphaonly at point as string if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotedq 'wordalphaonly arg))

(defun ar-triplequotesq-wordalphaonly-atpt (&optional arg)
  "Put triple singlequotes around wordalphaonly at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplequotesq 'wordalphaonly arg))

(defun ar-triplebacktick-wordalphaonly-atpt (&optional arg)
  "Put triple backticks around wordalphaonly at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'wordalphaonly arg))

(defun ar-delete-wordalphaonly-in-region (beg end)
  "Deletes WORDALPHAONLY at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'wordalphaonly beg end))
;; ar-thing-at-point-utils-nodelim-core ar-atpt-rest-list: end

;; ar-thing-at-point-utils-unpaired-delim-aktiv ar-unpaired-delimit-aktiv ar-atpt-rest-list: start


(defalias 'ar-colon-alnum-atpt 'ar-alnum-colon-atpt)
(defun ar-alnum-colon-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'alnum arg))

(defalias 'ar-cross-alnum-atpt 'ar-alnum-cross-atpt)
(defun ar-alnum-cross-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'alnum arg))

(defalias 'ar-doubleslash-alnum-atpt 'ar-alnum-doubleslash-atpt)
(defun ar-alnum-doubleslash-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'alnum arg))

(defalias 'ar-backslash-alnum-atpt 'ar-alnum-backslash-atpt)
(defun ar-alnum-backslash-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'alnum arg))

(defalias 'ar-backtick-alnum-atpt 'ar-alnum-backtick-atpt)
(defun ar-alnum-backtick-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'alnum arg))

(defalias 'ar-dollar-alnum-atpt 'ar-alnum-dollar-atpt)
(defun ar-alnum-dollar-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'alnum arg))

(defalias 'ar-doublebacktick-alnum-atpt 'ar-alnum-doublebacktick-atpt)
(defun ar-alnum-doublebacktick-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'alnum arg))

(defalias 'ar-doublequote-alnum-atpt 'ar-alnum-doublequote-atpt)
(defun ar-alnum-doublequote-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'alnum arg))

(defalias 'ar-equalize-alnum-atpt 'ar-alnum-equalize-atpt)
(defun ar-alnum-equalize-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'alnum arg))

(defalias 'ar-escape-alnum-atpt 'ar-alnum-escape-atpt)
(defun ar-alnum-escape-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'alnum arg))

(defalias 'ar-hash-alnum-atpt 'ar-alnum-hash-atpt)
(defun ar-alnum-hash-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'alnum arg))

(defalias 'ar-hyphen-alnum-atpt 'ar-alnum-hyphen-atpt)
(defun ar-alnum-hyphen-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'alnum arg))

(defalias 'ar-pipe-alnum-atpt 'ar-alnum-pipe-atpt)
(defun ar-alnum-pipe-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'alnum arg))

(defalias 'ar-singlequote-alnum-atpt 'ar-alnum-singlequote-atpt)
(defun ar-alnum-singlequote-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'alnum arg))

(defalias 'ar-slash-alnum-atpt 'ar-alnum-slash-atpt)
(defun ar-alnum-slash-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'alnum arg))

(defalias 'ar-star-alnum-atpt 'ar-alnum-star-atpt)
(defun ar-alnum-star-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'alnum arg))

(defalias 'ar-tild-alnum-atpt 'ar-alnum-tild-atpt)
(defun ar-alnum-tild-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'alnum arg))

(defalias 'ar-triplebacktick-alnum-atpt 'ar-alnum-triplebacktick-atpt)
(defun ar-alnum-triplebacktick-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'alnum arg))

(defalias 'ar-underscore-alnum-atpt 'ar-alnum-underscore-atpt)
(defun ar-alnum-underscore-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'alnum arg))

(defalias 'ar-whitespace-alnum-atpt 'ar-alnum-whitespace-atpt)
(defun ar-alnum-whitespace-atpt (&optional arg)
  "Returns ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'alnum arg))

(defalias 'ar-colon-alpha-atpt 'ar-alpha-colon-atpt)
(defun ar-alpha-colon-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'alpha arg))

(defalias 'ar-cross-alpha-atpt 'ar-alpha-cross-atpt)
(defun ar-alpha-cross-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'alpha arg))

(defalias 'ar-doubleslash-alpha-atpt 'ar-alpha-doubleslash-atpt)
(defun ar-alpha-doubleslash-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'alpha arg))

(defalias 'ar-backslash-alpha-atpt 'ar-alpha-backslash-atpt)
(defun ar-alpha-backslash-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'alpha arg))

(defalias 'ar-backtick-alpha-atpt 'ar-alpha-backtick-atpt)
(defun ar-alpha-backtick-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'alpha arg))

(defalias 'ar-dollar-alpha-atpt 'ar-alpha-dollar-atpt)
(defun ar-alpha-dollar-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'alpha arg))

(defalias 'ar-doublebacktick-alpha-atpt 'ar-alpha-doublebacktick-atpt)
(defun ar-alpha-doublebacktick-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'alpha arg))

(defalias 'ar-doublequote-alpha-atpt 'ar-alpha-doublequote-atpt)
(defun ar-alpha-doublequote-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'alpha arg))

(defalias 'ar-equalize-alpha-atpt 'ar-alpha-equalize-atpt)
(defun ar-alpha-equalize-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'alpha arg))

(defalias 'ar-escape-alpha-atpt 'ar-alpha-escape-atpt)
(defun ar-alpha-escape-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'alpha arg))

(defalias 'ar-hash-alpha-atpt 'ar-alpha-hash-atpt)
(defun ar-alpha-hash-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'alpha arg))

(defalias 'ar-hyphen-alpha-atpt 'ar-alpha-hyphen-atpt)
(defun ar-alpha-hyphen-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'alpha arg))

(defalias 'ar-pipe-alpha-atpt 'ar-alpha-pipe-atpt)
(defun ar-alpha-pipe-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'alpha arg))

(defalias 'ar-singlequote-alpha-atpt 'ar-alpha-singlequote-atpt)
(defun ar-alpha-singlequote-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'alpha arg))

(defalias 'ar-slash-alpha-atpt 'ar-alpha-slash-atpt)
(defun ar-alpha-slash-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'alpha arg))

(defalias 'ar-star-alpha-atpt 'ar-alpha-star-atpt)
(defun ar-alpha-star-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'alpha arg))

(defalias 'ar-tild-alpha-atpt 'ar-alpha-tild-atpt)
(defun ar-alpha-tild-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'alpha arg))

(defalias 'ar-triplebacktick-alpha-atpt 'ar-alpha-triplebacktick-atpt)
(defun ar-alpha-triplebacktick-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'alpha arg))

(defalias 'ar-underscore-alpha-atpt 'ar-alpha-underscore-atpt)
(defun ar-alpha-underscore-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'alpha arg))

(defalias 'ar-whitespace-alpha-atpt 'ar-alpha-whitespace-atpt)
(defun ar-alpha-whitespace-atpt (&optional arg)
  "Returns ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'alpha arg))

(defalias 'ar-colon-ascii-atpt 'ar-ascii-colon-atpt)
(defun ar-ascii-colon-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'ascii arg))

(defalias 'ar-cross-ascii-atpt 'ar-ascii-cross-atpt)
(defun ar-ascii-cross-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'ascii arg))

(defalias 'ar-doubleslash-ascii-atpt 'ar-ascii-doubleslash-atpt)
(defun ar-ascii-doubleslash-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'ascii arg))

(defalias 'ar-backslash-ascii-atpt 'ar-ascii-backslash-atpt)
(defun ar-ascii-backslash-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'ascii arg))

(defalias 'ar-backtick-ascii-atpt 'ar-ascii-backtick-atpt)
(defun ar-ascii-backtick-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'ascii arg))

(defalias 'ar-dollar-ascii-atpt 'ar-ascii-dollar-atpt)
(defun ar-ascii-dollar-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'ascii arg))

(defalias 'ar-doublebacktick-ascii-atpt 'ar-ascii-doublebacktick-atpt)
(defun ar-ascii-doublebacktick-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'ascii arg))

(defalias 'ar-doublequote-ascii-atpt 'ar-ascii-doublequote-atpt)
(defun ar-ascii-doublequote-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'ascii arg))

(defalias 'ar-equalize-ascii-atpt 'ar-ascii-equalize-atpt)
(defun ar-ascii-equalize-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'ascii arg))

(defalias 'ar-escape-ascii-atpt 'ar-ascii-escape-atpt)
(defun ar-ascii-escape-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'ascii arg))

(defalias 'ar-hash-ascii-atpt 'ar-ascii-hash-atpt)
(defun ar-ascii-hash-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'ascii arg))

(defalias 'ar-hyphen-ascii-atpt 'ar-ascii-hyphen-atpt)
(defun ar-ascii-hyphen-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'ascii arg))

(defalias 'ar-pipe-ascii-atpt 'ar-ascii-pipe-atpt)
(defun ar-ascii-pipe-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'ascii arg))

(defalias 'ar-singlequote-ascii-atpt 'ar-ascii-singlequote-atpt)
(defun ar-ascii-singlequote-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'ascii arg))

(defalias 'ar-slash-ascii-atpt 'ar-ascii-slash-atpt)
(defun ar-ascii-slash-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'ascii arg))

(defalias 'ar-star-ascii-atpt 'ar-ascii-star-atpt)
(defun ar-ascii-star-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'ascii arg))

(defalias 'ar-tild-ascii-atpt 'ar-ascii-tild-atpt)
(defun ar-ascii-tild-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'ascii arg))

(defalias 'ar-triplebacktick-ascii-atpt 'ar-ascii-triplebacktick-atpt)
(defun ar-ascii-triplebacktick-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'ascii arg))

(defalias 'ar-underscore-ascii-atpt 'ar-ascii-underscore-atpt)
(defun ar-ascii-underscore-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'ascii arg))

(defalias 'ar-whitespace-ascii-atpt 'ar-ascii-whitespace-atpt)
(defun ar-ascii-whitespace-atpt (&optional arg)
  "Returns ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'ascii arg))

(defalias 'ar-colon-blank-atpt 'ar-blank-colon-atpt)
(defun ar-blank-colon-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'blank arg))

(defalias 'ar-cross-blank-atpt 'ar-blank-cross-atpt)
(defun ar-blank-cross-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'blank arg))

(defalias 'ar-doubleslash-blank-atpt 'ar-blank-doubleslash-atpt)
(defun ar-blank-doubleslash-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'blank arg))

(defalias 'ar-backslash-blank-atpt 'ar-blank-backslash-atpt)
(defun ar-blank-backslash-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'blank arg))

(defalias 'ar-backtick-blank-atpt 'ar-blank-backtick-atpt)
(defun ar-blank-backtick-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'blank arg))

(defalias 'ar-dollar-blank-atpt 'ar-blank-dollar-atpt)
(defun ar-blank-dollar-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'blank arg))

(defalias 'ar-doublebacktick-blank-atpt 'ar-blank-doublebacktick-atpt)
(defun ar-blank-doublebacktick-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'blank arg))

(defalias 'ar-doublequote-blank-atpt 'ar-blank-doublequote-atpt)
(defun ar-blank-doublequote-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'blank arg))

(defalias 'ar-equalize-blank-atpt 'ar-blank-equalize-atpt)
(defun ar-blank-equalize-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'blank arg))

(defalias 'ar-escape-blank-atpt 'ar-blank-escape-atpt)
(defun ar-blank-escape-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'blank arg))

(defalias 'ar-hash-blank-atpt 'ar-blank-hash-atpt)
(defun ar-blank-hash-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'blank arg))

(defalias 'ar-hyphen-blank-atpt 'ar-blank-hyphen-atpt)
(defun ar-blank-hyphen-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'blank arg))

(defalias 'ar-pipe-blank-atpt 'ar-blank-pipe-atpt)
(defun ar-blank-pipe-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'blank arg))

(defalias 'ar-singlequote-blank-atpt 'ar-blank-singlequote-atpt)
(defun ar-blank-singlequote-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'blank arg))

(defalias 'ar-slash-blank-atpt 'ar-blank-slash-atpt)
(defun ar-blank-slash-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'blank arg))

(defalias 'ar-star-blank-atpt 'ar-blank-star-atpt)
(defun ar-blank-star-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'blank arg))

(defalias 'ar-tild-blank-atpt 'ar-blank-tild-atpt)
(defun ar-blank-tild-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'blank arg))

(defalias 'ar-triplebacktick-blank-atpt 'ar-blank-triplebacktick-atpt)
(defun ar-blank-triplebacktick-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'blank arg))

(defalias 'ar-underscore-blank-atpt 'ar-blank-underscore-atpt)
(defun ar-blank-underscore-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'blank arg))

(defalias 'ar-whitespace-blank-atpt 'ar-blank-whitespace-atpt)
(defun ar-blank-whitespace-atpt (&optional arg)
  "Returns BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'blank arg))

(defalias 'ar-colon-cntrl-atpt 'ar-cntrl-colon-atpt)
(defun ar-cntrl-colon-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'cntrl arg))

(defalias 'ar-cross-cntrl-atpt 'ar-cntrl-cross-atpt)
(defun ar-cntrl-cross-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'cntrl arg))

(defalias 'ar-doubleslash-cntrl-atpt 'ar-cntrl-doubleslash-atpt)
(defun ar-cntrl-doubleslash-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'cntrl arg))

(defalias 'ar-backslash-cntrl-atpt 'ar-cntrl-backslash-atpt)
(defun ar-cntrl-backslash-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'cntrl arg))

(defalias 'ar-backtick-cntrl-atpt 'ar-cntrl-backtick-atpt)
(defun ar-cntrl-backtick-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'cntrl arg))

(defalias 'ar-dollar-cntrl-atpt 'ar-cntrl-dollar-atpt)
(defun ar-cntrl-dollar-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'cntrl arg))

(defalias 'ar-doublebacktick-cntrl-atpt 'ar-cntrl-doublebacktick-atpt)
(defun ar-cntrl-doublebacktick-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'cntrl arg))

(defalias 'ar-doublequote-cntrl-atpt 'ar-cntrl-doublequote-atpt)
(defun ar-cntrl-doublequote-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'cntrl arg))

(defalias 'ar-equalize-cntrl-atpt 'ar-cntrl-equalize-atpt)
(defun ar-cntrl-equalize-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'cntrl arg))

(defalias 'ar-escape-cntrl-atpt 'ar-cntrl-escape-atpt)
(defun ar-cntrl-escape-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'cntrl arg))

(defalias 'ar-hash-cntrl-atpt 'ar-cntrl-hash-atpt)
(defun ar-cntrl-hash-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'cntrl arg))

(defalias 'ar-hyphen-cntrl-atpt 'ar-cntrl-hyphen-atpt)
(defun ar-cntrl-hyphen-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'cntrl arg))

(defalias 'ar-pipe-cntrl-atpt 'ar-cntrl-pipe-atpt)
(defun ar-cntrl-pipe-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'cntrl arg))

(defalias 'ar-singlequote-cntrl-atpt 'ar-cntrl-singlequote-atpt)
(defun ar-cntrl-singlequote-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'cntrl arg))

(defalias 'ar-slash-cntrl-atpt 'ar-cntrl-slash-atpt)
(defun ar-cntrl-slash-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'cntrl arg))

(defalias 'ar-star-cntrl-atpt 'ar-cntrl-star-atpt)
(defun ar-cntrl-star-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'cntrl arg))

(defalias 'ar-tild-cntrl-atpt 'ar-cntrl-tild-atpt)
(defun ar-cntrl-tild-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'cntrl arg))

(defalias 'ar-triplebacktick-cntrl-atpt 'ar-cntrl-triplebacktick-atpt)
(defun ar-cntrl-triplebacktick-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'cntrl arg))

(defalias 'ar-underscore-cntrl-atpt 'ar-cntrl-underscore-atpt)
(defun ar-cntrl-underscore-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'cntrl arg))

(defalias 'ar-whitespace-cntrl-atpt 'ar-cntrl-whitespace-atpt)
(defun ar-cntrl-whitespace-atpt (&optional arg)
  "Returns CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'cntrl arg))

(defalias 'ar-colon-digit-atpt 'ar-digit-colon-atpt)
(defun ar-digit-colon-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'digit arg))

(defalias 'ar-cross-digit-atpt 'ar-digit-cross-atpt)
(defun ar-digit-cross-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'digit arg))

(defalias 'ar-doubleslash-digit-atpt 'ar-digit-doubleslash-atpt)
(defun ar-digit-doubleslash-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'digit arg))

(defalias 'ar-backslash-digit-atpt 'ar-digit-backslash-atpt)
(defun ar-digit-backslash-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'digit arg))

(defalias 'ar-backtick-digit-atpt 'ar-digit-backtick-atpt)
(defun ar-digit-backtick-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'digit arg))

(defalias 'ar-dollar-digit-atpt 'ar-digit-dollar-atpt)
(defun ar-digit-dollar-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'digit arg))

(defalias 'ar-doublebacktick-digit-atpt 'ar-digit-doublebacktick-atpt)
(defun ar-digit-doublebacktick-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'digit arg))

(defalias 'ar-doublequote-digit-atpt 'ar-digit-doublequote-atpt)
(defun ar-digit-doublequote-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'digit arg))

(defalias 'ar-equalize-digit-atpt 'ar-digit-equalize-atpt)
(defun ar-digit-equalize-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'digit arg))

(defalias 'ar-escape-digit-atpt 'ar-digit-escape-atpt)
(defun ar-digit-escape-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'digit arg))

(defalias 'ar-hash-digit-atpt 'ar-digit-hash-atpt)
(defun ar-digit-hash-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'digit arg))

(defalias 'ar-hyphen-digit-atpt 'ar-digit-hyphen-atpt)
(defun ar-digit-hyphen-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'digit arg))

(defalias 'ar-pipe-digit-atpt 'ar-digit-pipe-atpt)
(defun ar-digit-pipe-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'digit arg))

(defalias 'ar-singlequote-digit-atpt 'ar-digit-singlequote-atpt)
(defun ar-digit-singlequote-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'digit arg))

(defalias 'ar-slash-digit-atpt 'ar-digit-slash-atpt)
(defun ar-digit-slash-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'digit arg))

(defalias 'ar-star-digit-atpt 'ar-digit-star-atpt)
(defun ar-digit-star-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'digit arg))

(defalias 'ar-tild-digit-atpt 'ar-digit-tild-atpt)
(defun ar-digit-tild-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'digit arg))

(defalias 'ar-triplebacktick-digit-atpt 'ar-digit-triplebacktick-atpt)
(defun ar-digit-triplebacktick-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'digit arg))

(defalias 'ar-underscore-digit-atpt 'ar-digit-underscore-atpt)
(defun ar-digit-underscore-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'digit arg))

(defalias 'ar-whitespace-digit-atpt 'ar-digit-whitespace-atpt)
(defun ar-digit-whitespace-atpt (&optional arg)
  "Returns DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'digit arg))

(defalias 'ar-colon-graph-atpt 'ar-graph-colon-atpt)
(defun ar-graph-colon-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'graph arg))

(defalias 'ar-cross-graph-atpt 'ar-graph-cross-atpt)
(defun ar-graph-cross-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'graph arg))

(defalias 'ar-doubleslash-graph-atpt 'ar-graph-doubleslash-atpt)
(defun ar-graph-doubleslash-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'graph arg))

(defalias 'ar-backslash-graph-atpt 'ar-graph-backslash-atpt)
(defun ar-graph-backslash-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'graph arg))

(defalias 'ar-backtick-graph-atpt 'ar-graph-backtick-atpt)
(defun ar-graph-backtick-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'graph arg))

(defalias 'ar-dollar-graph-atpt 'ar-graph-dollar-atpt)
(defun ar-graph-dollar-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'graph arg))

(defalias 'ar-doublebacktick-graph-atpt 'ar-graph-doublebacktick-atpt)
(defun ar-graph-doublebacktick-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'graph arg))

(defalias 'ar-doublequote-graph-atpt 'ar-graph-doublequote-atpt)
(defun ar-graph-doublequote-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'graph arg))

(defalias 'ar-equalize-graph-atpt 'ar-graph-equalize-atpt)
(defun ar-graph-equalize-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'graph arg))

(defalias 'ar-escape-graph-atpt 'ar-graph-escape-atpt)
(defun ar-graph-escape-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'graph arg))

(defalias 'ar-hash-graph-atpt 'ar-graph-hash-atpt)
(defun ar-graph-hash-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'graph arg))

(defalias 'ar-hyphen-graph-atpt 'ar-graph-hyphen-atpt)
(defun ar-graph-hyphen-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'graph arg))

(defalias 'ar-pipe-graph-atpt 'ar-graph-pipe-atpt)
(defun ar-graph-pipe-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'graph arg))

(defalias 'ar-singlequote-graph-atpt 'ar-graph-singlequote-atpt)
(defun ar-graph-singlequote-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'graph arg))

(defalias 'ar-slash-graph-atpt 'ar-graph-slash-atpt)
(defun ar-graph-slash-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'graph arg))

(defalias 'ar-star-graph-atpt 'ar-graph-star-atpt)
(defun ar-graph-star-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'graph arg))

(defalias 'ar-tild-graph-atpt 'ar-graph-tild-atpt)
(defun ar-graph-tild-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'graph arg))

(defalias 'ar-triplebacktick-graph-atpt 'ar-graph-triplebacktick-atpt)
(defun ar-graph-triplebacktick-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'graph arg))

(defalias 'ar-underscore-graph-atpt 'ar-graph-underscore-atpt)
(defun ar-graph-underscore-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'graph arg))

(defalias 'ar-whitespace-graph-atpt 'ar-graph-whitespace-atpt)
(defun ar-graph-whitespace-atpt (&optional arg)
  "Returns GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'graph arg))

(defalias 'ar-colon-lower-atpt 'ar-lower-colon-atpt)
(defun ar-lower-colon-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'lower arg))

(defalias 'ar-cross-lower-atpt 'ar-lower-cross-atpt)
(defun ar-lower-cross-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'lower arg))

(defalias 'ar-doubleslash-lower-atpt 'ar-lower-doubleslash-atpt)
(defun ar-lower-doubleslash-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'lower arg))

(defalias 'ar-backslash-lower-atpt 'ar-lower-backslash-atpt)
(defun ar-lower-backslash-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'lower arg))

(defalias 'ar-backtick-lower-atpt 'ar-lower-backtick-atpt)
(defun ar-lower-backtick-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'lower arg))

(defalias 'ar-dollar-lower-atpt 'ar-lower-dollar-atpt)
(defun ar-lower-dollar-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'lower arg))

(defalias 'ar-doublebacktick-lower-atpt 'ar-lower-doublebacktick-atpt)
(defun ar-lower-doublebacktick-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'lower arg))

(defalias 'ar-doublequote-lower-atpt 'ar-lower-doublequote-atpt)
(defun ar-lower-doublequote-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'lower arg))

(defalias 'ar-equalize-lower-atpt 'ar-lower-equalize-atpt)
(defun ar-lower-equalize-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'lower arg))

(defalias 'ar-escape-lower-atpt 'ar-lower-escape-atpt)
(defun ar-lower-escape-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'lower arg))

(defalias 'ar-hash-lower-atpt 'ar-lower-hash-atpt)
(defun ar-lower-hash-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'lower arg))

(defalias 'ar-hyphen-lower-atpt 'ar-lower-hyphen-atpt)
(defun ar-lower-hyphen-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'lower arg))

(defalias 'ar-pipe-lower-atpt 'ar-lower-pipe-atpt)
(defun ar-lower-pipe-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'lower arg))

(defalias 'ar-singlequote-lower-atpt 'ar-lower-singlequote-atpt)
(defun ar-lower-singlequote-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'lower arg))

(defalias 'ar-slash-lower-atpt 'ar-lower-slash-atpt)
(defun ar-lower-slash-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'lower arg))

(defalias 'ar-star-lower-atpt 'ar-lower-star-atpt)
(defun ar-lower-star-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'lower arg))

(defalias 'ar-tild-lower-atpt 'ar-lower-tild-atpt)
(defun ar-lower-tild-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'lower arg))

(defalias 'ar-triplebacktick-lower-atpt 'ar-lower-triplebacktick-atpt)
(defun ar-lower-triplebacktick-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'lower arg))

(defalias 'ar-underscore-lower-atpt 'ar-lower-underscore-atpt)
(defun ar-lower-underscore-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'lower arg))

(defalias 'ar-whitespace-lower-atpt 'ar-lower-whitespace-atpt)
(defun ar-lower-whitespace-atpt (&optional arg)
  "Returns LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'lower arg))

(defalias 'ar-colon-nonascii-atpt 'ar-nonascii-colon-atpt)
(defun ar-nonascii-colon-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'nonascii arg))

(defalias 'ar-cross-nonascii-atpt 'ar-nonascii-cross-atpt)
(defun ar-nonascii-cross-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'nonascii arg))

(defalias 'ar-doubleslash-nonascii-atpt 'ar-nonascii-doubleslash-atpt)
(defun ar-nonascii-doubleslash-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'nonascii arg))

(defalias 'ar-backslash-nonascii-atpt 'ar-nonascii-backslash-atpt)
(defun ar-nonascii-backslash-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'nonascii arg))

(defalias 'ar-backtick-nonascii-atpt 'ar-nonascii-backtick-atpt)
(defun ar-nonascii-backtick-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'nonascii arg))

(defalias 'ar-dollar-nonascii-atpt 'ar-nonascii-dollar-atpt)
(defun ar-nonascii-dollar-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'nonascii arg))

(defalias 'ar-doublebacktick-nonascii-atpt 'ar-nonascii-doublebacktick-atpt)
(defun ar-nonascii-doublebacktick-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'nonascii arg))

(defalias 'ar-doublequote-nonascii-atpt 'ar-nonascii-doublequote-atpt)
(defun ar-nonascii-doublequote-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'nonascii arg))

(defalias 'ar-equalize-nonascii-atpt 'ar-nonascii-equalize-atpt)
(defun ar-nonascii-equalize-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'nonascii arg))

(defalias 'ar-escape-nonascii-atpt 'ar-nonascii-escape-atpt)
(defun ar-nonascii-escape-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'nonascii arg))

(defalias 'ar-hash-nonascii-atpt 'ar-nonascii-hash-atpt)
(defun ar-nonascii-hash-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'nonascii arg))

(defalias 'ar-hyphen-nonascii-atpt 'ar-nonascii-hyphen-atpt)
(defun ar-nonascii-hyphen-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'nonascii arg))

(defalias 'ar-pipe-nonascii-atpt 'ar-nonascii-pipe-atpt)
(defun ar-nonascii-pipe-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'nonascii arg))

(defalias 'ar-singlequote-nonascii-atpt 'ar-nonascii-singlequote-atpt)
(defun ar-nonascii-singlequote-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'nonascii arg))

(defalias 'ar-slash-nonascii-atpt 'ar-nonascii-slash-atpt)
(defun ar-nonascii-slash-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'nonascii arg))

(defalias 'ar-star-nonascii-atpt 'ar-nonascii-star-atpt)
(defun ar-nonascii-star-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'nonascii arg))

(defalias 'ar-tild-nonascii-atpt 'ar-nonascii-tild-atpt)
(defun ar-nonascii-tild-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'nonascii arg))

(defalias 'ar-triplebacktick-nonascii-atpt 'ar-nonascii-triplebacktick-atpt)
(defun ar-nonascii-triplebacktick-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'nonascii arg))

(defalias 'ar-underscore-nonascii-atpt 'ar-nonascii-underscore-atpt)
(defun ar-nonascii-underscore-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'nonascii arg))

(defalias 'ar-whitespace-nonascii-atpt 'ar-nonascii-whitespace-atpt)
(defun ar-nonascii-whitespace-atpt (&optional arg)
  "Returns NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'nonascii arg))

(defalias 'ar-colon-print-atpt 'ar-print-colon-atpt)
(defun ar-print-colon-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'print arg))

(defalias 'ar-cross-print-atpt 'ar-print-cross-atpt)
(defun ar-print-cross-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'print arg))

(defalias 'ar-doubleslash-print-atpt 'ar-print-doubleslash-atpt)
(defun ar-print-doubleslash-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'print arg))

(defalias 'ar-backslash-print-atpt 'ar-print-backslash-atpt)
(defun ar-print-backslash-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'print arg))

(defalias 'ar-backtick-print-atpt 'ar-print-backtick-atpt)
(defun ar-print-backtick-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'print arg))

(defalias 'ar-dollar-print-atpt 'ar-print-dollar-atpt)
(defun ar-print-dollar-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'print arg))

(defalias 'ar-doublebacktick-print-atpt 'ar-print-doublebacktick-atpt)
(defun ar-print-doublebacktick-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'print arg))

(defalias 'ar-doublequote-print-atpt 'ar-print-doublequote-atpt)
(defun ar-print-doublequote-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'print arg))

(defalias 'ar-equalize-print-atpt 'ar-print-equalize-atpt)
(defun ar-print-equalize-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'print arg))

(defalias 'ar-escape-print-atpt 'ar-print-escape-atpt)
(defun ar-print-escape-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'print arg))

(defalias 'ar-hash-print-atpt 'ar-print-hash-atpt)
(defun ar-print-hash-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'print arg))

(defalias 'ar-hyphen-print-atpt 'ar-print-hyphen-atpt)
(defun ar-print-hyphen-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'print arg))

(defalias 'ar-pipe-print-atpt 'ar-print-pipe-atpt)
(defun ar-print-pipe-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'print arg))

(defalias 'ar-singlequote-print-atpt 'ar-print-singlequote-atpt)
(defun ar-print-singlequote-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'print arg))

(defalias 'ar-slash-print-atpt 'ar-print-slash-atpt)
(defun ar-print-slash-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'print arg))

(defalias 'ar-star-print-atpt 'ar-print-star-atpt)
(defun ar-print-star-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'print arg))

(defalias 'ar-tild-print-atpt 'ar-print-tild-atpt)
(defun ar-print-tild-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'print arg))

(defalias 'ar-triplebacktick-print-atpt 'ar-print-triplebacktick-atpt)
(defun ar-print-triplebacktick-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'print arg))

(defalias 'ar-underscore-print-atpt 'ar-print-underscore-atpt)
(defun ar-print-underscore-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'print arg))

(defalias 'ar-whitespace-print-atpt 'ar-print-whitespace-atpt)
(defun ar-print-whitespace-atpt (&optional arg)
  "Returns PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'print arg))

(defalias 'ar-colon-punct-atpt 'ar-punct-colon-atpt)
(defun ar-punct-colon-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'punct arg))

(defalias 'ar-cross-punct-atpt 'ar-punct-cross-atpt)
(defun ar-punct-cross-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'punct arg))

(defalias 'ar-doubleslash-punct-atpt 'ar-punct-doubleslash-atpt)
(defun ar-punct-doubleslash-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'punct arg))

(defalias 'ar-backslash-punct-atpt 'ar-punct-backslash-atpt)
(defun ar-punct-backslash-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'punct arg))

(defalias 'ar-backtick-punct-atpt 'ar-punct-backtick-atpt)
(defun ar-punct-backtick-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'punct arg))

(defalias 'ar-dollar-punct-atpt 'ar-punct-dollar-atpt)
(defun ar-punct-dollar-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'punct arg))

(defalias 'ar-doublebacktick-punct-atpt 'ar-punct-doublebacktick-atpt)
(defun ar-punct-doublebacktick-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'punct arg))

(defalias 'ar-doublequote-punct-atpt 'ar-punct-doublequote-atpt)
(defun ar-punct-doublequote-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'punct arg))

(defalias 'ar-equalize-punct-atpt 'ar-punct-equalize-atpt)
(defun ar-punct-equalize-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'punct arg))

(defalias 'ar-escape-punct-atpt 'ar-punct-escape-atpt)
(defun ar-punct-escape-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'punct arg))

(defalias 'ar-hash-punct-atpt 'ar-punct-hash-atpt)
(defun ar-punct-hash-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'punct arg))

(defalias 'ar-hyphen-punct-atpt 'ar-punct-hyphen-atpt)
(defun ar-punct-hyphen-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'punct arg))

(defalias 'ar-pipe-punct-atpt 'ar-punct-pipe-atpt)
(defun ar-punct-pipe-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'punct arg))

(defalias 'ar-singlequote-punct-atpt 'ar-punct-singlequote-atpt)
(defun ar-punct-singlequote-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'punct arg))

(defalias 'ar-slash-punct-atpt 'ar-punct-slash-atpt)
(defun ar-punct-slash-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'punct arg))

(defalias 'ar-star-punct-atpt 'ar-punct-star-atpt)
(defun ar-punct-star-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'punct arg))

(defalias 'ar-tild-punct-atpt 'ar-punct-tild-atpt)
(defun ar-punct-tild-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'punct arg))

(defalias 'ar-triplebacktick-punct-atpt 'ar-punct-triplebacktick-atpt)
(defun ar-punct-triplebacktick-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'punct arg))

(defalias 'ar-underscore-punct-atpt 'ar-punct-underscore-atpt)
(defun ar-punct-underscore-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'punct arg))

(defalias 'ar-whitespace-punct-atpt 'ar-punct-whitespace-atpt)
(defun ar-punct-whitespace-atpt (&optional arg)
  "Returns PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'punct arg))

(defalias 'ar-colon-space-atpt 'ar-space-colon-atpt)
(defun ar-space-colon-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'space arg))

(defalias 'ar-cross-space-atpt 'ar-space-cross-atpt)
(defun ar-space-cross-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'space arg))

(defalias 'ar-doubleslash-space-atpt 'ar-space-doubleslash-atpt)
(defun ar-space-doubleslash-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'space arg))

(defalias 'ar-backslash-space-atpt 'ar-space-backslash-atpt)
(defun ar-space-backslash-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'space arg))

(defalias 'ar-backtick-space-atpt 'ar-space-backtick-atpt)
(defun ar-space-backtick-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'space arg))

(defalias 'ar-dollar-space-atpt 'ar-space-dollar-atpt)
(defun ar-space-dollar-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'space arg))

(defalias 'ar-doublebacktick-space-atpt 'ar-space-doublebacktick-atpt)
(defun ar-space-doublebacktick-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'space arg))

(defalias 'ar-doublequote-space-atpt 'ar-space-doublequote-atpt)
(defun ar-space-doublequote-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'space arg))

(defalias 'ar-equalize-space-atpt 'ar-space-equalize-atpt)
(defun ar-space-equalize-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'space arg))

(defalias 'ar-escape-space-atpt 'ar-space-escape-atpt)
(defun ar-space-escape-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'space arg))

(defalias 'ar-hash-space-atpt 'ar-space-hash-atpt)
(defun ar-space-hash-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'space arg))

(defalias 'ar-hyphen-space-atpt 'ar-space-hyphen-atpt)
(defun ar-space-hyphen-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'space arg))

(defalias 'ar-pipe-space-atpt 'ar-space-pipe-atpt)
(defun ar-space-pipe-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'space arg))

(defalias 'ar-singlequote-space-atpt 'ar-space-singlequote-atpt)
(defun ar-space-singlequote-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'space arg))

(defalias 'ar-slash-space-atpt 'ar-space-slash-atpt)
(defun ar-space-slash-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'space arg))

(defalias 'ar-star-space-atpt 'ar-space-star-atpt)
(defun ar-space-star-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'space arg))

(defalias 'ar-tild-space-atpt 'ar-space-tild-atpt)
(defun ar-space-tild-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'space arg))

(defalias 'ar-triplebacktick-space-atpt 'ar-space-triplebacktick-atpt)
(defun ar-space-triplebacktick-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'space arg))

(defalias 'ar-underscore-space-atpt 'ar-space-underscore-atpt)
(defun ar-space-underscore-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'space arg))

(defalias 'ar-whitespace-space-atpt 'ar-space-whitespace-atpt)
(defun ar-space-whitespace-atpt (&optional arg)
  "Returns SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'space arg))

(defalias 'ar-colon-upper-atpt 'ar-upper-colon-atpt)
(defun ar-upper-colon-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'upper arg))

(defalias 'ar-cross-upper-atpt 'ar-upper-cross-atpt)
(defun ar-upper-cross-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'upper arg))

(defalias 'ar-doubleslash-upper-atpt 'ar-upper-doubleslash-atpt)
(defun ar-upper-doubleslash-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'upper arg))

(defalias 'ar-backslash-upper-atpt 'ar-upper-backslash-atpt)
(defun ar-upper-backslash-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'upper arg))

(defalias 'ar-backtick-upper-atpt 'ar-upper-backtick-atpt)
(defun ar-upper-backtick-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'upper arg))

(defalias 'ar-dollar-upper-atpt 'ar-upper-dollar-atpt)
(defun ar-upper-dollar-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'upper arg))

(defalias 'ar-doublebacktick-upper-atpt 'ar-upper-doublebacktick-atpt)
(defun ar-upper-doublebacktick-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'upper arg))

(defalias 'ar-doublequote-upper-atpt 'ar-upper-doublequote-atpt)
(defun ar-upper-doublequote-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'upper arg))

(defalias 'ar-equalize-upper-atpt 'ar-upper-equalize-atpt)
(defun ar-upper-equalize-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'upper arg))

(defalias 'ar-escape-upper-atpt 'ar-upper-escape-atpt)
(defun ar-upper-escape-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'upper arg))

(defalias 'ar-hash-upper-atpt 'ar-upper-hash-atpt)
(defun ar-upper-hash-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'upper arg))

(defalias 'ar-hyphen-upper-atpt 'ar-upper-hyphen-atpt)
(defun ar-upper-hyphen-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'upper arg))

(defalias 'ar-pipe-upper-atpt 'ar-upper-pipe-atpt)
(defun ar-upper-pipe-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'upper arg))

(defalias 'ar-singlequote-upper-atpt 'ar-upper-singlequote-atpt)
(defun ar-upper-singlequote-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'upper arg))

(defalias 'ar-slash-upper-atpt 'ar-upper-slash-atpt)
(defun ar-upper-slash-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'upper arg))

(defalias 'ar-star-upper-atpt 'ar-upper-star-atpt)
(defun ar-upper-star-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'upper arg))

(defalias 'ar-tild-upper-atpt 'ar-upper-tild-atpt)
(defun ar-upper-tild-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'upper arg))

(defalias 'ar-triplebacktick-upper-atpt 'ar-upper-triplebacktick-atpt)
(defun ar-upper-triplebacktick-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'upper arg))

(defalias 'ar-underscore-upper-atpt 'ar-upper-underscore-atpt)
(defun ar-upper-underscore-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'upper arg))

(defalias 'ar-whitespace-upper-atpt 'ar-upper-whitespace-atpt)
(defun ar-upper-whitespace-atpt (&optional arg)
  "Returns UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'upper arg))

(defalias 'ar-colon-xdigit-atpt 'ar-xdigit-colon-atpt)
(defun ar-xdigit-colon-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'xdigit arg))

(defalias 'ar-cross-xdigit-atpt 'ar-xdigit-cross-atpt)
(defun ar-xdigit-cross-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'xdigit arg))

(defalias 'ar-doubleslash-xdigit-atpt 'ar-xdigit-doubleslash-atpt)
(defun ar-xdigit-doubleslash-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'xdigit arg))

(defalias 'ar-backslash-xdigit-atpt 'ar-xdigit-backslash-atpt)
(defun ar-xdigit-backslash-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'xdigit arg))

(defalias 'ar-backtick-xdigit-atpt 'ar-xdigit-backtick-atpt)
(defun ar-xdigit-backtick-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'xdigit arg))

(defalias 'ar-dollar-xdigit-atpt 'ar-xdigit-dollar-atpt)
(defun ar-xdigit-dollar-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'xdigit arg))

(defalias 'ar-doublebacktick-xdigit-atpt 'ar-xdigit-doublebacktick-atpt)
(defun ar-xdigit-doublebacktick-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'xdigit arg))

(defalias 'ar-doublequote-xdigit-atpt 'ar-xdigit-doublequote-atpt)
(defun ar-xdigit-doublequote-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'xdigit arg))

(defalias 'ar-equalize-xdigit-atpt 'ar-xdigit-equalize-atpt)
(defun ar-xdigit-equalize-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'xdigit arg))

(defalias 'ar-escape-xdigit-atpt 'ar-xdigit-escape-atpt)
(defun ar-xdigit-escape-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'xdigit arg))

(defalias 'ar-hash-xdigit-atpt 'ar-xdigit-hash-atpt)
(defun ar-xdigit-hash-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'xdigit arg))

(defalias 'ar-hyphen-xdigit-atpt 'ar-xdigit-hyphen-atpt)
(defun ar-xdigit-hyphen-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'xdigit arg))

(defalias 'ar-pipe-xdigit-atpt 'ar-xdigit-pipe-atpt)
(defun ar-xdigit-pipe-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'xdigit arg))

(defalias 'ar-singlequote-xdigit-atpt 'ar-xdigit-singlequote-atpt)
(defun ar-xdigit-singlequote-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'xdigit arg))

(defalias 'ar-slash-xdigit-atpt 'ar-xdigit-slash-atpt)
(defun ar-xdigit-slash-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'xdigit arg))

(defalias 'ar-star-xdigit-atpt 'ar-xdigit-star-atpt)
(defun ar-xdigit-star-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'xdigit arg))

(defalias 'ar-tild-xdigit-atpt 'ar-xdigit-tild-atpt)
(defun ar-xdigit-tild-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'xdigit arg))

(defalias 'ar-triplebacktick-xdigit-atpt 'ar-xdigit-triplebacktick-atpt)
(defun ar-xdigit-triplebacktick-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'xdigit arg))

(defalias 'ar-underscore-xdigit-atpt 'ar-xdigit-underscore-atpt)
(defun ar-xdigit-underscore-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'xdigit arg))

(defalias 'ar-whitespace-xdigit-atpt 'ar-xdigit-whitespace-atpt)
(defun ar-xdigit-whitespace-atpt (&optional arg)
  "Returns XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'xdigit arg));; ar-thing-at-point-utils-unpaired-delim-aktiv ar-unpaired-delimit-aktiv ar-atpt-rest-list: end

;; ar-thing-at-point-utils-unpaired-paired: start


(defalias 'ar-colon-braced-atpt 'ar-braced-colon-atpt)
(defun ar-braced-colon-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'braced arg))

(defalias 'ar-cross-braced-atpt 'ar-braced-cross-atpt)
(defun ar-braced-cross-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'braced arg))

(defalias 'ar-doubleslash-braced-atpt 'ar-braced-doubleslash-atpt)
(defun ar-braced-doubleslash-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'braced arg))

(defalias 'ar-backslash-braced-atpt 'ar-braced-backslash-atpt)
(defun ar-braced-backslash-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'braced arg))

(defalias 'ar-backtick-braced-atpt 'ar-braced-backtick-atpt)
(defun ar-braced-backtick-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'braced arg))

(defalias 'ar-dollar-braced-atpt 'ar-braced-dollar-atpt)
(defun ar-braced-dollar-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'braced arg))

(defalias 'ar-doublebacktick-braced-atpt 'ar-braced-doublebacktick-atpt)
(defun ar-braced-doublebacktick-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'braced arg))

(defalias 'ar-doublequote-braced-atpt 'ar-braced-doublequote-atpt)
(defun ar-braced-doublequote-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'braced arg))

(defalias 'ar-equalize-braced-atpt 'ar-braced-equalize-atpt)
(defun ar-braced-equalize-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'braced arg))

(defalias 'ar-escape-braced-atpt 'ar-braced-escape-atpt)
(defun ar-braced-escape-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'braced arg))

(defalias 'ar-hash-braced-atpt 'ar-braced-hash-atpt)
(defun ar-braced-hash-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'braced arg))

(defalias 'ar-hyphen-braced-atpt 'ar-braced-hyphen-atpt)
(defun ar-braced-hyphen-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'braced arg))

(defalias 'ar-pipe-braced-atpt 'ar-braced-pipe-atpt)
(defun ar-braced-pipe-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'braced arg))

(defalias 'ar-singlequote-braced-atpt 'ar-braced-singlequote-atpt)
(defun ar-braced-singlequote-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'braced arg))

(defalias 'ar-slash-braced-atpt 'ar-braced-slash-atpt)
(defun ar-braced-slash-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'braced arg))

(defalias 'ar-star-braced-atpt 'ar-braced-star-atpt)
(defun ar-braced-star-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'braced arg))

(defalias 'ar-tild-braced-atpt 'ar-braced-tild-atpt)
(defun ar-braced-tild-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'braced arg))

(defalias 'ar-triplebacktick-braced-atpt 'ar-braced-triplebacktick-atpt)
(defun ar-braced-triplebacktick-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'braced arg))

(defalias 'ar-underscore-braced-atpt 'ar-braced-underscore-atpt)
(defun ar-braced-underscore-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'braced arg))

(defalias 'ar-whitespace-braced-atpt 'ar-braced-whitespace-atpt)
(defun ar-braced-whitespace-atpt (&optional arg)
  "Returns BRACED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'braced arg))

(defalias 'ar-colon-bracketed-atpt 'ar-bracketed-colon-atpt)
(defun ar-bracketed-colon-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'bracketed arg))

(defalias 'ar-cross-bracketed-atpt 'ar-bracketed-cross-atpt)
(defun ar-bracketed-cross-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'bracketed arg))

(defalias 'ar-doubleslash-bracketed-atpt 'ar-bracketed-doubleslash-atpt)
(defun ar-bracketed-doubleslash-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'bracketed arg))

(defalias 'ar-backslash-bracketed-atpt 'ar-bracketed-backslash-atpt)
(defun ar-bracketed-backslash-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'bracketed arg))

(defalias 'ar-backtick-bracketed-atpt 'ar-bracketed-backtick-atpt)
(defun ar-bracketed-backtick-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'bracketed arg))

(defalias 'ar-dollar-bracketed-atpt 'ar-bracketed-dollar-atpt)
(defun ar-bracketed-dollar-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'bracketed arg))

(defalias 'ar-doublebacktick-bracketed-atpt 'ar-bracketed-doublebacktick-atpt)
(defun ar-bracketed-doublebacktick-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'bracketed arg))

(defalias 'ar-doublequote-bracketed-atpt 'ar-bracketed-doublequote-atpt)
(defun ar-bracketed-doublequote-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'bracketed arg))

(defalias 'ar-equalize-bracketed-atpt 'ar-bracketed-equalize-atpt)
(defun ar-bracketed-equalize-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'bracketed arg))

(defalias 'ar-escape-bracketed-atpt 'ar-bracketed-escape-atpt)
(defun ar-bracketed-escape-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'bracketed arg))

(defalias 'ar-hash-bracketed-atpt 'ar-bracketed-hash-atpt)
(defun ar-bracketed-hash-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'bracketed arg))

(defalias 'ar-hyphen-bracketed-atpt 'ar-bracketed-hyphen-atpt)
(defun ar-bracketed-hyphen-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'bracketed arg))

(defalias 'ar-pipe-bracketed-atpt 'ar-bracketed-pipe-atpt)
(defun ar-bracketed-pipe-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'bracketed arg))

(defalias 'ar-singlequote-bracketed-atpt 'ar-bracketed-singlequote-atpt)
(defun ar-bracketed-singlequote-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'bracketed arg))

(defalias 'ar-slash-bracketed-atpt 'ar-bracketed-slash-atpt)
(defun ar-bracketed-slash-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'bracketed arg))

(defalias 'ar-star-bracketed-atpt 'ar-bracketed-star-atpt)
(defun ar-bracketed-star-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'bracketed arg))

(defalias 'ar-tild-bracketed-atpt 'ar-bracketed-tild-atpt)
(defun ar-bracketed-tild-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'bracketed arg))

(defalias 'ar-triplebacktick-bracketed-atpt 'ar-bracketed-triplebacktick-atpt)
(defun ar-bracketed-triplebacktick-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'bracketed arg))

(defalias 'ar-underscore-bracketed-atpt 'ar-bracketed-underscore-atpt)
(defun ar-bracketed-underscore-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'bracketed arg))

(defalias 'ar-whitespace-bracketed-atpt 'ar-bracketed-whitespace-atpt)
(defun ar-bracketed-whitespace-atpt (&optional arg)
  "Returns BRACKETED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'bracketed arg))

(defalias 'ar-colon-lesserangled-atpt 'ar-lesserangled-colon-atpt)
(defun ar-lesserangled-colon-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'lesserangled arg))

(defalias 'ar-cross-lesserangled-atpt 'ar-lesserangled-cross-atpt)
(defun ar-lesserangled-cross-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'lesserangled arg))

(defalias 'ar-doubleslash-lesserangled-atpt 'ar-lesserangled-doubleslash-atpt)
(defun ar-lesserangled-doubleslash-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'lesserangled arg))

(defalias 'ar-backslash-lesserangled-atpt 'ar-lesserangled-backslash-atpt)
(defun ar-lesserangled-backslash-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'lesserangled arg))

(defalias 'ar-backtick-lesserangled-atpt 'ar-lesserangled-backtick-atpt)
(defun ar-lesserangled-backtick-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'lesserangled arg))

(defalias 'ar-dollar-lesserangled-atpt 'ar-lesserangled-dollar-atpt)
(defun ar-lesserangled-dollar-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'lesserangled arg))

(defalias 'ar-doublebacktick-lesserangled-atpt 'ar-lesserangled-doublebacktick-atpt)
(defun ar-lesserangled-doublebacktick-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'lesserangled arg))

(defalias 'ar-doublequote-lesserangled-atpt 'ar-lesserangled-doublequote-atpt)
(defun ar-lesserangled-doublequote-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'lesserangled arg))

(defalias 'ar-equalize-lesserangled-atpt 'ar-lesserangled-equalize-atpt)
(defun ar-lesserangled-equalize-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'lesserangled arg))

(defalias 'ar-escape-lesserangled-atpt 'ar-lesserangled-escape-atpt)
(defun ar-lesserangled-escape-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'lesserangled arg))

(defalias 'ar-hash-lesserangled-atpt 'ar-lesserangled-hash-atpt)
(defun ar-lesserangled-hash-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'lesserangled arg))

(defalias 'ar-hyphen-lesserangled-atpt 'ar-lesserangled-hyphen-atpt)
(defun ar-lesserangled-hyphen-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'lesserangled arg))

(defalias 'ar-pipe-lesserangled-atpt 'ar-lesserangled-pipe-atpt)
(defun ar-lesserangled-pipe-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'lesserangled arg))

(defalias 'ar-singlequote-lesserangled-atpt 'ar-lesserangled-singlequote-atpt)
(defun ar-lesserangled-singlequote-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'lesserangled arg))

(defalias 'ar-slash-lesserangled-atpt 'ar-lesserangled-slash-atpt)
(defun ar-lesserangled-slash-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'lesserangled arg))

(defalias 'ar-star-lesserangled-atpt 'ar-lesserangled-star-atpt)
(defun ar-lesserangled-star-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'lesserangled arg))

(defalias 'ar-tild-lesserangled-atpt 'ar-lesserangled-tild-atpt)
(defun ar-lesserangled-tild-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'lesserangled arg))

(defalias 'ar-triplebacktick-lesserangled-atpt 'ar-lesserangled-triplebacktick-atpt)
(defun ar-lesserangled-triplebacktick-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'lesserangled arg))

(defalias 'ar-underscore-lesserangled-atpt 'ar-lesserangled-underscore-atpt)
(defun ar-lesserangled-underscore-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'lesserangled arg))

(defalias 'ar-whitespace-lesserangled-atpt 'ar-lesserangled-whitespace-atpt)
(defun ar-lesserangled-whitespace-atpt (&optional arg)
  "Returns LESSERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'lesserangled arg))

(defalias 'ar-colon-greaterangled-atpt 'ar-greaterangled-colon-atpt)
(defun ar-greaterangled-colon-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'greaterangled arg))

(defalias 'ar-cross-greaterangled-atpt 'ar-greaterangled-cross-atpt)
(defun ar-greaterangled-cross-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'greaterangled arg))

(defalias 'ar-doubleslash-greaterangled-atpt 'ar-greaterangled-doubleslash-atpt)
(defun ar-greaterangled-doubleslash-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'greaterangled arg))

(defalias 'ar-backslash-greaterangled-atpt 'ar-greaterangled-backslash-atpt)
(defun ar-greaterangled-backslash-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'greaterangled arg))

(defalias 'ar-backtick-greaterangled-atpt 'ar-greaterangled-backtick-atpt)
(defun ar-greaterangled-backtick-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'greaterangled arg))

(defalias 'ar-dollar-greaterangled-atpt 'ar-greaterangled-dollar-atpt)
(defun ar-greaterangled-dollar-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'greaterangled arg))

(defalias 'ar-doublebacktick-greaterangled-atpt 'ar-greaterangled-doublebacktick-atpt)
(defun ar-greaterangled-doublebacktick-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'greaterangled arg))

(defalias 'ar-doublequote-greaterangled-atpt 'ar-greaterangled-doublequote-atpt)
(defun ar-greaterangled-doublequote-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'greaterangled arg))

(defalias 'ar-equalize-greaterangled-atpt 'ar-greaterangled-equalize-atpt)
(defun ar-greaterangled-equalize-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'greaterangled arg))

(defalias 'ar-escape-greaterangled-atpt 'ar-greaterangled-escape-atpt)
(defun ar-greaterangled-escape-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'greaterangled arg))

(defalias 'ar-hash-greaterangled-atpt 'ar-greaterangled-hash-atpt)
(defun ar-greaterangled-hash-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'greaterangled arg))

(defalias 'ar-hyphen-greaterangled-atpt 'ar-greaterangled-hyphen-atpt)
(defun ar-greaterangled-hyphen-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'greaterangled arg))

(defalias 'ar-pipe-greaterangled-atpt 'ar-greaterangled-pipe-atpt)
(defun ar-greaterangled-pipe-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'greaterangled arg))

(defalias 'ar-singlequote-greaterangled-atpt 'ar-greaterangled-singlequote-atpt)
(defun ar-greaterangled-singlequote-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'greaterangled arg))

(defalias 'ar-slash-greaterangled-atpt 'ar-greaterangled-slash-atpt)
(defun ar-greaterangled-slash-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'greaterangled arg))

(defalias 'ar-star-greaterangled-atpt 'ar-greaterangled-star-atpt)
(defun ar-greaterangled-star-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'greaterangled arg))

(defalias 'ar-tild-greaterangled-atpt 'ar-greaterangled-tild-atpt)
(defun ar-greaterangled-tild-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'greaterangled arg))

(defalias 'ar-triplebacktick-greaterangled-atpt 'ar-greaterangled-triplebacktick-atpt)
(defun ar-greaterangled-triplebacktick-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'greaterangled arg))

(defalias 'ar-underscore-greaterangled-atpt 'ar-greaterangled-underscore-atpt)
(defun ar-greaterangled-underscore-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'greaterangled arg))

(defalias 'ar-whitespace-greaterangled-atpt 'ar-greaterangled-whitespace-atpt)
(defun ar-greaterangled-whitespace-atpt (&optional arg)
  "Returns GREATERANGLED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'greaterangled arg))

(defalias 'ar-colon-curvedsinglequoted-atpt 'ar-curvedsinglequoted-colon-atpt)
(defun ar-curvedsinglequoted-colon-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'curvedsinglequoted arg))

(defalias 'ar-cross-curvedsinglequoted-atpt 'ar-curvedsinglequoted-cross-atpt)
(defun ar-curvedsinglequoted-cross-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'curvedsinglequoted arg))

(defalias 'ar-doubleslash-curvedsinglequoted-atpt 'ar-curvedsinglequoted-doubleslash-atpt)
(defun ar-curvedsinglequoted-doubleslash-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'curvedsinglequoted arg))

(defalias 'ar-backslash-curvedsinglequoted-atpt 'ar-curvedsinglequoted-backslash-atpt)
(defun ar-curvedsinglequoted-backslash-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'curvedsinglequoted arg))

(defalias 'ar-backtick-curvedsinglequoted-atpt 'ar-curvedsinglequoted-backtick-atpt)
(defun ar-curvedsinglequoted-backtick-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'curvedsinglequoted arg))

(defalias 'ar-dollar-curvedsinglequoted-atpt 'ar-curvedsinglequoted-dollar-atpt)
(defun ar-curvedsinglequoted-dollar-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'curvedsinglequoted arg))

(defalias 'ar-doublebacktick-curvedsinglequoted-atpt 'ar-curvedsinglequoted-doublebacktick-atpt)
(defun ar-curvedsinglequoted-doublebacktick-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'curvedsinglequoted arg))

(defalias 'ar-doublequote-curvedsinglequoted-atpt 'ar-curvedsinglequoted-doublequote-atpt)
(defun ar-curvedsinglequoted-doublequote-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'curvedsinglequoted arg))

(defalias 'ar-equalize-curvedsinglequoted-atpt 'ar-curvedsinglequoted-equalize-atpt)
(defun ar-curvedsinglequoted-equalize-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'curvedsinglequoted arg))

(defalias 'ar-escape-curvedsinglequoted-atpt 'ar-curvedsinglequoted-escape-atpt)
(defun ar-curvedsinglequoted-escape-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'curvedsinglequoted arg))

(defalias 'ar-hash-curvedsinglequoted-atpt 'ar-curvedsinglequoted-hash-atpt)
(defun ar-curvedsinglequoted-hash-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'curvedsinglequoted arg))

(defalias 'ar-hyphen-curvedsinglequoted-atpt 'ar-curvedsinglequoted-hyphen-atpt)
(defun ar-curvedsinglequoted-hyphen-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'curvedsinglequoted arg))

(defalias 'ar-pipe-curvedsinglequoted-atpt 'ar-curvedsinglequoted-pipe-atpt)
(defun ar-curvedsinglequoted-pipe-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'curvedsinglequoted arg))

(defalias 'ar-singlequote-curvedsinglequoted-atpt 'ar-curvedsinglequoted-singlequote-atpt)
(defun ar-curvedsinglequoted-singlequote-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'curvedsinglequoted arg))

(defalias 'ar-slash-curvedsinglequoted-atpt 'ar-curvedsinglequoted-slash-atpt)
(defun ar-curvedsinglequoted-slash-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'curvedsinglequoted arg))

(defalias 'ar-star-curvedsinglequoted-atpt 'ar-curvedsinglequoted-star-atpt)
(defun ar-curvedsinglequoted-star-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'curvedsinglequoted arg))

(defalias 'ar-tild-curvedsinglequoted-atpt 'ar-curvedsinglequoted-tild-atpt)
(defun ar-curvedsinglequoted-tild-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'curvedsinglequoted arg))

(defalias 'ar-triplebacktick-curvedsinglequoted-atpt 'ar-curvedsinglequoted-triplebacktick-atpt)
(defun ar-curvedsinglequoted-triplebacktick-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'curvedsinglequoted arg))

(defalias 'ar-underscore-curvedsinglequoted-atpt 'ar-curvedsinglequoted-underscore-atpt)
(defun ar-curvedsinglequoted-underscore-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'curvedsinglequoted arg))

(defalias 'ar-whitespace-curvedsinglequoted-atpt 'ar-curvedsinglequoted-whitespace-atpt)
(defun ar-curvedsinglequoted-whitespace-atpt (&optional arg)
  "Returns CURVEDSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'curvedsinglequoted arg))

(defalias 'ar-colon-curveddoublequoted-atpt 'ar-curveddoublequoted-colon-atpt)
(defun ar-curveddoublequoted-colon-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'curveddoublequoted arg))

(defalias 'ar-cross-curveddoublequoted-atpt 'ar-curveddoublequoted-cross-atpt)
(defun ar-curveddoublequoted-cross-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'curveddoublequoted arg))

(defalias 'ar-doubleslash-curveddoublequoted-atpt 'ar-curveddoublequoted-doubleslash-atpt)
(defun ar-curveddoublequoted-doubleslash-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'curveddoublequoted arg))

(defalias 'ar-backslash-curveddoublequoted-atpt 'ar-curveddoublequoted-backslash-atpt)
(defun ar-curveddoublequoted-backslash-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'curveddoublequoted arg))

(defalias 'ar-backtick-curveddoublequoted-atpt 'ar-curveddoublequoted-backtick-atpt)
(defun ar-curveddoublequoted-backtick-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'curveddoublequoted arg))

(defalias 'ar-dollar-curveddoublequoted-atpt 'ar-curveddoublequoted-dollar-atpt)
(defun ar-curveddoublequoted-dollar-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'curveddoublequoted arg))

(defalias 'ar-doublebacktick-curveddoublequoted-atpt 'ar-curveddoublequoted-doublebacktick-atpt)
(defun ar-curveddoublequoted-doublebacktick-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'curveddoublequoted arg))

(defalias 'ar-doublequote-curveddoublequoted-atpt 'ar-curveddoublequoted-doublequote-atpt)
(defun ar-curveddoublequoted-doublequote-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'curveddoublequoted arg))

(defalias 'ar-equalize-curveddoublequoted-atpt 'ar-curveddoublequoted-equalize-atpt)
(defun ar-curveddoublequoted-equalize-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'curveddoublequoted arg))

(defalias 'ar-escape-curveddoublequoted-atpt 'ar-curveddoublequoted-escape-atpt)
(defun ar-curveddoublequoted-escape-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'curveddoublequoted arg))

(defalias 'ar-hash-curveddoublequoted-atpt 'ar-curveddoublequoted-hash-atpt)
(defun ar-curveddoublequoted-hash-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'curveddoublequoted arg))

(defalias 'ar-hyphen-curveddoublequoted-atpt 'ar-curveddoublequoted-hyphen-atpt)
(defun ar-curveddoublequoted-hyphen-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'curveddoublequoted arg))

(defalias 'ar-pipe-curveddoublequoted-atpt 'ar-curveddoublequoted-pipe-atpt)
(defun ar-curveddoublequoted-pipe-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'curveddoublequoted arg))

(defalias 'ar-singlequote-curveddoublequoted-atpt 'ar-curveddoublequoted-singlequote-atpt)
(defun ar-curveddoublequoted-singlequote-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'curveddoublequoted arg))

(defalias 'ar-slash-curveddoublequoted-atpt 'ar-curveddoublequoted-slash-atpt)
(defun ar-curveddoublequoted-slash-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'curveddoublequoted arg))

(defalias 'ar-star-curveddoublequoted-atpt 'ar-curveddoublequoted-star-atpt)
(defun ar-curveddoublequoted-star-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'curveddoublequoted arg))

(defalias 'ar-tild-curveddoublequoted-atpt 'ar-curveddoublequoted-tild-atpt)
(defun ar-curveddoublequoted-tild-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'curveddoublequoted arg))

(defalias 'ar-triplebacktick-curveddoublequoted-atpt 'ar-curveddoublequoted-triplebacktick-atpt)
(defun ar-curveddoublequoted-triplebacktick-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'curveddoublequoted arg))

(defalias 'ar-underscore-curveddoublequoted-atpt 'ar-curveddoublequoted-underscore-atpt)
(defun ar-curveddoublequoted-underscore-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'curveddoublequoted arg))

(defalias 'ar-whitespace-curveddoublequoted-atpt 'ar-curveddoublequoted-whitespace-atpt)
(defun ar-curveddoublequoted-whitespace-atpt (&optional arg)
  "Returns CURVEDDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'curveddoublequoted arg))

(defalias 'ar-colon-parentized-atpt 'ar-parentized-colon-atpt)
(defun ar-parentized-colon-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-colon 'parentized arg))

(defalias 'ar-cross-parentized-atpt 'ar-parentized-cross-atpt)
(defun ar-parentized-cross-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-cross 'parentized arg))

(defalias 'ar-doubleslash-parentized-atpt 'ar-parentized-doubleslash-atpt)
(defun ar-parentized-doubleslash-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doubleslash 'parentized arg))

(defalias 'ar-backslash-parentized-atpt 'ar-parentized-backslash-atpt)
(defun ar-parentized-backslash-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backslash 'parentized arg))

(defalias 'ar-backtick-parentized-atpt 'ar-parentized-backtick-atpt)
(defun ar-parentized-backtick-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-backtick 'parentized arg))

(defalias 'ar-dollar-parentized-atpt 'ar-parentized-dollar-atpt)
(defun ar-parentized-dollar-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-dollar 'parentized arg))

(defalias 'ar-doublebacktick-parentized-atpt 'ar-parentized-doublebacktick-atpt)
(defun ar-parentized-doublebacktick-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublebacktick 'parentized arg))

(defalias 'ar-doublequote-parentized-atpt 'ar-parentized-doublequote-atpt)
(defun ar-parentized-doublequote-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-doublequote 'parentized arg))

(defalias 'ar-equalize-parentized-atpt 'ar-parentized-equalize-atpt)
(defun ar-parentized-equalize-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-equalize 'parentized arg))

(defalias 'ar-escape-parentized-atpt 'ar-parentized-escape-atpt)
(defun ar-parentized-escape-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-escape 'parentized arg))

(defalias 'ar-hash-parentized-atpt 'ar-parentized-hash-atpt)
(defun ar-parentized-hash-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hash 'parentized arg))

(defalias 'ar-hyphen-parentized-atpt 'ar-parentized-hyphen-atpt)
(defun ar-parentized-hyphen-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-hyphen 'parentized arg))

(defalias 'ar-pipe-parentized-atpt 'ar-parentized-pipe-atpt)
(defun ar-parentized-pipe-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-pipe 'parentized arg))

(defalias 'ar-singlequote-parentized-atpt 'ar-parentized-singlequote-atpt)
(defun ar-parentized-singlequote-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-singlequote 'parentized arg))

(defalias 'ar-slash-parentized-atpt 'ar-parentized-slash-atpt)
(defun ar-parentized-slash-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-slash 'parentized arg))

(defalias 'ar-star-parentized-atpt 'ar-parentized-star-atpt)
(defun ar-parentized-star-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-star 'parentized arg))

(defalias 'ar-tild-parentized-atpt 'ar-parentized-tild-atpt)
(defun ar-parentized-tild-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-tild 'parentized arg))

(defalias 'ar-triplebacktick-parentized-atpt 'ar-parentized-triplebacktick-atpt)
(defun ar-parentized-triplebacktick-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-triplebacktick 'parentized arg))

(defalias 'ar-underscore-parentized-atpt 'ar-parentized-underscore-atpt)
(defun ar-parentized-underscore-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-underscore 'parentized arg))

(defalias 'ar-whitespace-parentized-atpt 'ar-parentized-whitespace-atpt)
(defun ar-parentized-whitespace-atpt (&optional arg)
  "Returns PARENTIZED at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-whitespace 'parentized arg));; ar-thing-at-point-utils-unpaired-paired: end

;; ar-thing-at-point-utils-delim-classes-paired ar-atpt-classes ar-paired-delimit-aktiv: start


(defalias 'ar-brace-alnum-atpt 'ar-alnum-brace-atpt)
(defun ar-alnum-brace-atpt (&optional arg)
  "Brace ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-brace 'alnum arg))

(defalias 'ar-bracket-alnum-atpt 'ar-alnum-bracket-atpt)
(defun ar-alnum-bracket-atpt (&optional arg)
  "Bracket ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-bracket 'alnum arg))

(defalias 'ar-lesserangle-alnum-atpt 'ar-alnum-lesserangle-atpt)
(defun ar-alnum-lesserangle-atpt (&optional arg)
  "Lesserangle ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-lesserangle 'alnum arg))

(defalias 'ar-greaterangle-alnum-atpt 'ar-alnum-greaterangle-atpt)
(defun ar-alnum-greaterangle-atpt (&optional arg)
  "Greaterangle ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-greaterangle 'alnum arg))

(defalias 'ar-curvedsinglequote-alnum-atpt 'ar-alnum-curvedsinglequote-atpt)
(defun ar-alnum-curvedsinglequote-atpt (&optional arg)
  "Curvedsinglequote ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curvedsinglequote 'alnum arg))

(defalias 'ar-curveddoublequote-alnum-atpt 'ar-alnum-curveddoublequote-atpt)
(defun ar-alnum-curveddoublequote-atpt (&optional arg)
  "Curveddoublequote ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curveddoublequote 'alnum arg))

(defalias 'ar-parentize-alnum-atpt 'ar-alnum-parentize-atpt)
(defun ar-alnum-parentize-atpt (&optional arg)
  "Parentize ALNUM at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-parentize 'alnum arg))

(defalias 'ar-brace-alpha-atpt 'ar-alpha-brace-atpt)
(defun ar-alpha-brace-atpt (&optional arg)
  "Brace ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-brace 'alpha arg))

(defalias 'ar-bracket-alpha-atpt 'ar-alpha-bracket-atpt)
(defun ar-alpha-bracket-atpt (&optional arg)
  "Bracket ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-bracket 'alpha arg))

(defalias 'ar-lesserangle-alpha-atpt 'ar-alpha-lesserangle-atpt)
(defun ar-alpha-lesserangle-atpt (&optional arg)
  "Lesserangle ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-lesserangle 'alpha arg))

(defalias 'ar-greaterangle-alpha-atpt 'ar-alpha-greaterangle-atpt)
(defun ar-alpha-greaterangle-atpt (&optional arg)
  "Greaterangle ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-greaterangle 'alpha arg))

(defalias 'ar-curvedsinglequote-alpha-atpt 'ar-alpha-curvedsinglequote-atpt)
(defun ar-alpha-curvedsinglequote-atpt (&optional arg)
  "Curvedsinglequote ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curvedsinglequote 'alpha arg))

(defalias 'ar-curveddoublequote-alpha-atpt 'ar-alpha-curveddoublequote-atpt)
(defun ar-alpha-curveddoublequote-atpt (&optional arg)
  "Curveddoublequote ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curveddoublequote 'alpha arg))

(defalias 'ar-parentize-alpha-atpt 'ar-alpha-parentize-atpt)
(defun ar-alpha-parentize-atpt (&optional arg)
  "Parentize ALPHA at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-parentize 'alpha arg))

(defalias 'ar-brace-ascii-atpt 'ar-ascii-brace-atpt)
(defun ar-ascii-brace-atpt (&optional arg)
  "Brace ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-brace 'ascii arg))

(defalias 'ar-bracket-ascii-atpt 'ar-ascii-bracket-atpt)
(defun ar-ascii-bracket-atpt (&optional arg)
  "Bracket ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-bracket 'ascii arg))

(defalias 'ar-lesserangle-ascii-atpt 'ar-ascii-lesserangle-atpt)
(defun ar-ascii-lesserangle-atpt (&optional arg)
  "Lesserangle ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-lesserangle 'ascii arg))

(defalias 'ar-greaterangle-ascii-atpt 'ar-ascii-greaterangle-atpt)
(defun ar-ascii-greaterangle-atpt (&optional arg)
  "Greaterangle ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-greaterangle 'ascii arg))

(defalias 'ar-curvedsinglequote-ascii-atpt 'ar-ascii-curvedsinglequote-atpt)
(defun ar-ascii-curvedsinglequote-atpt (&optional arg)
  "Curvedsinglequote ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curvedsinglequote 'ascii arg))

(defalias 'ar-curveddoublequote-ascii-atpt 'ar-ascii-curveddoublequote-atpt)
(defun ar-ascii-curveddoublequote-atpt (&optional arg)
  "Curveddoublequote ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curveddoublequote 'ascii arg))

(defalias 'ar-parentize-ascii-atpt 'ar-ascii-parentize-atpt)
(defun ar-ascii-parentize-atpt (&optional arg)
  "Parentize ASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-parentize 'ascii arg))

(defalias 'ar-brace-blank-atpt 'ar-blank-brace-atpt)
(defun ar-blank-brace-atpt (&optional arg)
  "Brace BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-brace 'blank arg))

(defalias 'ar-bracket-blank-atpt 'ar-blank-bracket-atpt)
(defun ar-blank-bracket-atpt (&optional arg)
  "Bracket BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-bracket 'blank arg))

(defalias 'ar-lesserangle-blank-atpt 'ar-blank-lesserangle-atpt)
(defun ar-blank-lesserangle-atpt (&optional arg)
  "Lesserangle BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-lesserangle 'blank arg))

(defalias 'ar-greaterangle-blank-atpt 'ar-blank-greaterangle-atpt)
(defun ar-blank-greaterangle-atpt (&optional arg)
  "Greaterangle BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-greaterangle 'blank arg))

(defalias 'ar-curvedsinglequote-blank-atpt 'ar-blank-curvedsinglequote-atpt)
(defun ar-blank-curvedsinglequote-atpt (&optional arg)
  "Curvedsinglequote BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curvedsinglequote 'blank arg))

(defalias 'ar-curveddoublequote-blank-atpt 'ar-blank-curveddoublequote-atpt)
(defun ar-blank-curveddoublequote-atpt (&optional arg)
  "Curveddoublequote BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curveddoublequote 'blank arg))

(defalias 'ar-parentize-blank-atpt 'ar-blank-parentize-atpt)
(defun ar-blank-parentize-atpt (&optional arg)
  "Parentize BLANK at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-parentize 'blank arg))

(defalias 'ar-brace-cntrl-atpt 'ar-cntrl-brace-atpt)
(defun ar-cntrl-brace-atpt (&optional arg)
  "Brace CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-brace 'cntrl arg))

(defalias 'ar-bracket-cntrl-atpt 'ar-cntrl-bracket-atpt)
(defun ar-cntrl-bracket-atpt (&optional arg)
  "Bracket CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-bracket 'cntrl arg))

(defalias 'ar-lesserangle-cntrl-atpt 'ar-cntrl-lesserangle-atpt)
(defun ar-cntrl-lesserangle-atpt (&optional arg)
  "Lesserangle CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-lesserangle 'cntrl arg))

(defalias 'ar-greaterangle-cntrl-atpt 'ar-cntrl-greaterangle-atpt)
(defun ar-cntrl-greaterangle-atpt (&optional arg)
  "Greaterangle CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-greaterangle 'cntrl arg))

(defalias 'ar-curvedsinglequote-cntrl-atpt 'ar-cntrl-curvedsinglequote-atpt)
(defun ar-cntrl-curvedsinglequote-atpt (&optional arg)
  "Curvedsinglequote CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curvedsinglequote 'cntrl arg))

(defalias 'ar-curveddoublequote-cntrl-atpt 'ar-cntrl-curveddoublequote-atpt)
(defun ar-cntrl-curveddoublequote-atpt (&optional arg)
  "Curveddoublequote CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curveddoublequote 'cntrl arg))

(defalias 'ar-parentize-cntrl-atpt 'ar-cntrl-parentize-atpt)
(defun ar-cntrl-parentize-atpt (&optional arg)
  "Parentize CNTRL at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-parentize 'cntrl arg))

(defalias 'ar-brace-digit-atpt 'ar-digit-brace-atpt)
(defun ar-digit-brace-atpt (&optional arg)
  "Brace DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-brace 'digit arg))

(defalias 'ar-bracket-digit-atpt 'ar-digit-bracket-atpt)
(defun ar-digit-bracket-atpt (&optional arg)
  "Bracket DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-bracket 'digit arg))

(defalias 'ar-lesserangle-digit-atpt 'ar-digit-lesserangle-atpt)
(defun ar-digit-lesserangle-atpt (&optional arg)
  "Lesserangle DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-lesserangle 'digit arg))

(defalias 'ar-greaterangle-digit-atpt 'ar-digit-greaterangle-atpt)
(defun ar-digit-greaterangle-atpt (&optional arg)
  "Greaterangle DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-greaterangle 'digit arg))

(defalias 'ar-curvedsinglequote-digit-atpt 'ar-digit-curvedsinglequote-atpt)
(defun ar-digit-curvedsinglequote-atpt (&optional arg)
  "Curvedsinglequote DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curvedsinglequote 'digit arg))

(defalias 'ar-curveddoublequote-digit-atpt 'ar-digit-curveddoublequote-atpt)
(defun ar-digit-curveddoublequote-atpt (&optional arg)
  "Curveddoublequote DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curveddoublequote 'digit arg))

(defalias 'ar-parentize-digit-atpt 'ar-digit-parentize-atpt)
(defun ar-digit-parentize-atpt (&optional arg)
  "Parentize DIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-parentize 'digit arg))

(defalias 'ar-brace-graph-atpt 'ar-graph-brace-atpt)
(defun ar-graph-brace-atpt (&optional arg)
  "Brace GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-brace 'graph arg))

(defalias 'ar-bracket-graph-atpt 'ar-graph-bracket-atpt)
(defun ar-graph-bracket-atpt (&optional arg)
  "Bracket GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-bracket 'graph arg))

(defalias 'ar-lesserangle-graph-atpt 'ar-graph-lesserangle-atpt)
(defun ar-graph-lesserangle-atpt (&optional arg)
  "Lesserangle GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-lesserangle 'graph arg))

(defalias 'ar-greaterangle-graph-atpt 'ar-graph-greaterangle-atpt)
(defun ar-graph-greaterangle-atpt (&optional arg)
  "Greaterangle GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-greaterangle 'graph arg))

(defalias 'ar-curvedsinglequote-graph-atpt 'ar-graph-curvedsinglequote-atpt)
(defun ar-graph-curvedsinglequote-atpt (&optional arg)
  "Curvedsinglequote GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curvedsinglequote 'graph arg))

(defalias 'ar-curveddoublequote-graph-atpt 'ar-graph-curveddoublequote-atpt)
(defun ar-graph-curveddoublequote-atpt (&optional arg)
  "Curveddoublequote GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curveddoublequote 'graph arg))

(defalias 'ar-parentize-graph-atpt 'ar-graph-parentize-atpt)
(defun ar-graph-parentize-atpt (&optional arg)
  "Parentize GRAPH at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-parentize 'graph arg))

(defalias 'ar-brace-lower-atpt 'ar-lower-brace-atpt)
(defun ar-lower-brace-atpt (&optional arg)
  "Brace LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-brace 'lower arg))

(defalias 'ar-bracket-lower-atpt 'ar-lower-bracket-atpt)
(defun ar-lower-bracket-atpt (&optional arg)
  "Bracket LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-bracket 'lower arg))

(defalias 'ar-lesserangle-lower-atpt 'ar-lower-lesserangle-atpt)
(defun ar-lower-lesserangle-atpt (&optional arg)
  "Lesserangle LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-lesserangle 'lower arg))

(defalias 'ar-greaterangle-lower-atpt 'ar-lower-greaterangle-atpt)
(defun ar-lower-greaterangle-atpt (&optional arg)
  "Greaterangle LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-greaterangle 'lower arg))

(defalias 'ar-curvedsinglequote-lower-atpt 'ar-lower-curvedsinglequote-atpt)
(defun ar-lower-curvedsinglequote-atpt (&optional arg)
  "Curvedsinglequote LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curvedsinglequote 'lower arg))

(defalias 'ar-curveddoublequote-lower-atpt 'ar-lower-curveddoublequote-atpt)
(defun ar-lower-curveddoublequote-atpt (&optional arg)
  "Curveddoublequote LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curveddoublequote 'lower arg))

(defalias 'ar-parentize-lower-atpt 'ar-lower-parentize-atpt)
(defun ar-lower-parentize-atpt (&optional arg)
  "Parentize LOWER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-parentize 'lower arg))

(defalias 'ar-brace-nonascii-atpt 'ar-nonascii-brace-atpt)
(defun ar-nonascii-brace-atpt (&optional arg)
  "Brace NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-brace 'nonascii arg))

(defalias 'ar-bracket-nonascii-atpt 'ar-nonascii-bracket-atpt)
(defun ar-nonascii-bracket-atpt (&optional arg)
  "Bracket NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-bracket 'nonascii arg))

(defalias 'ar-lesserangle-nonascii-atpt 'ar-nonascii-lesserangle-atpt)
(defun ar-nonascii-lesserangle-atpt (&optional arg)
  "Lesserangle NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-lesserangle 'nonascii arg))

(defalias 'ar-greaterangle-nonascii-atpt 'ar-nonascii-greaterangle-atpt)
(defun ar-nonascii-greaterangle-atpt (&optional arg)
  "Greaterangle NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-greaterangle 'nonascii arg))

(defalias 'ar-curvedsinglequote-nonascii-atpt 'ar-nonascii-curvedsinglequote-atpt)
(defun ar-nonascii-curvedsinglequote-atpt (&optional arg)
  "Curvedsinglequote NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curvedsinglequote 'nonascii arg))

(defalias 'ar-curveddoublequote-nonascii-atpt 'ar-nonascii-curveddoublequote-atpt)
(defun ar-nonascii-curveddoublequote-atpt (&optional arg)
  "Curveddoublequote NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curveddoublequote 'nonascii arg))

(defalias 'ar-parentize-nonascii-atpt 'ar-nonascii-parentize-atpt)
(defun ar-nonascii-parentize-atpt (&optional arg)
  "Parentize NONASCII at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-parentize 'nonascii arg))

(defalias 'ar-brace-print-atpt 'ar-print-brace-atpt)
(defun ar-print-brace-atpt (&optional arg)
  "Brace PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-brace 'print arg))

(defalias 'ar-bracket-print-atpt 'ar-print-bracket-atpt)
(defun ar-print-bracket-atpt (&optional arg)
  "Bracket PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-bracket 'print arg))

(defalias 'ar-lesserangle-print-atpt 'ar-print-lesserangle-atpt)
(defun ar-print-lesserangle-atpt (&optional arg)
  "Lesserangle PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-lesserangle 'print arg))

(defalias 'ar-greaterangle-print-atpt 'ar-print-greaterangle-atpt)
(defun ar-print-greaterangle-atpt (&optional arg)
  "Greaterangle PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-greaterangle 'print arg))

(defalias 'ar-curvedsinglequote-print-atpt 'ar-print-curvedsinglequote-atpt)
(defun ar-print-curvedsinglequote-atpt (&optional arg)
  "Curvedsinglequote PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curvedsinglequote 'print arg))

(defalias 'ar-curveddoublequote-print-atpt 'ar-print-curveddoublequote-atpt)
(defun ar-print-curveddoublequote-atpt (&optional arg)
  "Curveddoublequote PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curveddoublequote 'print arg))

(defalias 'ar-parentize-print-atpt 'ar-print-parentize-atpt)
(defun ar-print-parentize-atpt (&optional arg)
  "Parentize PRINT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-parentize 'print arg))

(defalias 'ar-brace-punct-atpt 'ar-punct-brace-atpt)
(defun ar-punct-brace-atpt (&optional arg)
  "Brace PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-brace 'punct arg))

(defalias 'ar-bracket-punct-atpt 'ar-punct-bracket-atpt)
(defun ar-punct-bracket-atpt (&optional arg)
  "Bracket PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-bracket 'punct arg))

(defalias 'ar-lesserangle-punct-atpt 'ar-punct-lesserangle-atpt)
(defun ar-punct-lesserangle-atpt (&optional arg)
  "Lesserangle PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-lesserangle 'punct arg))

(defalias 'ar-greaterangle-punct-atpt 'ar-punct-greaterangle-atpt)
(defun ar-punct-greaterangle-atpt (&optional arg)
  "Greaterangle PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-greaterangle 'punct arg))

(defalias 'ar-curvedsinglequote-punct-atpt 'ar-punct-curvedsinglequote-atpt)
(defun ar-punct-curvedsinglequote-atpt (&optional arg)
  "Curvedsinglequote PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curvedsinglequote 'punct arg))

(defalias 'ar-curveddoublequote-punct-atpt 'ar-punct-curveddoublequote-atpt)
(defun ar-punct-curveddoublequote-atpt (&optional arg)
  "Curveddoublequote PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curveddoublequote 'punct arg))

(defalias 'ar-parentize-punct-atpt 'ar-punct-parentize-atpt)
(defun ar-punct-parentize-atpt (&optional arg)
  "Parentize PUNCT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-parentize 'punct arg))

(defalias 'ar-brace-space-atpt 'ar-space-brace-atpt)
(defun ar-space-brace-atpt (&optional arg)
  "Brace SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-brace 'space arg))

(defalias 'ar-bracket-space-atpt 'ar-space-bracket-atpt)
(defun ar-space-bracket-atpt (&optional arg)
  "Bracket SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-bracket 'space arg))

(defalias 'ar-lesserangle-space-atpt 'ar-space-lesserangle-atpt)
(defun ar-space-lesserangle-atpt (&optional arg)
  "Lesserangle SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-lesserangle 'space arg))

(defalias 'ar-greaterangle-space-atpt 'ar-space-greaterangle-atpt)
(defun ar-space-greaterangle-atpt (&optional arg)
  "Greaterangle SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-greaterangle 'space arg))

(defalias 'ar-curvedsinglequote-space-atpt 'ar-space-curvedsinglequote-atpt)
(defun ar-space-curvedsinglequote-atpt (&optional arg)
  "Curvedsinglequote SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curvedsinglequote 'space arg))

(defalias 'ar-curveddoublequote-space-atpt 'ar-space-curveddoublequote-atpt)
(defun ar-space-curveddoublequote-atpt (&optional arg)
  "Curveddoublequote SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curveddoublequote 'space arg))

(defalias 'ar-parentize-space-atpt 'ar-space-parentize-atpt)
(defun ar-space-parentize-atpt (&optional arg)
  "Parentize SPACE at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-parentize 'space arg))

(defalias 'ar-brace-upper-atpt 'ar-upper-brace-atpt)
(defun ar-upper-brace-atpt (&optional arg)
  "Brace UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-brace 'upper arg))

(defalias 'ar-bracket-upper-atpt 'ar-upper-bracket-atpt)
(defun ar-upper-bracket-atpt (&optional arg)
  "Bracket UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-bracket 'upper arg))

(defalias 'ar-lesserangle-upper-atpt 'ar-upper-lesserangle-atpt)
(defun ar-upper-lesserangle-atpt (&optional arg)
  "Lesserangle UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-lesserangle 'upper arg))

(defalias 'ar-greaterangle-upper-atpt 'ar-upper-greaterangle-atpt)
(defun ar-upper-greaterangle-atpt (&optional arg)
  "Greaterangle UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-greaterangle 'upper arg))

(defalias 'ar-curvedsinglequote-upper-atpt 'ar-upper-curvedsinglequote-atpt)
(defun ar-upper-curvedsinglequote-atpt (&optional arg)
  "Curvedsinglequote UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curvedsinglequote 'upper arg))

(defalias 'ar-curveddoublequote-upper-atpt 'ar-upper-curveddoublequote-atpt)
(defun ar-upper-curveddoublequote-atpt (&optional arg)
  "Curveddoublequote UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curveddoublequote 'upper arg))

(defalias 'ar-parentize-upper-atpt 'ar-upper-parentize-atpt)
(defun ar-upper-parentize-atpt (&optional arg)
  "Parentize UPPER at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-parentize 'upper arg))

(defalias 'ar-brace-xdigit-atpt 'ar-xdigit-brace-atpt)
(defun ar-xdigit-brace-atpt (&optional arg)
  "Brace XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-brace 'xdigit arg))

(defalias 'ar-bracket-xdigit-atpt 'ar-xdigit-bracket-atpt)
(defun ar-xdigit-bracket-atpt (&optional arg)
  "Bracket XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-bracket 'xdigit arg))

(defalias 'ar-lesserangle-xdigit-atpt 'ar-xdigit-lesserangle-atpt)
(defun ar-xdigit-lesserangle-atpt (&optional arg)
  "Lesserangle XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-lesserangle 'xdigit arg))

(defalias 'ar-greaterangle-xdigit-atpt 'ar-xdigit-greaterangle-atpt)
(defun ar-xdigit-greaterangle-atpt (&optional arg)
  "Greaterangle XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-greaterangle 'xdigit arg))

(defalias 'ar-curvedsinglequote-xdigit-atpt 'ar-xdigit-curvedsinglequote-atpt)
(defun ar-xdigit-curvedsinglequote-atpt (&optional arg)
  "Curvedsinglequote XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curvedsinglequote 'xdigit arg))

(defalias 'ar-curveddoublequote-xdigit-atpt 'ar-xdigit-curveddoublequote-atpt)
(defun ar-xdigit-curveddoublequote-atpt (&optional arg)
  "Curveddoublequote XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-curveddoublequote 'xdigit arg))

(defalias 'ar-parentize-xdigit-atpt 'ar-xdigit-parentize-atpt)
(defun ar-xdigit-parentize-atpt (&optional arg)
  "Parentize XDIGIT at point if any, nil otherwise "
  (interactive "*P")
  (ar-th-parentize 'xdigit arg));; ar-thing-at-point-utils-delim-classes-paired ar-atpt-classes ar-paired-delimit-aktiv: end

;; ar-thing-at-point-utils-nodelim-einzeln: start

(defun ar-blok-alnum-atpt (&optional arg)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around alnum.
  Returns blok or nil if no ALNUM at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'alnum arg))

(defun ar-comment-alnum-atpt (&optional arg)
  "Comments ALNUM at point if any. "
  (interactive "*P")
  (ar-th-comment 'alnum arg))

(defun ar-commatize-alnum-atpt (&optional arg)
  "Put a comma after ALNUM at point if any. "
  (interactive "*P")
  (ar-th-commatize 'alnum arg))

(defun ar-mark-alnum-atpt (&optional arg)
  "Marks ALNUM at point if any. "
  (interactive "P")
  (ar-th-mark 'alnum))

(defun ar-hide-alnum-atpt (&optional arg)
  "Hides ALNUM at point. "
  (interactive "P")
  (ar-th-hide 'alnum))

(defun ar-show-alnum-atpt (&optional arg)
  "Shows hidden ALNUM at point. "
  (interactive "P")
  (ar-th-show 'alnum))

(defun ar-hide-show-alnum-atpt (&optional arg)
  "Alternatively hides or shows ALNUM at point. "
  (interactive "P")
  (ar-th-hide-show 'alnum))

(defun ar-highlight-alnum-atpt-mode (&optional arg)
  "Toggles alnum-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'alnum arg))

(defun ar-kill-alnum-atpt (&optional arg)
  "Kills ALNUM at point if any. "
  (interactive "*P")
  (ar-th-kill 'alnum arg))

(defun ar-separate-alnum-atpt (&optional arg)
  "Separates ALNUM at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'alnum arg))

(defun ar-triplequotedq-alnum-atpt (&optional arg)
  "Put triplequotes composed of doublequotes around alnum. "
  (interactive "*P")
  (ar-th-triplequotedq 'alnum arg))

(defun ar-triplequotesq-alnum-atpt (&optional arg)
  "Put triplequotes composed of singlequotes around alnum. "
  (interactive "*P")
  (ar-th-triplequotesq 'alnum arg))

(defun ar-forward-alnum-atpt (&optional arg)
  "Moves forward over ALNUM at point if any, does nothing otherwise.
Returns end position of ALNUM "
  (interactive "P")
  (ar-th-forward 'alnum arg))

(defun ar-backward-alnum-atpt (&optional arg)
  "Moves backward over ALNUM before point if any, does nothing otherwise.
Returns beginning position of ALNUM "
  (interactive "P")
  (ar-th-backward 'alnum arg))

(defun ar-transpose-alnum-atpt (&optional arg)
  "Transposes ALNUM with ALNUM before point if any. "
  (interactive "*P")
  (ar-th-transpose 'alnum arg))

(defun ar-sort-alnum-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts alnums in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'alnum reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-alnum-atpt (&optional arg) 
  "Return t if a ALNUM at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-alnum-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-alnum-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-blok-alpha-atpt (&optional arg)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around alpha.
  Returns blok or nil if no ALPHA at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'alpha arg))

(defun ar-comment-alpha-atpt (&optional arg)
  "Comments ALPHA at point if any. "
  (interactive "*P")
  (ar-th-comment 'alpha arg))

(defun ar-commatize-alpha-atpt (&optional arg)
  "Put a comma after ALPHA at point if any. "
  (interactive "*P")
  (ar-th-commatize 'alpha arg))

(defun ar-mark-alpha-atpt (&optional arg)
  "Marks ALPHA at point if any. "
  (interactive "P")
  (ar-th-mark 'alpha))

(defun ar-hide-alpha-atpt (&optional arg)
  "Hides ALPHA at point. "
  (interactive "P")
  (ar-th-hide 'alpha))

(defun ar-show-alpha-atpt (&optional arg)
  "Shows hidden ALPHA at point. "
  (interactive "P")
  (ar-th-show 'alpha))

(defun ar-hide-show-alpha-atpt (&optional arg)
  "Alternatively hides or shows ALPHA at point. "
  (interactive "P")
  (ar-th-hide-show 'alpha))

(defun ar-highlight-alpha-atpt-mode (&optional arg)
  "Toggles alpha-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'alpha arg))

(defun ar-kill-alpha-atpt (&optional arg)
  "Kills ALPHA at point if any. "
  (interactive "*P")
  (ar-th-kill 'alpha arg))

(defun ar-separate-alpha-atpt (&optional arg)
  "Separates ALPHA at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'alpha arg))

(defun ar-triplequotedq-alpha-atpt (&optional arg)
  "Put triplequotes composed of doublequotes around alpha. "
  (interactive "*P")
  (ar-th-triplequotedq 'alpha arg))

(defun ar-triplequotesq-alpha-atpt (&optional arg)
  "Put triplequotes composed of singlequotes around alpha. "
  (interactive "*P")
  (ar-th-triplequotesq 'alpha arg))

(defun ar-forward-alpha-atpt (&optional arg)
  "Moves forward over ALPHA at point if any, does nothing otherwise.
Returns end position of ALPHA "
  (interactive "P")
  (ar-th-forward 'alpha arg))

(defun ar-backward-alpha-atpt (&optional arg)
  "Moves backward over ALPHA before point if any, does nothing otherwise.
Returns beginning position of ALPHA "
  (interactive "P")
  (ar-th-backward 'alpha arg))

(defun ar-transpose-alpha-atpt (&optional arg)
  "Transposes ALPHA with ALPHA before point if any. "
  (interactive "*P")
  (ar-th-transpose 'alpha arg))

(defun ar-sort-alpha-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts alphas in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'alpha reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-alpha-atpt (&optional arg) 
  "Return t if a ALPHA at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-alpha-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-alpha-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-blok-ascii-atpt (&optional arg)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around ascii.
  Returns blok or nil if no ASCII at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'ascii arg))

(defun ar-comment-ascii-atpt (&optional arg)
  "Comments ASCII at point if any. "
  (interactive "*P")
  (ar-th-comment 'ascii arg))

(defun ar-commatize-ascii-atpt (&optional arg)
  "Put a comma after ASCII at point if any. "
  (interactive "*P")
  (ar-th-commatize 'ascii arg))

(defun ar-mark-ascii-atpt (&optional arg)
  "Marks ASCII at point if any. "
  (interactive "P")
  (ar-th-mark 'ascii))

(defun ar-hide-ascii-atpt (&optional arg)
  "Hides ASCII at point. "
  (interactive "P")
  (ar-th-hide 'ascii))

(defun ar-show-ascii-atpt (&optional arg)
  "Shows hidden ASCII at point. "
  (interactive "P")
  (ar-th-show 'ascii))

(defun ar-hide-show-ascii-atpt (&optional arg)
  "Alternatively hides or shows ASCII at point. "
  (interactive "P")
  (ar-th-hide-show 'ascii))

(defun ar-highlight-ascii-atpt-mode (&optional arg)
  "Toggles ascii-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'ascii arg))

(defun ar-kill-ascii-atpt (&optional arg)
  "Kills ASCII at point if any. "
  (interactive "*P")
  (ar-th-kill 'ascii arg))

(defun ar-separate-ascii-atpt (&optional arg)
  "Separates ASCII at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'ascii arg))

(defun ar-triplequotedq-ascii-atpt (&optional arg)
  "Put triplequotes composed of doublequotes around ascii. "
  (interactive "*P")
  (ar-th-triplequotedq 'ascii arg))

(defun ar-triplequotesq-ascii-atpt (&optional arg)
  "Put triplequotes composed of singlequotes around ascii. "
  (interactive "*P")
  (ar-th-triplequotesq 'ascii arg))

(defun ar-forward-ascii-atpt (&optional arg)
  "Moves forward over ASCII at point if any, does nothing otherwise.
Returns end position of ASCII "
  (interactive "P")
  (ar-th-forward 'ascii arg))

(defun ar-backward-ascii-atpt (&optional arg)
  "Moves backward over ASCII before point if any, does nothing otherwise.
Returns beginning position of ASCII "
  (interactive "P")
  (ar-th-backward 'ascii arg))

(defun ar-transpose-ascii-atpt (&optional arg)
  "Transposes ASCII with ASCII before point if any. "
  (interactive "*P")
  (ar-th-transpose 'ascii arg))

(defun ar-sort-ascii-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts asciis in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'ascii reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-ascii-atpt (&optional arg) 
  "Return t if a ASCII at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-ascii-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-ascii-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-blok-blank-atpt (&optional arg)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around blank.
  Returns blok or nil if no BLANK at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'blank arg))

(defun ar-comment-blank-atpt (&optional arg)
  "Comments BLANK at point if any. "
  (interactive "*P")
  (ar-th-comment 'blank arg))

(defun ar-commatize-blank-atpt (&optional arg)
  "Put a comma after BLANK at point if any. "
  (interactive "*P")
  (ar-th-commatize 'blank arg))

(defun ar-mark-blank-atpt (&optional arg)
  "Marks BLANK at point if any. "
  (interactive "P")
  (ar-th-mark 'blank))

(defun ar-hide-blank-atpt (&optional arg)
  "Hides BLANK at point. "
  (interactive "P")
  (ar-th-hide 'blank))

(defun ar-show-blank-atpt (&optional arg)
  "Shows hidden BLANK at point. "
  (interactive "P")
  (ar-th-show 'blank))

(defun ar-hide-show-blank-atpt (&optional arg)
  "Alternatively hides or shows BLANK at point. "
  (interactive "P")
  (ar-th-hide-show 'blank))

(defun ar-highlight-blank-atpt-mode (&optional arg)
  "Toggles blank-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'blank arg))

(defun ar-kill-blank-atpt (&optional arg)
  "Kills BLANK at point if any. "
  (interactive "*P")
  (ar-th-kill 'blank arg))

(defun ar-separate-blank-atpt (&optional arg)
  "Separates BLANK at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'blank arg))

(defun ar-triplequotedq-blank-atpt (&optional arg)
  "Put triplequotes composed of doublequotes around blank. "
  (interactive "*P")
  (ar-th-triplequotedq 'blank arg))

(defun ar-triplequotesq-blank-atpt (&optional arg)
  "Put triplequotes composed of singlequotes around blank. "
  (interactive "*P")
  (ar-th-triplequotesq 'blank arg))

(defun ar-forward-blank-atpt (&optional arg)
  "Moves forward over BLANK at point if any, does nothing otherwise.
Returns end position of BLANK "
  (interactive "P")
  (ar-th-forward 'blank arg))

(defun ar-backward-blank-atpt (&optional arg)
  "Moves backward over BLANK before point if any, does nothing otherwise.
Returns beginning position of BLANK "
  (interactive "P")
  (ar-th-backward 'blank arg))

(defun ar-transpose-blank-atpt (&optional arg)
  "Transposes BLANK with BLANK before point if any. "
  (interactive "*P")
  (ar-th-transpose 'blank arg))

(defun ar-sort-blank-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts blanks in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'blank reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-blank-atpt (&optional arg) 
  "Return t if a BLANK at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-blank-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-blank-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-blok-cntrl-atpt (&optional arg)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around cntrl.
  Returns blok or nil if no CNTRL at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'cntrl arg))

(defun ar-comment-cntrl-atpt (&optional arg)
  "Comments CNTRL at point if any. "
  (interactive "*P")
  (ar-th-comment 'cntrl arg))

(defun ar-commatize-cntrl-atpt (&optional arg)
  "Put a comma after CNTRL at point if any. "
  (interactive "*P")
  (ar-th-commatize 'cntrl arg))

(defun ar-mark-cntrl-atpt (&optional arg)
  "Marks CNTRL at point if any. "
  (interactive "P")
  (ar-th-mark 'cntrl))

(defun ar-hide-cntrl-atpt (&optional arg)
  "Hides CNTRL at point. "
  (interactive "P")
  (ar-th-hide 'cntrl))

(defun ar-show-cntrl-atpt (&optional arg)
  "Shows hidden CNTRL at point. "
  (interactive "P")
  (ar-th-show 'cntrl))

(defun ar-hide-show-cntrl-atpt (&optional arg)
  "Alternatively hides or shows CNTRL at point. "
  (interactive "P")
  (ar-th-hide-show 'cntrl))

(defun ar-highlight-cntrl-atpt-mode (&optional arg)
  "Toggles cntrl-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'cntrl arg))

(defun ar-kill-cntrl-atpt (&optional arg)
  "Kills CNTRL at point if any. "
  (interactive "*P")
  (ar-th-kill 'cntrl arg))

(defun ar-separate-cntrl-atpt (&optional arg)
  "Separates CNTRL at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'cntrl arg))

(defun ar-triplequotedq-cntrl-atpt (&optional arg)
  "Put triplequotes composed of doublequotes around cntrl. "
  (interactive "*P")
  (ar-th-triplequotedq 'cntrl arg))

(defun ar-triplequotesq-cntrl-atpt (&optional arg)
  "Put triplequotes composed of singlequotes around cntrl. "
  (interactive "*P")
  (ar-th-triplequotesq 'cntrl arg))

(defun ar-forward-cntrl-atpt (&optional arg)
  "Moves forward over CNTRL at point if any, does nothing otherwise.
Returns end position of CNTRL "
  (interactive "P")
  (ar-th-forward 'cntrl arg))

(defun ar-backward-cntrl-atpt (&optional arg)
  "Moves backward over CNTRL before point if any, does nothing otherwise.
Returns beginning position of CNTRL "
  (interactive "P")
  (ar-th-backward 'cntrl arg))

(defun ar-transpose-cntrl-atpt (&optional arg)
  "Transposes CNTRL with CNTRL before point if any. "
  (interactive "*P")
  (ar-th-transpose 'cntrl arg))

(defun ar-sort-cntrl-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts cntrls in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'cntrl reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-cntrl-atpt (&optional arg) 
  "Return t if a CNTRL at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-cntrl-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-cntrl-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-blok-digit-atpt (&optional arg)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around digit.
  Returns blok or nil if no DIGIT at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'digit arg))

(defun ar-comment-digit-atpt (&optional arg)
  "Comments DIGIT at point if any. "
  (interactive "*P")
  (ar-th-comment 'digit arg))

(defun ar-commatize-digit-atpt (&optional arg)
  "Put a comma after DIGIT at point if any. "
  (interactive "*P")
  (ar-th-commatize 'digit arg))

(defun ar-mark-digit-atpt (&optional arg)
  "Marks DIGIT at point if any. "
  (interactive "P")
  (ar-th-mark 'digit))

(defun ar-hide-digit-atpt (&optional arg)
  "Hides DIGIT at point. "
  (interactive "P")
  (ar-th-hide 'digit))

(defun ar-show-digit-atpt (&optional arg)
  "Shows hidden DIGIT at point. "
  (interactive "P")
  (ar-th-show 'digit))

(defun ar-hide-show-digit-atpt (&optional arg)
  "Alternatively hides or shows DIGIT at point. "
  (interactive "P")
  (ar-th-hide-show 'digit))

(defun ar-highlight-digit-atpt-mode (&optional arg)
  "Toggles digit-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'digit arg))

(defun ar-kill-digit-atpt (&optional arg)
  "Kills DIGIT at point if any. "
  (interactive "*P")
  (ar-th-kill 'digit arg))

(defun ar-separate-digit-atpt (&optional arg)
  "Separates DIGIT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'digit arg))

(defun ar-triplequotedq-digit-atpt (&optional arg)
  "Put triplequotes composed of doublequotes around digit. "
  (interactive "*P")
  (ar-th-triplequotedq 'digit arg))

(defun ar-triplequotesq-digit-atpt (&optional arg)
  "Put triplequotes composed of singlequotes around digit. "
  (interactive "*P")
  (ar-th-triplequotesq 'digit arg))

(defun ar-forward-digit-atpt (&optional arg)
  "Moves forward over DIGIT at point if any, does nothing otherwise.
Returns end position of DIGIT "
  (interactive "P")
  (ar-th-forward 'digit arg))

(defun ar-backward-digit-atpt (&optional arg)
  "Moves backward over DIGIT before point if any, does nothing otherwise.
Returns beginning position of DIGIT "
  (interactive "P")
  (ar-th-backward 'digit arg))

(defun ar-transpose-digit-atpt (&optional arg)
  "Transposes DIGIT with DIGIT before point if any. "
  (interactive "*P")
  (ar-th-transpose 'digit arg))

(defun ar-sort-digit-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts digits in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'digit reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-digit-atpt (&optional arg) 
  "Return t if a DIGIT at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-digit-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-digit-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-blok-graph-atpt (&optional arg)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around graph.
  Returns blok or nil if no GRAPH at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'graph arg))

(defun ar-comment-graph-atpt (&optional arg)
  "Comments GRAPH at point if any. "
  (interactive "*P")
  (ar-th-comment 'graph arg))

(defun ar-commatize-graph-atpt (&optional arg)
  "Put a comma after GRAPH at point if any. "
  (interactive "*P")
  (ar-th-commatize 'graph arg))

(defun ar-mark-graph-atpt (&optional arg)
  "Marks GRAPH at point if any. "
  (interactive "P")
  (ar-th-mark 'graph))

(defun ar-hide-graph-atpt (&optional arg)
  "Hides GRAPH at point. "
  (interactive "P")
  (ar-th-hide 'graph))

(defun ar-show-graph-atpt (&optional arg)
  "Shows hidden GRAPH at point. "
  (interactive "P")
  (ar-th-show 'graph))

(defun ar-hide-show-graph-atpt (&optional arg)
  "Alternatively hides or shows GRAPH at point. "
  (interactive "P")
  (ar-th-hide-show 'graph))

(defun ar-highlight-graph-atpt-mode (&optional arg)
  "Toggles graph-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'graph arg))

(defun ar-kill-graph-atpt (&optional arg)
  "Kills GRAPH at point if any. "
  (interactive "*P")
  (ar-th-kill 'graph arg))

(defun ar-separate-graph-atpt (&optional arg)
  "Separates GRAPH at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'graph arg))

(defun ar-triplequotedq-graph-atpt (&optional arg)
  "Put triplequotes composed of doublequotes around graph. "
  (interactive "*P")
  (ar-th-triplequotedq 'graph arg))

(defun ar-triplequotesq-graph-atpt (&optional arg)
  "Put triplequotes composed of singlequotes around graph. "
  (interactive "*P")
  (ar-th-triplequotesq 'graph arg))

(defun ar-forward-graph-atpt (&optional arg)
  "Moves forward over GRAPH at point if any, does nothing otherwise.
Returns end position of GRAPH "
  (interactive "P")
  (ar-th-forward 'graph arg))

(defun ar-backward-graph-atpt (&optional arg)
  "Moves backward over GRAPH before point if any, does nothing otherwise.
Returns beginning position of GRAPH "
  (interactive "P")
  (ar-th-backward 'graph arg))

(defun ar-transpose-graph-atpt (&optional arg)
  "Transposes GRAPH with GRAPH before point if any. "
  (interactive "*P")
  (ar-th-transpose 'graph arg))

(defun ar-sort-graph-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts graphs in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'graph reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-graph-atpt (&optional arg) 
  "Return t if a GRAPH at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-graph-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-graph-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-blok-lower-atpt (&optional arg)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around lower.
  Returns blok or nil if no LOWER at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'lower arg))

(defun ar-comment-lower-atpt (&optional arg)
  "Comments LOWER at point if any. "
  (interactive "*P")
  (ar-th-comment 'lower arg))

(defun ar-commatize-lower-atpt (&optional arg)
  "Put a comma after LOWER at point if any. "
  (interactive "*P")
  (ar-th-commatize 'lower arg))

(defun ar-mark-lower-atpt (&optional arg)
  "Marks LOWER at point if any. "
  (interactive "P")
  (ar-th-mark 'lower))

(defun ar-hide-lower-atpt (&optional arg)
  "Hides LOWER at point. "
  (interactive "P")
  (ar-th-hide 'lower))

(defun ar-show-lower-atpt (&optional arg)
  "Shows hidden LOWER at point. "
  (interactive "P")
  (ar-th-show 'lower))

(defun ar-hide-show-lower-atpt (&optional arg)
  "Alternatively hides or shows LOWER at point. "
  (interactive "P")
  (ar-th-hide-show 'lower))

(defun ar-highlight-lower-atpt-mode (&optional arg)
  "Toggles lower-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'lower arg))

(defun ar-kill-lower-atpt (&optional arg)
  "Kills LOWER at point if any. "
  (interactive "*P")
  (ar-th-kill 'lower arg))

(defun ar-separate-lower-atpt (&optional arg)
  "Separates LOWER at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'lower arg))

(defun ar-triplequotedq-lower-atpt (&optional arg)
  "Put triplequotes composed of doublequotes around lower. "
  (interactive "*P")
  (ar-th-triplequotedq 'lower arg))

(defun ar-triplequotesq-lower-atpt (&optional arg)
  "Put triplequotes composed of singlequotes around lower. "
  (interactive "*P")
  (ar-th-triplequotesq 'lower arg))

(defun ar-forward-lower-atpt (&optional arg)
  "Moves forward over LOWER at point if any, does nothing otherwise.
Returns end position of LOWER "
  (interactive "P")
  (ar-th-forward 'lower arg))

(defun ar-backward-lower-atpt (&optional arg)
  "Moves backward over LOWER before point if any, does nothing otherwise.
Returns beginning position of LOWER "
  (interactive "P")
  (ar-th-backward 'lower arg))

(defun ar-transpose-lower-atpt (&optional arg)
  "Transposes LOWER with LOWER before point if any. "
  (interactive "*P")
  (ar-th-transpose 'lower arg))

(defun ar-sort-lower-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts lowers in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'lower reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-lower-atpt (&optional arg) 
  "Return t if a LOWER at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-lower-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-lower-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-blok-nonascii-atpt (&optional arg)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around nonascii.
  Returns blok or nil if no NONASCII at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'nonascii arg))

(defun ar-comment-nonascii-atpt (&optional arg)
  "Comments NONASCII at point if any. "
  (interactive "*P")
  (ar-th-comment 'nonascii arg))

(defun ar-commatize-nonascii-atpt (&optional arg)
  "Put a comma after NONASCII at point if any. "
  (interactive "*P")
  (ar-th-commatize 'nonascii arg))

(defun ar-mark-nonascii-atpt (&optional arg)
  "Marks NONASCII at point if any. "
  (interactive "P")
  (ar-th-mark 'nonascii))

(defun ar-hide-nonascii-atpt (&optional arg)
  "Hides NONASCII at point. "
  (interactive "P")
  (ar-th-hide 'nonascii))

(defun ar-show-nonascii-atpt (&optional arg)
  "Shows hidden NONASCII at point. "
  (interactive "P")
  (ar-th-show 'nonascii))

(defun ar-hide-show-nonascii-atpt (&optional arg)
  "Alternatively hides or shows NONASCII at point. "
  (interactive "P")
  (ar-th-hide-show 'nonascii))

(defun ar-highlight-nonascii-atpt-mode (&optional arg)
  "Toggles nonascii-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'nonascii arg))

(defun ar-kill-nonascii-atpt (&optional arg)
  "Kills NONASCII at point if any. "
  (interactive "*P")
  (ar-th-kill 'nonascii arg))

(defun ar-separate-nonascii-atpt (&optional arg)
  "Separates NONASCII at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'nonascii arg))

(defun ar-triplequotedq-nonascii-atpt (&optional arg)
  "Put triplequotes composed of doublequotes around nonascii. "
  (interactive "*P")
  (ar-th-triplequotedq 'nonascii arg))

(defun ar-triplequotesq-nonascii-atpt (&optional arg)
  "Put triplequotes composed of singlequotes around nonascii. "
  (interactive "*P")
  (ar-th-triplequotesq 'nonascii arg))

(defun ar-forward-nonascii-atpt (&optional arg)
  "Moves forward over NONASCII at point if any, does nothing otherwise.
Returns end position of NONASCII "
  (interactive "P")
  (ar-th-forward 'nonascii arg))

(defun ar-backward-nonascii-atpt (&optional arg)
  "Moves backward over NONASCII before point if any, does nothing otherwise.
Returns beginning position of NONASCII "
  (interactive "P")
  (ar-th-backward 'nonascii arg))

(defun ar-transpose-nonascii-atpt (&optional arg)
  "Transposes NONASCII with NONASCII before point if any. "
  (interactive "*P")
  (ar-th-transpose 'nonascii arg))

(defun ar-sort-nonascii-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts nonasciis in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'nonascii reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-nonascii-atpt (&optional arg) 
  "Return t if a NONASCII at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-nonascii-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-nonascii-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-blok-print-atpt (&optional arg)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around print.
  Returns blok or nil if no PRINT at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'print arg))

(defun ar-comment-print-atpt (&optional arg)
  "Comments PRINT at point if any. "
  (interactive "*P")
  (ar-th-comment 'print arg))

(defun ar-commatize-print-atpt (&optional arg)
  "Put a comma after PRINT at point if any. "
  (interactive "*P")
  (ar-th-commatize 'print arg))

(defun ar-mark-print-atpt (&optional arg)
  "Marks PRINT at point if any. "
  (interactive "P")
  (ar-th-mark 'print))

(defun ar-hide-print-atpt (&optional arg)
  "Hides PRINT at point. "
  (interactive "P")
  (ar-th-hide 'print))

(defun ar-show-print-atpt (&optional arg)
  "Shows hidden PRINT at point. "
  (interactive "P")
  (ar-th-show 'print))

(defun ar-hide-show-print-atpt (&optional arg)
  "Alternatively hides or shows PRINT at point. "
  (interactive "P")
  (ar-th-hide-show 'print))

(defun ar-highlight-print-atpt-mode (&optional arg)
  "Toggles print-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'print arg))

(defun ar-kill-print-atpt (&optional arg)
  "Kills PRINT at point if any. "
  (interactive "*P")
  (ar-th-kill 'print arg))

(defun ar-separate-print-atpt (&optional arg)
  "Separates PRINT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'print arg))

(defun ar-triplequotedq-print-atpt (&optional arg)
  "Put triplequotes composed of doublequotes around print. "
  (interactive "*P")
  (ar-th-triplequotedq 'print arg))

(defun ar-triplequotesq-print-atpt (&optional arg)
  "Put triplequotes composed of singlequotes around print. "
  (interactive "*P")
  (ar-th-triplequotesq 'print arg))

(defun ar-forward-print-atpt (&optional arg)
  "Moves forward over PRINT at point if any, does nothing otherwise.
Returns end position of PRINT "
  (interactive "P")
  (ar-th-forward 'print arg))

(defun ar-backward-print-atpt (&optional arg)
  "Moves backward over PRINT before point if any, does nothing otherwise.
Returns beginning position of PRINT "
  (interactive "P")
  (ar-th-backward 'print arg))

(defun ar-transpose-print-atpt (&optional arg)
  "Transposes PRINT with PRINT before point if any. "
  (interactive "*P")
  (ar-th-transpose 'print arg))

(defun ar-sort-print-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts prints in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'print reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-print-atpt (&optional arg) 
  "Return t if a PRINT at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-print-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-print-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-blok-punct-atpt (&optional arg)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around punct.
  Returns blok or nil if no PUNCT at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'punct arg))

(defun ar-comment-punct-atpt (&optional arg)
  "Comments PUNCT at point if any. "
  (interactive "*P")
  (ar-th-comment 'punct arg))

(defun ar-commatize-punct-atpt (&optional arg)
  "Put a comma after PUNCT at point if any. "
  (interactive "*P")
  (ar-th-commatize 'punct arg))

(defun ar-mark-punct-atpt (&optional arg)
  "Marks PUNCT at point if any. "
  (interactive "P")
  (ar-th-mark 'punct))

(defun ar-hide-punct-atpt (&optional arg)
  "Hides PUNCT at point. "
  (interactive "P")
  (ar-th-hide 'punct))

(defun ar-show-punct-atpt (&optional arg)
  "Shows hidden PUNCT at point. "
  (interactive "P")
  (ar-th-show 'punct))

(defun ar-hide-show-punct-atpt (&optional arg)
  "Alternatively hides or shows PUNCT at point. "
  (interactive "P")
  (ar-th-hide-show 'punct))

(defun ar-highlight-punct-atpt-mode (&optional arg)
  "Toggles punct-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'punct arg))

(defun ar-kill-punct-atpt (&optional arg)
  "Kills PUNCT at point if any. "
  (interactive "*P")
  (ar-th-kill 'punct arg))

(defun ar-separate-punct-atpt (&optional arg)
  "Separates PUNCT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'punct arg))

(defun ar-triplequotedq-punct-atpt (&optional arg)
  "Put triplequotes composed of doublequotes around punct. "
  (interactive "*P")
  (ar-th-triplequotedq 'punct arg))

(defun ar-triplequotesq-punct-atpt (&optional arg)
  "Put triplequotes composed of singlequotes around punct. "
  (interactive "*P")
  (ar-th-triplequotesq 'punct arg))

(defun ar-forward-punct-atpt (&optional arg)
  "Moves forward over PUNCT at point if any, does nothing otherwise.
Returns end position of PUNCT "
  (interactive "P")
  (ar-th-forward 'punct arg))

(defun ar-backward-punct-atpt (&optional arg)
  "Moves backward over PUNCT before point if any, does nothing otherwise.
Returns beginning position of PUNCT "
  (interactive "P")
  (ar-th-backward 'punct arg))

(defun ar-transpose-punct-atpt (&optional arg)
  "Transposes PUNCT with PUNCT before point if any. "
  (interactive "*P")
  (ar-th-transpose 'punct arg))

(defun ar-sort-punct-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts puncts in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'punct reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-punct-atpt (&optional arg) 
  "Return t if a PUNCT at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-punct-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-punct-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-blok-space-atpt (&optional arg)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around space.
  Returns blok or nil if no SPACE at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'space arg))

(defun ar-comment-space-atpt (&optional arg)
  "Comments SPACE at point if any. "
  (interactive "*P")
  (ar-th-comment 'space arg))

(defun ar-commatize-space-atpt (&optional arg)
  "Put a comma after SPACE at point if any. "
  (interactive "*P")
  (ar-th-commatize 'space arg))

(defun ar-mark-space-atpt (&optional arg)
  "Marks SPACE at point if any. "
  (interactive "P")
  (ar-th-mark 'space))

(defun ar-hide-space-atpt (&optional arg)
  "Hides SPACE at point. "
  (interactive "P")
  (ar-th-hide 'space))

(defun ar-show-space-atpt (&optional arg)
  "Shows hidden SPACE at point. "
  (interactive "P")
  (ar-th-show 'space))

(defun ar-hide-show-space-atpt (&optional arg)
  "Alternatively hides or shows SPACE at point. "
  (interactive "P")
  (ar-th-hide-show 'space))

(defun ar-highlight-space-atpt-mode (&optional arg)
  "Toggles space-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'space arg))

(defun ar-kill-space-atpt (&optional arg)
  "Kills SPACE at point if any. "
  (interactive "*P")
  (ar-th-kill 'space arg))

(defun ar-separate-space-atpt (&optional arg)
  "Separates SPACE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'space arg))

(defun ar-triplequotedq-space-atpt (&optional arg)
  "Put triplequotes composed of doublequotes around space. "
  (interactive "*P")
  (ar-th-triplequotedq 'space arg))

(defun ar-triplequotesq-space-atpt (&optional arg)
  "Put triplequotes composed of singlequotes around space. "
  (interactive "*P")
  (ar-th-triplequotesq 'space arg))

(defun ar-forward-space-atpt (&optional arg)
  "Moves forward over SPACE at point if any, does nothing otherwise.
Returns end position of SPACE "
  (interactive "P")
  (ar-th-forward 'space arg))

(defun ar-backward-space-atpt (&optional arg)
  "Moves backward over SPACE before point if any, does nothing otherwise.
Returns beginning position of SPACE "
  (interactive "P")
  (ar-th-backward 'space arg))

(defun ar-transpose-space-atpt (&optional arg)
  "Transposes SPACE with SPACE before point if any. "
  (interactive "*P")
  (ar-th-transpose 'space arg))

(defun ar-sort-space-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts spaces in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'space reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-space-atpt (&optional arg) 
  "Return t if a SPACE at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-space-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-space-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-blok-upper-atpt (&optional arg)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around upper.
  Returns blok or nil if no UPPER at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'upper arg))

(defun ar-comment-upper-atpt (&optional arg)
  "Comments UPPER at point if any. "
  (interactive "*P")
  (ar-th-comment 'upper arg))

(defun ar-commatize-upper-atpt (&optional arg)
  "Put a comma after UPPER at point if any. "
  (interactive "*P")
  (ar-th-commatize 'upper arg))

(defun ar-mark-upper-atpt (&optional arg)
  "Marks UPPER at point if any. "
  (interactive "P")
  (ar-th-mark 'upper))

(defun ar-hide-upper-atpt (&optional arg)
  "Hides UPPER at point. "
  (interactive "P")
  (ar-th-hide 'upper))

(defun ar-show-upper-atpt (&optional arg)
  "Shows hidden UPPER at point. "
  (interactive "P")
  (ar-th-show 'upper))

(defun ar-hide-show-upper-atpt (&optional arg)
  "Alternatively hides or shows UPPER at point. "
  (interactive "P")
  (ar-th-hide-show 'upper))

(defun ar-highlight-upper-atpt-mode (&optional arg)
  "Toggles upper-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'upper arg))

(defun ar-kill-upper-atpt (&optional arg)
  "Kills UPPER at point if any. "
  (interactive "*P")
  (ar-th-kill 'upper arg))

(defun ar-separate-upper-atpt (&optional arg)
  "Separates UPPER at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'upper arg))

(defun ar-triplequotedq-upper-atpt (&optional arg)
  "Put triplequotes composed of doublequotes around upper. "
  (interactive "*P")
  (ar-th-triplequotedq 'upper arg))

(defun ar-triplequotesq-upper-atpt (&optional arg)
  "Put triplequotes composed of singlequotes around upper. "
  (interactive "*P")
  (ar-th-triplequotesq 'upper arg))

(defun ar-forward-upper-atpt (&optional arg)
  "Moves forward over UPPER at point if any, does nothing otherwise.
Returns end position of UPPER "
  (interactive "P")
  (ar-th-forward 'upper arg))

(defun ar-backward-upper-atpt (&optional arg)
  "Moves backward over UPPER before point if any, does nothing otherwise.
Returns beginning position of UPPER "
  (interactive "P")
  (ar-th-backward 'upper arg))

(defun ar-transpose-upper-atpt (&optional arg)
  "Transposes UPPER with UPPER before point if any. "
  (interactive "*P")
  (ar-th-transpose 'upper arg))

(defun ar-sort-upper-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts uppers in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'upper reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-upper-atpt (&optional arg) 
  "Return t if a UPPER at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-upper-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-upper-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-blok-xdigit-atpt (&optional arg)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around xdigit.
  Returns blok or nil if no XDIGIT at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'xdigit arg))

(defun ar-comment-xdigit-atpt (&optional arg)
  "Comments XDIGIT at point if any. "
  (interactive "*P")
  (ar-th-comment 'xdigit arg))

(defun ar-commatize-xdigit-atpt (&optional arg)
  "Put a comma after XDIGIT at point if any. "
  (interactive "*P")
  (ar-th-commatize 'xdigit arg))

(defun ar-mark-xdigit-atpt (&optional arg)
  "Marks XDIGIT at point if any. "
  (interactive "P")
  (ar-th-mark 'xdigit))

(defun ar-hide-xdigit-atpt (&optional arg)
  "Hides XDIGIT at point. "
  (interactive "P")
  (ar-th-hide 'xdigit))

(defun ar-show-xdigit-atpt (&optional arg)
  "Shows hidden XDIGIT at point. "
  (interactive "P")
  (ar-th-show 'xdigit))

(defun ar-hide-show-xdigit-atpt (&optional arg)
  "Alternatively hides or shows XDIGIT at point. "
  (interactive "P")
  (ar-th-hide-show 'xdigit))

(defun ar-highlight-xdigit-atpt-mode (&optional arg)
  "Toggles xdigit-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'xdigit arg))

(defun ar-kill-xdigit-atpt (&optional arg)
  "Kills XDIGIT at point if any. "
  (interactive "*P")
  (ar-th-kill 'xdigit arg))

(defun ar-separate-xdigit-atpt (&optional arg)
  "Separates XDIGIT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'xdigit arg))

(defun ar-triplequotedq-xdigit-atpt (&optional arg)
  "Put triplequotes composed of doublequotes around xdigit. "
  (interactive "*P")
  (ar-th-triplequotedq 'xdigit arg))

(defun ar-triplequotesq-xdigit-atpt (&optional arg)
  "Put triplequotes composed of singlequotes around xdigit. "
  (interactive "*P")
  (ar-th-triplequotesq 'xdigit arg))

(defun ar-forward-xdigit-atpt (&optional arg)
  "Moves forward over XDIGIT at point if any, does nothing otherwise.
Returns end position of XDIGIT "
  (interactive "P")
  (ar-th-forward 'xdigit arg))

(defun ar-backward-xdigit-atpt (&optional arg)
  "Moves backward over XDIGIT before point if any, does nothing otherwise.
Returns beginning position of XDIGIT "
  (interactive "P")
  (ar-th-backward 'xdigit arg))

(defun ar-transpose-xdigit-atpt (&optional arg)
  "Transposes XDIGIT with XDIGIT before point if any. "
  (interactive "*P")
  (ar-th-transpose 'xdigit arg))

(defun ar-sort-xdigit-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts xdigits in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'xdigit reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-xdigit-atpt (&optional arg) 
  "Return t if a XDIGIT at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-xdigit-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-xdigit-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))
;; ar-thing-at-point-utils-nodelim-einzeln: end

;; ar-thing-at-point-utils-delimited-intern: ar-unpaired-delimited-raw: start

(defun ar-backslashed-atpt (&optional no-delimiters nested)
  "Returns backslashed at point if any, nil otherwise.

With numerical ARG 2 `ar-scan-whole-buffer' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'backslashed no-delimiters (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-backslashed-atpt (&optional no-delimiters check)
  "Returns a list, borders of backslashed if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'backslashed no-delimiters (interactive-p) check))

(defun ar-backslashed-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position backslashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'backslashed no-delimiters (interactive-p) check))

(defun ar-backslashed-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of backslashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'backslashed no-delimiters (interactive-p) check))

(defun ar-beginning-of-backslashed-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class backslashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'backslashed no-delimiters (interactive-p) check))

(defun ar-end-of-backslashed-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class backslashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'backslashed no-delimiters check))

(defun ar-length-of-backslashed-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class backslashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'backslashed no-delimiters check))

(defun ar-copy-backslashed-atpt (&optional no-delimiters check)
  "Returns a copy of backslashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'backslashed no-delimiters))

(defun ar-delete-backslashed-atpt (&optional arg)
  "Deletes backslashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'backslashed arg arg))

(defun ar-delete-backslashed-in-region (beg end &optional arg check no-delimiters)
  "Deletes backslashed at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'backslashed beg end (interactive-p)))

(defun ar-blok-backslashed-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around backslashed.

If region is active, do that for all elements \"backslashed\" in region.
  Returns blok or nil if no backslashed at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'backslashed no-delimiters (interactive-p)))

(defun ar-doubleslash-backslashed-atpt (&optional no-delimiters check)
  "Puts doubled slashes around backslashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'backslashed no-delimiters))

(defun ar-backslashparen-backslashed-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around backslashed at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'backslashed no-delimiters))

(defun ar-comment-backslashed-atpt (&optional no-delimiters check)
  "Comments backslashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'backslashed no-delimiters))

(defun ar-commatize-backslashed-atpt (&optional no-delimiters check)
  "Put a comma after backslashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'backslashed no-delimiters))

(defun ar-mark-backslashed-atpt (&optional no-delimiters check)
  "Marks backslashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'backslashed))

(defun ar-hide-backslashed-atpt (&optional arg)
  "Hides backslashed at point. "
  (interactive "p")
  (ar-th-hide 'backslashed))

(defun ar-show-backslashed-atpt (&optional arg)
  "Shows hidden backslashed at point. "
  (interactive "p")
  (ar-th-show 'backslashed))

(defun ar-hide-show-backslashed-atpt (&optional arg)
  "Alternatively hides or shows backslashed at point. "
  (interactive "p")
  (ar-th-hide-show 'backslashed))

(defun ar-highlight-backslashed-atpt-mode (&optional no-delimiters check)
  "Toggles backslashed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'backslashed no-delimiters))

(defun ar-kill-backslashed-atpt (&optional no-delimiters check)
  "Kills backslashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'backslashed no-delimiters))

(defun ar-separate-backslashed-atpt (&optional no-delimiters check)
  "Separates backslashed at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'backslashed (eq 4  (prefix-numeric-value no-delimiters)) check))

(defun ar-trim-backslashed-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
 (interactive "*P")
 (ar-th-trim 'backslashed (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-backslashed-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*P")
  (ar-th-trim 'backslashed (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-backslashed-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "P*")
  (ar-th-trim 'backslashed (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-forward-backslashed-atpt (&optional arg)
  "Moves forward over backslashed at point if any, does nothing otherwise.
Returns end position of backslashed "
  (interactive "p")
  (ar-th-forward 'backslashed arg))

(defun ar-backward-backslashed-atpt (&optional arg)
  "Moves backward over backslashed before point if any, does nothing otherwise.
Returns beginning position of backslashed "
  (interactive "p")
  (ar-th-backward 'backslashed arg))

(defun ar-transpose-backslashed-atpt (&optional arg)
  "Transposes backslashed at point with backslashed before if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'backslashed arg))

(defun ar-sort-backslashed-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts backslasheds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'backslashed reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-backslashed-atpt (&optional arg) 
  "Return t if a backslashed at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-backslashed-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-backslashed-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-backticked-atpt (&optional no-delimiters nested)
  "Returns backticked at point if any, nil otherwise.

With numerical ARG 2 `ar-scan-whole-buffer' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'backticked no-delimiters (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-backticked-atpt (&optional no-delimiters check)
  "Returns a list, borders of backticked if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'backticked no-delimiters (interactive-p) check))

(defun ar-backticked-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position backticked at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'backticked no-delimiters (interactive-p) check))

(defun ar-backticked-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of backticked at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'backticked no-delimiters (interactive-p) check))

(defun ar-beginning-of-backticked-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class backticked at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'backticked no-delimiters (interactive-p) check))

(defun ar-end-of-backticked-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class backticked at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'backticked no-delimiters check))

(defun ar-length-of-backticked-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class backticked at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'backticked no-delimiters check))

(defun ar-copy-backticked-atpt (&optional no-delimiters check)
  "Returns a copy of backticked at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'backticked no-delimiters))

(defun ar-delete-backticked-atpt (&optional arg)
  "Deletes backticked at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'backticked arg arg))

(defun ar-delete-backticked-in-region (beg end &optional arg check no-delimiters)
  "Deletes backticked at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'backticked beg end (interactive-p)))

(defun ar-blok-backticked-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around backticked.

If region is active, do that for all elements \"backticked\" in region.
  Returns blok or nil if no backticked at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'backticked no-delimiters (interactive-p)))

(defun ar-doubleslash-backticked-atpt (&optional no-delimiters check)
  "Puts doubled slashes around backticked at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'backticked no-delimiters))

(defun ar-backslashparen-backticked-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around backticked at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'backticked no-delimiters))

(defun ar-comment-backticked-atpt (&optional no-delimiters check)
  "Comments backticked at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'backticked no-delimiters))

(defun ar-commatize-backticked-atpt (&optional no-delimiters check)
  "Put a comma after backticked at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'backticked no-delimiters))

(defun ar-mark-backticked-atpt (&optional no-delimiters check)
  "Marks backticked at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'backticked))

(defun ar-hide-backticked-atpt (&optional arg)
  "Hides backticked at point. "
  (interactive "p")
  (ar-th-hide 'backticked))

(defun ar-show-backticked-atpt (&optional arg)
  "Shows hidden backticked at point. "
  (interactive "p")
  (ar-th-show 'backticked))

(defun ar-hide-show-backticked-atpt (&optional arg)
  "Alternatively hides or shows backticked at point. "
  (interactive "p")
  (ar-th-hide-show 'backticked))

(defun ar-highlight-backticked-atpt-mode (&optional no-delimiters check)
  "Toggles backticked-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'backticked no-delimiters))

(defun ar-kill-backticked-atpt (&optional no-delimiters check)
  "Kills backticked at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'backticked no-delimiters))

(defun ar-separate-backticked-atpt (&optional no-delimiters check)
  "Separates backticked at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'backticked (eq 4  (prefix-numeric-value no-delimiters)) check))

(defun ar-trim-backticked-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
 (interactive "*P")
 (ar-th-trim 'backticked (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-backticked-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*P")
  (ar-th-trim 'backticked (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-backticked-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "P*")
  (ar-th-trim 'backticked (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-forward-backticked-atpt (&optional arg)
  "Moves forward over backticked at point if any, does nothing otherwise.
Returns end position of backticked "
  (interactive "p")
  (ar-th-forward 'backticked arg))

(defun ar-backward-backticked-atpt (&optional arg)
  "Moves backward over backticked before point if any, does nothing otherwise.
Returns beginning position of backticked "
  (interactive "p")
  (ar-th-backward 'backticked arg))

(defun ar-transpose-backticked-atpt (&optional arg)
  "Transposes backticked at point with backticked before if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'backticked arg))

(defun ar-sort-backticked-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts backtickeds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'backticked reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-backticked-atpt (&optional arg) 
  "Return t if a backticked at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-backticked-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-backticked-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-coloned-atpt (&optional no-delimiters nested)
  "Returns coloned at point if any, nil otherwise.

With numerical ARG 2 `ar-scan-whole-buffer' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'coloned no-delimiters (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-coloned-atpt (&optional no-delimiters check)
  "Returns a list, borders of coloned if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'coloned no-delimiters (interactive-p) check))

(defun ar-coloned-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position coloned at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'coloned no-delimiters (interactive-p) check))

(defun ar-coloned-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of coloned at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'coloned no-delimiters (interactive-p) check))

(defun ar-beginning-of-coloned-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class coloned at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'coloned no-delimiters (interactive-p) check))

(defun ar-end-of-coloned-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class coloned at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'coloned no-delimiters check))

(defun ar-length-of-coloned-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class coloned at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'coloned no-delimiters check))

(defun ar-copy-coloned-atpt (&optional no-delimiters check)
  "Returns a copy of coloned at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'coloned no-delimiters))

(defun ar-delete-coloned-atpt (&optional arg)
  "Deletes coloned at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'coloned arg arg))

(defun ar-delete-coloned-in-region (beg end &optional arg check no-delimiters)
  "Deletes coloned at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'coloned beg end (interactive-p)))

(defun ar-blok-coloned-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around coloned.

If region is active, do that for all elements \"coloned\" in region.
  Returns blok or nil if no coloned at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'coloned no-delimiters (interactive-p)))

(defun ar-doubleslash-coloned-atpt (&optional no-delimiters check)
  "Puts doubled slashes around coloned at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'coloned no-delimiters))

(defun ar-backslashparen-coloned-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around coloned at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'coloned no-delimiters))

(defun ar-comment-coloned-atpt (&optional no-delimiters check)
  "Comments coloned at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'coloned no-delimiters))

(defun ar-commatize-coloned-atpt (&optional no-delimiters check)
  "Put a comma after coloned at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'coloned no-delimiters))

(defun ar-mark-coloned-atpt (&optional no-delimiters check)
  "Marks coloned at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'coloned))

(defun ar-hide-coloned-atpt (&optional arg)
  "Hides coloned at point. "
  (interactive "p")
  (ar-th-hide 'coloned))

(defun ar-show-coloned-atpt (&optional arg)
  "Shows hidden coloned at point. "
  (interactive "p")
  (ar-th-show 'coloned))

(defun ar-hide-show-coloned-atpt (&optional arg)
  "Alternatively hides or shows coloned at point. "
  (interactive "p")
  (ar-th-hide-show 'coloned))

(defun ar-highlight-coloned-atpt-mode (&optional no-delimiters check)
  "Toggles coloned-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'coloned no-delimiters))

(defun ar-kill-coloned-atpt (&optional no-delimiters check)
  "Kills coloned at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'coloned no-delimiters))

(defun ar-separate-coloned-atpt (&optional no-delimiters check)
  "Separates coloned at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'coloned (eq 4  (prefix-numeric-value no-delimiters)) check))

(defun ar-trim-coloned-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
 (interactive "*P")
 (ar-th-trim 'coloned (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-coloned-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*P")
  (ar-th-trim 'coloned (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-coloned-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "P*")
  (ar-th-trim 'coloned (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-forward-coloned-atpt (&optional arg)
  "Moves forward over coloned at point if any, does nothing otherwise.
Returns end position of coloned "
  (interactive "p")
  (ar-th-forward 'coloned arg))

(defun ar-backward-coloned-atpt (&optional arg)
  "Moves backward over coloned before point if any, does nothing otherwise.
Returns beginning position of coloned "
  (interactive "p")
  (ar-th-backward 'coloned arg))

(defun ar-transpose-coloned-atpt (&optional arg)
  "Transposes coloned at point with coloned before if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'coloned arg))

(defun ar-sort-coloned-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts coloneds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'coloned reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-coloned-atpt (&optional arg) 
  "Return t if a coloned at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-coloned-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-coloned-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-dollared-atpt (&optional no-delimiters nested)
  "Returns dollared at point if any, nil otherwise.

With numerical ARG 2 `ar-scan-whole-buffer' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'dollared no-delimiters (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-dollared-atpt (&optional no-delimiters check)
  "Returns a list, borders of dollared if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'dollared no-delimiters (interactive-p) check))

(defun ar-dollared-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position dollared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'dollared no-delimiters (interactive-p) check))

(defun ar-dollared-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of dollared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'dollared no-delimiters (interactive-p) check))

(defun ar-beginning-of-dollared-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class dollared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'dollared no-delimiters (interactive-p) check))

(defun ar-end-of-dollared-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class dollared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'dollared no-delimiters check))

(defun ar-length-of-dollared-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class dollared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'dollared no-delimiters check))

(defun ar-copy-dollared-atpt (&optional no-delimiters check)
  "Returns a copy of dollared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'dollared no-delimiters))

(defun ar-delete-dollared-atpt (&optional arg)
  "Deletes dollared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'dollared arg arg))

(defun ar-delete-dollared-in-region (beg end &optional arg check no-delimiters)
  "Deletes dollared at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'dollared beg end (interactive-p)))

(defun ar-blok-dollared-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around dollared.

If region is active, do that for all elements \"dollared\" in region.
  Returns blok or nil if no dollared at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'dollared no-delimiters (interactive-p)))

(defun ar-doubleslash-dollared-atpt (&optional no-delimiters check)
  "Puts doubled slashes around dollared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'dollared no-delimiters))

(defun ar-backslashparen-dollared-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around dollared at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'dollared no-delimiters))

(defun ar-comment-dollared-atpt (&optional no-delimiters check)
  "Comments dollared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'dollared no-delimiters))

(defun ar-commatize-dollared-atpt (&optional no-delimiters check)
  "Put a comma after dollared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'dollared no-delimiters))

(defun ar-mark-dollared-atpt (&optional no-delimiters check)
  "Marks dollared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'dollared))

(defun ar-hide-dollared-atpt (&optional arg)
  "Hides dollared at point. "
  (interactive "p")
  (ar-th-hide 'dollared))

(defun ar-show-dollared-atpt (&optional arg)
  "Shows hidden dollared at point. "
  (interactive "p")
  (ar-th-show 'dollared))

(defun ar-hide-show-dollared-atpt (&optional arg)
  "Alternatively hides or shows dollared at point. "
  (interactive "p")
  (ar-th-hide-show 'dollared))

(defun ar-highlight-dollared-atpt-mode (&optional no-delimiters check)
  "Toggles dollared-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'dollared no-delimiters))

(defun ar-kill-dollared-atpt (&optional no-delimiters check)
  "Kills dollared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'dollared no-delimiters))

(defun ar-separate-dollared-atpt (&optional no-delimiters check)
  "Separates dollared at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'dollared (eq 4  (prefix-numeric-value no-delimiters)) check))

(defun ar-trim-dollared-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
 (interactive "*P")
 (ar-th-trim 'dollared (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-dollared-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*P")
  (ar-th-trim 'dollared (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-dollared-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "P*")
  (ar-th-trim 'dollared (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-forward-dollared-atpt (&optional arg)
  "Moves forward over dollared at point if any, does nothing otherwise.
Returns end position of dollared "
  (interactive "p")
  (ar-th-forward 'dollared arg))

(defun ar-backward-dollared-atpt (&optional arg)
  "Moves backward over dollared before point if any, does nothing otherwise.
Returns beginning position of dollared "
  (interactive "p")
  (ar-th-backward 'dollared arg))

(defun ar-transpose-dollared-atpt (&optional arg)
  "Transposes dollared at point with dollared before if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'dollared arg))

(defun ar-sort-dollared-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts dollareds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'dollared reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-dollared-atpt (&optional arg) 
  "Return t if a dollared at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-dollared-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-dollared-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-doublequoted-atpt (&optional no-delimiters nested)
  "Returns doublequoted at point if any, nil otherwise.

With numerical ARG 2 `ar-scan-whole-buffer' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'doublequoted no-delimiters (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-doublequoted-atpt (&optional no-delimiters check)
  "Returns a list, borders of doublequoted if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'doublequoted no-delimiters (interactive-p) check))

(defun ar-doublequoted-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position doublequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'doublequoted no-delimiters (interactive-p) check))

(defun ar-doublequoted-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of doublequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'doublequoted no-delimiters (interactive-p) check))

(defun ar-beginning-of-doublequoted-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class doublequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'doublequoted no-delimiters (interactive-p) check))

(defun ar-end-of-doublequoted-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class doublequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'doublequoted no-delimiters check))

(defun ar-length-of-doublequoted-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class doublequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'doublequoted no-delimiters check))

(defun ar-copy-doublequoted-atpt (&optional no-delimiters check)
  "Returns a copy of doublequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'doublequoted no-delimiters))

(defun ar-delete-doublequoted-atpt (&optional arg)
  "Deletes doublequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'doublequoted arg arg))

(defun ar-delete-doublequoted-in-region (beg end &optional arg check no-delimiters)
  "Deletes doublequoted at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'doublequoted beg end (interactive-p)))

(defun ar-blok-doublequoted-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around doublequoted.

If region is active, do that for all elements \"doublequoted\" in region.
  Returns blok or nil if no doublequoted at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'doublequoted no-delimiters (interactive-p)))

(defun ar-doubleslash-doublequoted-atpt (&optional no-delimiters check)
  "Puts doubled slashes around doublequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'doublequoted no-delimiters))

(defun ar-backslashparen-doublequoted-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around doublequoted at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'doublequoted no-delimiters))

(defun ar-comment-doublequoted-atpt (&optional no-delimiters check)
  "Comments doublequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'doublequoted no-delimiters))

(defun ar-commatize-doublequoted-atpt (&optional no-delimiters check)
  "Put a comma after doublequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'doublequoted no-delimiters))

(defun ar-mark-doublequoted-atpt (&optional no-delimiters check)
  "Marks doublequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'doublequoted))

(defun ar-hide-doublequoted-atpt (&optional arg)
  "Hides doublequoted at point. "
  (interactive "p")
  (ar-th-hide 'doublequoted))

(defun ar-show-doublequoted-atpt (&optional arg)
  "Shows hidden doublequoted at point. "
  (interactive "p")
  (ar-th-show 'doublequoted))

(defun ar-hide-show-doublequoted-atpt (&optional arg)
  "Alternatively hides or shows doublequoted at point. "
  (interactive "p")
  (ar-th-hide-show 'doublequoted))

(defun ar-highlight-doublequoted-atpt-mode (&optional no-delimiters check)
  "Toggles doublequoted-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'doublequoted no-delimiters))

(defun ar-kill-doublequoted-atpt (&optional no-delimiters check)
  "Kills doublequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'doublequoted no-delimiters))

(defun ar-separate-doublequoted-atpt (&optional no-delimiters check)
  "Separates doublequoted at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'doublequoted (eq 4  (prefix-numeric-value no-delimiters)) check))

(defun ar-trim-doublequoted-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
 (interactive "*P")
 (ar-th-trim 'doublequoted (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-doublequoted-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*P")
  (ar-th-trim 'doublequoted (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-doublequoted-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "P*")
  (ar-th-trim 'doublequoted (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-forward-doublequoted-atpt (&optional arg)
  "Moves forward over doublequoted at point if any, does nothing otherwise.
Returns end position of doublequoted "
  (interactive "p")
  (ar-th-forward 'doublequoted arg))

(defun ar-backward-doublequoted-atpt (&optional arg)
  "Moves backward over doublequoted before point if any, does nothing otherwise.
Returns beginning position of doublequoted "
  (interactive "p")
  (ar-th-backward 'doublequoted arg))

(defun ar-transpose-doublequoted-atpt (&optional arg)
  "Transposes doublequoted at point with doublequoted before if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'doublequoted arg))

(defun ar-sort-doublequoted-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts doublequoteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'doublequoted reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-doublequoted-atpt (&optional arg) 
  "Return t if a doublequoted at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-doublequoted-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-doublequoted-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-equalized-atpt (&optional no-delimiters nested)
  "Returns equalized at point if any, nil otherwise.

With numerical ARG 2 `ar-scan-whole-buffer' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'equalized no-delimiters (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-equalized-atpt (&optional no-delimiters check)
  "Returns a list, borders of equalized if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'equalized no-delimiters (interactive-p) check))

(defun ar-equalized-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position equalized at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'equalized no-delimiters (interactive-p) check))

(defun ar-equalized-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of equalized at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'equalized no-delimiters (interactive-p) check))

(defun ar-beginning-of-equalized-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class equalized at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'equalized no-delimiters (interactive-p) check))

(defun ar-end-of-equalized-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class equalized at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'equalized no-delimiters check))

(defun ar-length-of-equalized-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class equalized at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'equalized no-delimiters check))

(defun ar-copy-equalized-atpt (&optional no-delimiters check)
  "Returns a copy of equalized at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'equalized no-delimiters))

(defun ar-delete-equalized-atpt (&optional arg)
  "Deletes equalized at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'equalized arg arg))

(defun ar-delete-equalized-in-region (beg end &optional arg check no-delimiters)
  "Deletes equalized at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'equalized beg end (interactive-p)))

(defun ar-blok-equalized-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around equalized.

If region is active, do that for all elements \"equalized\" in region.
  Returns blok or nil if no equalized at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'equalized no-delimiters (interactive-p)))

(defun ar-doubleslash-equalized-atpt (&optional no-delimiters check)
  "Puts doubled slashes around equalized at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'equalized no-delimiters))

(defun ar-backslashparen-equalized-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around equalized at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'equalized no-delimiters))

(defun ar-comment-equalized-atpt (&optional no-delimiters check)
  "Comments equalized at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'equalized no-delimiters))

(defun ar-commatize-equalized-atpt (&optional no-delimiters check)
  "Put a comma after equalized at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'equalized no-delimiters))

(defun ar-mark-equalized-atpt (&optional no-delimiters check)
  "Marks equalized at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'equalized))

(defun ar-hide-equalized-atpt (&optional arg)
  "Hides equalized at point. "
  (interactive "p")
  (ar-th-hide 'equalized))

(defun ar-show-equalized-atpt (&optional arg)
  "Shows hidden equalized at point. "
  (interactive "p")
  (ar-th-show 'equalized))

(defun ar-hide-show-equalized-atpt (&optional arg)
  "Alternatively hides or shows equalized at point. "
  (interactive "p")
  (ar-th-hide-show 'equalized))

(defun ar-highlight-equalized-atpt-mode (&optional no-delimiters check)
  "Toggles equalized-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'equalized no-delimiters))

(defun ar-kill-equalized-atpt (&optional no-delimiters check)
  "Kills equalized at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'equalized no-delimiters))

(defun ar-separate-equalized-atpt (&optional no-delimiters check)
  "Separates equalized at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'equalized (eq 4  (prefix-numeric-value no-delimiters)) check))

(defun ar-trim-equalized-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
 (interactive "*P")
 (ar-th-trim 'equalized (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-equalized-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*P")
  (ar-th-trim 'equalized (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-equalized-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "P*")
  (ar-th-trim 'equalized (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-forward-equalized-atpt (&optional arg)
  "Moves forward over equalized at point if any, does nothing otherwise.
Returns end position of equalized "
  (interactive "p")
  (ar-th-forward 'equalized arg))

(defun ar-backward-equalized-atpt (&optional arg)
  "Moves backward over equalized before point if any, does nothing otherwise.
Returns beginning position of equalized "
  (interactive "p")
  (ar-th-backward 'equalized arg))

(defun ar-transpose-equalized-atpt (&optional arg)
  "Transposes equalized at point with equalized before if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'equalized arg))

(defun ar-sort-equalized-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts equalizeds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'equalized reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-equalized-atpt (&optional arg) 
  "Return t if a equalized at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-equalized-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-equalized-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-hyphened-atpt (&optional no-delimiters nested)
  "Returns hyphened at point if any, nil otherwise.

With numerical ARG 2 `ar-scan-whole-buffer' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'hyphened no-delimiters (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-hyphened-atpt (&optional no-delimiters check)
  "Returns a list, borders of hyphened if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'hyphened no-delimiters (interactive-p) check))

(defun ar-hyphened-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position hyphened at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'hyphened no-delimiters (interactive-p) check))

(defun ar-hyphened-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of hyphened at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'hyphened no-delimiters (interactive-p) check))

(defun ar-beginning-of-hyphened-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class hyphened at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'hyphened no-delimiters (interactive-p) check))

(defun ar-end-of-hyphened-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class hyphened at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'hyphened no-delimiters check))

(defun ar-length-of-hyphened-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class hyphened at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'hyphened no-delimiters check))

(defun ar-copy-hyphened-atpt (&optional no-delimiters check)
  "Returns a copy of hyphened at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'hyphened no-delimiters))

(defun ar-delete-hyphened-atpt (&optional arg)
  "Deletes hyphened at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'hyphened arg arg))

(defun ar-delete-hyphened-in-region (beg end &optional arg check no-delimiters)
  "Deletes hyphened at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'hyphened beg end (interactive-p)))

(defun ar-blok-hyphened-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around hyphened.

If region is active, do that for all elements \"hyphened\" in region.
  Returns blok or nil if no hyphened at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'hyphened no-delimiters (interactive-p)))

(defun ar-doubleslash-hyphened-atpt (&optional no-delimiters check)
  "Puts doubled slashes around hyphened at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'hyphened no-delimiters))

(defun ar-backslashparen-hyphened-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around hyphened at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'hyphened no-delimiters))

(defun ar-comment-hyphened-atpt (&optional no-delimiters check)
  "Comments hyphened at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'hyphened no-delimiters))

(defun ar-commatize-hyphened-atpt (&optional no-delimiters check)
  "Put a comma after hyphened at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'hyphened no-delimiters))

(defun ar-mark-hyphened-atpt (&optional no-delimiters check)
  "Marks hyphened at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'hyphened))

(defun ar-hide-hyphened-atpt (&optional arg)
  "Hides hyphened at point. "
  (interactive "p")
  (ar-th-hide 'hyphened))

(defun ar-show-hyphened-atpt (&optional arg)
  "Shows hidden hyphened at point. "
  (interactive "p")
  (ar-th-show 'hyphened))

(defun ar-hide-show-hyphened-atpt (&optional arg)
  "Alternatively hides or shows hyphened at point. "
  (interactive "p")
  (ar-th-hide-show 'hyphened))

(defun ar-highlight-hyphened-atpt-mode (&optional no-delimiters check)
  "Toggles hyphened-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'hyphened no-delimiters))

(defun ar-kill-hyphened-atpt (&optional no-delimiters check)
  "Kills hyphened at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'hyphened no-delimiters))

(defun ar-separate-hyphened-atpt (&optional no-delimiters check)
  "Separates hyphened at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'hyphened (eq 4  (prefix-numeric-value no-delimiters)) check))

(defun ar-trim-hyphened-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
 (interactive "*P")
 (ar-th-trim 'hyphened (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-hyphened-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*P")
  (ar-th-trim 'hyphened (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-hyphened-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "P*")
  (ar-th-trim 'hyphened (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-forward-hyphened-atpt (&optional arg)
  "Moves forward over hyphened at point if any, does nothing otherwise.
Returns end position of hyphened "
  (interactive "p")
  (ar-th-forward 'hyphened arg))

(defun ar-backward-hyphened-atpt (&optional arg)
  "Moves backward over hyphened before point if any, does nothing otherwise.
Returns beginning position of hyphened "
  (interactive "p")
  (ar-th-backward 'hyphened arg))

(defun ar-transpose-hyphened-atpt (&optional arg)
  "Transposes hyphened at point with hyphened before if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'hyphened arg))

(defun ar-sort-hyphened-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts hypheneds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'hyphened reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-hyphened-atpt (&optional arg) 
  "Return t if a hyphened at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-hyphened-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-hyphened-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-singlequoted-atpt (&optional no-delimiters nested)
  "Returns singlequoted at point if any, nil otherwise.

With numerical ARG 2 `ar-scan-whole-buffer' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'singlequoted no-delimiters (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-singlequoted-atpt (&optional no-delimiters check)
  "Returns a list, borders of singlequoted if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'singlequoted no-delimiters (interactive-p) check))

(defun ar-singlequoted-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position singlequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'singlequoted no-delimiters (interactive-p) check))

(defun ar-singlequoted-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of singlequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'singlequoted no-delimiters (interactive-p) check))

(defun ar-beginning-of-singlequoted-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class singlequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'singlequoted no-delimiters (interactive-p) check))

(defun ar-end-of-singlequoted-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class singlequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'singlequoted no-delimiters check))

(defun ar-length-of-singlequoted-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class singlequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'singlequoted no-delimiters check))

(defun ar-copy-singlequoted-atpt (&optional no-delimiters check)
  "Returns a copy of singlequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'singlequoted no-delimiters))

(defun ar-delete-singlequoted-atpt (&optional arg)
  "Deletes singlequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'singlequoted arg arg))

(defun ar-delete-singlequoted-in-region (beg end &optional arg check no-delimiters)
  "Deletes singlequoted at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'singlequoted beg end (interactive-p)))

(defun ar-blok-singlequoted-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around singlequoted.

If region is active, do that for all elements \"singlequoted\" in region.
  Returns blok or nil if no singlequoted at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'singlequoted no-delimiters (interactive-p)))

(defun ar-doubleslash-singlequoted-atpt (&optional no-delimiters check)
  "Puts doubled slashes around singlequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'singlequoted no-delimiters))

(defun ar-backslashparen-singlequoted-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around singlequoted at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'singlequoted no-delimiters))

(defun ar-comment-singlequoted-atpt (&optional no-delimiters check)
  "Comments singlequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'singlequoted no-delimiters))

(defun ar-commatize-singlequoted-atpt (&optional no-delimiters check)
  "Put a comma after singlequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'singlequoted no-delimiters))

(defun ar-mark-singlequoted-atpt (&optional no-delimiters check)
  "Marks singlequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'singlequoted))

(defun ar-hide-singlequoted-atpt (&optional arg)
  "Hides singlequoted at point. "
  (interactive "p")
  (ar-th-hide 'singlequoted))

(defun ar-show-singlequoted-atpt (&optional arg)
  "Shows hidden singlequoted at point. "
  (interactive "p")
  (ar-th-show 'singlequoted))

(defun ar-hide-show-singlequoted-atpt (&optional arg)
  "Alternatively hides or shows singlequoted at point. "
  (interactive "p")
  (ar-th-hide-show 'singlequoted))

(defun ar-highlight-singlequoted-atpt-mode (&optional no-delimiters check)
  "Toggles singlequoted-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'singlequoted no-delimiters))

(defun ar-kill-singlequoted-atpt (&optional no-delimiters check)
  "Kills singlequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'singlequoted no-delimiters))

(defun ar-separate-singlequoted-atpt (&optional no-delimiters check)
  "Separates singlequoted at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'singlequoted (eq 4  (prefix-numeric-value no-delimiters)) check))

(defun ar-trim-singlequoted-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
 (interactive "*P")
 (ar-th-trim 'singlequoted (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-singlequoted-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*P")
  (ar-th-trim 'singlequoted (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-singlequoted-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "P*")
  (ar-th-trim 'singlequoted (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-forward-singlequoted-atpt (&optional arg)
  "Moves forward over singlequoted at point if any, does nothing otherwise.
Returns end position of singlequoted "
  (interactive "p")
  (ar-th-forward 'singlequoted arg))

(defun ar-backward-singlequoted-atpt (&optional arg)
  "Moves backward over singlequoted before point if any, does nothing otherwise.
Returns beginning position of singlequoted "
  (interactive "p")
  (ar-th-backward 'singlequoted arg))

(defun ar-transpose-singlequoted-atpt (&optional arg)
  "Transposes singlequoted at point with singlequoted before if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'singlequoted arg))

(defun ar-sort-singlequoted-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts singlequoteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'singlequoted reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-singlequoted-atpt (&optional arg) 
  "Return t if a singlequoted at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-singlequoted-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-singlequoted-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-slashed-atpt (&optional no-delimiters nested)
  "Returns slashed at point if any, nil otherwise.

With numerical ARG 2 `ar-scan-whole-buffer' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'slashed no-delimiters (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-slashed-atpt (&optional no-delimiters check)
  "Returns a list, borders of slashed if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'slashed no-delimiters (interactive-p) check))

(defun ar-slashed-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position slashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'slashed no-delimiters (interactive-p) check))

(defun ar-slashed-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of slashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'slashed no-delimiters (interactive-p) check))

(defun ar-beginning-of-slashed-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class slashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'slashed no-delimiters (interactive-p) check))

(defun ar-end-of-slashed-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class slashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'slashed no-delimiters check))

(defun ar-length-of-slashed-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class slashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'slashed no-delimiters check))

(defun ar-copy-slashed-atpt (&optional no-delimiters check)
  "Returns a copy of slashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'slashed no-delimiters))

(defun ar-delete-slashed-atpt (&optional arg)
  "Deletes slashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'slashed arg arg))

(defun ar-delete-slashed-in-region (beg end &optional arg check no-delimiters)
  "Deletes slashed at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'slashed beg end (interactive-p)))

(defun ar-blok-slashed-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around slashed.

If region is active, do that for all elements \"slashed\" in region.
  Returns blok or nil if no slashed at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'slashed no-delimiters (interactive-p)))

(defun ar-doubleslash-slashed-atpt (&optional no-delimiters check)
  "Puts doubled slashes around slashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'slashed no-delimiters))

(defun ar-backslashparen-slashed-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around slashed at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'slashed no-delimiters))

(defun ar-comment-slashed-atpt (&optional no-delimiters check)
  "Comments slashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'slashed no-delimiters))

(defun ar-commatize-slashed-atpt (&optional no-delimiters check)
  "Put a comma after slashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'slashed no-delimiters))

(defun ar-mark-slashed-atpt (&optional no-delimiters check)
  "Marks slashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'slashed))

(defun ar-hide-slashed-atpt (&optional arg)
  "Hides slashed at point. "
  (interactive "p")
  (ar-th-hide 'slashed))

(defun ar-show-slashed-atpt (&optional arg)
  "Shows hidden slashed at point. "
  (interactive "p")
  (ar-th-show 'slashed))

(defun ar-hide-show-slashed-atpt (&optional arg)
  "Alternatively hides or shows slashed at point. "
  (interactive "p")
  (ar-th-hide-show 'slashed))

(defun ar-highlight-slashed-atpt-mode (&optional no-delimiters check)
  "Toggles slashed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'slashed no-delimiters))

(defun ar-kill-slashed-atpt (&optional no-delimiters check)
  "Kills slashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'slashed no-delimiters))

(defun ar-separate-slashed-atpt (&optional no-delimiters check)
  "Separates slashed at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'slashed (eq 4  (prefix-numeric-value no-delimiters)) check))

(defun ar-trim-slashed-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
 (interactive "*P")
 (ar-th-trim 'slashed (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-slashed-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*P")
  (ar-th-trim 'slashed (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-slashed-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "P*")
  (ar-th-trim 'slashed (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-forward-slashed-atpt (&optional arg)
  "Moves forward over slashed at point if any, does nothing otherwise.
Returns end position of slashed "
  (interactive "p")
  (ar-th-forward 'slashed arg))

(defun ar-backward-slashed-atpt (&optional arg)
  "Moves backward over slashed before point if any, does nothing otherwise.
Returns beginning position of slashed "
  (interactive "p")
  (ar-th-backward 'slashed arg))

(defun ar-transpose-slashed-atpt (&optional arg)
  "Transposes slashed at point with slashed before if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'slashed arg))

(defun ar-sort-slashed-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts slasheds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'slashed reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-slashed-atpt (&optional arg) 
  "Return t if a slashed at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-slashed-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-slashed-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-stared-atpt (&optional no-delimiters nested)
  "Returns stared at point if any, nil otherwise.

With numerical ARG 2 `ar-scan-whole-buffer' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'stared no-delimiters (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-stared-atpt (&optional no-delimiters check)
  "Returns a list, borders of stared if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'stared no-delimiters (interactive-p) check))

(defun ar-stared-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position stared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'stared no-delimiters (interactive-p) check))

(defun ar-stared-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of stared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'stared no-delimiters (interactive-p) check))

(defun ar-beginning-of-stared-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class stared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'stared no-delimiters (interactive-p) check))

(defun ar-end-of-stared-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class stared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'stared no-delimiters check))

(defun ar-length-of-stared-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class stared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'stared no-delimiters check))

(defun ar-copy-stared-atpt (&optional no-delimiters check)
  "Returns a copy of stared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'stared no-delimiters))

(defun ar-delete-stared-atpt (&optional arg)
  "Deletes stared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'stared arg arg))

(defun ar-delete-stared-in-region (beg end &optional arg check no-delimiters)
  "Deletes stared at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'stared beg end (interactive-p)))

(defun ar-blok-stared-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around stared.

If region is active, do that for all elements \"stared\" in region.
  Returns blok or nil if no stared at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'stared no-delimiters (interactive-p)))

(defun ar-doubleslash-stared-atpt (&optional no-delimiters check)
  "Puts doubled slashes around stared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'stared no-delimiters))

(defun ar-backslashparen-stared-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around stared at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'stared no-delimiters))

(defun ar-comment-stared-atpt (&optional no-delimiters check)
  "Comments stared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'stared no-delimiters))

(defun ar-commatize-stared-atpt (&optional no-delimiters check)
  "Put a comma after stared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'stared no-delimiters))

(defun ar-mark-stared-atpt (&optional no-delimiters check)
  "Marks stared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'stared))

(defun ar-hide-stared-atpt (&optional arg)
  "Hides stared at point. "
  (interactive "p")
  (ar-th-hide 'stared))

(defun ar-show-stared-atpt (&optional arg)
  "Shows hidden stared at point. "
  (interactive "p")
  (ar-th-show 'stared))

(defun ar-hide-show-stared-atpt (&optional arg)
  "Alternatively hides or shows stared at point. "
  (interactive "p")
  (ar-th-hide-show 'stared))

(defun ar-highlight-stared-atpt-mode (&optional no-delimiters check)
  "Toggles stared-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'stared no-delimiters))

(defun ar-kill-stared-atpt (&optional no-delimiters check)
  "Kills stared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'stared no-delimiters))

(defun ar-separate-stared-atpt (&optional no-delimiters check)
  "Separates stared at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'stared (eq 4  (prefix-numeric-value no-delimiters)) check))

(defun ar-trim-stared-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
 (interactive "*P")
 (ar-th-trim 'stared (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-stared-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*P")
  (ar-th-trim 'stared (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-stared-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "P*")
  (ar-th-trim 'stared (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-forward-stared-atpt (&optional arg)
  "Moves forward over stared at point if any, does nothing otherwise.
Returns end position of stared "
  (interactive "p")
  (ar-th-forward 'stared arg))

(defun ar-backward-stared-atpt (&optional arg)
  "Moves backward over stared before point if any, does nothing otherwise.
Returns beginning position of stared "
  (interactive "p")
  (ar-th-backward 'stared arg))

(defun ar-transpose-stared-atpt (&optional arg)
  "Transposes stared at point with stared before if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'stared arg))

(defun ar-sort-stared-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts stareds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'stared reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-stared-atpt (&optional arg) 
  "Return t if a stared at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-stared-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-stared-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-underscored-atpt (&optional no-delimiters nested)
  "Returns underscored at point if any, nil otherwise.

With numerical ARG 2 `ar-scan-whole-buffer' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'underscored no-delimiters (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-underscored-atpt (&optional no-delimiters check)
  "Returns a list, borders of underscored if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'underscored no-delimiters (interactive-p) check))

(defun ar-underscored-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position underscored at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'underscored no-delimiters (interactive-p) check))

(defun ar-underscored-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of underscored at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'underscored no-delimiters (interactive-p) check))

(defun ar-beginning-of-underscored-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class underscored at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'underscored no-delimiters (interactive-p) check))

(defun ar-end-of-underscored-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class underscored at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'underscored no-delimiters check))

(defun ar-length-of-underscored-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class underscored at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'underscored no-delimiters check))

(defun ar-copy-underscored-atpt (&optional no-delimiters check)
  "Returns a copy of underscored at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'underscored no-delimiters))

(defun ar-delete-underscored-atpt (&optional arg)
  "Deletes underscored at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'underscored arg arg))

(defun ar-delete-underscored-in-region (beg end &optional arg check no-delimiters)
  "Deletes underscored at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'underscored beg end (interactive-p)))

(defun ar-blok-underscored-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around underscored.

If region is active, do that for all elements \"underscored\" in region.
  Returns blok or nil if no underscored at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'underscored no-delimiters (interactive-p)))

(defun ar-doubleslash-underscored-atpt (&optional no-delimiters check)
  "Puts doubled slashes around underscored at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'underscored no-delimiters))

(defun ar-backslashparen-underscored-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around underscored at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'underscored no-delimiters))

(defun ar-comment-underscored-atpt (&optional no-delimiters check)
  "Comments underscored at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'underscored no-delimiters))

(defun ar-commatize-underscored-atpt (&optional no-delimiters check)
  "Put a comma after underscored at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'underscored no-delimiters))

(defun ar-mark-underscored-atpt (&optional no-delimiters check)
  "Marks underscored at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'underscored))

(defun ar-hide-underscored-atpt (&optional arg)
  "Hides underscored at point. "
  (interactive "p")
  (ar-th-hide 'underscored))

(defun ar-show-underscored-atpt (&optional arg)
  "Shows hidden underscored at point. "
  (interactive "p")
  (ar-th-show 'underscored))

(defun ar-hide-show-underscored-atpt (&optional arg)
  "Alternatively hides or shows underscored at point. "
  (interactive "p")
  (ar-th-hide-show 'underscored))

(defun ar-highlight-underscored-atpt-mode (&optional no-delimiters check)
  "Toggles underscored-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'underscored no-delimiters))

(defun ar-kill-underscored-atpt (&optional no-delimiters check)
  "Kills underscored at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'underscored no-delimiters))

(defun ar-separate-underscored-atpt (&optional no-delimiters check)
  "Separates underscored at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'underscored (eq 4  (prefix-numeric-value no-delimiters)) check))

(defun ar-trim-underscored-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
 (interactive "*P")
 (ar-th-trim 'underscored (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-underscored-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*P")
  (ar-th-trim 'underscored (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-underscored-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "P*")
  (ar-th-trim 'underscored (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-forward-underscored-atpt (&optional arg)
  "Moves forward over underscored at point if any, does nothing otherwise.
Returns end position of underscored "
  (interactive "p")
  (ar-th-forward 'underscored arg))

(defun ar-backward-underscored-atpt (&optional arg)
  "Moves backward over underscored before point if any, does nothing otherwise.
Returns beginning position of underscored "
  (interactive "p")
  (ar-th-backward 'underscored arg))

(defun ar-transpose-underscored-atpt (&optional arg)
  "Transposes underscored at point with underscored before if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'underscored arg))

(defun ar-sort-underscored-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts underscoreds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'underscored reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-underscored-atpt (&optional arg) 
  "Return t if a underscored at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-underscored-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-underscored-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-whitespaced-atpt (&optional no-delimiters nested)
  "Returns whitespaced at point if any, nil otherwise.

With numerical ARG 2 `ar-scan-whole-buffer' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'whitespaced no-delimiters (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-whitespaced-atpt (&optional no-delimiters check)
  "Returns a list, borders of whitespaced if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'whitespaced no-delimiters (interactive-p) check))

(defun ar-whitespaced-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position whitespaced at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'whitespaced no-delimiters (interactive-p) check))

(defun ar-whitespaced-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of whitespaced at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'whitespaced no-delimiters (interactive-p) check))

(defun ar-beginning-of-whitespaced-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class whitespaced at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'whitespaced no-delimiters (interactive-p) check))

(defun ar-end-of-whitespaced-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class whitespaced at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'whitespaced no-delimiters check))

(defun ar-length-of-whitespaced-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class whitespaced at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'whitespaced no-delimiters check))

(defun ar-copy-whitespaced-atpt (&optional no-delimiters check)
  "Returns a copy of whitespaced at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'whitespaced no-delimiters))

(defun ar-delete-whitespaced-atpt (&optional arg)
  "Deletes whitespaced at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-delete 'whitespaced arg arg))

(defun ar-delete-whitespaced-in-region (beg end &optional arg check no-delimiters)
  "Deletes whitespaced at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'whitespaced beg end (interactive-p)))

(defun ar-blok-whitespaced-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around whitespaced.

If region is active, do that for all elements \"whitespaced\" in region.
  Returns blok or nil if no whitespaced at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'whitespaced no-delimiters (interactive-p)))

(defun ar-doubleslash-whitespaced-atpt (&optional no-delimiters check)
  "Puts doubled slashes around whitespaced at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'whitespaced no-delimiters))

(defun ar-backslashparen-whitespaced-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around whitespaced at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'whitespaced no-delimiters))

(defun ar-comment-whitespaced-atpt (&optional no-delimiters check)
  "Comments whitespaced at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'whitespaced no-delimiters))

(defun ar-commatize-whitespaced-atpt (&optional no-delimiters check)
  "Put a comma after whitespaced at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'whitespaced no-delimiters))

(defun ar-mark-whitespaced-atpt (&optional no-delimiters check)
  "Marks whitespaced at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'whitespaced))

(defun ar-hide-whitespaced-atpt (&optional arg)
  "Hides whitespaced at point. "
  (interactive "p")
  (ar-th-hide 'whitespaced))

(defun ar-show-whitespaced-atpt (&optional arg)
  "Shows hidden whitespaced at point. "
  (interactive "p")
  (ar-th-show 'whitespaced))

(defun ar-hide-show-whitespaced-atpt (&optional arg)
  "Alternatively hides or shows whitespaced at point. "
  (interactive "p")
  (ar-th-hide-show 'whitespaced))

(defun ar-highlight-whitespaced-atpt-mode (&optional no-delimiters check)
  "Toggles whitespaced-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'whitespaced no-delimiters))

(defun ar-kill-whitespaced-atpt (&optional no-delimiters check)
  "Kills whitespaced at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'whitespaced no-delimiters))

(defun ar-separate-whitespaced-atpt (&optional no-delimiters check)
  "Separates whitespaced at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'whitespaced (eq 4  (prefix-numeric-value no-delimiters)) check))

(defun ar-trim-whitespaced-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
 (interactive "*P")
 (ar-th-trim 'whitespaced (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-whitespaced-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*P")
  (ar-th-trim 'whitespaced (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-whitespaced-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "P*")
  (ar-th-trim 'whitespaced (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-forward-whitespaced-atpt (&optional arg)
  "Moves forward over whitespaced at point if any, does nothing otherwise.
Returns end position of whitespaced "
  (interactive "p")
  (ar-th-forward 'whitespaced arg))

(defun ar-backward-whitespaced-atpt (&optional arg)
  "Moves backward over whitespaced before point if any, does nothing otherwise.
Returns beginning position of whitespaced "
  (interactive "p")
  (ar-th-backward 'whitespaced arg))

(defun ar-transpose-whitespaced-atpt (&optional arg)
  "Transposes whitespaced at point with whitespaced before if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'whitespaced arg))

(defun ar-sort-whitespaced-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts whitespaceds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'whitespaced reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-whitespaced-atpt (&optional arg) 
  "Return t if a whitespaced at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-whitespaced-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-whitespaced-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;; ar-thing-at-point-utils-delimited-intern: ar-unpaired-delimited-raw: end

;; ar-thing-at-point-utils-delimited-unpaired-anlegen: ar-unpaired-delimited-raw: start 

(defun ar-in-backslashed-p-atpt (&optional arg condition)
  "Returns beginning position of ` backslashed' if inside, a number or a list, nil otherwise.

Optional CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive "p")
  (let ((erg (ar-in-delimiter-base "\\\\" condition)))
    (when arg (message "%s" erg))
    erg))

(defun ar-in-backticked-p-atpt (&optional arg condition)
  "Returns beginning position of ` backticked' if inside, a number or a list, nil otherwise.

Optional CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive "p")
  (let ((erg (ar-in-delimiter-base "`" condition)))
    (when arg (message "%s" erg))
    erg))

(defun ar-in-coloned-p-atpt (&optional arg condition)
  "Returns beginning position of ` coloned' if inside, a number or a list, nil otherwise.

Optional CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive "p")
  (let ((erg (ar-in-delimiter-base ":" condition)))
    (when arg (message "%s" erg))
    erg))

(defun ar-in-dollared-p-atpt (&optional arg condition)
  "Returns beginning position of ` dollared' if inside, a number or a list, nil otherwise.

Optional CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive "p")
  (let ((erg (ar-in-delimiter-base "\\$" condition)))
    (when arg (message "%s" erg))
    erg))

(defun ar-in-doublequoted-p-atpt (&optional arg condition)
  "Returns beginning position of ` doublequoted' if inside, a number or a list, nil otherwise.

Optional CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive "p")
  (let ((erg (ar-in-delimiter-base "\"" condition)))
    (when arg (message "%s" erg))
    erg))

(defun ar-in-equalized-p-atpt (&optional arg condition)
  "Returns beginning position of ` equalized' if inside, a number or a list, nil otherwise.

Optional CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive "p")
  (let ((erg (ar-in-delimiter-base "=" condition)))
    (when arg (message "%s" erg))
    erg))

(defun ar-in-hyphened-p-atpt (&optional arg condition)
  "Returns beginning position of ` hyphened' if inside, a number or a list, nil otherwise.

Optional CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive "p")
  (let ((erg (ar-in-delimiter-base "-" condition)))
    (when arg (message "%s" erg))
    erg))

(defun ar-in-singlequoted-p-atpt (&optional arg condition)
  "Returns beginning position of ` singlequoted' if inside, a number or a list, nil otherwise.

Optional CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive "p")
  (let ((erg (ar-in-delimiter-base "'" condition)))
    (when arg (message "%s" erg))
    erg))

(defun ar-in-slashed-p-atpt (&optional arg condition)
  "Returns beginning position of ` slashed' if inside, a number or a list, nil otherwise.

Optional CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive "p")
  (let ((erg (ar-in-delimiter-base "/" condition)))
    (when arg (message "%s" erg))
    erg))

(defun ar-in-stared-p-atpt (&optional arg condition)
  "Returns beginning position of ` stared' if inside, a number or a list, nil otherwise.

Optional CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive "p")
  (let ((erg (ar-in-delimiter-base "*" condition)))
    (when arg (message "%s" erg))
    erg))

(defun ar-in-underscored-p-atpt (&optional arg condition)
  "Returns beginning position of ` underscored' if inside, a number or a list, nil otherwise.

Optional CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive "p")
  (let ((erg (ar-in-delimiter-base "_" condition)))
    (when arg (message "%s" erg))
    erg))

(defun ar-in-whitespaced-p-atpt (&optional arg condition)
  "Returns beginning position of ` whitespaced' if inside, a number or a list, nil otherwise.

Optional CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive "p")
  (let ((erg (ar-in-delimiter-base " " condition)))
    (when arg (message "%s" erg))
    erg))

;; ar-thing-at-point-utils-delimited-unpaired-anlegen: ar-unpaired-delimited-raw: end

;; ar-thing-at-point-utils-delimiters-core: ar-paired-delimited-passiv: start

(defun ar-braced-atpt (&optional no-delimiters)
  "Returns braced at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'braced no-delimiters))

(defun ar-bounds-of-braced-atpt (&optional no-delimiters check)
  "Returns a list, borders of braced if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'braced no-delimiters))

(defun ar-braced-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position BRACED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'braced no-delimiters))

(defun ar-braced-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of BRACED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'braced no-delimiters))

(defun ar-beginning-of-braced-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class BRACED at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'braced no-delimiters))

(defun ar-end-of-braced-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class BRACED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'braced no-delimiters))

(defun ar-in-braced-p-atpt (&optional no-delimiters check)
  "Returns bounds of BRACED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'braced no-delimiters))

(defun ar-length-of-braced-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class BRACED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'braced no-delimiters))

(defun ar-copy-braced-atpt (&optional no-delimiters check)
  "Returns a copy of BRACED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'braced no-delimiters))

(defun ar-delete-braced-atpt (&optional arg)
  "Deletes BRACED at point if any. "
  (interactive "*P")
  (ar-th-delete 'braced arg))

(defun ar-delete-braced-in-region (beg end)
  "Deletes BRACED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'braced beg end))

(defun ar-blok-braced-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around braced.
  Returns blok or nil if no BRACED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'braced no-delimiters))

(defun ar-backslashparen-braced-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around braced at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'braced no-delimiters))

(defun ar-doublebackslash-braced-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around BRACED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'braced no-delimiters))

(defun ar-doubleslash-braced-atpt (&optional no-delimiters check)
  "Puts doubled slashes around BRACED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'braced no-delimiters))

(defun ar-doublebackslashparen-braced-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around BRACED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'braced no-delimiters))

(defun ar-doublebacktick-braced-atpt (&optional no-delimiters check)
  "Provides double backticks around BRACED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'braced no-delimiters))

(defun ar-slashparen-braced-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around BRACED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'braced no-delimiters))

(defun ar-slashparen-braced-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around BRACED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'braced no-delimiters))

(defun ar-comment-braced-atpt (&optional no-delimiters check)
  "Comments BRACED at point if any. "
  (interactive "*P")
  (ar-th-comment 'braced no-delimiters))

(defun ar-commatize-braced-atpt (&optional no-delimiters check)
  "Put a comma after BRACED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'braced no-delimiters))

(defun ar-quote-braced-atpt (&optional no-delimiters check)
  "Put a singlequote before BRACED at point if any. "
  (interactive "*P")
  (ar-th-quote 'braced no-delimiters))


(defun ar-mark-braced-atpt (&optional arg)
  "Marks BRACED at point if any. "
  (interactive "P")
  (ar-th-mark 'braced))

(defun ar-hide-braced-atpt (&optional arg)
  "Hides BRACED at point. "
  (interactive "P")
  (ar-th-hide 'braced))

(defun ar-show-braced-atpt (&optional arg)
  "Shows hidden BRACED at point. "
  (interactive "P")
  (ar-th-show 'braced))

(defun ar-hide-show-braced-atpt (&optional arg)
  "Alternatively hides or shows BRACED at point. "
  (interactive "P")
  (ar-th-hide-show 'braced))

(defun ar-highlight-braced-atpt-mode (&optional no-delimiters check)
  "Toggles braced-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'braced no-delimiters))

(defun ar-kill-braced-atpt (&optional no-delimiters check)
  "Kills BRACED at point if any. "
  (interactive "*P")
  (ar-th-kill 'braced no-delimiters))

(defun ar-curvedsinglequote-braced-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'braced no-delimiters))

(defun ar-separate-braced-atpt (&optional no-delimiters check)
  "Separates BRACED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'braced no-delimiters))

(defun ar-triplequotedq-braced-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around braced. "
  (interactive "*P")
  (ar-th-triplequotedq 'braced no-delimiters))

(defun ar-triplequotesq-braced-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around braced. "
  (interactive "*P")
  (ar-th-triplequotesq 'braced no-delimiters))

(defun ar-triplebacktick-braced-atpt (&optional arg)
  "Deletes braced at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'braced arg))

(defun ar-trim-braced-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'braced no-delimiters iact check t t))

(defun ar-left-trim-braced-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'braced no-delimiters iact check t))

(defun ar-right-trim-braced-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'braced n no-delimiters iact check nil t))

(defun ar-underscore-braced-atpt (&optional no-delimiters check)
  "Put underscore char around BRACED. "
  (interactive "*P")
  (ar-th-underscore 'braced no-delimiters))

;; (defalias 'ar-braced-whitespace-atpt 'ar-whitespace-braced-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-braced-atpt (&optional no-delimiters check)
;;   "Put whitespace char around BRACED. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'braced nil t))

(defun ar-forward-braced-atpt (&optional arg)
  "Moves forward over BRACED at point if any, does nothing otherwise.
Returns end position of BRACED "
  (interactive "P")
  (ar-th-forward 'braced arg))

(defun ar-backward-braced-atpt (&optional arg)
  "Moves backward over BRACED before point if any, does nothing otherwise.
Returns beginning position of BRACED "
  (interactive "P")
  (ar-th-backward 'braced arg))

(defun ar-transpose-braced-atpt (&optional arg)
  "Transposes BRACED with BRACED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'braced arg))

(defun ar-sort-braced-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts braceds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'braced reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-braced-atpt (&optional arg) 
  "Return t if a BRACED at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-braced-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-braced-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-bracketed-atpt (&optional no-delimiters)
  "Returns bracketed at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'bracketed no-delimiters))

(defun ar-bounds-of-bracketed-atpt (&optional no-delimiters check)
  "Returns a list, borders of bracketed if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'bracketed no-delimiters))

(defun ar-bracketed-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position BRACKETED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'bracketed no-delimiters))

(defun ar-bracketed-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of BRACKETED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'bracketed no-delimiters))

(defun ar-beginning-of-bracketed-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class BRACKETED at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'bracketed no-delimiters))

(defun ar-end-of-bracketed-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class BRACKETED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'bracketed no-delimiters))

(defun ar-in-bracketed-p-atpt (&optional no-delimiters check)
  "Returns bounds of BRACKETED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'bracketed no-delimiters))

(defun ar-length-of-bracketed-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class BRACKETED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'bracketed no-delimiters))

(defun ar-copy-bracketed-atpt (&optional no-delimiters check)
  "Returns a copy of BRACKETED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'bracketed no-delimiters))

(defun ar-delete-bracketed-atpt (&optional arg)
  "Deletes BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-delete 'bracketed arg))

(defun ar-delete-bracketed-in-region (beg end)
  "Deletes BRACKETED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'bracketed beg end))

(defun ar-blok-bracketed-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around bracketed.
  Returns blok or nil if no BRACKETED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'bracketed no-delimiters))

(defun ar-backslashparen-bracketed-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around bracketed at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'bracketed no-delimiters))

(defun ar-doublebackslash-bracketed-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'bracketed no-delimiters))

(defun ar-doubleslash-bracketed-atpt (&optional no-delimiters check)
  "Puts doubled slashes around BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'bracketed no-delimiters))

(defun ar-doublebackslashparen-bracketed-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'bracketed no-delimiters))

(defun ar-doublebacktick-bracketed-atpt (&optional no-delimiters check)
  "Provides double backticks around BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'bracketed no-delimiters))

(defun ar-slashparen-bracketed-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'bracketed no-delimiters))

(defun ar-slashparen-bracketed-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'bracketed no-delimiters))

(defun ar-comment-bracketed-atpt (&optional no-delimiters check)
  "Comments BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-comment 'bracketed no-delimiters))

(defun ar-commatize-bracketed-atpt (&optional no-delimiters check)
  "Put a comma after BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'bracketed no-delimiters))

(defun ar-quote-bracketed-atpt (&optional no-delimiters check)
  "Put a singlequote before BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-quote 'bracketed no-delimiters))


(defun ar-mark-bracketed-atpt (&optional arg)
  "Marks BRACKETED at point if any. "
  (interactive "P")
  (ar-th-mark 'bracketed))

(defun ar-hide-bracketed-atpt (&optional arg)
  "Hides BRACKETED at point. "
  (interactive "P")
  (ar-th-hide 'bracketed))

(defun ar-show-bracketed-atpt (&optional arg)
  "Shows hidden BRACKETED at point. "
  (interactive "P")
  (ar-th-show 'bracketed))

(defun ar-hide-show-bracketed-atpt (&optional arg)
  "Alternatively hides or shows BRACKETED at point. "
  (interactive "P")
  (ar-th-hide-show 'bracketed))

(defun ar-highlight-bracketed-atpt-mode (&optional no-delimiters check)
  "Toggles bracketed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'bracketed no-delimiters))

(defun ar-kill-bracketed-atpt (&optional no-delimiters check)
  "Kills BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-kill 'bracketed no-delimiters))

(defun ar-curvedsinglequote-bracketed-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'bracketed no-delimiters))

(defun ar-separate-bracketed-atpt (&optional no-delimiters check)
  "Separates BRACKETED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'bracketed no-delimiters))

(defun ar-triplequotedq-bracketed-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around bracketed. "
  (interactive "*P")
  (ar-th-triplequotedq 'bracketed no-delimiters))

(defun ar-triplequotesq-bracketed-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around bracketed. "
  (interactive "*P")
  (ar-th-triplequotesq 'bracketed no-delimiters))

(defun ar-triplebacktick-bracketed-atpt (&optional arg)
  "Deletes bracketed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'bracketed arg))

(defun ar-trim-bracketed-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'bracketed no-delimiters iact check t t))

(defun ar-left-trim-bracketed-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'bracketed no-delimiters iact check t))

(defun ar-right-trim-bracketed-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'bracketed n no-delimiters iact check nil t))

(defun ar-underscore-bracketed-atpt (&optional no-delimiters check)
  "Put underscore char around BRACKETED. "
  (interactive "*P")
  (ar-th-underscore 'bracketed no-delimiters))

;; (defalias 'ar-bracketed-whitespace-atpt 'ar-whitespace-bracketed-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-bracketed-atpt (&optional no-delimiters check)
;;   "Put whitespace char around BRACKETED. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'bracketed nil t))

(defun ar-forward-bracketed-atpt (&optional arg)
  "Moves forward over BRACKETED at point if any, does nothing otherwise.
Returns end position of BRACKETED "
  (interactive "P")
  (ar-th-forward 'bracketed arg))

(defun ar-backward-bracketed-atpt (&optional arg)
  "Moves backward over BRACKETED before point if any, does nothing otherwise.
Returns beginning position of BRACKETED "
  (interactive "P")
  (ar-th-backward 'bracketed arg))

(defun ar-transpose-bracketed-atpt (&optional arg)
  "Transposes BRACKETED with BRACKETED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'bracketed arg))

(defun ar-sort-bracketed-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts bracketeds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'bracketed reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-bracketed-atpt (&optional arg) 
  "Return t if a BRACKETED at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-bracketed-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-bracketed-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-lesserangled-atpt (&optional no-delimiters)
  "Returns lesserangled at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'lesserangled no-delimiters))

(defun ar-bounds-of-lesserangled-atpt (&optional no-delimiters check)
  "Returns a list, borders of lesserangled if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'lesserangled no-delimiters))

(defun ar-lesserangled-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position LESSERANGLED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'lesserangled no-delimiters))

(defun ar-lesserangled-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of LESSERANGLED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'lesserangled no-delimiters))

(defun ar-beginning-of-lesserangled-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class LESSERANGLED at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'lesserangled no-delimiters))

(defun ar-end-of-lesserangled-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class LESSERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'lesserangled no-delimiters))

(defun ar-in-lesserangled-p-atpt (&optional no-delimiters check)
  "Returns bounds of LESSERANGLED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'lesserangled no-delimiters))

(defun ar-length-of-lesserangled-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class LESSERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'lesserangled no-delimiters))

(defun ar-copy-lesserangled-atpt (&optional no-delimiters check)
  "Returns a copy of LESSERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'lesserangled no-delimiters))

(defun ar-delete-lesserangled-atpt (&optional arg)
  "Deletes LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-delete 'lesserangled arg))

(defun ar-delete-lesserangled-in-region (beg end)
  "Deletes LESSERANGLED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'lesserangled beg end))

(defun ar-blok-lesserangled-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around lesserangled.
  Returns blok or nil if no LESSERANGLED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'lesserangled no-delimiters))

(defun ar-backslashparen-lesserangled-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around lesserangled at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'lesserangled no-delimiters))

(defun ar-doublebackslash-lesserangled-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'lesserangled no-delimiters))

(defun ar-doubleslash-lesserangled-atpt (&optional no-delimiters check)
  "Puts doubled slashes around LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'lesserangled no-delimiters))

(defun ar-doublebackslashparen-lesserangled-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'lesserangled no-delimiters))

(defun ar-doublebacktick-lesserangled-atpt (&optional no-delimiters check)
  "Provides double backticks around LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'lesserangled no-delimiters))

(defun ar-slashparen-lesserangled-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'lesserangled no-delimiters))

(defun ar-slashparen-lesserangled-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'lesserangled no-delimiters))

(defun ar-comment-lesserangled-atpt (&optional no-delimiters check)
  "Comments LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-comment 'lesserangled no-delimiters))

(defun ar-commatize-lesserangled-atpt (&optional no-delimiters check)
  "Put a comma after LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'lesserangled no-delimiters))

(defun ar-quote-lesserangled-atpt (&optional no-delimiters check)
  "Put a singlequote before LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-quote 'lesserangled no-delimiters))


(defun ar-mark-lesserangled-atpt (&optional arg)
  "Marks LESSERANGLED at point if any. "
  (interactive "P")
  (ar-th-mark 'lesserangled))

(defun ar-hide-lesserangled-atpt (&optional arg)
  "Hides LESSERANGLED at point. "
  (interactive "P")
  (ar-th-hide 'lesserangled))

(defun ar-show-lesserangled-atpt (&optional arg)
  "Shows hidden LESSERANGLED at point. "
  (interactive "P")
  (ar-th-show 'lesserangled))

(defun ar-hide-show-lesserangled-atpt (&optional arg)
  "Alternatively hides or shows LESSERANGLED at point. "
  (interactive "P")
  (ar-th-hide-show 'lesserangled))

(defun ar-highlight-lesserangled-atpt-mode (&optional no-delimiters check)
  "Toggles lesserangled-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'lesserangled no-delimiters))

(defun ar-kill-lesserangled-atpt (&optional no-delimiters check)
  "Kills LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-kill 'lesserangled no-delimiters))

(defun ar-curvedsinglequote-lesserangled-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'lesserangled no-delimiters))

(defun ar-separate-lesserangled-atpt (&optional no-delimiters check)
  "Separates LESSERANGLED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'lesserangled no-delimiters))

(defun ar-triplequotedq-lesserangled-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around lesserangled. "
  (interactive "*P")
  (ar-th-triplequotedq 'lesserangled no-delimiters))

(defun ar-triplequotesq-lesserangled-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around lesserangled. "
  (interactive "*P")
  (ar-th-triplequotesq 'lesserangled no-delimiters))

(defun ar-triplebacktick-lesserangled-atpt (&optional arg)
  "Deletes lesserangled at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'lesserangled arg))

(defun ar-trim-lesserangled-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'lesserangled no-delimiters iact check t t))

(defun ar-left-trim-lesserangled-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'lesserangled no-delimiters iact check t))

(defun ar-right-trim-lesserangled-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'lesserangled n no-delimiters iact check nil t))

(defun ar-underscore-lesserangled-atpt (&optional no-delimiters check)
  "Put underscore char around LESSERANGLED. "
  (interactive "*P")
  (ar-th-underscore 'lesserangled no-delimiters))

;; (defalias 'ar-lesserangled-whitespace-atpt 'ar-whitespace-lesserangled-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-lesserangled-atpt (&optional no-delimiters check)
;;   "Put whitespace char around LESSERANGLED. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'lesserangled nil t))

(defun ar-forward-lesserangled-atpt (&optional arg)
  "Moves forward over LESSERANGLED at point if any, does nothing otherwise.
Returns end position of LESSERANGLED "
  (interactive "P")
  (ar-th-forward 'lesserangled arg))

(defun ar-backward-lesserangled-atpt (&optional arg)
  "Moves backward over LESSERANGLED before point if any, does nothing otherwise.
Returns beginning position of LESSERANGLED "
  (interactive "P")
  (ar-th-backward 'lesserangled arg))

(defun ar-transpose-lesserangled-atpt (&optional arg)
  "Transposes LESSERANGLED with LESSERANGLED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'lesserangled arg))

(defun ar-sort-lesserangled-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts lesserangleds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'lesserangled reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-lesserangled-atpt (&optional arg) 
  "Return t if a LESSERANGLED at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-lesserangled-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-lesserangled-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-greaterangled-atpt (&optional no-delimiters)
  "Returns greaterangled at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'greaterangled no-delimiters))

(defun ar-bounds-of-greaterangled-atpt (&optional no-delimiters check)
  "Returns a list, borders of greaterangled if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'greaterangled no-delimiters))

(defun ar-greaterangled-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position GREATERANGLED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'greaterangled no-delimiters))

(defun ar-greaterangled-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of GREATERANGLED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'greaterangled no-delimiters))

(defun ar-beginning-of-greaterangled-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class GREATERANGLED at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'greaterangled no-delimiters))

(defun ar-end-of-greaterangled-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class GREATERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'greaterangled no-delimiters))

(defun ar-in-greaterangled-p-atpt (&optional no-delimiters check)
  "Returns bounds of GREATERANGLED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'greaterangled no-delimiters))

(defun ar-length-of-greaterangled-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class GREATERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'greaterangled no-delimiters))

(defun ar-copy-greaterangled-atpt (&optional no-delimiters check)
  "Returns a copy of GREATERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'greaterangled no-delimiters))

(defun ar-delete-greaterangled-atpt (&optional arg)
  "Deletes GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-delete 'greaterangled arg))

(defun ar-delete-greaterangled-in-region (beg end)
  "Deletes GREATERANGLED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'greaterangled beg end))

(defun ar-blok-greaterangled-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around greaterangled.
  Returns blok or nil if no GREATERANGLED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'greaterangled no-delimiters))

(defun ar-backslashparen-greaterangled-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around greaterangled at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'greaterangled no-delimiters))

(defun ar-doublebackslash-greaterangled-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'greaterangled no-delimiters))

(defun ar-doubleslash-greaterangled-atpt (&optional no-delimiters check)
  "Puts doubled slashes around GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'greaterangled no-delimiters))

(defun ar-doublebackslashparen-greaterangled-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'greaterangled no-delimiters))

(defun ar-doublebacktick-greaterangled-atpt (&optional no-delimiters check)
  "Provides double backticks around GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'greaterangled no-delimiters))

(defun ar-slashparen-greaterangled-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'greaterangled no-delimiters))

(defun ar-slashparen-greaterangled-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'greaterangled no-delimiters))

(defun ar-comment-greaterangled-atpt (&optional no-delimiters check)
  "Comments GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-comment 'greaterangled no-delimiters))

(defun ar-commatize-greaterangled-atpt (&optional no-delimiters check)
  "Put a comma after GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'greaterangled no-delimiters))

(defun ar-quote-greaterangled-atpt (&optional no-delimiters check)
  "Put a singlequote before GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-quote 'greaterangled no-delimiters))


(defun ar-mark-greaterangled-atpt (&optional arg)
  "Marks GREATERANGLED at point if any. "
  (interactive "P")
  (ar-th-mark 'greaterangled))

(defun ar-hide-greaterangled-atpt (&optional arg)
  "Hides GREATERANGLED at point. "
  (interactive "P")
  (ar-th-hide 'greaterangled))

(defun ar-show-greaterangled-atpt (&optional arg)
  "Shows hidden GREATERANGLED at point. "
  (interactive "P")
  (ar-th-show 'greaterangled))

(defun ar-hide-show-greaterangled-atpt (&optional arg)
  "Alternatively hides or shows GREATERANGLED at point. "
  (interactive "P")
  (ar-th-hide-show 'greaterangled))

(defun ar-highlight-greaterangled-atpt-mode (&optional no-delimiters check)
  "Toggles greaterangled-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'greaterangled no-delimiters))

(defun ar-kill-greaterangled-atpt (&optional no-delimiters check)
  "Kills GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-kill 'greaterangled no-delimiters))

(defun ar-curvedsinglequote-greaterangled-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'greaterangled no-delimiters))

(defun ar-separate-greaterangled-atpt (&optional no-delimiters check)
  "Separates GREATERANGLED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'greaterangled no-delimiters))

(defun ar-triplequotedq-greaterangled-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around greaterangled. "
  (interactive "*P")
  (ar-th-triplequotedq 'greaterangled no-delimiters))

(defun ar-triplequotesq-greaterangled-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around greaterangled. "
  (interactive "*P")
  (ar-th-triplequotesq 'greaterangled no-delimiters))

(defun ar-triplebacktick-greaterangled-atpt (&optional arg)
  "Deletes greaterangled at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'greaterangled arg))

(defun ar-trim-greaterangled-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'greaterangled no-delimiters iact check t t))

(defun ar-left-trim-greaterangled-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'greaterangled no-delimiters iact check t))

(defun ar-right-trim-greaterangled-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'greaterangled n no-delimiters iact check nil t))

(defun ar-underscore-greaterangled-atpt (&optional no-delimiters check)
  "Put underscore char around GREATERANGLED. "
  (interactive "*P")
  (ar-th-underscore 'greaterangled no-delimiters))

;; (defalias 'ar-greaterangled-whitespace-atpt 'ar-whitespace-greaterangled-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-greaterangled-atpt (&optional no-delimiters check)
;;   "Put whitespace char around GREATERANGLED. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'greaterangled nil t))

(defun ar-forward-greaterangled-atpt (&optional arg)
  "Moves forward over GREATERANGLED at point if any, does nothing otherwise.
Returns end position of GREATERANGLED "
  (interactive "P")
  (ar-th-forward 'greaterangled arg))

(defun ar-backward-greaterangled-atpt (&optional arg)
  "Moves backward over GREATERANGLED before point if any, does nothing otherwise.
Returns beginning position of GREATERANGLED "
  (interactive "P")
  (ar-th-backward 'greaterangled arg))

(defun ar-transpose-greaterangled-atpt (&optional arg)
  "Transposes GREATERANGLED with GREATERANGLED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'greaterangled arg))

(defun ar-sort-greaterangled-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts greaterangleds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'greaterangled reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-greaterangled-atpt (&optional arg) 
  "Return t if a GREATERANGLED at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-greaterangled-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-greaterangled-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-curvedsinglequoted-atpt (&optional no-delimiters)
  "Returns curvedsinglequoted at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'curvedsinglequoted no-delimiters))

(defun ar-bounds-of-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Returns a list, borders of curvedsinglequoted if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'curvedsinglequoted no-delimiters))

(defun ar-curvedsinglequoted-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position CURVEDSINGLEQUOTED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'curvedsinglequoted no-delimiters))

(defun ar-curvedsinglequoted-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of CURVEDSINGLEQUOTED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'curvedsinglequoted no-delimiters))

(defun ar-beginning-of-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class CURVEDSINGLEQUOTED at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'curvedsinglequoted no-delimiters))

(defun ar-end-of-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class CURVEDSINGLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'curvedsinglequoted no-delimiters))

(defun ar-in-curvedsinglequoted-p-atpt (&optional no-delimiters check)
  "Returns bounds of CURVEDSINGLEQUOTED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'curvedsinglequoted no-delimiters))

(defun ar-length-of-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class CURVEDSINGLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'curvedsinglequoted no-delimiters))

(defun ar-copy-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Returns a copy of CURVEDSINGLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'curvedsinglequoted no-delimiters))

(defun ar-delete-curvedsinglequoted-atpt (&optional arg)
  "Deletes CURVEDSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-delete 'curvedsinglequoted arg))

(defun ar-delete-curvedsinglequoted-in-region (beg end)
  "Deletes CURVEDSINGLEQUOTED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'curvedsinglequoted beg end))

(defun ar-blok-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around curvedsinglequoted.
  Returns blok or nil if no CURVEDSINGLEQUOTED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'curvedsinglequoted no-delimiters))

(defun ar-backslashparen-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around curvedsinglequoted at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'curvedsinglequoted no-delimiters))

(defun ar-doublebackslash-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around CURVEDSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'curvedsinglequoted no-delimiters))

(defun ar-doubleslash-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Puts doubled slashes around CURVEDSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'curvedsinglequoted no-delimiters))

(defun ar-doublebackslashparen-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around CURVEDSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'curvedsinglequoted no-delimiters))

(defun ar-doublebacktick-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Provides double backticks around CURVEDSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'curvedsinglequoted no-delimiters))

(defun ar-slashparen-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around CURVEDSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'curvedsinglequoted no-delimiters))

(defun ar-slashparen-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around CURVEDSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'curvedsinglequoted no-delimiters))

(defun ar-comment-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Comments CURVEDSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-comment 'curvedsinglequoted no-delimiters))

(defun ar-commatize-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Put a comma after CURVEDSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'curvedsinglequoted no-delimiters))

(defun ar-quote-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Put a singlequote before CURVEDSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-quote 'curvedsinglequoted no-delimiters))


(defun ar-mark-curvedsinglequoted-atpt (&optional arg)
  "Marks CURVEDSINGLEQUOTED at point if any. "
  (interactive "P")
  (ar-th-mark 'curvedsinglequoted))

(defun ar-hide-curvedsinglequoted-atpt (&optional arg)
  "Hides CURVEDSINGLEQUOTED at point. "
  (interactive "P")
  (ar-th-hide 'curvedsinglequoted))

(defun ar-show-curvedsinglequoted-atpt (&optional arg)
  "Shows hidden CURVEDSINGLEQUOTED at point. "
  (interactive "P")
  (ar-th-show 'curvedsinglequoted))

(defun ar-hide-show-curvedsinglequoted-atpt (&optional arg)
  "Alternatively hides or shows CURVEDSINGLEQUOTED at point. "
  (interactive "P")
  (ar-th-hide-show 'curvedsinglequoted))

(defun ar-highlight-curvedsinglequoted-atpt-mode (&optional no-delimiters check)
  "Toggles curvedsinglequoted-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'curvedsinglequoted no-delimiters))

(defun ar-kill-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Kills CURVEDSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-kill 'curvedsinglequoted no-delimiters))

(defun ar-curvedsinglequote-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'curvedsinglequoted no-delimiters))

(defun ar-separate-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Separates CURVEDSINGLEQUOTED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'curvedsinglequoted no-delimiters))

(defun ar-triplequotedq-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around curvedsinglequoted. "
  (interactive "*P")
  (ar-th-triplequotedq 'curvedsinglequoted no-delimiters))

(defun ar-triplequotesq-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around curvedsinglequoted. "
  (interactive "*P")
  (ar-th-triplequotesq 'curvedsinglequoted no-delimiters))

(defun ar-triplebacktick-curvedsinglequoted-atpt (&optional arg)
  "Deletes curvedsinglequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'curvedsinglequoted arg))

(defun ar-trim-curvedsinglequoted-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'curvedsinglequoted no-delimiters iact check t t))

(defun ar-left-trim-curvedsinglequoted-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'curvedsinglequoted no-delimiters iact check t))

(defun ar-right-trim-curvedsinglequoted-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'curvedsinglequoted n no-delimiters iact check nil t))

(defun ar-underscore-curvedsinglequoted-atpt (&optional no-delimiters check)
  "Put underscore char around CURVEDSINGLEQUOTED. "
  (interactive "*P")
  (ar-th-underscore 'curvedsinglequoted no-delimiters))

;; (defalias 'ar-curvedsinglequoted-whitespace-atpt 'ar-whitespace-curvedsinglequoted-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-curvedsinglequoted-atpt (&optional no-delimiters check)
;;   "Put whitespace char around CURVEDSINGLEQUOTED. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'curvedsinglequoted nil t))

(defun ar-forward-curvedsinglequoted-atpt (&optional arg)
  "Moves forward over CURVEDSINGLEQUOTED at point if any, does nothing otherwise.
Returns end position of CURVEDSINGLEQUOTED "
  (interactive "P")
  (ar-th-forward 'curvedsinglequoted arg))

(defun ar-backward-curvedsinglequoted-atpt (&optional arg)
  "Moves backward over CURVEDSINGLEQUOTED before point if any, does nothing otherwise.
Returns beginning position of CURVEDSINGLEQUOTED "
  (interactive "P")
  (ar-th-backward 'curvedsinglequoted arg))

(defun ar-transpose-curvedsinglequoted-atpt (&optional arg)
  "Transposes CURVEDSINGLEQUOTED with CURVEDSINGLEQUOTED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'curvedsinglequoted arg))

(defun ar-sort-curvedsinglequoted-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts curvedsinglequoteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'curvedsinglequoted reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-curvedsinglequoted-atpt (&optional arg) 
  "Return t if a CURVEDSINGLEQUOTED at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-curvedsinglequoted-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-curvedsinglequoted-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-curveddoublequoted-atpt (&optional no-delimiters)
  "Returns curveddoublequoted at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'curveddoublequoted no-delimiters))

(defun ar-bounds-of-curveddoublequoted-atpt (&optional no-delimiters check)
  "Returns a list, borders of curveddoublequoted if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'curveddoublequoted no-delimiters))

(defun ar-curveddoublequoted-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position CURVEDDOUBLEQUOTED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'curveddoublequoted no-delimiters))

(defun ar-curveddoublequoted-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of CURVEDDOUBLEQUOTED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'curveddoublequoted no-delimiters))

(defun ar-beginning-of-curveddoublequoted-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class CURVEDDOUBLEQUOTED at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'curveddoublequoted no-delimiters))

(defun ar-end-of-curveddoublequoted-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class CURVEDDOUBLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'curveddoublequoted no-delimiters))

(defun ar-in-curveddoublequoted-p-atpt (&optional no-delimiters check)
  "Returns bounds of CURVEDDOUBLEQUOTED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'curveddoublequoted no-delimiters))

(defun ar-length-of-curveddoublequoted-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class CURVEDDOUBLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'curveddoublequoted no-delimiters))

(defun ar-copy-curveddoublequoted-atpt (&optional no-delimiters check)
  "Returns a copy of CURVEDDOUBLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'curveddoublequoted no-delimiters))

(defun ar-delete-curveddoublequoted-atpt (&optional arg)
  "Deletes CURVEDDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-delete 'curveddoublequoted arg))

(defun ar-delete-curveddoublequoted-in-region (beg end)
  "Deletes CURVEDDOUBLEQUOTED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'curveddoublequoted beg end))

(defun ar-blok-curveddoublequoted-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around curveddoublequoted.
  Returns blok or nil if no CURVEDDOUBLEQUOTED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'curveddoublequoted no-delimiters))

(defun ar-backslashparen-curveddoublequoted-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around curveddoublequoted at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'curveddoublequoted no-delimiters))

(defun ar-doublebackslash-curveddoublequoted-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around CURVEDDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'curveddoublequoted no-delimiters))

(defun ar-doubleslash-curveddoublequoted-atpt (&optional no-delimiters check)
  "Puts doubled slashes around CURVEDDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'curveddoublequoted no-delimiters))

(defun ar-doublebackslashparen-curveddoublequoted-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around CURVEDDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'curveddoublequoted no-delimiters))

(defun ar-doublebacktick-curveddoublequoted-atpt (&optional no-delimiters check)
  "Provides double backticks around CURVEDDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'curveddoublequoted no-delimiters))

(defun ar-slashparen-curveddoublequoted-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around CURVEDDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'curveddoublequoted no-delimiters))

(defun ar-slashparen-curveddoublequoted-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around CURVEDDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'curveddoublequoted no-delimiters))

(defun ar-comment-curveddoublequoted-atpt (&optional no-delimiters check)
  "Comments CURVEDDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-comment 'curveddoublequoted no-delimiters))

(defun ar-commatize-curveddoublequoted-atpt (&optional no-delimiters check)
  "Put a comma after CURVEDDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'curveddoublequoted no-delimiters))

(defun ar-quote-curveddoublequoted-atpt (&optional no-delimiters check)
  "Put a singlequote before CURVEDDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-quote 'curveddoublequoted no-delimiters))


(defun ar-mark-curveddoublequoted-atpt (&optional arg)
  "Marks CURVEDDOUBLEQUOTED at point if any. "
  (interactive "P")
  (ar-th-mark 'curveddoublequoted))

(defun ar-hide-curveddoublequoted-atpt (&optional arg)
  "Hides CURVEDDOUBLEQUOTED at point. "
  (interactive "P")
  (ar-th-hide 'curveddoublequoted))

(defun ar-show-curveddoublequoted-atpt (&optional arg)
  "Shows hidden CURVEDDOUBLEQUOTED at point. "
  (interactive "P")
  (ar-th-show 'curveddoublequoted))

(defun ar-hide-show-curveddoublequoted-atpt (&optional arg)
  "Alternatively hides or shows CURVEDDOUBLEQUOTED at point. "
  (interactive "P")
  (ar-th-hide-show 'curveddoublequoted))

(defun ar-highlight-curveddoublequoted-atpt-mode (&optional no-delimiters check)
  "Toggles curveddoublequoted-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'curveddoublequoted no-delimiters))

(defun ar-kill-curveddoublequoted-atpt (&optional no-delimiters check)
  "Kills CURVEDDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-kill 'curveddoublequoted no-delimiters))

(defun ar-curvedsinglequote-curveddoublequoted-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'curveddoublequoted no-delimiters))

(defun ar-separate-curveddoublequoted-atpt (&optional no-delimiters check)
  "Separates CURVEDDOUBLEQUOTED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'curveddoublequoted no-delimiters))

(defun ar-triplequotedq-curveddoublequoted-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around curveddoublequoted. "
  (interactive "*P")
  (ar-th-triplequotedq 'curveddoublequoted no-delimiters))

(defun ar-triplequotesq-curveddoublequoted-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around curveddoublequoted. "
  (interactive "*P")
  (ar-th-triplequotesq 'curveddoublequoted no-delimiters))

(defun ar-triplebacktick-curveddoublequoted-atpt (&optional arg)
  "Deletes curveddoublequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'curveddoublequoted arg))

(defun ar-trim-curveddoublequoted-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'curveddoublequoted no-delimiters iact check t t))

(defun ar-left-trim-curveddoublequoted-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'curveddoublequoted no-delimiters iact check t))

(defun ar-right-trim-curveddoublequoted-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'curveddoublequoted n no-delimiters iact check nil t))

(defun ar-underscore-curveddoublequoted-atpt (&optional no-delimiters check)
  "Put underscore char around CURVEDDOUBLEQUOTED. "
  (interactive "*P")
  (ar-th-underscore 'curveddoublequoted no-delimiters))

;; (defalias 'ar-curveddoublequoted-whitespace-atpt 'ar-whitespace-curveddoublequoted-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-curveddoublequoted-atpt (&optional no-delimiters check)
;;   "Put whitespace char around CURVEDDOUBLEQUOTED. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'curveddoublequoted nil t))

(defun ar-forward-curveddoublequoted-atpt (&optional arg)
  "Moves forward over CURVEDDOUBLEQUOTED at point if any, does nothing otherwise.
Returns end position of CURVEDDOUBLEQUOTED "
  (interactive "P")
  (ar-th-forward 'curveddoublequoted arg))

(defun ar-backward-curveddoublequoted-atpt (&optional arg)
  "Moves backward over CURVEDDOUBLEQUOTED before point if any, does nothing otherwise.
Returns beginning position of CURVEDDOUBLEQUOTED "
  (interactive "P")
  (ar-th-backward 'curveddoublequoted arg))

(defun ar-transpose-curveddoublequoted-atpt (&optional arg)
  "Transposes CURVEDDOUBLEQUOTED with CURVEDDOUBLEQUOTED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'curveddoublequoted arg))

(defun ar-sort-curveddoublequoted-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts curveddoublequoteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'curveddoublequoted reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-curveddoublequoted-atpt (&optional arg) 
  "Return t if a CURVEDDOUBLEQUOTED at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-curveddoublequoted-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-curveddoublequoted-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-parentized-atpt (&optional no-delimiters)
  "Returns parentized at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'parentized no-delimiters))

(defun ar-bounds-of-parentized-atpt (&optional no-delimiters check)
  "Returns a list, borders of parentized if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'parentized no-delimiters))

(defun ar-parentized-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position PARENTIZED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'parentized no-delimiters))

(defun ar-parentized-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of PARENTIZED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'parentized no-delimiters))

(defun ar-beginning-of-parentized-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class PARENTIZED at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'parentized no-delimiters))

(defun ar-end-of-parentized-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class PARENTIZED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'parentized no-delimiters))

(defun ar-in-parentized-p-atpt (&optional no-delimiters check)
  "Returns bounds of PARENTIZED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'parentized no-delimiters))

(defun ar-length-of-parentized-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class PARENTIZED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'parentized no-delimiters))

(defun ar-copy-parentized-atpt (&optional no-delimiters check)
  "Returns a copy of PARENTIZED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'parentized no-delimiters))

(defun ar-delete-parentized-atpt (&optional arg)
  "Deletes PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-delete 'parentized arg))

(defun ar-delete-parentized-in-region (beg end)
  "Deletes PARENTIZED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'parentized beg end))

(defun ar-blok-parentized-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around parentized.
  Returns blok or nil if no PARENTIZED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'parentized no-delimiters))

(defun ar-backslashparen-parentized-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around parentized at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'parentized no-delimiters))

(defun ar-doublebackslash-parentized-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'parentized no-delimiters))

(defun ar-doubleslash-parentized-atpt (&optional no-delimiters check)
  "Puts doubled slashes around PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'parentized no-delimiters))

(defun ar-doublebackslashparen-parentized-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'parentized no-delimiters))

(defun ar-doublebacktick-parentized-atpt (&optional no-delimiters check)
  "Provides double backticks around PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'parentized no-delimiters))

(defun ar-slashparen-parentized-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'parentized no-delimiters))

(defun ar-slashparen-parentized-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'parentized no-delimiters))

(defun ar-comment-parentized-atpt (&optional no-delimiters check)
  "Comments PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-comment 'parentized no-delimiters))

(defun ar-commatize-parentized-atpt (&optional no-delimiters check)
  "Put a comma after PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'parentized no-delimiters))

(defun ar-quote-parentized-atpt (&optional no-delimiters check)
  "Put a singlequote before PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-quote 'parentized no-delimiters))


(defun ar-mark-parentized-atpt (&optional arg)
  "Marks PARENTIZED at point if any. "
  (interactive "P")
  (ar-th-mark 'parentized))

(defun ar-hide-parentized-atpt (&optional arg)
  "Hides PARENTIZED at point. "
  (interactive "P")
  (ar-th-hide 'parentized))

(defun ar-show-parentized-atpt (&optional arg)
  "Shows hidden PARENTIZED at point. "
  (interactive "P")
  (ar-th-show 'parentized))

(defun ar-hide-show-parentized-atpt (&optional arg)
  "Alternatively hides or shows PARENTIZED at point. "
  (interactive "P")
  (ar-th-hide-show 'parentized))

(defun ar-highlight-parentized-atpt-mode (&optional no-delimiters check)
  "Toggles parentized-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'parentized no-delimiters))

(defun ar-kill-parentized-atpt (&optional no-delimiters check)
  "Kills PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-kill 'parentized no-delimiters))

(defun ar-curvedsinglequote-parentized-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'parentized no-delimiters))

(defun ar-separate-parentized-atpt (&optional no-delimiters check)
  "Separates PARENTIZED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'parentized no-delimiters))

(defun ar-triplequotedq-parentized-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around parentized. "
  (interactive "*P")
  (ar-th-triplequotedq 'parentized no-delimiters))

(defun ar-triplequotesq-parentized-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around parentized. "
  (interactive "*P")
  (ar-th-triplequotesq 'parentized no-delimiters))

(defun ar-triplebacktick-parentized-atpt (&optional arg)
  "Deletes parentized at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'parentized arg))

(defun ar-trim-parentized-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'parentized no-delimiters iact check t t))

(defun ar-left-trim-parentized-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'parentized no-delimiters iact check t))

(defun ar-right-trim-parentized-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'parentized n no-delimiters iact check nil t))

(defun ar-underscore-parentized-atpt (&optional no-delimiters check)
  "Put underscore char around PARENTIZED. "
  (interactive "*P")
  (ar-th-underscore 'parentized no-delimiters))

;; (defalias 'ar-parentized-whitespace-atpt 'ar-whitespace-parentized-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-parentized-atpt (&optional no-delimiters check)
;;   "Put whitespace char around PARENTIZED. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'parentized nil t))

(defun ar-forward-parentized-atpt (&optional arg)
  "Moves forward over PARENTIZED at point if any, does nothing otherwise.
Returns end position of PARENTIZED "
  (interactive "P")
  (ar-th-forward 'parentized arg))

(defun ar-backward-parentized-atpt (&optional arg)
  "Moves backward over PARENTIZED before point if any, does nothing otherwise.
Returns beginning position of PARENTIZED "
  (interactive "P")
  (ar-th-backward 'parentized arg))

(defun ar-transpose-parentized-atpt (&optional arg)
  "Transposes PARENTIZED with PARENTIZED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'parentized arg))

(defun ar-sort-parentized-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts parentizeds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'parentized reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-parentized-atpt (&optional arg) 
  "Return t if a PARENTIZED at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-parentized-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-parentized-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;; ar-thing-at-point-utils-delimiters-core: ar-paired-delimited-passiv: end

;; ar-thing-at-point-utils-delimiters-core list: start

(defun ar-list-atpt (&optional no-delimiters)
  "Returns list at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'list no-delimiters))

(defun ar-bounds-of-list-atpt (&optional no-delimiters check)
  "Returns a list, borders of list if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'list no-delimiters))

(defun ar-list-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position LIST at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'list no-delimiters))

(defun ar-list-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of LIST at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'list no-delimiters))

(defun ar-beginning-of-list-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class LIST at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'list no-delimiters))

(defun ar-end-of-list-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class LIST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'list no-delimiters))

(defun ar-in-list-p-atpt (&optional no-delimiters check)
  "Returns bounds of LIST at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'list no-delimiters))

(defun ar-length-of-list-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class LIST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'list no-delimiters))

(defun ar-copy-list-atpt (&optional no-delimiters check)
  "Returns a copy of LIST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'list no-delimiters))

(defun ar-delete-list-atpt (&optional arg)
  "Deletes LIST at point if any. "
  (interactive "*P")
  (ar-th-delete 'list arg))

(defun ar-delete-list-in-region (beg end)
  "Deletes LIST at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'list beg end))

(defun ar-blok-list-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around list.
  Returns blok or nil if no LIST at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'list no-delimiters))

(defun ar-backslashparen-list-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around list at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'list no-delimiters))

(defun ar-doublebackslash-list-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around LIST at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'list no-delimiters))

(defun ar-doubleslash-list-atpt (&optional no-delimiters check)
  "Puts doubled slashes around LIST at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'list no-delimiters))

(defun ar-doublebackslashparen-list-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around LIST at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'list no-delimiters))

(defun ar-doublebacktick-list-atpt (&optional no-delimiters check)
  "Provides double backticks around LIST at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'list no-delimiters))

(defun ar-slashparen-list-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around LIST at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'list no-delimiters))

(defun ar-slashparen-list-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around LIST at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'list no-delimiters))

(defun ar-comment-list-atpt (&optional no-delimiters check)
  "Comments LIST at point if any. "
  (interactive "*P")
  (ar-th-comment 'list no-delimiters))

(defun ar-commatize-list-atpt (&optional no-delimiters check)
  "Put a comma after LIST at point if any. "
  (interactive "*P")
  (ar-th-commatize 'list no-delimiters))

(defun ar-quote-list-atpt (&optional no-delimiters check)
  "Put a singlequote before LIST at point if any. "
  (interactive "*P")
  (ar-th-quote 'list no-delimiters))


(defun ar-mark-list-atpt (&optional arg)
  "Marks LIST at point if any. "
  (interactive "P")
  (ar-th-mark 'list))

(defun ar-hide-list-atpt (&optional arg)
  "Hides LIST at point. "
  (interactive "P")
  (ar-th-hide 'list))

(defun ar-show-list-atpt (&optional arg)
  "Shows hidden LIST at point. "
  (interactive "P")
  (ar-th-show 'list))

(defun ar-hide-show-list-atpt (&optional arg)
  "Alternatively hides or shows LIST at point. "
  (interactive "P")
  (ar-th-hide-show 'list))

(defun ar-highlight-list-atpt-mode (&optional no-delimiters check)
  "Toggles list-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'list no-delimiters))

(defun ar-kill-list-atpt (&optional no-delimiters check)
  "Kills LIST at point if any. "
  (interactive "*P")
  (ar-th-kill 'list no-delimiters))

(defun ar-curvedsinglequote-list-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'list no-delimiters))

(defun ar-separate-list-atpt (&optional no-delimiters check)
  "Separates LIST at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'list no-delimiters))

(defun ar-triplequotedq-list-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around list. "
  (interactive "*P")
  (ar-th-triplequotedq 'list no-delimiters))

(defun ar-triplequotesq-list-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around list. "
  (interactive "*P")
  (ar-th-triplequotesq 'list no-delimiters))

(defun ar-triplebacktick-list-atpt (&optional arg)
  "Deletes list at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'list arg))

(defun ar-trim-list-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'list no-delimiters iact check t t))

(defun ar-left-trim-list-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'list no-delimiters iact check t))

(defun ar-right-trim-list-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'list n no-delimiters iact check nil t))

(defun ar-underscore-list-atpt (&optional no-delimiters check)
  "Put underscore char around LIST. "
  (interactive "*P")
  (ar-th-underscore 'list no-delimiters))

;; (defalias 'ar-list-whitespace-atpt 'ar-whitespace-list-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-list-atpt (&optional no-delimiters check)
;;   "Put whitespace char around LIST. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'list nil t))

(defun ar-forward-list-atpt (&optional arg)
  "Moves forward over LIST at point if any, does nothing otherwise.
Returns end position of LIST "
  (interactive "P")
  (ar-th-forward 'list arg))

(defun ar-backward-list-atpt (&optional arg)
  "Moves backward over LIST before point if any, does nothing otherwise.
Returns beginning position of LIST "
  (interactive "P")
  (ar-th-backward 'list arg))

(defun ar-transpose-list-atpt (&optional arg)
  "Transposes LIST with LIST before point if any. "
  (interactive "*P")
  (ar-th-transpose 'list arg))

(defun ar-sort-list-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts lists in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'list reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-list-atpt (&optional arg) 
  "Return t if a LIST at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-list-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-list-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;; ar-thing-at-point-utils-delimiters-core list: end

;; ar-thing-at-point-utils-delimiters-core: ar-atpt-expression-list: start

(defun ar-block-atpt (&optional no-delimiters)
  "Returns block at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'block no-delimiters))

(defun ar-bounds-of-block-atpt (&optional no-delimiters check)
  "Returns a list, borders of block if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'block no-delimiters))

(defun ar-block-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position BLOCK at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'block no-delimiters))

(defun ar-block-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of BLOCK at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'block no-delimiters))

(defun ar-beginning-of-block-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class BLOCK at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'block no-delimiters))

(defun ar-end-of-block-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class BLOCK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'block no-delimiters))

(defun ar-in-block-p-atpt (&optional no-delimiters check)
  "Returns bounds of BLOCK at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'block no-delimiters))

(defun ar-length-of-block-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class BLOCK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'block no-delimiters))

(defun ar-copy-block-atpt (&optional no-delimiters check)
  "Returns a copy of BLOCK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'block no-delimiters))

(defun ar-delete-block-atpt (&optional arg)
  "Deletes BLOCK at point if any. "
  (interactive "*P")
  (ar-th-delete 'block arg))

(defun ar-delete-block-in-region (beg end)
  "Deletes BLOCK at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'block beg end))

(defun ar-blok-block-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around block.
  Returns blok or nil if no BLOCK at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'block no-delimiters))

(defun ar-backslashparen-block-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around block at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'block no-delimiters))

(defun ar-doublebackslash-block-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around BLOCK at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'block no-delimiters))

(defun ar-doubleslash-block-atpt (&optional no-delimiters check)
  "Puts doubled slashes around BLOCK at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'block no-delimiters))

(defun ar-doublebackslashparen-block-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around BLOCK at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'block no-delimiters))

(defun ar-doublebacktick-block-atpt (&optional no-delimiters check)
  "Provides double backticks around BLOCK at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'block no-delimiters))

(defun ar-slashparen-block-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around BLOCK at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'block no-delimiters))

(defun ar-slashparen-block-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around BLOCK at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'block no-delimiters))

(defun ar-comment-block-atpt (&optional no-delimiters check)
  "Comments BLOCK at point if any. "
  (interactive "*P")
  (ar-th-comment 'block no-delimiters))

(defun ar-commatize-block-atpt (&optional no-delimiters check)
  "Put a comma after BLOCK at point if any. "
  (interactive "*P")
  (ar-th-commatize 'block no-delimiters))

(defun ar-quote-block-atpt (&optional no-delimiters check)
  "Put a singlequote before BLOCK at point if any. "
  (interactive "*P")
  (ar-th-quote 'block no-delimiters))


(defun ar-mark-block-atpt (&optional arg)
  "Marks BLOCK at point if any. "
  (interactive "P")
  (ar-th-mark 'block))

(defun ar-hide-block-atpt (&optional arg)
  "Hides BLOCK at point. "
  (interactive "P")
  (ar-th-hide 'block))

(defun ar-show-block-atpt (&optional arg)
  "Shows hidden BLOCK at point. "
  (interactive "P")
  (ar-th-show 'block))

(defun ar-hide-show-block-atpt (&optional arg)
  "Alternatively hides or shows BLOCK at point. "
  (interactive "P")
  (ar-th-hide-show 'block))

(defun ar-highlight-block-atpt-mode (&optional no-delimiters check)
  "Toggles block-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'block no-delimiters))

(defun ar-kill-block-atpt (&optional no-delimiters check)
  "Kills BLOCK at point if any. "
  (interactive "*P")
  (ar-th-kill 'block no-delimiters))

(defun ar-curvedsinglequote-block-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'block no-delimiters))

(defun ar-separate-block-atpt (&optional no-delimiters check)
  "Separates BLOCK at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'block no-delimiters))

(defun ar-triplequotedq-block-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around block. "
  (interactive "*P")
  (ar-th-triplequotedq 'block no-delimiters))

(defun ar-triplequotesq-block-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around block. "
  (interactive "*P")
  (ar-th-triplequotesq 'block no-delimiters))

(defun ar-triplebacktick-block-atpt (&optional arg)
  "Deletes block at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'block arg))

(defun ar-trim-block-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'block no-delimiters iact check t t))

(defun ar-left-trim-block-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'block no-delimiters iact check t))

(defun ar-right-trim-block-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'block n no-delimiters iact check nil t))

(defun ar-underscore-block-atpt (&optional no-delimiters check)
  "Put underscore char around BLOCK. "
  (interactive "*P")
  (ar-th-underscore 'block no-delimiters))

;; (defalias 'ar-block-whitespace-atpt 'ar-whitespace-block-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-block-atpt (&optional no-delimiters check)
;;   "Put whitespace char around BLOCK. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'block nil t))

(defun ar-forward-block-atpt (&optional arg)
  "Moves forward over BLOCK at point if any, does nothing otherwise.
Returns end position of BLOCK "
  (interactive "P")
  (ar-th-forward 'block arg))

(defun ar-backward-block-atpt (&optional arg)
  "Moves backward over BLOCK before point if any, does nothing otherwise.
Returns beginning position of BLOCK "
  (interactive "P")
  (ar-th-backward 'block arg))

(defun ar-transpose-block-atpt (&optional arg)
  "Transposes BLOCK with BLOCK before point if any. "
  (interactive "*P")
  (ar-th-transpose 'block arg))

(defun ar-sort-block-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts blocks in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'block reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-block-atpt (&optional arg) 
  "Return t if a BLOCK at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-block-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-block-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-block-or-clause-atpt (&optional no-delimiters)
  "Returns block-or-clause at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'block-or-clause no-delimiters))

(defun ar-bounds-of-block-or-clause-atpt (&optional no-delimiters check)
  "Returns a list, borders of block-or-clause if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'block-or-clause no-delimiters))

(defun ar-block-or-clause-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position BLOCK-OR-CLAUSE at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'block-or-clause no-delimiters))

(defun ar-block-or-clause-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of BLOCK-OR-CLAUSE at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'block-or-clause no-delimiters))

(defun ar-beginning-of-block-or-clause-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class BLOCK-OR-CLAUSE at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'block-or-clause no-delimiters))

(defun ar-end-of-block-or-clause-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class BLOCK-OR-CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'block-or-clause no-delimiters))

(defun ar-in-block-or-clause-p-atpt (&optional no-delimiters check)
  "Returns bounds of BLOCK-OR-CLAUSE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'block-or-clause no-delimiters))

(defun ar-length-of-block-or-clause-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class BLOCK-OR-CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'block-or-clause no-delimiters))

(defun ar-copy-block-or-clause-atpt (&optional no-delimiters check)
  "Returns a copy of BLOCK-OR-CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'block-or-clause no-delimiters))

(defun ar-delete-block-or-clause-atpt (&optional arg)
  "Deletes BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-delete 'block-or-clause arg))

(defun ar-delete-block-or-clause-in-region (beg end)
  "Deletes BLOCK-OR-CLAUSE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'block-or-clause beg end))

(defun ar-blok-block-or-clause-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around block-or-clause.
  Returns blok or nil if no BLOCK-OR-CLAUSE at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'block-or-clause no-delimiters))

(defun ar-backslashparen-block-or-clause-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around block-or-clause at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'block-or-clause no-delimiters))

(defun ar-doublebackslash-block-or-clause-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'block-or-clause no-delimiters))

(defun ar-doubleslash-block-or-clause-atpt (&optional no-delimiters check)
  "Puts doubled slashes around BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'block-or-clause no-delimiters))

(defun ar-doublebackslashparen-block-or-clause-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'block-or-clause no-delimiters))

(defun ar-doublebacktick-block-or-clause-atpt (&optional no-delimiters check)
  "Provides double backticks around BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'block-or-clause no-delimiters))

(defun ar-slashparen-block-or-clause-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'block-or-clause no-delimiters))

(defun ar-slashparen-block-or-clause-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'block-or-clause no-delimiters))

(defun ar-comment-block-or-clause-atpt (&optional no-delimiters check)
  "Comments BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-comment 'block-or-clause no-delimiters))

(defun ar-commatize-block-or-clause-atpt (&optional no-delimiters check)
  "Put a comma after BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-commatize 'block-or-clause no-delimiters))

(defun ar-quote-block-or-clause-atpt (&optional no-delimiters check)
  "Put a singlequote before BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-quote 'block-or-clause no-delimiters))


(defun ar-mark-block-or-clause-atpt (&optional arg)
  "Marks BLOCK-OR-CLAUSE at point if any. "
  (interactive "P")
  (ar-th-mark 'block-or-clause))

(defun ar-hide-block-or-clause-atpt (&optional arg)
  "Hides BLOCK-OR-CLAUSE at point. "
  (interactive "P")
  (ar-th-hide 'block-or-clause))

(defun ar-show-block-or-clause-atpt (&optional arg)
  "Shows hidden BLOCK-OR-CLAUSE at point. "
  (interactive "P")
  (ar-th-show 'block-or-clause))

(defun ar-hide-show-block-or-clause-atpt (&optional arg)
  "Alternatively hides or shows BLOCK-OR-CLAUSE at point. "
  (interactive "P")
  (ar-th-hide-show 'block-or-clause))

(defun ar-highlight-block-or-clause-atpt-mode (&optional no-delimiters check)
  "Toggles block-or-clause-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'block-or-clause no-delimiters))

(defun ar-kill-block-or-clause-atpt (&optional no-delimiters check)
  "Kills BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-kill 'block-or-clause no-delimiters))

(defun ar-curvedsinglequote-block-or-clause-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'block-or-clause no-delimiters))

(defun ar-separate-block-or-clause-atpt (&optional no-delimiters check)
  "Separates BLOCK-OR-CLAUSE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'block-or-clause no-delimiters))

(defun ar-triplequotedq-block-or-clause-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around block-or-clause. "
  (interactive "*P")
  (ar-th-triplequotedq 'block-or-clause no-delimiters))

(defun ar-triplequotesq-block-or-clause-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around block-or-clause. "
  (interactive "*P")
  (ar-th-triplequotesq 'block-or-clause no-delimiters))

(defun ar-triplebacktick-block-or-clause-atpt (&optional arg)
  "Deletes block-or-clause at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'block-or-clause arg))

(defun ar-trim-block-or-clause-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'block-or-clause no-delimiters iact check t t))

(defun ar-left-trim-block-or-clause-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'block-or-clause no-delimiters iact check t))

(defun ar-right-trim-block-or-clause-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'block-or-clause n no-delimiters iact check nil t))

(defun ar-underscore-block-or-clause-atpt (&optional no-delimiters check)
  "Put underscore char around BLOCK-OR-CLAUSE. "
  (interactive "*P")
  (ar-th-underscore 'block-or-clause no-delimiters))

;; (defalias 'ar-block-or-clause-whitespace-atpt 'ar-whitespace-block-or-clause-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-block-or-clause-atpt (&optional no-delimiters check)
;;   "Put whitespace char around BLOCK-OR-CLAUSE. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'block-or-clause nil t))

(defun ar-forward-block-or-clause-atpt (&optional arg)
  "Moves forward over BLOCK-OR-CLAUSE at point if any, does nothing otherwise.
Returns end position of BLOCK-OR-CLAUSE "
  (interactive "P")
  (ar-th-forward 'block-or-clause arg))

(defun ar-backward-block-or-clause-atpt (&optional arg)
  "Moves backward over BLOCK-OR-CLAUSE before point if any, does nothing otherwise.
Returns beginning position of BLOCK-OR-CLAUSE "
  (interactive "P")
  (ar-th-backward 'block-or-clause arg))

(defun ar-transpose-block-or-clause-atpt (&optional arg)
  "Transposes BLOCK-OR-CLAUSE with BLOCK-OR-CLAUSE before point if any. "
  (interactive "*P")
  (ar-th-transpose 'block-or-clause arg))

(defun ar-sort-block-or-clause-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts block-or-clauses in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'block-or-clause reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-block-or-clause-atpt (&optional arg) 
  "Return t if a BLOCK-OR-CLAUSE at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-block-or-clause-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-block-or-clause-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-char-atpt (&optional no-delimiters)
  "Returns char at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'char no-delimiters))

(defun ar-bounds-of-char-atpt (&optional no-delimiters check)
  "Returns a list, borders of char if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'char no-delimiters))

(defun ar-char-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position CHAR at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'char no-delimiters))

(defun ar-char-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of CHAR at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'char no-delimiters))

(defun ar-beginning-of-char-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class CHAR at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'char no-delimiters))

(defun ar-end-of-char-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class CHAR at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'char no-delimiters))

(defun ar-in-char-p-atpt (&optional no-delimiters check)
  "Returns bounds of CHAR at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'char no-delimiters))

(defun ar-length-of-char-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class CHAR at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'char no-delimiters))

(defun ar-copy-char-atpt (&optional no-delimiters check)
  "Returns a copy of CHAR at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'char no-delimiters))

(defun ar-delete-char-atpt (&optional arg)
  "Deletes CHAR at point if any. "
  (interactive "*P")
  (ar-th-delete 'char arg))

(defun ar-delete-char-in-region (beg end)
  "Deletes CHAR at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'char beg end))

(defun ar-blok-char-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around char.
  Returns blok or nil if no CHAR at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'char no-delimiters))

(defun ar-backslashparen-char-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around char at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'char no-delimiters))

(defun ar-doublebackslash-char-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around CHAR at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'char no-delimiters))

(defun ar-doubleslash-char-atpt (&optional no-delimiters check)
  "Puts doubled slashes around CHAR at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'char no-delimiters))

(defun ar-doublebackslashparen-char-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around CHAR at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'char no-delimiters))

(defun ar-doublebacktick-char-atpt (&optional no-delimiters check)
  "Provides double backticks around CHAR at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'char no-delimiters))

(defun ar-slashparen-char-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around CHAR at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'char no-delimiters))

(defun ar-slashparen-char-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around CHAR at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'char no-delimiters))

(defun ar-comment-char-atpt (&optional no-delimiters check)
  "Comments CHAR at point if any. "
  (interactive "*P")
  (ar-th-comment 'char no-delimiters))

(defun ar-commatize-char-atpt (&optional no-delimiters check)
  "Put a comma after CHAR at point if any. "
  (interactive "*P")
  (ar-th-commatize 'char no-delimiters))

(defun ar-quote-char-atpt (&optional no-delimiters check)
  "Put a singlequote before CHAR at point if any. "
  (interactive "*P")
  (ar-th-quote 'char no-delimiters))


(defun ar-mark-char-atpt (&optional arg)
  "Marks CHAR at point if any. "
  (interactive "P")
  (ar-th-mark 'char))

(defun ar-hide-char-atpt (&optional arg)
  "Hides CHAR at point. "
  (interactive "P")
  (ar-th-hide 'char))

(defun ar-show-char-atpt (&optional arg)
  "Shows hidden CHAR at point. "
  (interactive "P")
  (ar-th-show 'char))

(defun ar-hide-show-char-atpt (&optional arg)
  "Alternatively hides or shows CHAR at point. "
  (interactive "P")
  (ar-th-hide-show 'char))

(defun ar-highlight-char-atpt-mode (&optional no-delimiters check)
  "Toggles char-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'char no-delimiters))

(defun ar-kill-char-atpt (&optional no-delimiters check)
  "Kills CHAR at point if any. "
  (interactive "*P")
  (ar-th-kill 'char no-delimiters))

(defun ar-curvedsinglequote-char-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'char no-delimiters))

(defun ar-separate-char-atpt (&optional no-delimiters check)
  "Separates CHAR at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'char no-delimiters))

(defun ar-triplequotedq-char-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around char. "
  (interactive "*P")
  (ar-th-triplequotedq 'char no-delimiters))

(defun ar-triplequotesq-char-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around char. "
  (interactive "*P")
  (ar-th-triplequotesq 'char no-delimiters))

(defun ar-triplebacktick-char-atpt (&optional arg)
  "Deletes char at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'char arg))

(defun ar-trim-char-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'char no-delimiters iact check t t))

(defun ar-left-trim-char-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'char no-delimiters iact check t))

(defun ar-right-trim-char-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'char n no-delimiters iact check nil t))

(defun ar-underscore-char-atpt (&optional no-delimiters check)
  "Put underscore char around CHAR. "
  (interactive "*P")
  (ar-th-underscore 'char no-delimiters))

;; (defalias 'ar-char-whitespace-atpt 'ar-whitespace-char-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-char-atpt (&optional no-delimiters check)
;;   "Put whitespace char around CHAR. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'char nil t))

(defun ar-forward-char-atpt (&optional arg)
  "Moves forward over CHAR at point if any, does nothing otherwise.
Returns end position of CHAR "
  (interactive "P")
  (ar-th-forward 'char arg))

(defun ar-backward-char-atpt (&optional arg)
  "Moves backward over CHAR before point if any, does nothing otherwise.
Returns beginning position of CHAR "
  (interactive "P")
  (ar-th-backward 'char arg))

(defun ar-transpose-char-atpt (&optional arg)
  "Transposes CHAR with CHAR before point if any. "
  (interactive "*P")
  (ar-th-transpose 'char arg))

(defun ar-sort-char-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts chars in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'char reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-char-atpt (&optional arg) 
  "Return t if a CHAR at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-char-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-char-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-class-atpt (&optional no-delimiters)
  "Returns class at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'class no-delimiters))

(defun ar-bounds-of-class-atpt (&optional no-delimiters check)
  "Returns a list, borders of class if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'class no-delimiters))

(defun ar-class-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position CLASS at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'class no-delimiters))

(defun ar-class-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of CLASS at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'class no-delimiters))

(defun ar-beginning-of-class-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class CLASS at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'class no-delimiters))

(defun ar-end-of-class-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'class no-delimiters))

(defun ar-in-class-p-atpt (&optional no-delimiters check)
  "Returns bounds of CLASS at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'class no-delimiters))

(defun ar-length-of-class-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'class no-delimiters))

(defun ar-copy-class-atpt (&optional no-delimiters check)
  "Returns a copy of CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'class no-delimiters))

(defun ar-delete-class-atpt (&optional arg)
  "Deletes CLASS at point if any. "
  (interactive "*P")
  (ar-th-delete 'class arg))

(defun ar-delete-class-in-region (beg end)
  "Deletes CLASS at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'class beg end))

(defun ar-blok-class-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around class.
  Returns blok or nil if no CLASS at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'class no-delimiters))

(defun ar-backslashparen-class-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around class at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'class no-delimiters))

(defun ar-doublebackslash-class-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around CLASS at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'class no-delimiters))

(defun ar-doubleslash-class-atpt (&optional no-delimiters check)
  "Puts doubled slashes around CLASS at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'class no-delimiters))

(defun ar-doublebackslashparen-class-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around CLASS at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'class no-delimiters))

(defun ar-doublebacktick-class-atpt (&optional no-delimiters check)
  "Provides double backticks around CLASS at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'class no-delimiters))

(defun ar-slashparen-class-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around CLASS at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'class no-delimiters))

(defun ar-slashparen-class-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around CLASS at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'class no-delimiters))

(defun ar-comment-class-atpt (&optional no-delimiters check)
  "Comments CLASS at point if any. "
  (interactive "*P")
  (ar-th-comment 'class no-delimiters))

(defun ar-commatize-class-atpt (&optional no-delimiters check)
  "Put a comma after CLASS at point if any. "
  (interactive "*P")
  (ar-th-commatize 'class no-delimiters))

(defun ar-quote-class-atpt (&optional no-delimiters check)
  "Put a singlequote before CLASS at point if any. "
  (interactive "*P")
  (ar-th-quote 'class no-delimiters))


(defun ar-mark-class-atpt (&optional arg)
  "Marks CLASS at point if any. "
  (interactive "P")
  (ar-th-mark 'class))

(defun ar-hide-class-atpt (&optional arg)
  "Hides CLASS at point. "
  (interactive "P")
  (ar-th-hide 'class))

(defun ar-show-class-atpt (&optional arg)
  "Shows hidden CLASS at point. "
  (interactive "P")
  (ar-th-show 'class))

(defun ar-hide-show-class-atpt (&optional arg)
  "Alternatively hides or shows CLASS at point. "
  (interactive "P")
  (ar-th-hide-show 'class))

(defun ar-highlight-class-atpt-mode (&optional no-delimiters check)
  "Toggles class-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'class no-delimiters))

(defun ar-kill-class-atpt (&optional no-delimiters check)
  "Kills CLASS at point if any. "
  (interactive "*P")
  (ar-th-kill 'class no-delimiters))

(defun ar-curvedsinglequote-class-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'class no-delimiters))

(defun ar-separate-class-atpt (&optional no-delimiters check)
  "Separates CLASS at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'class no-delimiters))

(defun ar-triplequotedq-class-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around class. "
  (interactive "*P")
  (ar-th-triplequotedq 'class no-delimiters))

(defun ar-triplequotesq-class-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around class. "
  (interactive "*P")
  (ar-th-triplequotesq 'class no-delimiters))

(defun ar-triplebacktick-class-atpt (&optional arg)
  "Deletes class at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'class arg))

(defun ar-trim-class-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'class no-delimiters iact check t t))

(defun ar-left-trim-class-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'class no-delimiters iact check t))

(defun ar-right-trim-class-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'class n no-delimiters iact check nil t))

(defun ar-underscore-class-atpt (&optional no-delimiters check)
  "Put underscore char around CLASS. "
  (interactive "*P")
  (ar-th-underscore 'class no-delimiters))

;; (defalias 'ar-class-whitespace-atpt 'ar-whitespace-class-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-class-atpt (&optional no-delimiters check)
;;   "Put whitespace char around CLASS. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'class nil t))

(defun ar-forward-class-atpt (&optional arg)
  "Moves forward over CLASS at point if any, does nothing otherwise.
Returns end position of CLASS "
  (interactive "P")
  (ar-th-forward 'class arg))

(defun ar-backward-class-atpt (&optional arg)
  "Moves backward over CLASS before point if any, does nothing otherwise.
Returns beginning position of CLASS "
  (interactive "P")
  (ar-th-backward 'class arg))

(defun ar-transpose-class-atpt (&optional arg)
  "Transposes CLASS with CLASS before point if any. "
  (interactive "*P")
  (ar-th-transpose 'class arg))

(defun ar-sort-class-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts classs in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'class reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-class-atpt (&optional arg) 
  "Return t if a CLASS at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-class-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-class-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-clause-atpt (&optional no-delimiters)
  "Returns clause at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'clause no-delimiters))

(defun ar-bounds-of-clause-atpt (&optional no-delimiters check)
  "Returns a list, borders of clause if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'clause no-delimiters))

(defun ar-clause-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position CLAUSE at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'clause no-delimiters))

(defun ar-clause-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of CLAUSE at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'clause no-delimiters))

(defun ar-beginning-of-clause-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class CLAUSE at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'clause no-delimiters))

(defun ar-end-of-clause-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'clause no-delimiters))

(defun ar-in-clause-p-atpt (&optional no-delimiters check)
  "Returns bounds of CLAUSE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'clause no-delimiters))

(defun ar-length-of-clause-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'clause no-delimiters))

(defun ar-copy-clause-atpt (&optional no-delimiters check)
  "Returns a copy of CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'clause no-delimiters))

(defun ar-delete-clause-atpt (&optional arg)
  "Deletes CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-delete 'clause arg))

(defun ar-delete-clause-in-region (beg end)
  "Deletes CLAUSE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'clause beg end))

(defun ar-blok-clause-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around clause.
  Returns blok or nil if no CLAUSE at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'clause no-delimiters))

(defun ar-backslashparen-clause-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around clause at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'clause no-delimiters))

(defun ar-doublebackslash-clause-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'clause no-delimiters))

(defun ar-doubleslash-clause-atpt (&optional no-delimiters check)
  "Puts doubled slashes around CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'clause no-delimiters))

(defun ar-doublebackslashparen-clause-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'clause no-delimiters))

(defun ar-doublebacktick-clause-atpt (&optional no-delimiters check)
  "Provides double backticks around CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'clause no-delimiters))

(defun ar-slashparen-clause-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'clause no-delimiters))

(defun ar-slashparen-clause-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'clause no-delimiters))

(defun ar-comment-clause-atpt (&optional no-delimiters check)
  "Comments CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-comment 'clause no-delimiters))

(defun ar-commatize-clause-atpt (&optional no-delimiters check)
  "Put a comma after CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-commatize 'clause no-delimiters))

(defun ar-quote-clause-atpt (&optional no-delimiters check)
  "Put a singlequote before CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-quote 'clause no-delimiters))


(defun ar-mark-clause-atpt (&optional arg)
  "Marks CLAUSE at point if any. "
  (interactive "P")
  (ar-th-mark 'clause))

(defun ar-hide-clause-atpt (&optional arg)
  "Hides CLAUSE at point. "
  (interactive "P")
  (ar-th-hide 'clause))

(defun ar-show-clause-atpt (&optional arg)
  "Shows hidden CLAUSE at point. "
  (interactive "P")
  (ar-th-show 'clause))

(defun ar-hide-show-clause-atpt (&optional arg)
  "Alternatively hides or shows CLAUSE at point. "
  (interactive "P")
  (ar-th-hide-show 'clause))

(defun ar-highlight-clause-atpt-mode (&optional no-delimiters check)
  "Toggles clause-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'clause no-delimiters))

(defun ar-kill-clause-atpt (&optional no-delimiters check)
  "Kills CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-kill 'clause no-delimiters))

(defun ar-curvedsinglequote-clause-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'clause no-delimiters))

(defun ar-separate-clause-atpt (&optional no-delimiters check)
  "Separates CLAUSE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'clause no-delimiters))

(defun ar-triplequotedq-clause-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around clause. "
  (interactive "*P")
  (ar-th-triplequotedq 'clause no-delimiters))

(defun ar-triplequotesq-clause-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around clause. "
  (interactive "*P")
  (ar-th-triplequotesq 'clause no-delimiters))

(defun ar-triplebacktick-clause-atpt (&optional arg)
  "Deletes clause at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'clause arg))

(defun ar-trim-clause-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'clause no-delimiters iact check t t))

(defun ar-left-trim-clause-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'clause no-delimiters iact check t))

(defun ar-right-trim-clause-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'clause n no-delimiters iact check nil t))

(defun ar-underscore-clause-atpt (&optional no-delimiters check)
  "Put underscore char around CLAUSE. "
  (interactive "*P")
  (ar-th-underscore 'clause no-delimiters))

;; (defalias 'ar-clause-whitespace-atpt 'ar-whitespace-clause-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-clause-atpt (&optional no-delimiters check)
;;   "Put whitespace char around CLAUSE. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'clause nil t))

(defun ar-forward-clause-atpt (&optional arg)
  "Moves forward over CLAUSE at point if any, does nothing otherwise.
Returns end position of CLAUSE "
  (interactive "P")
  (ar-th-forward 'clause arg))

(defun ar-backward-clause-atpt (&optional arg)
  "Moves backward over CLAUSE before point if any, does nothing otherwise.
Returns beginning position of CLAUSE "
  (interactive "P")
  (ar-th-backward 'clause arg))

(defun ar-transpose-clause-atpt (&optional arg)
  "Transposes CLAUSE with CLAUSE before point if any. "
  (interactive "*P")
  (ar-th-transpose 'clause arg))

(defun ar-sort-clause-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts clauses in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'clause reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-clause-atpt (&optional arg) 
  "Return t if a CLAUSE at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-clause-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-clause-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-def-or-class-atpt (&optional no-delimiters)
  "Returns def-or-class at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'def-or-class no-delimiters))

(defun ar-bounds-of-def-or-class-atpt (&optional no-delimiters check)
  "Returns a list, borders of def-or-class if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'def-or-class no-delimiters))

(defun ar-def-or-class-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position DEF-OR-CLASS at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'def-or-class no-delimiters))

(defun ar-def-or-class-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of DEF-OR-CLASS at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'def-or-class no-delimiters))

(defun ar-beginning-of-def-or-class-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class DEF-OR-CLASS at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'def-or-class no-delimiters))

(defun ar-end-of-def-or-class-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class DEF-OR-CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'def-or-class no-delimiters))

(defun ar-in-def-or-class-p-atpt (&optional no-delimiters check)
  "Returns bounds of DEF-OR-CLASS at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'def-or-class no-delimiters))

(defun ar-length-of-def-or-class-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class DEF-OR-CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'def-or-class no-delimiters))

(defun ar-copy-def-or-class-atpt (&optional no-delimiters check)
  "Returns a copy of DEF-OR-CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'def-or-class no-delimiters))

(defun ar-delete-def-or-class-atpt (&optional arg)
  "Deletes DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-delete 'def-or-class arg))

(defun ar-delete-def-or-class-in-region (beg end)
  "Deletes DEF-OR-CLASS at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'def-or-class beg end))

(defun ar-blok-def-or-class-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around def-or-class.
  Returns blok or nil if no DEF-OR-CLASS at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'def-or-class no-delimiters))

(defun ar-backslashparen-def-or-class-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around def-or-class at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'def-or-class no-delimiters))

(defun ar-doublebackslash-def-or-class-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'def-or-class no-delimiters))

(defun ar-doubleslash-def-or-class-atpt (&optional no-delimiters check)
  "Puts doubled slashes around DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'def-or-class no-delimiters))

(defun ar-doublebackslashparen-def-or-class-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'def-or-class no-delimiters))

(defun ar-doublebacktick-def-or-class-atpt (&optional no-delimiters check)
  "Provides double backticks around DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'def-or-class no-delimiters))

(defun ar-slashparen-def-or-class-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'def-or-class no-delimiters))

(defun ar-slashparen-def-or-class-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'def-or-class no-delimiters))

(defun ar-comment-def-or-class-atpt (&optional no-delimiters check)
  "Comments DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-comment 'def-or-class no-delimiters))

(defun ar-commatize-def-or-class-atpt (&optional no-delimiters check)
  "Put a comma after DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-commatize 'def-or-class no-delimiters))

(defun ar-quote-def-or-class-atpt (&optional no-delimiters check)
  "Put a singlequote before DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-quote 'def-or-class no-delimiters))


(defun ar-mark-def-or-class-atpt (&optional arg)
  "Marks DEF-OR-CLASS at point if any. "
  (interactive "P")
  (ar-th-mark 'def-or-class))

(defun ar-hide-def-or-class-atpt (&optional arg)
  "Hides DEF-OR-CLASS at point. "
  (interactive "P")
  (ar-th-hide 'def-or-class))

(defun ar-show-def-or-class-atpt (&optional arg)
  "Shows hidden DEF-OR-CLASS at point. "
  (interactive "P")
  (ar-th-show 'def-or-class))

(defun ar-hide-show-def-or-class-atpt (&optional arg)
  "Alternatively hides or shows DEF-OR-CLASS at point. "
  (interactive "P")
  (ar-th-hide-show 'def-or-class))

(defun ar-highlight-def-or-class-atpt-mode (&optional no-delimiters check)
  "Toggles def-or-class-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'def-or-class no-delimiters))

(defun ar-kill-def-or-class-atpt (&optional no-delimiters check)
  "Kills DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-kill 'def-or-class no-delimiters))

(defun ar-curvedsinglequote-def-or-class-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'def-or-class no-delimiters))

(defun ar-separate-def-or-class-atpt (&optional no-delimiters check)
  "Separates DEF-OR-CLASS at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'def-or-class no-delimiters))

(defun ar-triplequotedq-def-or-class-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around def-or-class. "
  (interactive "*P")
  (ar-th-triplequotedq 'def-or-class no-delimiters))

(defun ar-triplequotesq-def-or-class-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around def-or-class. "
  (interactive "*P")
  (ar-th-triplequotesq 'def-or-class no-delimiters))

(defun ar-triplebacktick-def-or-class-atpt (&optional arg)
  "Deletes def-or-class at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'def-or-class arg))

(defun ar-trim-def-or-class-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'def-or-class no-delimiters iact check t t))

(defun ar-left-trim-def-or-class-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'def-or-class no-delimiters iact check t))

(defun ar-right-trim-def-or-class-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'def-or-class n no-delimiters iact check nil t))

(defun ar-underscore-def-or-class-atpt (&optional no-delimiters check)
  "Put underscore char around DEF-OR-CLASS. "
  (interactive "*P")
  (ar-th-underscore 'def-or-class no-delimiters))

;; (defalias 'ar-def-or-class-whitespace-atpt 'ar-whitespace-def-or-class-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-def-or-class-atpt (&optional no-delimiters check)
;;   "Put whitespace char around DEF-OR-CLASS. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'def-or-class nil t))

(defun ar-forward-def-or-class-atpt (&optional arg)
  "Moves forward over DEF-OR-CLASS at point if any, does nothing otherwise.
Returns end position of DEF-OR-CLASS "
  (interactive "P")
  (ar-th-forward 'def-or-class arg))

(defun ar-backward-def-or-class-atpt (&optional arg)
  "Moves backward over DEF-OR-CLASS before point if any, does nothing otherwise.
Returns beginning position of DEF-OR-CLASS "
  (interactive "P")
  (ar-th-backward 'def-or-class arg))

(defun ar-transpose-def-or-class-atpt (&optional arg)
  "Transposes DEF-OR-CLASS with DEF-OR-CLASS before point if any. "
  (interactive "*P")
  (ar-th-transpose 'def-or-class arg))

(defun ar-sort-def-or-class-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts def-or-classs in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'def-or-class reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-def-or-class-atpt (&optional arg) 
  "Return t if a DEF-OR-CLASS at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-def-or-class-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-def-or-class-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-def-atpt (&optional no-delimiters)
  "Returns def at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'def no-delimiters))

(defun ar-bounds-of-def-atpt (&optional no-delimiters check)
  "Returns a list, borders of def if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'def no-delimiters))

(defun ar-def-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position DEF at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'def no-delimiters))

(defun ar-def-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of DEF at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'def no-delimiters))

(defun ar-beginning-of-def-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class DEF at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'def no-delimiters))

(defun ar-end-of-def-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class DEF at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'def no-delimiters))

(defun ar-in-def-p-atpt (&optional no-delimiters check)
  "Returns bounds of DEF at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'def no-delimiters))

(defun ar-length-of-def-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class DEF at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'def no-delimiters))

(defun ar-copy-def-atpt (&optional no-delimiters check)
  "Returns a copy of DEF at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'def no-delimiters))

(defun ar-delete-def-atpt (&optional arg)
  "Deletes DEF at point if any. "
  (interactive "*P")
  (ar-th-delete 'def arg))

(defun ar-delete-def-in-region (beg end)
  "Deletes DEF at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'def beg end))

(defun ar-blok-def-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around def.
  Returns blok or nil if no DEF at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'def no-delimiters))

(defun ar-backslashparen-def-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around def at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'def no-delimiters))

(defun ar-doublebackslash-def-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around DEF at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'def no-delimiters))

(defun ar-doubleslash-def-atpt (&optional no-delimiters check)
  "Puts doubled slashes around DEF at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'def no-delimiters))

(defun ar-doublebackslashparen-def-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around DEF at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'def no-delimiters))

(defun ar-doublebacktick-def-atpt (&optional no-delimiters check)
  "Provides double backticks around DEF at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'def no-delimiters))

(defun ar-slashparen-def-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around DEF at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'def no-delimiters))

(defun ar-slashparen-def-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around DEF at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'def no-delimiters))

(defun ar-comment-def-atpt (&optional no-delimiters check)
  "Comments DEF at point if any. "
  (interactive "*P")
  (ar-th-comment 'def no-delimiters))

(defun ar-commatize-def-atpt (&optional no-delimiters check)
  "Put a comma after DEF at point if any. "
  (interactive "*P")
  (ar-th-commatize 'def no-delimiters))

(defun ar-quote-def-atpt (&optional no-delimiters check)
  "Put a singlequote before DEF at point if any. "
  (interactive "*P")
  (ar-th-quote 'def no-delimiters))


(defun ar-mark-def-atpt (&optional arg)
  "Marks DEF at point if any. "
  (interactive "P")
  (ar-th-mark 'def))

(defun ar-hide-def-atpt (&optional arg)
  "Hides DEF at point. "
  (interactive "P")
  (ar-th-hide 'def))

(defun ar-show-def-atpt (&optional arg)
  "Shows hidden DEF at point. "
  (interactive "P")
  (ar-th-show 'def))

(defun ar-hide-show-def-atpt (&optional arg)
  "Alternatively hides or shows DEF at point. "
  (interactive "P")
  (ar-th-hide-show 'def))

(defun ar-highlight-def-atpt-mode (&optional no-delimiters check)
  "Toggles def-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'def no-delimiters))

(defun ar-kill-def-atpt (&optional no-delimiters check)
  "Kills DEF at point if any. "
  (interactive "*P")
  (ar-th-kill 'def no-delimiters))

(defun ar-curvedsinglequote-def-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'def no-delimiters))

(defun ar-separate-def-atpt (&optional no-delimiters check)
  "Separates DEF at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'def no-delimiters))

(defun ar-triplequotedq-def-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around def. "
  (interactive "*P")
  (ar-th-triplequotedq 'def no-delimiters))

(defun ar-triplequotesq-def-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around def. "
  (interactive "*P")
  (ar-th-triplequotesq 'def no-delimiters))

(defun ar-triplebacktick-def-atpt (&optional arg)
  "Deletes def at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'def arg))

(defun ar-trim-def-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'def no-delimiters iact check t t))

(defun ar-left-trim-def-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'def no-delimiters iact check t))

(defun ar-right-trim-def-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'def n no-delimiters iact check nil t))

(defun ar-underscore-def-atpt (&optional no-delimiters check)
  "Put underscore char around DEF. "
  (interactive "*P")
  (ar-th-underscore 'def no-delimiters))

;; (defalias 'ar-def-whitespace-atpt 'ar-whitespace-def-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-def-atpt (&optional no-delimiters check)
;;   "Put whitespace char around DEF. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'def nil t))

(defun ar-forward-def-atpt (&optional arg)
  "Moves forward over DEF at point if any, does nothing otherwise.
Returns end position of DEF "
  (interactive "P")
  (ar-th-forward 'def arg))

(defun ar-backward-def-atpt (&optional arg)
  "Moves backward over DEF before point if any, does nothing otherwise.
Returns beginning position of DEF "
  (interactive "P")
  (ar-th-backward 'def arg))

(defun ar-transpose-def-atpt (&optional arg)
  "Transposes DEF with DEF before point if any. "
  (interactive "*P")
  (ar-th-transpose 'def arg))

(defun ar-sort-def-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts defs in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'def reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-def-atpt (&optional arg) 
  "Return t if a DEF at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-def-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-def-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-delimited-atpt (&optional no-delimiters)
  "Returns delimited at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'delimited no-delimiters))

(defun ar-bounds-of-delimited-atpt (&optional no-delimiters check)
  "Returns a list, borders of delimited if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'delimited no-delimiters))

(defun ar-delimited-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position DELIMITED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'delimited no-delimiters))

(defun ar-delimited-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of DELIMITED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'delimited no-delimiters))

(defun ar-beginning-of-delimited-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class DELIMITED at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'delimited no-delimiters))

(defun ar-end-of-delimited-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'delimited no-delimiters))

(defun ar-in-delimited-p-atpt (&optional no-delimiters check)
  "Returns bounds of DELIMITED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'delimited no-delimiters))

(defun ar-length-of-delimited-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'delimited no-delimiters))

(defun ar-copy-delimited-atpt (&optional no-delimiters check)
  "Returns a copy of DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'delimited no-delimiters))

(defun ar-delete-delimited-atpt (&optional arg)
  "Deletes DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-delete 'delimited arg))

(defun ar-delete-delimited-in-region (beg end)
  "Deletes DELIMITED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'delimited beg end))

(defun ar-blok-delimited-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around delimited.
  Returns blok or nil if no DELIMITED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'delimited no-delimiters))

(defun ar-backslashparen-delimited-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around delimited at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'delimited no-delimiters))

(defun ar-doublebackslash-delimited-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'delimited no-delimiters))

(defun ar-doubleslash-delimited-atpt (&optional no-delimiters check)
  "Puts doubled slashes around DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'delimited no-delimiters))

(defun ar-doublebackslashparen-delimited-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'delimited no-delimiters))

(defun ar-doublebacktick-delimited-atpt (&optional no-delimiters check)
  "Provides double backticks around DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'delimited no-delimiters))

(defun ar-slashparen-delimited-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'delimited no-delimiters))

(defun ar-slashparen-delimited-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'delimited no-delimiters))

(defun ar-comment-delimited-atpt (&optional no-delimiters check)
  "Comments DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-comment 'delimited no-delimiters))

(defun ar-commatize-delimited-atpt (&optional no-delimiters check)
  "Put a comma after DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'delimited no-delimiters))

(defun ar-quote-delimited-atpt (&optional no-delimiters check)
  "Put a singlequote before DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-quote 'delimited no-delimiters))


(defun ar-mark-delimited-atpt (&optional arg)
  "Marks DELIMITED at point if any. "
  (interactive "P")
  (ar-th-mark 'delimited))

(defun ar-hide-delimited-atpt (&optional arg)
  "Hides DELIMITED at point. "
  (interactive "P")
  (ar-th-hide 'delimited))

(defun ar-show-delimited-atpt (&optional arg)
  "Shows hidden DELIMITED at point. "
  (interactive "P")
  (ar-th-show 'delimited))

(defun ar-hide-show-delimited-atpt (&optional arg)
  "Alternatively hides or shows DELIMITED at point. "
  (interactive "P")
  (ar-th-hide-show 'delimited))

(defun ar-highlight-delimited-atpt-mode (&optional no-delimiters check)
  "Toggles delimited-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'delimited no-delimiters))

(defun ar-kill-delimited-atpt (&optional no-delimiters check)
  "Kills DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-kill 'delimited no-delimiters))

(defun ar-curvedsinglequote-delimited-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'delimited no-delimiters))

(defun ar-separate-delimited-atpt (&optional no-delimiters check)
  "Separates DELIMITED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'delimited no-delimiters))

(defun ar-triplequotedq-delimited-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around delimited. "
  (interactive "*P")
  (ar-th-triplequotedq 'delimited no-delimiters))

(defun ar-triplequotesq-delimited-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around delimited. "
  (interactive "*P")
  (ar-th-triplequotesq 'delimited no-delimiters))

(defun ar-triplebacktick-delimited-atpt (&optional arg)
  "Deletes delimited at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'delimited arg))

(defun ar-trim-delimited-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'delimited no-delimiters iact check t t))

(defun ar-left-trim-delimited-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'delimited no-delimiters iact check t))

(defun ar-right-trim-delimited-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'delimited n no-delimiters iact check nil t))

(defun ar-underscore-delimited-atpt (&optional no-delimiters check)
  "Put underscore char around DELIMITED. "
  (interactive "*P")
  (ar-th-underscore 'delimited no-delimiters))

;; (defalias 'ar-delimited-whitespace-atpt 'ar-whitespace-delimited-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-delimited-atpt (&optional no-delimiters check)
;;   "Put whitespace char around DELIMITED. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'delimited nil t))

(defun ar-forward-delimited-atpt (&optional arg)
  "Moves forward over DELIMITED at point if any, does nothing otherwise.
Returns end position of DELIMITED "
  (interactive "P")
  (ar-th-forward 'delimited arg))

(defun ar-backward-delimited-atpt (&optional arg)
  "Moves backward over DELIMITED before point if any, does nothing otherwise.
Returns beginning position of DELIMITED "
  (interactive "P")
  (ar-th-backward 'delimited arg))

(defun ar-transpose-delimited-atpt (&optional arg)
  "Transposes DELIMITED with DELIMITED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'delimited arg))

(defun ar-sort-delimited-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts delimiteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'delimited reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-delimited-atpt (&optional arg) 
  "Return t if a DELIMITED at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-delimited-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-delimited-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-expression-atpt (&optional no-delimiters)
  "Returns expression at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'expression no-delimiters))

(defun ar-bounds-of-expression-atpt (&optional no-delimiters check)
  "Returns a list, borders of expression if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'expression no-delimiters))

(defun ar-expression-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position EXPRESSION at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'expression no-delimiters))

(defun ar-expression-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of EXPRESSION at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'expression no-delimiters))

(defun ar-beginning-of-expression-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class EXPRESSION at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'expression no-delimiters))

(defun ar-end-of-expression-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'expression no-delimiters))

(defun ar-in-expression-p-atpt (&optional no-delimiters check)
  "Returns bounds of EXPRESSION at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'expression no-delimiters))

(defun ar-length-of-expression-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'expression no-delimiters))

(defun ar-copy-expression-atpt (&optional no-delimiters check)
  "Returns a copy of EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'expression no-delimiters))

(defun ar-delete-expression-atpt (&optional arg)
  "Deletes EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-delete 'expression arg))

(defun ar-delete-expression-in-region (beg end)
  "Deletes EXPRESSION at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'expression beg end))

(defun ar-blok-expression-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around expression.
  Returns blok or nil if no EXPRESSION at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'expression no-delimiters))

(defun ar-backslashparen-expression-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around expression at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'expression no-delimiters))

(defun ar-doublebackslash-expression-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'expression no-delimiters))

(defun ar-doubleslash-expression-atpt (&optional no-delimiters check)
  "Puts doubled slashes around EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'expression no-delimiters))

(defun ar-doublebackslashparen-expression-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'expression no-delimiters))

(defun ar-doublebacktick-expression-atpt (&optional no-delimiters check)
  "Provides double backticks around EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'expression no-delimiters))

(defun ar-slashparen-expression-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'expression no-delimiters))

(defun ar-slashparen-expression-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'expression no-delimiters))

(defun ar-comment-expression-atpt (&optional no-delimiters check)
  "Comments EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-comment 'expression no-delimiters))

(defun ar-commatize-expression-atpt (&optional no-delimiters check)
  "Put a comma after EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-commatize 'expression no-delimiters))

(defun ar-quote-expression-atpt (&optional no-delimiters check)
  "Put a singlequote before EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-quote 'expression no-delimiters))


(defun ar-mark-expression-atpt (&optional arg)
  "Marks EXPRESSION at point if any. "
  (interactive "P")
  (ar-th-mark 'expression))

(defun ar-hide-expression-atpt (&optional arg)
  "Hides EXPRESSION at point. "
  (interactive "P")
  (ar-th-hide 'expression))

(defun ar-show-expression-atpt (&optional arg)
  "Shows hidden EXPRESSION at point. "
  (interactive "P")
  (ar-th-show 'expression))

(defun ar-hide-show-expression-atpt (&optional arg)
  "Alternatively hides or shows EXPRESSION at point. "
  (interactive "P")
  (ar-th-hide-show 'expression))

(defun ar-highlight-expression-atpt-mode (&optional no-delimiters check)
  "Toggles expression-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'expression no-delimiters))

(defun ar-kill-expression-atpt (&optional no-delimiters check)
  "Kills EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-kill 'expression no-delimiters))

(defun ar-curvedsinglequote-expression-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'expression no-delimiters))

(defun ar-separate-expression-atpt (&optional no-delimiters check)
  "Separates EXPRESSION at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'expression no-delimiters))

(defun ar-triplequotedq-expression-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around expression. "
  (interactive "*P")
  (ar-th-triplequotedq 'expression no-delimiters))

(defun ar-triplequotesq-expression-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around expression. "
  (interactive "*P")
  (ar-th-triplequotesq 'expression no-delimiters))

(defun ar-triplebacktick-expression-atpt (&optional arg)
  "Deletes expression at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'expression arg))

(defun ar-trim-expression-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'expression no-delimiters iact check t t))

(defun ar-left-trim-expression-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'expression no-delimiters iact check t))

(defun ar-right-trim-expression-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'expression n no-delimiters iact check nil t))

(defun ar-underscore-expression-atpt (&optional no-delimiters check)
  "Put underscore char around EXPRESSION. "
  (interactive "*P")
  (ar-th-underscore 'expression no-delimiters))

;; (defalias 'ar-expression-whitespace-atpt 'ar-whitespace-expression-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-expression-atpt (&optional no-delimiters check)
;;   "Put whitespace char around EXPRESSION. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'expression nil t))

(defun ar-forward-expression-atpt (&optional arg)
  "Moves forward over EXPRESSION at point if any, does nothing otherwise.
Returns end position of EXPRESSION "
  (interactive "P")
  (ar-th-forward 'expression arg))

(defun ar-backward-expression-atpt (&optional arg)
  "Moves backward over EXPRESSION before point if any, does nothing otherwise.
Returns beginning position of EXPRESSION "
  (interactive "P")
  (ar-th-backward 'expression arg))

(defun ar-transpose-expression-atpt (&optional arg)
  "Transposes EXPRESSION with EXPRESSION before point if any. "
  (interactive "*P")
  (ar-th-transpose 'expression arg))

(defun ar-sort-expression-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts expressions in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'expression reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-expression-atpt (&optional arg) 
  "Return t if a EXPRESSION at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-expression-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-expression-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-partial-expression-atpt (&optional no-delimiters)
  "Returns partial-expression at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'partial-expression no-delimiters))

(defun ar-bounds-of-partial-expression-atpt (&optional no-delimiters check)
  "Returns a list, borders of partial-expression if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'partial-expression no-delimiters))

(defun ar-partial-expression-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position PARTIAL-EXPRESSION at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'partial-expression no-delimiters))

(defun ar-partial-expression-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of PARTIAL-EXPRESSION at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'partial-expression no-delimiters))

(defun ar-beginning-of-partial-expression-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class PARTIAL-EXPRESSION at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'partial-expression no-delimiters))

(defun ar-end-of-partial-expression-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class PARTIAL-EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'partial-expression no-delimiters))

(defun ar-in-partial-expression-p-atpt (&optional no-delimiters check)
  "Returns bounds of PARTIAL-EXPRESSION at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'partial-expression no-delimiters))

(defun ar-length-of-partial-expression-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class PARTIAL-EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'partial-expression no-delimiters))

(defun ar-copy-partial-expression-atpt (&optional no-delimiters check)
  "Returns a copy of PARTIAL-EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'partial-expression no-delimiters))

(defun ar-delete-partial-expression-atpt (&optional arg)
  "Deletes PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-delete 'partial-expression arg))

(defun ar-delete-partial-expression-in-region (beg end)
  "Deletes PARTIAL-EXPRESSION at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'partial-expression beg end))

(defun ar-blok-partial-expression-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around partial-expression.
  Returns blok or nil if no PARTIAL-EXPRESSION at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'partial-expression no-delimiters))

(defun ar-backslashparen-partial-expression-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around partial-expression at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'partial-expression no-delimiters))

(defun ar-doublebackslash-partial-expression-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'partial-expression no-delimiters))

(defun ar-doubleslash-partial-expression-atpt (&optional no-delimiters check)
  "Puts doubled slashes around PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'partial-expression no-delimiters))

(defun ar-doublebackslashparen-partial-expression-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'partial-expression no-delimiters))

(defun ar-doublebacktick-partial-expression-atpt (&optional no-delimiters check)
  "Provides double backticks around PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'partial-expression no-delimiters))

(defun ar-slashparen-partial-expression-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'partial-expression no-delimiters))

(defun ar-slashparen-partial-expression-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'partial-expression no-delimiters))

(defun ar-comment-partial-expression-atpt (&optional no-delimiters check)
  "Comments PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-comment 'partial-expression no-delimiters))

(defun ar-commatize-partial-expression-atpt (&optional no-delimiters check)
  "Put a comma after PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-commatize 'partial-expression no-delimiters))

(defun ar-quote-partial-expression-atpt (&optional no-delimiters check)
  "Put a singlequote before PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-quote 'partial-expression no-delimiters))


(defun ar-mark-partial-expression-atpt (&optional arg)
  "Marks PARTIAL-EXPRESSION at point if any. "
  (interactive "P")
  (ar-th-mark 'partial-expression))

(defun ar-hide-partial-expression-atpt (&optional arg)
  "Hides PARTIAL-EXPRESSION at point. "
  (interactive "P")
  (ar-th-hide 'partial-expression))

(defun ar-show-partial-expression-atpt (&optional arg)
  "Shows hidden PARTIAL-EXPRESSION at point. "
  (interactive "P")
  (ar-th-show 'partial-expression))

(defun ar-hide-show-partial-expression-atpt (&optional arg)
  "Alternatively hides or shows PARTIAL-EXPRESSION at point. "
  (interactive "P")
  (ar-th-hide-show 'partial-expression))

(defun ar-highlight-partial-expression-atpt-mode (&optional no-delimiters check)
  "Toggles partial-expression-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'partial-expression no-delimiters))

(defun ar-kill-partial-expression-atpt (&optional no-delimiters check)
  "Kills PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-kill 'partial-expression no-delimiters))

(defun ar-curvedsinglequote-partial-expression-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'partial-expression no-delimiters))

(defun ar-separate-partial-expression-atpt (&optional no-delimiters check)
  "Separates PARTIAL-EXPRESSION at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'partial-expression no-delimiters))

(defun ar-triplequotedq-partial-expression-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around partial-expression. "
  (interactive "*P")
  (ar-th-triplequotedq 'partial-expression no-delimiters))

(defun ar-triplequotesq-partial-expression-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around partial-expression. "
  (interactive "*P")
  (ar-th-triplequotesq 'partial-expression no-delimiters))

(defun ar-triplebacktick-partial-expression-atpt (&optional arg)
  "Deletes partial-expression at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'partial-expression arg))

(defun ar-trim-partial-expression-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'partial-expression no-delimiters iact check t t))

(defun ar-left-trim-partial-expression-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'partial-expression no-delimiters iact check t))

(defun ar-right-trim-partial-expression-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'partial-expression n no-delimiters iact check nil t))

(defun ar-underscore-partial-expression-atpt (&optional no-delimiters check)
  "Put underscore char around PARTIAL-EXPRESSION. "
  (interactive "*P")
  (ar-th-underscore 'partial-expression no-delimiters))

;; (defalias 'ar-partial-expression-whitespace-atpt 'ar-whitespace-partial-expression-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-partial-expression-atpt (&optional no-delimiters check)
;;   "Put whitespace char around PARTIAL-EXPRESSION. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'partial-expression nil t))

(defun ar-forward-partial-expression-atpt (&optional arg)
  "Moves forward over PARTIAL-EXPRESSION at point if any, does nothing otherwise.
Returns end position of PARTIAL-EXPRESSION "
  (interactive "P")
  (ar-th-forward 'partial-expression arg))

(defun ar-backward-partial-expression-atpt (&optional arg)
  "Moves backward over PARTIAL-EXPRESSION before point if any, does nothing otherwise.
Returns beginning position of PARTIAL-EXPRESSION "
  (interactive "P")
  (ar-th-backward 'partial-expression arg))

(defun ar-transpose-partial-expression-atpt (&optional arg)
  "Transposes PARTIAL-EXPRESSION with PARTIAL-EXPRESSION before point if any. "
  (interactive "*P")
  (ar-th-transpose 'partial-expression arg))

(defun ar-sort-partial-expression-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts partial-expressions in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'partial-expression reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-partial-expression-atpt (&optional arg) 
  "Return t if a PARTIAL-EXPRESSION at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-partial-expression-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-partial-expression-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-statement-atpt (&optional no-delimiters)
  "Returns statement at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'statement no-delimiters))

(defun ar-bounds-of-statement-atpt (&optional no-delimiters check)
  "Returns a list, borders of statement if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'statement no-delimiters))

(defun ar-statement-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position STATEMENT at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'statement no-delimiters))

(defun ar-statement-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of STATEMENT at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'statement no-delimiters))

(defun ar-beginning-of-statement-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class STATEMENT at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'statement no-delimiters))

(defun ar-end-of-statement-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class STATEMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'statement no-delimiters))

(defun ar-in-statement-p-atpt (&optional no-delimiters check)
  "Returns bounds of STATEMENT at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'statement no-delimiters))

(defun ar-length-of-statement-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class STATEMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'statement no-delimiters))

(defun ar-copy-statement-atpt (&optional no-delimiters check)
  "Returns a copy of STATEMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'statement no-delimiters))

(defun ar-delete-statement-atpt (&optional arg)
  "Deletes STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-delete 'statement arg))

(defun ar-delete-statement-in-region (beg end)
  "Deletes STATEMENT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'statement beg end))

(defun ar-blok-statement-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around statement.
  Returns blok or nil if no STATEMENT at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'statement no-delimiters))

(defun ar-backslashparen-statement-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around statement at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'statement no-delimiters))

(defun ar-doublebackslash-statement-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'statement no-delimiters))

(defun ar-doubleslash-statement-atpt (&optional no-delimiters check)
  "Puts doubled slashes around STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'statement no-delimiters))

(defun ar-doublebackslashparen-statement-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'statement no-delimiters))

(defun ar-doublebacktick-statement-atpt (&optional no-delimiters check)
  "Provides double backticks around STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'statement no-delimiters))

(defun ar-slashparen-statement-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'statement no-delimiters))

(defun ar-slashparen-statement-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'statement no-delimiters))

(defun ar-comment-statement-atpt (&optional no-delimiters check)
  "Comments STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-comment 'statement no-delimiters))

(defun ar-commatize-statement-atpt (&optional no-delimiters check)
  "Put a comma after STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-commatize 'statement no-delimiters))

(defun ar-quote-statement-atpt (&optional no-delimiters check)
  "Put a singlequote before STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-quote 'statement no-delimiters))


(defun ar-mark-statement-atpt (&optional arg)
  "Marks STATEMENT at point if any. "
  (interactive "P")
  (ar-th-mark 'statement))

(defun ar-hide-statement-atpt (&optional arg)
  "Hides STATEMENT at point. "
  (interactive "P")
  (ar-th-hide 'statement))

(defun ar-show-statement-atpt (&optional arg)
  "Shows hidden STATEMENT at point. "
  (interactive "P")
  (ar-th-show 'statement))

(defun ar-hide-show-statement-atpt (&optional arg)
  "Alternatively hides or shows STATEMENT at point. "
  (interactive "P")
  (ar-th-hide-show 'statement))

(defun ar-highlight-statement-atpt-mode (&optional no-delimiters check)
  "Toggles statement-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'statement no-delimiters))

(defun ar-kill-statement-atpt (&optional no-delimiters check)
  "Kills STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-kill 'statement no-delimiters))

(defun ar-curvedsinglequote-statement-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'statement no-delimiters))

(defun ar-separate-statement-atpt (&optional no-delimiters check)
  "Separates STATEMENT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'statement no-delimiters))

(defun ar-triplequotedq-statement-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around statement. "
  (interactive "*P")
  (ar-th-triplequotedq 'statement no-delimiters))

(defun ar-triplequotesq-statement-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around statement. "
  (interactive "*P")
  (ar-th-triplequotesq 'statement no-delimiters))

(defun ar-triplebacktick-statement-atpt (&optional arg)
  "Deletes statement at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'statement arg))

(defun ar-trim-statement-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'statement no-delimiters iact check t t))

(defun ar-left-trim-statement-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'statement no-delimiters iact check t))

(defun ar-right-trim-statement-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'statement n no-delimiters iact check nil t))

(defun ar-underscore-statement-atpt (&optional no-delimiters check)
  "Put underscore char around STATEMENT. "
  (interactive "*P")
  (ar-th-underscore 'statement no-delimiters))

;; (defalias 'ar-statement-whitespace-atpt 'ar-whitespace-statement-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-statement-atpt (&optional no-delimiters check)
;;   "Put whitespace char around STATEMENT. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'statement nil t))

(defun ar-forward-statement-atpt (&optional arg)
  "Moves forward over STATEMENT at point if any, does nothing otherwise.
Returns end position of STATEMENT "
  (interactive "P")
  (ar-th-forward 'statement arg))

(defun ar-backward-statement-atpt (&optional arg)
  "Moves backward over STATEMENT before point if any, does nothing otherwise.
Returns beginning position of STATEMENT "
  (interactive "P")
  (ar-th-backward 'statement arg))

(defun ar-transpose-statement-atpt (&optional arg)
  "Transposes STATEMENT with STATEMENT before point if any. "
  (interactive "*P")
  (ar-th-transpose 'statement arg))

(defun ar-sort-statement-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts statements in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'statement reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-statement-atpt (&optional arg) 
  "Return t if a STATEMENT at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-statement-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-statement-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-string-atpt (&optional no-delimiters)
  "Returns string at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'string no-delimiters))

(defun ar-bounds-of-string-atpt (&optional no-delimiters check)
  "Returns a list, borders of string if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'string no-delimiters))

(defun ar-string-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position STRING at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'string no-delimiters))

(defun ar-string-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of STRING at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'string no-delimiters))

(defun ar-beginning-of-string-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class STRING at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'string no-delimiters))

(defun ar-end-of-string-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'string no-delimiters))

(defun ar-in-string-p-atpt (&optional no-delimiters check)
  "Returns bounds of STRING at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'string no-delimiters))

(defun ar-length-of-string-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'string no-delimiters))

(defun ar-copy-string-atpt (&optional no-delimiters check)
  "Returns a copy of STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'string no-delimiters))

(defun ar-delete-string-atpt (&optional arg)
  "Deletes STRING at point if any. "
  (interactive "*P")
  (ar-th-delete 'string arg))

(defun ar-delete-string-in-region (beg end)
  "Deletes STRING at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'string beg end))

(defun ar-blok-string-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around string.
  Returns blok or nil if no STRING at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'string no-delimiters))

(defun ar-backslashparen-string-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around string at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'string no-delimiters))

(defun ar-doublebackslash-string-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around STRING at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'string no-delimiters))

(defun ar-doubleslash-string-atpt (&optional no-delimiters check)
  "Puts doubled slashes around STRING at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'string no-delimiters))

(defun ar-doublebackslashparen-string-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around STRING at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'string no-delimiters))

(defun ar-doublebacktick-string-atpt (&optional no-delimiters check)
  "Provides double backticks around STRING at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'string no-delimiters))

(defun ar-slashparen-string-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around STRING at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'string no-delimiters))

(defun ar-slashparen-string-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around STRING at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'string no-delimiters))

(defun ar-comment-string-atpt (&optional no-delimiters check)
  "Comments STRING at point if any. "
  (interactive "*P")
  (ar-th-comment 'string no-delimiters))

(defun ar-commatize-string-atpt (&optional no-delimiters check)
  "Put a comma after STRING at point if any. "
  (interactive "*P")
  (ar-th-commatize 'string no-delimiters))

(defun ar-quote-string-atpt (&optional no-delimiters check)
  "Put a singlequote before STRING at point if any. "
  (interactive "*P")
  (ar-th-quote 'string no-delimiters))


(defun ar-mark-string-atpt (&optional arg)
  "Marks STRING at point if any. "
  (interactive "P")
  (ar-th-mark 'string))

(defun ar-hide-string-atpt (&optional arg)
  "Hides STRING at point. "
  (interactive "P")
  (ar-th-hide 'string))

(defun ar-show-string-atpt (&optional arg)
  "Shows hidden STRING at point. "
  (interactive "P")
  (ar-th-show 'string))

(defun ar-hide-show-string-atpt (&optional arg)
  "Alternatively hides or shows STRING at point. "
  (interactive "P")
  (ar-th-hide-show 'string))

(defun ar-highlight-string-atpt-mode (&optional no-delimiters check)
  "Toggles string-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'string no-delimiters))

(defun ar-kill-string-atpt (&optional no-delimiters check)
  "Kills STRING at point if any. "
  (interactive "*P")
  (ar-th-kill 'string no-delimiters))

(defun ar-curvedsinglequote-string-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'string no-delimiters))

(defun ar-separate-string-atpt (&optional no-delimiters check)
  "Separates STRING at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'string no-delimiters))

(defun ar-triplequotedq-string-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around string. "
  (interactive "*P")
  (ar-th-triplequotedq 'string no-delimiters))

(defun ar-triplequotesq-string-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around string. "
  (interactive "*P")
  (ar-th-triplequotesq 'string no-delimiters))

(defun ar-triplebacktick-string-atpt (&optional arg)
  "Deletes string at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'string arg))

(defun ar-trim-string-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'string no-delimiters iact check t t))

(defun ar-left-trim-string-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'string no-delimiters iact check t))

(defun ar-right-trim-string-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'string n no-delimiters iact check nil t))

(defun ar-underscore-string-atpt (&optional no-delimiters check)
  "Put underscore char around STRING. "
  (interactive "*P")
  (ar-th-underscore 'string no-delimiters))

;; (defalias 'ar-string-whitespace-atpt 'ar-whitespace-string-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-string-atpt (&optional no-delimiters check)
;;   "Put whitespace char around STRING. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'string nil t))

(defun ar-forward-string-atpt (&optional arg)
  "Moves forward over STRING at point if any, does nothing otherwise.
Returns end position of STRING "
  (interactive "P")
  (ar-th-forward 'string arg))

(defun ar-backward-string-atpt (&optional arg)
  "Moves backward over STRING before point if any, does nothing otherwise.
Returns beginning position of STRING "
  (interactive "P")
  (ar-th-backward 'string arg))

(defun ar-transpose-string-atpt (&optional arg)
  "Transposes STRING with STRING before point if any. "
  (interactive "*P")
  (ar-th-transpose 'string arg))

(defun ar-sort-string-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts strings in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'string reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-string-atpt (&optional arg) 
  "Return t if a STRING at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-string-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-string-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;; ar-thing-at-point-utils-delimiters-core: ar-atpt-expression-list: end


(defun ar-colon-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with colon(s).

  Otherwise copy colon(ed) at point
  With NO-DELIMITERS, copy colon(ed) without delimiters
  With numerical argument 2 trim colon
  With negative argument kill colon(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'colon no-delimiters check))

(defun ar-cross-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with cross(s).

  Otherwise copy cross(ed) at point
  With NO-DELIMITERS, copy cross(ed) without delimiters
  With numerical argument 2 trim cross
  With negative argument kill cross(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'cross no-delimiters check))

(defun ar-doubleslash-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with doubleslash(s).

  Otherwise copy doubleslash(ed) at point
  With NO-DELIMITERS, copy doubleslash(ed) without delimiters
  With numerical argument 2 trim doubleslash
  With negative argument kill doubleslash(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'doubleslash no-delimiters check))

(defun ar-backslash-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with backslash(s).

  Otherwise copy backslash(ed) at point
  With NO-DELIMITERS, copy backslash(ed) without delimiters
  With numerical argument 2 trim backslash
  With negative argument kill backslash(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'backslash no-delimiters check))

(defun ar-backtick-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with backtick(s).

  Otherwise copy backtick(ed) at point
  With NO-DELIMITERS, copy backtick(ed) without delimiters
  With numerical argument 2 trim backtick
  With negative argument kill backtick(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'backtick no-delimiters check))

(defun ar-dollar-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with dollar(s).

  Otherwise copy dollar(ed) at point
  With NO-DELIMITERS, copy dollar(ed) without delimiters
  With numerical argument 2 trim dollar
  With negative argument kill dollar(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'dollar no-delimiters check))

(defun ar-doublebacktick-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with doublebacktick(s).

  Otherwise copy doublebacktick(ed) at point
  With NO-DELIMITERS, copy doublebacktick(ed) without delimiters
  With numerical argument 2 trim doublebacktick
  With negative argument kill doublebacktick(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'doublebacktick no-delimiters check))

(defun ar-doublequote-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with doublequote(s).

  Otherwise copy doublequote(ed) at point
  With NO-DELIMITERS, copy doublequote(ed) without delimiters
  With numerical argument 2 trim doublequote
  With negative argument kill doublequote(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'doublequote no-delimiters check))

(defun ar-equalize-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with equalize(s).

  Otherwise copy equalize(ed) at point
  With NO-DELIMITERS, copy equalize(ed) without delimiters
  With numerical argument 2 trim equalize
  With negative argument kill equalize(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'equalize no-delimiters check))

(defun ar-escape-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with escape(s).

  Otherwise copy escape(ed) at point
  With NO-DELIMITERS, copy escape(ed) without delimiters
  With numerical argument 2 trim escape
  With negative argument kill escape(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'escape no-delimiters check))

(defun ar-hash-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with hash(s).

  Otherwise copy hash(ed) at point
  With NO-DELIMITERS, copy hash(ed) without delimiters
  With numerical argument 2 trim hash
  With negative argument kill hash(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'hash no-delimiters check))

(defun ar-hyphen-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with hyphen(s).

  Otherwise copy hyphen(ed) at point
  With NO-DELIMITERS, copy hyphen(ed) without delimiters
  With numerical argument 2 trim hyphen
  With negative argument kill hyphen(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'hyphen no-delimiters check))

(defun ar-pipe-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with pipe(s).

  Otherwise copy pipe(ed) at point
  With NO-DELIMITERS, copy pipe(ed) without delimiters
  With numerical argument 2 trim pipe
  With negative argument kill pipe(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'pipe no-delimiters check))

(defun ar-singlequote-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with singlequote(s).

  Otherwise copy singlequote(ed) at point
  With NO-DELIMITERS, copy singlequote(ed) without delimiters
  With numerical argument 2 trim singlequote
  With negative argument kill singlequote(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'singlequote no-delimiters check))

(defun ar-slash-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with slash(s).

  Otherwise copy slash(ed) at point
  With NO-DELIMITERS, copy slash(ed) without delimiters
  With numerical argument 2 trim slash
  With negative argument kill slash(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'slash no-delimiters check))

(defun ar-star-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with star(s).

  Otherwise copy star(ed) at point
  With NO-DELIMITERS, copy star(ed) without delimiters
  With numerical argument 2 trim star
  With negative argument kill star(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'star no-delimiters check))

(defun ar-tild-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with tild(s).

  Otherwise copy tild(ed) at point
  With NO-DELIMITERS, copy tild(ed) without delimiters
  With numerical argument 2 trim tild
  With negative argument kill tild(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'tild no-delimiters check))

(defun ar-triplebacktick-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with triplebacktick(s).

  Otherwise copy triplebacktick(ed) at point
  With NO-DELIMITERS, copy triplebacktick(ed) without delimiters
  With numerical argument 2 trim triplebacktick
  With negative argument kill triplebacktick(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'triplebacktick no-delimiters check))

(defun ar-underscore-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with underscore(s).

  Otherwise copy underscore(ed) at point
  With NO-DELIMITERS, copy underscore(ed) without delimiters
  With numerical argument 2 trim underscore
  With negative argument kill underscore(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'underscore no-delimiters check))

(defun ar-whitespace-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with whitespace(s).

  Otherwise copy whitespace(ed) at point
  With NO-DELIMITERS, copy whitespace(ed) without delimiters
  With numerical argument 2 trim whitespace
  With negative argument kill whitespace(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'whitespace no-delimiters check))

(defun ar-brace-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with brace(s).

  Otherwise copy brace(ed) at point
  With NO-DELIMITERS, copy brace(ed) without delimiters
  With numerical argument 2 trim brace
  With negative argument kill brace(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'brace no-delimiters check))

(defun ar-bracket-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with bracket(s).

  Otherwise copy bracket(ed) at point
  With NO-DELIMITERS, copy bracket(ed) without delimiters
  With numerical argument 2 trim bracket
  With negative argument kill bracket(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'bracket no-delimiters check))

(defun ar-lesserangle-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with lesserangle(s).

  Otherwise copy lesserangle(ed) at point
  With NO-DELIMITERS, copy lesserangle(ed) without delimiters
  With numerical argument 2 trim lesserangle
  With negative argument kill lesserangle(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'lesserangle no-delimiters check))

(defun ar-greaterangle-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with greaterangle(s).

  Otherwise copy greaterangle(ed) at point
  With NO-DELIMITERS, copy greaterangle(ed) without delimiters
  With numerical argument 2 trim greaterangle
  With negative argument kill greaterangle(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'greaterangle no-delimiters check))

(defun ar-curvedsinglequote-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with curvedsinglequote(s).

  Otherwise copy curvedsinglequote(ed) at point
  With NO-DELIMITERS, copy curvedsinglequote(ed) without delimiters
  With numerical argument 2 trim curvedsinglequote
  With negative argument kill curvedsinglequote(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'curvedsinglequote no-delimiters check))

(defun ar-curveddoublequote-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with curveddoublequote(s).

  Otherwise copy curveddoublequote(ed) at point
  With NO-DELIMITERS, copy curveddoublequote(ed) without delimiters
  With numerical argument 2 trim curveddoublequote
  With negative argument kill curveddoublequote(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'curveddoublequote no-delimiters check))

(defun ar-parentize-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with parentize(s).

  Otherwise copy parentize(ed) at point
  With NO-DELIMITERS, copy parentize(ed) without delimiters
  With numerical argument 2 trim parentize
  With negative argument kill parentize(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'parentize no-delimiters check))

(defun ar-greateranglednested-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with greateranglednested(s).

  Otherwise copy greateranglednested(ed) at point
  With NO-DELIMITERS, copy greateranglednested(ed) without delimiters
  With numerical argument 2 trim greateranglednested
  With negative argument kill greateranglednested(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'greateranglednested no-delimiters check))

(defun ar-lesseranglednested-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with lesseranglednested(s).

  Otherwise copy lesseranglednested(ed) at point
  With NO-DELIMITERS, copy lesseranglednested(ed) without delimiters
  With numerical argument 2 trim lesseranglednested
  With negative argument kill lesseranglednested(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'lesseranglednested no-delimiters check))

(defun ar-buffer-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with buffer(s).

  Otherwise copy buffer(ed) at point
  With NO-DELIMITERS, copy buffer(ed) without delimiters
  With numerical argument 2 trim buffer
  With negative argument kill buffer(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'buffer no-delimiters check))

(defun ar-char-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with char(s).

  Otherwise copy char(ed) at point
  With NO-DELIMITERS, copy char(ed) without delimiters
  With numerical argument 2 trim char
  With negative argument kill char(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'char no-delimiters check))

(defun ar-comment-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with comment(s).

  Otherwise copy comment(ed) at point
  With NO-DELIMITERS, copy comment(ed) without delimiters
  With numerical argument 2 trim comment
  With negative argument kill comment(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'comment no-delimiters check))

(defun ar-csv-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with csv(s).

  Otherwise copy csv(ed) at point
  With NO-DELIMITERS, copy csv(ed) without delimiters
  With numerical argument 2 trim csv
  With negative argument kill csv(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'csv no-delimiters check))

(defun ar-date-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with date(s).

  Otherwise copy date(ed) at point
  With NO-DELIMITERS, copy date(ed) without delimiters
  With numerical argument 2 trim date
  With negative argument kill date(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'date no-delimiters check))

(defun ar-delimited-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with delimited(s).

  Otherwise copy delimited(ed) at point
  With NO-DELIMITERS, copy delimited(ed) without delimiters
  With numerical argument 2 trim delimited
  With negative argument kill delimited(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'delimited no-delimiters check))

(defun ar-email-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with email(s).

  Otherwise copy email(ed) at point
  With NO-DELIMITERS, copy email(ed) without delimiters
  With numerical argument 2 trim email
  With negative argument kill email(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'email no-delimiters check))

(defun ar-filename-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with filename(s).

  Otherwise copy filename(ed) at point
  With NO-DELIMITERS, copy filename(ed) without delimiters
  With numerical argument 2 trim filename
  With negative argument kill filename(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'filename no-delimiters check))

(defun ar-filenamenondirectory-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with filenamenondirectory(s).

  Otherwise copy filenamenondirectory(ed) at point
  With NO-DELIMITERS, copy filenamenondirectory(ed) without delimiters
  With numerical argument 2 trim filenamenondirectory
  With negative argument kill filenamenondirectory(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'filenamenondirectory no-delimiters check))

(defun ar-float-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with float(s).

  Otherwise copy float(ed) at point
  With NO-DELIMITERS, copy float(ed) without delimiters
  With numerical argument 2 trim float
  With negative argument kill float(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'float no-delimiters check))

(defun ar-function-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with function(s).

  Otherwise copy function(ed) at point
  With NO-DELIMITERS, copy function(ed) without delimiters
  With numerical argument 2 trim function
  With negative argument kill function(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'function no-delimiters check))

(defun ar-ip-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with ip(s).

  Otherwise copy ip(ed) at point
  With NO-DELIMITERS, copy ip(ed) without delimiters
  With numerical argument 2 trim ip
  With negative argument kill ip(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'ip no-delimiters check))

(defun ar-isbn-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with isbn(s).

  Otherwise copy isbn(ed) at point
  With NO-DELIMITERS, copy isbn(ed) without delimiters
  With numerical argument 2 trim isbn
  With negative argument kill isbn(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'isbn no-delimiters check))

(defun ar-line-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with line(s).

  Otherwise copy line(ed) at point
  With NO-DELIMITERS, copy line(ed) without delimiters
  With numerical argument 2 trim line
  With negative argument kill line(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'line no-delimiters check))

(defun ar-list-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with list(s).

  Otherwise copy list(ed) at point
  With NO-DELIMITERS, copy list(ed) without delimiters
  With numerical argument 2 trim list
  With negative argument kill list(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'list no-delimiters check))

(defun ar-name-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with name(s).

  Otherwise copy name(ed) at point
  With NO-DELIMITERS, copy name(ed) without delimiters
  With numerical argument 2 trim name
  With negative argument kill name(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'name no-delimiters check))

(defun ar-number-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with number(s).

  Otherwise copy number(ed) at point
  With NO-DELIMITERS, copy number(ed) without delimiters
  With numerical argument 2 trim number
  With negative argument kill number(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'number no-delimiters check))

(defun ar-page-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with page(s).

  Otherwise copy page(ed) at point
  With NO-DELIMITERS, copy page(ed) without delimiters
  With numerical argument 2 trim page
  With negative argument kill page(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'page no-delimiters check))

(defun ar-paragraph-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with paragraph(s).

  Otherwise copy paragraph(ed) at point
  With NO-DELIMITERS, copy paragraph(ed) without delimiters
  With numerical argument 2 trim paragraph
  With negative argument kill paragraph(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'paragraph no-delimiters check))

(defun ar-phone-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with phone(s).

  Otherwise copy phone(ed) at point
  With NO-DELIMITERS, copy phone(ed) without delimiters
  With numerical argument 2 trim phone
  With negative argument kill phone(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'phone no-delimiters check))

(defun ar-region-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with region(s).

  Otherwise copy region(ed) at point
  With NO-DELIMITERS, copy region(ed) without delimiters
  With numerical argument 2 trim region
  With negative argument kill region(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'region no-delimiters check))

(defun ar-sentence-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with sentence(s).

  Otherwise copy sentence(ed) at point
  With NO-DELIMITERS, copy sentence(ed) without delimiters
  With numerical argument 2 trim sentence
  With negative argument kill sentence(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'sentence no-delimiters check))

(defun ar-sexp-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with sexp(s).

  Otherwise copy sexp(ed) at point
  With NO-DELIMITERS, copy sexp(ed) without delimiters
  With numerical argument 2 trim sexp
  With negative argument kill sexp(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'sexp no-delimiters check))

(defun ar-shstruct-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with shstruct(s).

  Otherwise copy shstruct(ed) at point
  With NO-DELIMITERS, copy shstruct(ed) without delimiters
  With numerical argument 2 trim shstruct
  With negative argument kill shstruct(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'shstruct no-delimiters check))

(defun ar-symbol-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with symbol(s).

  Otherwise copy symbol(ed) at point
  With NO-DELIMITERS, copy symbol(ed) without delimiters
  With numerical argument 2 trim symbol
  With negative argument kill symbol(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'symbol no-delimiters check))

(defun ar-url-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with url(s).

  Otherwise copy url(ed) at point
  With NO-DELIMITERS, copy url(ed) without delimiters
  With numerical argument 2 trim url
  With negative argument kill url(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'url no-delimiters check))

(defun ar-word-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with word(s).

  Otherwise copy word(ed) at point
  With NO-DELIMITERS, copy word(ed) without delimiters
  With numerical argument 2 trim word
  With negative argument kill word(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'word no-delimiters check))

(defun ar-wordalphaonly-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with wordalphaonly(s).

  Otherwise copy wordalphaonly(ed) at point
  With NO-DELIMITERS, copy wordalphaonly(ed) without delimiters
  With numerical argument 2 trim wordalphaonly
  With negative argument kill wordalphaonly(ed) at point "
  (interactive "P")
  (ar-th-base-copy-or 'wordalphaonly no-delimiters check))

(defun emacs-batch-expression (&optional arg)
  "Copy and highlight an expression starting with \"eval\" or \"load\". "
  (interactive "P")
  (unless (looking-back "[ \t\r\n\f]" (line-beginning-position))
    (skip-chars-backward " \t\r\n\f"))

  (let ((beg (cond ((or (looking-at "--eval")(looking-at "-load"))
                    (match-beginning 0))
                   ((re-search-backward "--eval\\|-load\\|--funcall" (line-beginning-position) 'move)
                    (match-beginning 0))))
         end)
    (if beg
        (progn
          (push-mark (point) t t)
          (setq end
                (progn
                  (skip-chars-forward "^ \t\r\n\f")
                  (skip-chars-forward " \t\r\n\f")
                  (if (looking-at "\"")
                      (progn
                        (forward-char 1)
                        (ar-end-of-doublequoted-atpt)
                        (forward-char 1)
                        (point))
                    (skip-chars-forward "^ \t\r\n\f")
                    (point))))
          (exchange-point-and-mark)
          (kill-new (buffer-substring-no-properties beg end)))
      (message "%s" "Can't detect beginning of emacs-batch-expression"))))



(provide 'thing-at-point-utils)
;;; thing-at-point-utils.el ends here
