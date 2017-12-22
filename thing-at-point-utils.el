;;; thing-at-point-utils.el --- th-at-point edit functions -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2017 Andreas Röhler, unless
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
(require 'thingatpt-utils-map)

;; ar-thing-at-point-utils-aktiv-passiv ar-unpaired-delimit-aktiv ar-atpt-rest-list: start
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

;; ar-thing-at-point-utils-aktiv-passiv ar-unpaired-delimit-aktiv ar-atpt-rest-list: end
;; ar-thing-at-point-utils-pair-rest ar-paired-delimit-aktiv-raw ar-atpt-rest-list: start
(defun ar-brace-greateranglednested-atpt (&optional arg)
  "Brace GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'greateranglednested "{" "}" arg arg))

(defun ar-brace-lesseranglednested-atpt (&optional arg)
  "Brace LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'lesseranglednested "{" "}" arg arg))

(defun ar-brace-buffer-atpt (&optional arg)
  "Brace BUFFER at point."
  (interactive "*P")
  (ar-th-delimit--intern 'buffer "{" "}" arg arg))

(defun ar-brace-comment-atpt (&optional arg)
  "Brace COMMENT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'comment "{" "}" arg arg))

(defun ar-brace-csv-atpt (&optional arg)
  "Brace CSV at point."
  (interactive "*P")
  (ar-th-delimit--intern 'csv "{" "}" arg arg))

(defun ar-brace-date-atpt (&optional arg)
  "Brace DATE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'date "{" "}" arg arg))

(defun ar-brace-email-atpt (&optional arg)
  "Brace EMAIL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'email "{" "}" arg arg))

(defun ar-brace-filename-atpt (&optional arg)
  "Brace FILENAME at point."
  (interactive "*P")
  (ar-th-delimit--intern 'filename "{" "}" arg arg))

(defun ar-brace-filenamenondirectory-atpt (&optional arg)
  "Brace FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-delimit--intern 'filenamenondirectory "{" "}" arg arg))

(defun ar-brace-float-atpt (&optional arg)
  "Brace FLOAT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'float "{" "}" arg arg))

(defun ar-brace-function-atpt (&optional arg)
  "Brace FUNCTION at point."
  (interactive "*P")
  (ar-th-delimit--intern 'function "{" "}" arg arg))

(defun ar-brace-ip-atpt (&optional arg)
  "Brace IP at point."
  (interactive "*P")
  (ar-th-delimit--intern 'ip "{" "}" arg arg))

(defun ar-brace-isbn-atpt (&optional arg)
  "Brace ISBN at point."
  (interactive "*P")
  (ar-th-delimit--intern 'isbn "{" "}" arg arg))

(defun ar-brace-line-atpt (&optional arg)
  "Brace LINE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'line "{" "}" arg arg))

(defun ar-brace-list-atpt (&optional arg)
  "Brace LIST at point."
  (interactive "*P")
  (ar-th-delimit--intern 'list "{" "}" arg arg))

(defun ar-brace-name-atpt (&optional arg)
  "Brace NAME at point."
  (interactive "*P")
  (ar-th-delimit--intern 'name "{" "}" arg arg))

(defun ar-brace-number-atpt (&optional arg)
  "Brace NUMBER at point."
  (interactive "*P")
  (ar-th-delimit--intern 'number "{" "}" arg arg))

(defun ar-brace-page-atpt (&optional arg)
  "Brace PAGE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'page "{" "}" arg arg))

(defun ar-brace-paragraph-atpt (&optional arg)
  "Brace PARAGRAPH at point."
  (interactive "*P")
  (ar-th-delimit--intern 'paragraph "{" "}" arg arg))

(defun ar-brace-phone-atpt (&optional arg)
  "Brace PHONE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'phone "{" "}" arg arg))

(defun ar-brace-region-atpt (&optional arg)
  "Brace REGION at point."
  (interactive "*P")
  (ar-th-delimit--intern 'region "{" "}" arg arg))

(defun ar-brace-sentence-atpt (&optional arg)
  "Brace SENTENCE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'sentence "{" "}" arg arg))

(defun ar-brace-sexp-atpt (&optional arg)
  "Brace SEXP at point."
  (interactive "*P")
  (ar-th-delimit--intern 'sexp "{" "}" arg arg))

(defun ar-brace-shstruct-atpt (&optional arg)
  "Brace SHSTRUCT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'shstruct "{" "}" arg arg))

(defun ar-brace-symbol-atpt (&optional arg)
  "Brace SYMBOL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'symbol "{" "}" arg arg))

(defun ar-brace-url-atpt (&optional arg)
  "Brace URL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'url "{" "}" arg arg))

(defun ar-brace-word-atpt (&optional arg)
  "Brace WORD at point."
  (interactive "*P")
  (ar-th-delimit--intern 'word "{" "}" arg arg))

(defun ar-brace-wordalphaonly-atpt (&optional arg)
  "Brace WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-delimit--intern 'wordalphaonly "{" "}" arg arg))

(defun ar-bracket-greateranglednested-atpt (&optional arg)
  "Bracket GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'greateranglednested "[" "]" arg arg))

(defun ar-bracket-lesseranglednested-atpt (&optional arg)
  "Bracket LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'lesseranglednested "[" "]" arg arg))

(defun ar-bracket-buffer-atpt (&optional arg)
  "Bracket BUFFER at point."
  (interactive "*P")
  (ar-th-delimit--intern 'buffer "[" "]" arg arg))

(defun ar-bracket-comment-atpt (&optional arg)
  "Bracket COMMENT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'comment "[" "]" arg arg))

(defun ar-bracket-csv-atpt (&optional arg)
  "Bracket CSV at point."
  (interactive "*P")
  (ar-th-delimit--intern 'csv "[" "]" arg arg))

(defun ar-bracket-date-atpt (&optional arg)
  "Bracket DATE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'date "[" "]" arg arg))

(defun ar-bracket-email-atpt (&optional arg)
  "Bracket EMAIL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'email "[" "]" arg arg))

(defun ar-bracket-filename-atpt (&optional arg)
  "Bracket FILENAME at point."
  (interactive "*P")
  (ar-th-delimit--intern 'filename "[" "]" arg arg))

(defun ar-bracket-filenamenondirectory-atpt (&optional arg)
  "Bracket FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-delimit--intern 'filenamenondirectory "[" "]" arg arg))

(defun ar-bracket-float-atpt (&optional arg)
  "Bracket FLOAT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'float "[" "]" arg arg))

(defun ar-bracket-function-atpt (&optional arg)
  "Bracket FUNCTION at point."
  (interactive "*P")
  (ar-th-delimit--intern 'function "[" "]" arg arg))

(defun ar-bracket-ip-atpt (&optional arg)
  "Bracket IP at point."
  (interactive "*P")
  (ar-th-delimit--intern 'ip "[" "]" arg arg))

(defun ar-bracket-isbn-atpt (&optional arg)
  "Bracket ISBN at point."
  (interactive "*P")
  (ar-th-delimit--intern 'isbn "[" "]" arg arg))

(defun ar-bracket-line-atpt (&optional arg)
  "Bracket LINE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'line "[" "]" arg arg))

(defun ar-bracket-list-atpt (&optional arg)
  "Bracket LIST at point."
  (interactive "*P")
  (ar-th-delimit--intern 'list "[" "]" arg arg))

(defun ar-bracket-name-atpt (&optional arg)
  "Bracket NAME at point."
  (interactive "*P")
  (ar-th-delimit--intern 'name "[" "]" arg arg))

(defun ar-bracket-number-atpt (&optional arg)
  "Bracket NUMBER at point."
  (interactive "*P")
  (ar-th-delimit--intern 'number "[" "]" arg arg))

(defun ar-bracket-page-atpt (&optional arg)
  "Bracket PAGE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'page "[" "]" arg arg))

(defun ar-bracket-paragraph-atpt (&optional arg)
  "Bracket PARAGRAPH at point."
  (interactive "*P")
  (ar-th-delimit--intern 'paragraph "[" "]" arg arg))

(defun ar-bracket-phone-atpt (&optional arg)
  "Bracket PHONE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'phone "[" "]" arg arg))

(defun ar-bracket-region-atpt (&optional arg)
  "Bracket REGION at point."
  (interactive "*P")
  (ar-th-delimit--intern 'region "[" "]" arg arg))

(defun ar-bracket-sentence-atpt (&optional arg)
  "Bracket SENTENCE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'sentence "[" "]" arg arg))

(defun ar-bracket-sexp-atpt (&optional arg)
  "Bracket SEXP at point."
  (interactive "*P")
  (ar-th-delimit--intern 'sexp "[" "]" arg arg))

(defun ar-bracket-shstruct-atpt (&optional arg)
  "Bracket SHSTRUCT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'shstruct "[" "]" arg arg))

(defun ar-bracket-symbol-atpt (&optional arg)
  "Bracket SYMBOL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'symbol "[" "]" arg arg))

(defun ar-bracket-url-atpt (&optional arg)
  "Bracket URL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'url "[" "]" arg arg))

(defun ar-bracket-word-atpt (&optional arg)
  "Bracket WORD at point."
  (interactive "*P")
  (ar-th-delimit--intern 'word "[" "]" arg arg))

(defun ar-bracket-wordalphaonly-atpt (&optional arg)
  "Bracket WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-delimit--intern 'wordalphaonly "[" "]" arg arg))

(defun ar-lesserangle-greateranglednested-atpt (&optional arg)
  "Lesserangle GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'greateranglednested "<" ">" arg arg))

(defun ar-lesserangle-lesseranglednested-atpt (&optional arg)
  "Lesserangle LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'lesseranglednested "<" ">" arg arg))

(defun ar-lesserangle-buffer-atpt (&optional arg)
  "Lesserangle BUFFER at point."
  (interactive "*P")
  (ar-th-delimit--intern 'buffer "<" ">" arg arg))

(defun ar-lesserangle-comment-atpt (&optional arg)
  "Lesserangle COMMENT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'comment "<" ">" arg arg))

(defun ar-lesserangle-csv-atpt (&optional arg)
  "Lesserangle CSV at point."
  (interactive "*P")
  (ar-th-delimit--intern 'csv "<" ">" arg arg))

(defun ar-lesserangle-date-atpt (&optional arg)
  "Lesserangle DATE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'date "<" ">" arg arg))

(defun ar-lesserangle-email-atpt (&optional arg)
  "Lesserangle EMAIL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'email "<" ">" arg arg))

(defun ar-lesserangle-filename-atpt (&optional arg)
  "Lesserangle FILENAME at point."
  (interactive "*P")
  (ar-th-delimit--intern 'filename "<" ">" arg arg))

(defun ar-lesserangle-filenamenondirectory-atpt (&optional arg)
  "Lesserangle FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-delimit--intern 'filenamenondirectory "<" ">" arg arg))

(defun ar-lesserangle-float-atpt (&optional arg)
  "Lesserangle FLOAT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'float "<" ">" arg arg))

(defun ar-lesserangle-function-atpt (&optional arg)
  "Lesserangle FUNCTION at point."
  (interactive "*P")
  (ar-th-delimit--intern 'function "<" ">" arg arg))

(defun ar-lesserangle-ip-atpt (&optional arg)
  "Lesserangle IP at point."
  (interactive "*P")
  (ar-th-delimit--intern 'ip "<" ">" arg arg))

(defun ar-lesserangle-isbn-atpt (&optional arg)
  "Lesserangle ISBN at point."
  (interactive "*P")
  (ar-th-delimit--intern 'isbn "<" ">" arg arg))

(defun ar-lesserangle-line-atpt (&optional arg)
  "Lesserangle LINE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'line "<" ">" arg arg))

(defun ar-lesserangle-list-atpt (&optional arg)
  "Lesserangle LIST at point."
  (interactive "*P")
  (ar-th-delimit--intern 'list "<" ">" arg arg))

(defun ar-lesserangle-name-atpt (&optional arg)
  "Lesserangle NAME at point."
  (interactive "*P")
  (ar-th-delimit--intern 'name "<" ">" arg arg))

(defun ar-lesserangle-number-atpt (&optional arg)
  "Lesserangle NUMBER at point."
  (interactive "*P")
  (ar-th-delimit--intern 'number "<" ">" arg arg))

(defun ar-lesserangle-page-atpt (&optional arg)
  "Lesserangle PAGE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'page "<" ">" arg arg))

(defun ar-lesserangle-paragraph-atpt (&optional arg)
  "Lesserangle PARAGRAPH at point."
  (interactive "*P")
  (ar-th-delimit--intern 'paragraph "<" ">" arg arg))

(defun ar-lesserangle-phone-atpt (&optional arg)
  "Lesserangle PHONE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'phone "<" ">" arg arg))

(defun ar-lesserangle-region-atpt (&optional arg)
  "Lesserangle REGION at point."
  (interactive "*P")
  (ar-th-delimit--intern 'region "<" ">" arg arg))

(defun ar-lesserangle-sentence-atpt (&optional arg)
  "Lesserangle SENTENCE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'sentence "<" ">" arg arg))

(defun ar-lesserangle-sexp-atpt (&optional arg)
  "Lesserangle SEXP at point."
  (interactive "*P")
  (ar-th-delimit--intern 'sexp "<" ">" arg arg))

(defun ar-lesserangle-shstruct-atpt (&optional arg)
  "Lesserangle SHSTRUCT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'shstruct "<" ">" arg arg))

(defun ar-lesserangle-symbol-atpt (&optional arg)
  "Lesserangle SYMBOL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'symbol "<" ">" arg arg))

(defun ar-lesserangle-url-atpt (&optional arg)
  "Lesserangle URL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'url "<" ">" arg arg))

(defun ar-lesserangle-word-atpt (&optional arg)
  "Lesserangle WORD at point."
  (interactive "*P")
  (ar-th-delimit--intern 'word "<" ">" arg arg))

(defun ar-lesserangle-wordalphaonly-atpt (&optional arg)
  "Lesserangle WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-delimit--intern 'wordalphaonly "<" ">" arg arg))

(defun ar-greaterangle-greateranglednested-atpt (&optional arg)
  "Greaterangle GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'greateranglednested ">" "<" arg arg))

(defun ar-greaterangle-lesseranglednested-atpt (&optional arg)
  "Greaterangle LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'lesseranglednested ">" "<" arg arg))

(defun ar-greaterangle-buffer-atpt (&optional arg)
  "Greaterangle BUFFER at point."
  (interactive "*P")
  (ar-th-delimit--intern 'buffer ">" "<" arg arg))

(defun ar-greaterangle-comment-atpt (&optional arg)
  "Greaterangle COMMENT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'comment ">" "<" arg arg))

(defun ar-greaterangle-csv-atpt (&optional arg)
  "Greaterangle CSV at point."
  (interactive "*P")
  (ar-th-delimit--intern 'csv ">" "<" arg arg))

(defun ar-greaterangle-date-atpt (&optional arg)
  "Greaterangle DATE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'date ">" "<" arg arg))

(defun ar-greaterangle-email-atpt (&optional arg)
  "Greaterangle EMAIL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'email ">" "<" arg arg))

(defun ar-greaterangle-filename-atpt (&optional arg)
  "Greaterangle FILENAME at point."
  (interactive "*P")
  (ar-th-delimit--intern 'filename ">" "<" arg arg))

(defun ar-greaterangle-filenamenondirectory-atpt (&optional arg)
  "Greaterangle FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-delimit--intern 'filenamenondirectory ">" "<" arg arg))

(defun ar-greaterangle-float-atpt (&optional arg)
  "Greaterangle FLOAT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'float ">" "<" arg arg))

(defun ar-greaterangle-function-atpt (&optional arg)
  "Greaterangle FUNCTION at point."
  (interactive "*P")
  (ar-th-delimit--intern 'function ">" "<" arg arg))

(defun ar-greaterangle-ip-atpt (&optional arg)
  "Greaterangle IP at point."
  (interactive "*P")
  (ar-th-delimit--intern 'ip ">" "<" arg arg))

(defun ar-greaterangle-isbn-atpt (&optional arg)
  "Greaterangle ISBN at point."
  (interactive "*P")
  (ar-th-delimit--intern 'isbn ">" "<" arg arg))

(defun ar-greaterangle-line-atpt (&optional arg)
  "Greaterangle LINE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'line ">" "<" arg arg))

(defun ar-greaterangle-list-atpt (&optional arg)
  "Greaterangle LIST at point."
  (interactive "*P")
  (ar-th-delimit--intern 'list ">" "<" arg arg))

(defun ar-greaterangle-name-atpt (&optional arg)
  "Greaterangle NAME at point."
  (interactive "*P")
  (ar-th-delimit--intern 'name ">" "<" arg arg))

(defun ar-greaterangle-number-atpt (&optional arg)
  "Greaterangle NUMBER at point."
  (interactive "*P")
  (ar-th-delimit--intern 'number ">" "<" arg arg))

(defun ar-greaterangle-page-atpt (&optional arg)
  "Greaterangle PAGE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'page ">" "<" arg arg))

(defun ar-greaterangle-paragraph-atpt (&optional arg)
  "Greaterangle PARAGRAPH at point."
  (interactive "*P")
  (ar-th-delimit--intern 'paragraph ">" "<" arg arg))

(defun ar-greaterangle-phone-atpt (&optional arg)
  "Greaterangle PHONE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'phone ">" "<" arg arg))

(defun ar-greaterangle-region-atpt (&optional arg)
  "Greaterangle REGION at point."
  (interactive "*P")
  (ar-th-delimit--intern 'region ">" "<" arg arg))

(defun ar-greaterangle-sentence-atpt (&optional arg)
  "Greaterangle SENTENCE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'sentence ">" "<" arg arg))

(defun ar-greaterangle-sexp-atpt (&optional arg)
  "Greaterangle SEXP at point."
  (interactive "*P")
  (ar-th-delimit--intern 'sexp ">" "<" arg arg))

(defun ar-greaterangle-shstruct-atpt (&optional arg)
  "Greaterangle SHSTRUCT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'shstruct ">" "<" arg arg))

(defun ar-greaterangle-symbol-atpt (&optional arg)
  "Greaterangle SYMBOL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'symbol ">" "<" arg arg))

(defun ar-greaterangle-url-atpt (&optional arg)
  "Greaterangle URL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'url ">" "<" arg arg))

(defun ar-greaterangle-word-atpt (&optional arg)
  "Greaterangle WORD at point."
  (interactive "*P")
  (ar-th-delimit--intern 'word ">" "<" arg arg))

(defun ar-greaterangle-wordalphaonly-atpt (&optional arg)
  "Greaterangle WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-delimit--intern 'wordalphaonly ">" "<" arg arg))

(defun ar-leftrightsinglequote-greateranglednested-atpt (&optional arg)
  "Leftrightsinglequote GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'greateranglednested "‘" "’" arg arg))

(defun ar-leftrightsinglequote-lesseranglednested-atpt (&optional arg)
  "Leftrightsinglequote LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'lesseranglednested "‘" "’" arg arg))

(defun ar-leftrightsinglequote-buffer-atpt (&optional arg)
  "Leftrightsinglequote BUFFER at point."
  (interactive "*P")
  (ar-th-delimit--intern 'buffer "‘" "’" arg arg))

(defun ar-leftrightsinglequote-comment-atpt (&optional arg)
  "Leftrightsinglequote COMMENT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'comment "‘" "’" arg arg))

(defun ar-leftrightsinglequote-csv-atpt (&optional arg)
  "Leftrightsinglequote CSV at point."
  (interactive "*P")
  (ar-th-delimit--intern 'csv "‘" "’" arg arg))

(defun ar-leftrightsinglequote-date-atpt (&optional arg)
  "Leftrightsinglequote DATE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'date "‘" "’" arg arg))

(defun ar-leftrightsinglequote-email-atpt (&optional arg)
  "Leftrightsinglequote EMAIL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'email "‘" "’" arg arg))

(defun ar-leftrightsinglequote-filename-atpt (&optional arg)
  "Leftrightsinglequote FILENAME at point."
  (interactive "*P")
  (ar-th-delimit--intern 'filename "‘" "’" arg arg))

(defun ar-leftrightsinglequote-filenamenondirectory-atpt (&optional arg)
  "Leftrightsinglequote FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-delimit--intern 'filenamenondirectory "‘" "’" arg arg))

(defun ar-leftrightsinglequote-float-atpt (&optional arg)
  "Leftrightsinglequote FLOAT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'float "‘" "’" arg arg))

(defun ar-leftrightsinglequote-function-atpt (&optional arg)
  "Leftrightsinglequote FUNCTION at point."
  (interactive "*P")
  (ar-th-delimit--intern 'function "‘" "’" arg arg))

(defun ar-leftrightsinglequote-ip-atpt (&optional arg)
  "Leftrightsinglequote IP at point."
  (interactive "*P")
  (ar-th-delimit--intern 'ip "‘" "’" arg arg))

(defun ar-leftrightsinglequote-isbn-atpt (&optional arg)
  "Leftrightsinglequote ISBN at point."
  (interactive "*P")
  (ar-th-delimit--intern 'isbn "‘" "’" arg arg))

(defun ar-leftrightsinglequote-line-atpt (&optional arg)
  "Leftrightsinglequote LINE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'line "‘" "’" arg arg))

(defun ar-leftrightsinglequote-list-atpt (&optional arg)
  "Leftrightsinglequote LIST at point."
  (interactive "*P")
  (ar-th-delimit--intern 'list "‘" "’" arg arg))

(defun ar-leftrightsinglequote-name-atpt (&optional arg)
  "Leftrightsinglequote NAME at point."
  (interactive "*P")
  (ar-th-delimit--intern 'name "‘" "’" arg arg))

(defun ar-leftrightsinglequote-number-atpt (&optional arg)
  "Leftrightsinglequote NUMBER at point."
  (interactive "*P")
  (ar-th-delimit--intern 'number "‘" "’" arg arg))

(defun ar-leftrightsinglequote-page-atpt (&optional arg)
  "Leftrightsinglequote PAGE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'page "‘" "’" arg arg))

(defun ar-leftrightsinglequote-paragraph-atpt (&optional arg)
  "Leftrightsinglequote PARAGRAPH at point."
  (interactive "*P")
  (ar-th-delimit--intern 'paragraph "‘" "’" arg arg))

(defun ar-leftrightsinglequote-phone-atpt (&optional arg)
  "Leftrightsinglequote PHONE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'phone "‘" "’" arg arg))

(defun ar-leftrightsinglequote-region-atpt (&optional arg)
  "Leftrightsinglequote REGION at point."
  (interactive "*P")
  (ar-th-delimit--intern 'region "‘" "’" arg arg))

(defun ar-leftrightsinglequote-sentence-atpt (&optional arg)
  "Leftrightsinglequote SENTENCE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'sentence "‘" "’" arg arg))

(defun ar-leftrightsinglequote-sexp-atpt (&optional arg)
  "Leftrightsinglequote SEXP at point."
  (interactive "*P")
  (ar-th-delimit--intern 'sexp "‘" "’" arg arg))

(defun ar-leftrightsinglequote-shstruct-atpt (&optional arg)
  "Leftrightsinglequote SHSTRUCT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'shstruct "‘" "’" arg arg))

(defun ar-leftrightsinglequote-symbol-atpt (&optional arg)
  "Leftrightsinglequote SYMBOL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'symbol "‘" "’" arg arg))

(defun ar-leftrightsinglequote-url-atpt (&optional arg)
  "Leftrightsinglequote URL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'url "‘" "’" arg arg))

(defun ar-leftrightsinglequote-word-atpt (&optional arg)
  "Leftrightsinglequote WORD at point."
  (interactive "*P")
  (ar-th-delimit--intern 'word "‘" "’" arg arg))

(defun ar-leftrightsinglequote-wordalphaonly-atpt (&optional arg)
  "Leftrightsinglequote WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-delimit--intern 'wordalphaonly "‘" "’" arg arg))

(defun ar-leftrightdoublequote-greateranglednested-atpt (&optional arg)
  "Leftrightdoublequote GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'greateranglednested "“" "”" arg arg))

(defun ar-leftrightdoublequote-lesseranglednested-atpt (&optional arg)
  "Leftrightdoublequote LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'lesseranglednested "“" "”" arg arg))

(defun ar-leftrightdoublequote-buffer-atpt (&optional arg)
  "Leftrightdoublequote BUFFER at point."
  (interactive "*P")
  (ar-th-delimit--intern 'buffer "“" "”" arg arg))

(defun ar-leftrightdoublequote-comment-atpt (&optional arg)
  "Leftrightdoublequote COMMENT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'comment "“" "”" arg arg))

(defun ar-leftrightdoublequote-csv-atpt (&optional arg)
  "Leftrightdoublequote CSV at point."
  (interactive "*P")
  (ar-th-delimit--intern 'csv "“" "”" arg arg))

(defun ar-leftrightdoublequote-date-atpt (&optional arg)
  "Leftrightdoublequote DATE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'date "“" "”" arg arg))

(defun ar-leftrightdoublequote-email-atpt (&optional arg)
  "Leftrightdoublequote EMAIL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'email "“" "”" arg arg))

(defun ar-leftrightdoublequote-filename-atpt (&optional arg)
  "Leftrightdoublequote FILENAME at point."
  (interactive "*P")
  (ar-th-delimit--intern 'filename "“" "”" arg arg))

(defun ar-leftrightdoublequote-filenamenondirectory-atpt (&optional arg)
  "Leftrightdoublequote FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-delimit--intern 'filenamenondirectory "“" "”" arg arg))

(defun ar-leftrightdoublequote-float-atpt (&optional arg)
  "Leftrightdoublequote FLOAT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'float "“" "”" arg arg))

(defun ar-leftrightdoublequote-function-atpt (&optional arg)
  "Leftrightdoublequote FUNCTION at point."
  (interactive "*P")
  (ar-th-delimit--intern 'function "“" "”" arg arg))

(defun ar-leftrightdoublequote-ip-atpt (&optional arg)
  "Leftrightdoublequote IP at point."
  (interactive "*P")
  (ar-th-delimit--intern 'ip "“" "”" arg arg))

(defun ar-leftrightdoublequote-isbn-atpt (&optional arg)
  "Leftrightdoublequote ISBN at point."
  (interactive "*P")
  (ar-th-delimit--intern 'isbn "“" "”" arg arg))

(defun ar-leftrightdoublequote-line-atpt (&optional arg)
  "Leftrightdoublequote LINE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'line "“" "”" arg arg))

(defun ar-leftrightdoublequote-list-atpt (&optional arg)
  "Leftrightdoublequote LIST at point."
  (interactive "*P")
  (ar-th-delimit--intern 'list "“" "”" arg arg))

(defun ar-leftrightdoublequote-name-atpt (&optional arg)
  "Leftrightdoublequote NAME at point."
  (interactive "*P")
  (ar-th-delimit--intern 'name "“" "”" arg arg))

(defun ar-leftrightdoublequote-number-atpt (&optional arg)
  "Leftrightdoublequote NUMBER at point."
  (interactive "*P")
  (ar-th-delimit--intern 'number "“" "”" arg arg))

(defun ar-leftrightdoublequote-page-atpt (&optional arg)
  "Leftrightdoublequote PAGE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'page "“" "”" arg arg))

(defun ar-leftrightdoublequote-paragraph-atpt (&optional arg)
  "Leftrightdoublequote PARAGRAPH at point."
  (interactive "*P")
  (ar-th-delimit--intern 'paragraph "“" "”" arg arg))

(defun ar-leftrightdoublequote-phone-atpt (&optional arg)
  "Leftrightdoublequote PHONE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'phone "“" "”" arg arg))

(defun ar-leftrightdoublequote-region-atpt (&optional arg)
  "Leftrightdoublequote REGION at point."
  (interactive "*P")
  (ar-th-delimit--intern 'region "“" "”" arg arg))

(defun ar-leftrightdoublequote-sentence-atpt (&optional arg)
  "Leftrightdoublequote SENTENCE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'sentence "“" "”" arg arg))

(defun ar-leftrightdoublequote-sexp-atpt (&optional arg)
  "Leftrightdoublequote SEXP at point."
  (interactive "*P")
  (ar-th-delimit--intern 'sexp "“" "”" arg arg))

(defun ar-leftrightdoublequote-shstruct-atpt (&optional arg)
  "Leftrightdoublequote SHSTRUCT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'shstruct "“" "”" arg arg))

(defun ar-leftrightdoublequote-symbol-atpt (&optional arg)
  "Leftrightdoublequote SYMBOL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'symbol "“" "”" arg arg))

(defun ar-leftrightdoublequote-url-atpt (&optional arg)
  "Leftrightdoublequote URL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'url "“" "”" arg arg))

(defun ar-leftrightdoublequote-word-atpt (&optional arg)
  "Leftrightdoublequote WORD at point."
  (interactive "*P")
  (ar-th-delimit--intern 'word "“" "”" arg arg))

(defun ar-leftrightdoublequote-wordalphaonly-atpt (&optional arg)
  "Leftrightdoublequote WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-delimit--intern 'wordalphaonly "“" "”" arg arg))

(defun ar-parentize-greateranglednested-atpt (&optional arg)
  "Parentize GREATERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'greateranglednested "(" ")" arg arg))

(defun ar-parentize-lesseranglednested-atpt (&optional arg)
  "Parentize LESSERANGLEDNESTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'lesseranglednested "(" ")" arg arg))

(defun ar-parentize-buffer-atpt (&optional arg)
  "Parentize BUFFER at point."
  (interactive "*P")
  (ar-th-delimit--intern 'buffer "(" ")" arg arg))

(defun ar-parentize-comment-atpt (&optional arg)
  "Parentize COMMENT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'comment "(" ")" arg arg))

(defun ar-parentize-csv-atpt (&optional arg)
  "Parentize CSV at point."
  (interactive "*P")
  (ar-th-delimit--intern 'csv "(" ")" arg arg))

(defun ar-parentize-date-atpt (&optional arg)
  "Parentize DATE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'date "(" ")" arg arg))

(defun ar-parentize-email-atpt (&optional arg)
  "Parentize EMAIL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'email "(" ")" arg arg))

(defun ar-parentize-filename-atpt (&optional arg)
  "Parentize FILENAME at point."
  (interactive "*P")
  (ar-th-delimit--intern 'filename "(" ")" arg arg))

(defun ar-parentize-filenamenondirectory-atpt (&optional arg)
  "Parentize FILENAMENONDIRECTORY at point."
  (interactive "*P")
  (ar-th-delimit--intern 'filenamenondirectory "(" ")" arg arg))

(defun ar-parentize-float-atpt (&optional arg)
  "Parentize FLOAT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'float "(" ")" arg arg))

(defun ar-parentize-function-atpt (&optional arg)
  "Parentize FUNCTION at point."
  (interactive "*P")
  (ar-th-delimit--intern 'function "(" ")" arg arg))

(defun ar-parentize-ip-atpt (&optional arg)
  "Parentize IP at point."
  (interactive "*P")
  (ar-th-delimit--intern 'ip "(" ")" arg arg))

(defun ar-parentize-isbn-atpt (&optional arg)
  "Parentize ISBN at point."
  (interactive "*P")
  (ar-th-delimit--intern 'isbn "(" ")" arg arg))

(defun ar-parentize-line-atpt (&optional arg)
  "Parentize LINE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'line "(" ")" arg arg))

(defun ar-parentize-list-atpt (&optional arg)
  "Parentize LIST at point."
  (interactive "*P")
  (ar-th-delimit--intern 'list "(" ")" arg arg))

(defun ar-parentize-name-atpt (&optional arg)
  "Parentize NAME at point."
  (interactive "*P")
  (ar-th-delimit--intern 'name "(" ")" arg arg))

(defun ar-parentize-number-atpt (&optional arg)
  "Parentize NUMBER at point."
  (interactive "*P")
  (ar-th-delimit--intern 'number "(" ")" arg arg))

(defun ar-parentize-page-atpt (&optional arg)
  "Parentize PAGE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'page "(" ")" arg arg))

(defun ar-parentize-paragraph-atpt (&optional arg)
  "Parentize PARAGRAPH at point."
  (interactive "*P")
  (ar-th-delimit--intern 'paragraph "(" ")" arg arg))

(defun ar-parentize-phone-atpt (&optional arg)
  "Parentize PHONE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'phone "(" ")" arg arg))

(defun ar-parentize-region-atpt (&optional arg)
  "Parentize REGION at point."
  (interactive "*P")
  (ar-th-delimit--intern 'region "(" ")" arg arg))

(defun ar-parentize-sentence-atpt (&optional arg)
  "Parentize SENTENCE at point."
  (interactive "*P")
  (ar-th-delimit--intern 'sentence "(" ")" arg arg))

(defun ar-parentize-sexp-atpt (&optional arg)
  "Parentize SEXP at point."
  (interactive "*P")
  (ar-th-delimit--intern 'sexp "(" ")" arg arg))

(defun ar-parentize-shstruct-atpt (&optional arg)
  "Parentize SHSTRUCT at point."
  (interactive "*P")
  (ar-th-delimit--intern 'shstruct "(" ")" arg arg))

(defun ar-parentize-symbol-atpt (&optional arg)
  "Parentize SYMBOL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'symbol "(" ")" arg arg))

(defun ar-parentize-url-atpt (&optional arg)
  "Parentize URL at point."
  (interactive "*P")
  (ar-th-delimit--intern 'url "(" ")" arg arg))

(defun ar-parentize-word-atpt (&optional arg)
  "Parentize WORD at point."
  (interactive "*P")
  (ar-th-delimit--intern 'word "(" ")" arg arg))

(defun ar-parentize-wordalphaonly-atpt (&optional arg)
  "Parentize WORDALPHAONLY at point."
  (interactive "*P")
  (ar-th-delimit--intern 'wordalphaonly "(" ")" arg arg))

;; ar-thing-at-point-utils-pair-rest ar-paired-delimit-aktiv-raw ar-atpt-rest-list: end
;; ar-thing-at-point-utils-pair-rest ar-paired-delimit-aktiv-raw ar-unpaired-delimited-passiv: start
(defun ar-brace-backslashed-atpt (&optional arg)
  "Brace BACKSLASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'backslashed "{" "}" arg arg))

(defun ar-brace-backticked-atpt (&optional arg)
  "Brace BACKTICKED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'backticked "{" "}" arg arg))

(defun ar-brace-coloned-atpt (&optional arg)
  "Brace COLONED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'coloned "{" "}" arg arg))

(defun ar-brace-crossed-atpt (&optional arg)
  "Brace CROSSED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'crossed "{" "}" arg arg))

(defun ar-brace-dollared-atpt (&optional arg)
  "Brace DOLLARED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'dollared "{" "}" arg arg))

(defun ar-brace-doublequoted-atpt (&optional arg)
  "Brace DOUBLEQUOTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'doublequoted "{" "}" arg arg))

(defun ar-brace-equalized-atpt (&optional arg)
  "Brace EQUALIZED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'equalized "{" "}" arg arg))

(defun ar-brace-hashed-atpt (&optional arg)
  "Brace HASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'hashed "{" "}" arg arg))

(defun ar-brace-hyphened-atpt (&optional arg)
  "Brace HYPHENED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'hyphened "{" "}" arg arg))

(defun ar-brace-singlequoted-atpt (&optional arg)
  "Brace SINGLEQUOTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'singlequoted "{" "}" arg arg))

(defun ar-brace-slashed-atpt (&optional arg)
  "Brace SLASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'slashed "{" "}" arg arg))

(defun ar-brace-stared-atpt (&optional arg)
  "Brace STARED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'stared "{" "}" arg arg))

(defun ar-brace-tilded-atpt (&optional arg)
  "Brace TILDED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'tilded "{" "}" arg arg))

(defun ar-brace-underscored-atpt (&optional arg)
  "Brace UNDERSCORED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'underscored "{" "}" arg arg))

(defun ar-brace-whitespaced-atpt (&optional arg)
  "Brace WHITESPACED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'whitespaced "{" "}" arg arg))

(defun ar-bracket-backslashed-atpt (&optional arg)
  "Bracket BACKSLASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'backslashed "[" "]" arg arg))

(defun ar-bracket-backticked-atpt (&optional arg)
  "Bracket BACKTICKED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'backticked "[" "]" arg arg))

(defun ar-bracket-coloned-atpt (&optional arg)
  "Bracket COLONED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'coloned "[" "]" arg arg))

(defun ar-bracket-crossed-atpt (&optional arg)
  "Bracket CROSSED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'crossed "[" "]" arg arg))

(defun ar-bracket-dollared-atpt (&optional arg)
  "Bracket DOLLARED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'dollared "[" "]" arg arg))

(defun ar-bracket-doublequoted-atpt (&optional arg)
  "Bracket DOUBLEQUOTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'doublequoted "[" "]" arg arg))

(defun ar-bracket-equalized-atpt (&optional arg)
  "Bracket EQUALIZED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'equalized "[" "]" arg arg))

(defun ar-bracket-hashed-atpt (&optional arg)
  "Bracket HASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'hashed "[" "]" arg arg))

(defun ar-bracket-hyphened-atpt (&optional arg)
  "Bracket HYPHENED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'hyphened "[" "]" arg arg))

(defun ar-bracket-singlequoted-atpt (&optional arg)
  "Bracket SINGLEQUOTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'singlequoted "[" "]" arg arg))

(defun ar-bracket-slashed-atpt (&optional arg)
  "Bracket SLASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'slashed "[" "]" arg arg))

(defun ar-bracket-stared-atpt (&optional arg)
  "Bracket STARED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'stared "[" "]" arg arg))

(defun ar-bracket-tilded-atpt (&optional arg)
  "Bracket TILDED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'tilded "[" "]" arg arg))

(defun ar-bracket-underscored-atpt (&optional arg)
  "Bracket UNDERSCORED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'underscored "[" "]" arg arg))

(defun ar-bracket-whitespaced-atpt (&optional arg)
  "Bracket WHITESPACED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'whitespaced "[" "]" arg arg))

(defun ar-lesserangle-backslashed-atpt (&optional arg)
  "Lesserangle BACKSLASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'backslashed "<" ">" arg arg))

(defun ar-lesserangle-backticked-atpt (&optional arg)
  "Lesserangle BACKTICKED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'backticked "<" ">" arg arg))

(defun ar-lesserangle-coloned-atpt (&optional arg)
  "Lesserangle COLONED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'coloned "<" ">" arg arg))

(defun ar-lesserangle-crossed-atpt (&optional arg)
  "Lesserangle CROSSED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'crossed "<" ">" arg arg))

(defun ar-lesserangle-dollared-atpt (&optional arg)
  "Lesserangle DOLLARED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'dollared "<" ">" arg arg))

(defun ar-lesserangle-doublequoted-atpt (&optional arg)
  "Lesserangle DOUBLEQUOTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'doublequoted "<" ">" arg arg))

(defun ar-lesserangle-equalized-atpt (&optional arg)
  "Lesserangle EQUALIZED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'equalized "<" ">" arg arg))

(defun ar-lesserangle-hashed-atpt (&optional arg)
  "Lesserangle HASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'hashed "<" ">" arg arg))

(defun ar-lesserangle-hyphened-atpt (&optional arg)
  "Lesserangle HYPHENED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'hyphened "<" ">" arg arg))

(defun ar-lesserangle-singlequoted-atpt (&optional arg)
  "Lesserangle SINGLEQUOTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'singlequoted "<" ">" arg arg))

(defun ar-lesserangle-slashed-atpt (&optional arg)
  "Lesserangle SLASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'slashed "<" ">" arg arg))

(defun ar-lesserangle-stared-atpt (&optional arg)
  "Lesserangle STARED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'stared "<" ">" arg arg))

(defun ar-lesserangle-tilded-atpt (&optional arg)
  "Lesserangle TILDED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'tilded "<" ">" arg arg))

(defun ar-lesserangle-underscored-atpt (&optional arg)
  "Lesserangle UNDERSCORED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'underscored "<" ">" arg arg))

(defun ar-lesserangle-whitespaced-atpt (&optional arg)
  "Lesserangle WHITESPACED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'whitespaced "<" ">" arg arg))

(defun ar-greaterangle-backslashed-atpt (&optional arg)
  "Greaterangle BACKSLASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'backslashed ">" "<" arg arg))

(defun ar-greaterangle-backticked-atpt (&optional arg)
  "Greaterangle BACKTICKED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'backticked ">" "<" arg arg))

(defun ar-greaterangle-coloned-atpt (&optional arg)
  "Greaterangle COLONED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'coloned ">" "<" arg arg))

(defun ar-greaterangle-crossed-atpt (&optional arg)
  "Greaterangle CROSSED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'crossed ">" "<" arg arg))

(defun ar-greaterangle-dollared-atpt (&optional arg)
  "Greaterangle DOLLARED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'dollared ">" "<" arg arg))

(defun ar-greaterangle-doublequoted-atpt (&optional arg)
  "Greaterangle DOUBLEQUOTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'doublequoted ">" "<" arg arg))

(defun ar-greaterangle-equalized-atpt (&optional arg)
  "Greaterangle EQUALIZED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'equalized ">" "<" arg arg))

(defun ar-greaterangle-hashed-atpt (&optional arg)
  "Greaterangle HASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'hashed ">" "<" arg arg))

(defun ar-greaterangle-hyphened-atpt (&optional arg)
  "Greaterangle HYPHENED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'hyphened ">" "<" arg arg))

(defun ar-greaterangle-singlequoted-atpt (&optional arg)
  "Greaterangle SINGLEQUOTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'singlequoted ">" "<" arg arg))

(defun ar-greaterangle-slashed-atpt (&optional arg)
  "Greaterangle SLASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'slashed ">" "<" arg arg))

(defun ar-greaterangle-stared-atpt (&optional arg)
  "Greaterangle STARED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'stared ">" "<" arg arg))

(defun ar-greaterangle-tilded-atpt (&optional arg)
  "Greaterangle TILDED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'tilded ">" "<" arg arg))

(defun ar-greaterangle-underscored-atpt (&optional arg)
  "Greaterangle UNDERSCORED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'underscored ">" "<" arg arg))

(defun ar-greaterangle-whitespaced-atpt (&optional arg)
  "Greaterangle WHITESPACED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'whitespaced ">" "<" arg arg))

(defun ar-leftrightsinglequote-backslashed-atpt (&optional arg)
  "Leftrightsinglequote BACKSLASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'backslashed "‘" "’" arg arg))

(defun ar-leftrightsinglequote-backticked-atpt (&optional arg)
  "Leftrightsinglequote BACKTICKED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'backticked "‘" "’" arg arg))

(defun ar-leftrightsinglequote-coloned-atpt (&optional arg)
  "Leftrightsinglequote COLONED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'coloned "‘" "’" arg arg))

(defun ar-leftrightsinglequote-crossed-atpt (&optional arg)
  "Leftrightsinglequote CROSSED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'crossed "‘" "’" arg arg))

(defun ar-leftrightsinglequote-dollared-atpt (&optional arg)
  "Leftrightsinglequote DOLLARED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'dollared "‘" "’" arg arg))

(defun ar-leftrightsinglequote-doublequoted-atpt (&optional arg)
  "Leftrightsinglequote DOUBLEQUOTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'doublequoted "‘" "’" arg arg))

(defun ar-leftrightsinglequote-equalized-atpt (&optional arg)
  "Leftrightsinglequote EQUALIZED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'equalized "‘" "’" arg arg))

(defun ar-leftrightsinglequote-hashed-atpt (&optional arg)
  "Leftrightsinglequote HASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'hashed "‘" "’" arg arg))

(defun ar-leftrightsinglequote-hyphened-atpt (&optional arg)
  "Leftrightsinglequote HYPHENED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'hyphened "‘" "’" arg arg))

(defun ar-leftrightsinglequote-singlequoted-atpt (&optional arg)
  "Leftrightsinglequote SINGLEQUOTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'singlequoted "‘" "’" arg arg))

(defun ar-leftrightsinglequote-slashed-atpt (&optional arg)
  "Leftrightsinglequote SLASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'slashed "‘" "’" arg arg))

(defun ar-leftrightsinglequote-stared-atpt (&optional arg)
  "Leftrightsinglequote STARED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'stared "‘" "’" arg arg))

(defun ar-leftrightsinglequote-tilded-atpt (&optional arg)
  "Leftrightsinglequote TILDED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'tilded "‘" "’" arg arg))

(defun ar-leftrightsinglequote-underscored-atpt (&optional arg)
  "Leftrightsinglequote UNDERSCORED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'underscored "‘" "’" arg arg))

(defun ar-leftrightsinglequote-whitespaced-atpt (&optional arg)
  "Leftrightsinglequote WHITESPACED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'whitespaced "‘" "’" arg arg))

(defun ar-leftrightdoublequote-backslashed-atpt (&optional arg)
  "Leftrightdoublequote BACKSLASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'backslashed "“" "”" arg arg))

(defun ar-leftrightdoublequote-backticked-atpt (&optional arg)
  "Leftrightdoublequote BACKTICKED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'backticked "“" "”" arg arg))

(defun ar-leftrightdoublequote-coloned-atpt (&optional arg)
  "Leftrightdoublequote COLONED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'coloned "“" "”" arg arg))

(defun ar-leftrightdoublequote-crossed-atpt (&optional arg)
  "Leftrightdoublequote CROSSED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'crossed "“" "”" arg arg))

(defun ar-leftrightdoublequote-dollared-atpt (&optional arg)
  "Leftrightdoublequote DOLLARED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'dollared "“" "”" arg arg))

(defun ar-leftrightdoublequote-doublequoted-atpt (&optional arg)
  "Leftrightdoublequote DOUBLEQUOTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'doublequoted "“" "”" arg arg))

(defun ar-leftrightdoublequote-equalized-atpt (&optional arg)
  "Leftrightdoublequote EQUALIZED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'equalized "“" "”" arg arg))

(defun ar-leftrightdoublequote-hashed-atpt (&optional arg)
  "Leftrightdoublequote HASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'hashed "“" "”" arg arg))

(defun ar-leftrightdoublequote-hyphened-atpt (&optional arg)
  "Leftrightdoublequote HYPHENED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'hyphened "“" "”" arg arg))

(defun ar-leftrightdoublequote-singlequoted-atpt (&optional arg)
  "Leftrightdoublequote SINGLEQUOTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'singlequoted "“" "”" arg arg))

(defun ar-leftrightdoublequote-slashed-atpt (&optional arg)
  "Leftrightdoublequote SLASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'slashed "“" "”" arg arg))

(defun ar-leftrightdoublequote-stared-atpt (&optional arg)
  "Leftrightdoublequote STARED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'stared "“" "”" arg arg))

(defun ar-leftrightdoublequote-tilded-atpt (&optional arg)
  "Leftrightdoublequote TILDED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'tilded "“" "”" arg arg))

(defun ar-leftrightdoublequote-underscored-atpt (&optional arg)
  "Leftrightdoublequote UNDERSCORED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'underscored "“" "”" arg arg))

(defun ar-leftrightdoublequote-whitespaced-atpt (&optional arg)
  "Leftrightdoublequote WHITESPACED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'whitespaced "“" "”" arg arg))

(defun ar-parentize-backslashed-atpt (&optional arg)
  "Parentize BACKSLASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'backslashed "(" ")" arg arg))

(defun ar-parentize-backticked-atpt (&optional arg)
  "Parentize BACKTICKED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'backticked "(" ")" arg arg))

(defun ar-parentize-coloned-atpt (&optional arg)
  "Parentize COLONED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'coloned "(" ")" arg arg))

(defun ar-parentize-crossed-atpt (&optional arg)
  "Parentize CROSSED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'crossed "(" ")" arg arg))

(defun ar-parentize-dollared-atpt (&optional arg)
  "Parentize DOLLARED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'dollared "(" ")" arg arg))

(defun ar-parentize-doublequoted-atpt (&optional arg)
  "Parentize DOUBLEQUOTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'doublequoted "(" ")" arg arg))

(defun ar-parentize-equalized-atpt (&optional arg)
  "Parentize EQUALIZED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'equalized "(" ")" arg arg))

(defun ar-parentize-hashed-atpt (&optional arg)
  "Parentize HASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'hashed "(" ")" arg arg))

(defun ar-parentize-hyphened-atpt (&optional arg)
  "Parentize HYPHENED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'hyphened "(" ")" arg arg))

(defun ar-parentize-singlequoted-atpt (&optional arg)
  "Parentize SINGLEQUOTED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'singlequoted "(" ")" arg arg))

(defun ar-parentize-slashed-atpt (&optional arg)
  "Parentize SLASHED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'slashed "(" ")" arg arg))

(defun ar-parentize-stared-atpt (&optional arg)
  "Parentize STARED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'stared "(" ")" arg arg))

(defun ar-parentize-tilded-atpt (&optional arg)
  "Parentize TILDED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'tilded "(" ")" arg arg))

(defun ar-parentize-underscored-atpt (&optional arg)
  "Parentize UNDERSCORED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'underscored "(" ")" arg arg))

(defun ar-parentize-whitespaced-atpt (&optional arg)
  "Parentize WHITESPACED at point."
  (interactive "*P")
  (ar-th-delimit--intern 'whitespaced "(" ")" arg arg))

;; ar-thing-at-point-utils-pair-rest ar-paired-delimit-aktiv-raw ar-unpaired-delimited-passiv: end
;; ar-thing-at-point-utils-activ-passiv ar-paired-delimited-passiv-raw: start
(defun ar-brace-braced-atpt ()
  "Brace BRACED at point."
  (interactive "*")
  (ar-th-delimit--intern 'braced "{" "}"))

(defun ar-brace-bracketed-atpt ()
  "Brace BRACKETED at point."
  (interactive "*")
  (ar-th-delimit--intern 'bracketed "{" "}"))

(defun ar-brace-lesserangled-atpt ()
  "Brace LESSERANGLED at point."
  (interactive "*")
  (ar-th-delimit--intern 'lesserangled "{" "}"))

(defun ar-brace-greaterangled-atpt ()
  "Brace GREATERANGLED at point."
  (interactive "*")
  (ar-th-delimit--intern 'greaterangled "{" "}"))

(defun ar-brace-leftrightsinglequoted-atpt ()
  "Brace LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  (ar-th-delimit--intern 'leftrightsinglequoted "{" "}"))

(defun ar-brace-leftrightdoublequoted-atpt ()
  "Brace LEFTRIGHTDOUBLEQUOTED at point."
  (interactive "*")
  (ar-th-delimit--intern 'leftrightdoublequoted "{" "}"))

(defun ar-brace-parentized-atpt ()
  "Brace PARENTIZED at point."
  (interactive "*")
  (ar-th-delimit--intern 'parentized "{" "}"))

(defun ar-bracket-braced-atpt ()
  "Bracket BRACED at point."
  (interactive "*")
  (ar-th-delimit--intern 'braced "[" "]"))

(defun ar-bracket-bracketed-atpt ()
  "Bracket BRACKETED at point."
  (interactive "*")
  (ar-th-delimit--intern 'bracketed "[" "]"))

(defun ar-bracket-lesserangled-atpt ()
  "Bracket LESSERANGLED at point."
  (interactive "*")
  (ar-th-delimit--intern 'lesserangled "[" "]"))

(defun ar-bracket-greaterangled-atpt ()
  "Bracket GREATERANGLED at point."
  (interactive "*")
  (ar-th-delimit--intern 'greaterangled "[" "]"))

(defun ar-bracket-leftrightsinglequoted-atpt ()
  "Bracket LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  (ar-th-delimit--intern 'leftrightsinglequoted "[" "]"))

(defun ar-bracket-leftrightdoublequoted-atpt ()
  "Bracket LEFTRIGHTDOUBLEQUOTED at point."
  (interactive "*")
  (ar-th-delimit--intern 'leftrightdoublequoted "[" "]"))

(defun ar-bracket-parentized-atpt ()
  "Bracket PARENTIZED at point."
  (interactive "*")
  (ar-th-delimit--intern 'parentized "[" "]"))

(defun ar-lesserangle-braced-atpt ()
  "Lesserangle BRACED at point."
  (interactive "*")
  (ar-th-delimit--intern 'braced "<" ">"))

(defun ar-lesserangle-bracketed-atpt ()
  "Lesserangle BRACKETED at point."
  (interactive "*")
  (ar-th-delimit--intern 'bracketed "<" ">"))

(defun ar-lesserangle-lesserangled-atpt ()
  "Lesserangle LESSERANGLED at point."
  (interactive "*")
  (ar-th-delimit--intern 'lesserangled "<" ">"))

(defun ar-lesserangle-greaterangled-atpt ()
  "Lesserangle GREATERANGLED at point."
  (interactive "*")
  (ar-th-delimit--intern 'greaterangled "<" ">"))

(defun ar-lesserangle-leftrightsinglequoted-atpt ()
  "Lesserangle LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  (ar-th-delimit--intern 'leftrightsinglequoted "<" ">"))

(defun ar-lesserangle-leftrightdoublequoted-atpt ()
  "Lesserangle LEFTRIGHTDOUBLEQUOTED at point."
  (interactive "*")
  (ar-th-delimit--intern 'leftrightdoublequoted "<" ">"))

(defun ar-lesserangle-parentized-atpt ()
  "Lesserangle PARENTIZED at point."
  (interactive "*")
  (ar-th-delimit--intern 'parentized "<" ">"))

(defun ar-greaterangle-braced-atpt ()
  "Greaterangle BRACED at point."
  (interactive "*")
  (ar-th-delimit--intern 'braced ">" "<"))

(defun ar-greaterangle-bracketed-atpt ()
  "Greaterangle BRACKETED at point."
  (interactive "*")
  (ar-th-delimit--intern 'bracketed ">" "<"))

(defun ar-greaterangle-lesserangled-atpt ()
  "Greaterangle LESSERANGLED at point."
  (interactive "*")
  (ar-th-delimit--intern 'lesserangled ">" "<"))

(defun ar-greaterangle-greaterangled-atpt ()
  "Greaterangle GREATERANGLED at point."
  (interactive "*")
  (ar-th-delimit--intern 'greaterangled ">" "<"))

(defun ar-greaterangle-leftrightsinglequoted-atpt ()
  "Greaterangle LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  (ar-th-delimit--intern 'leftrightsinglequoted ">" "<"))

(defun ar-greaterangle-leftrightdoublequoted-atpt ()
  "Greaterangle LEFTRIGHTDOUBLEQUOTED at point."
  (interactive "*")
  (ar-th-delimit--intern 'leftrightdoublequoted ">" "<"))

(defun ar-greaterangle-parentized-atpt ()
  "Greaterangle PARENTIZED at point."
  (interactive "*")
  (ar-th-delimit--intern 'parentized ">" "<"))

(defun ar-leftrightsinglequote-braced-atpt ()
  "Leftrightsinglequote BRACED at point."
  (interactive "*")
  (ar-th-delimit--intern 'braced "‘" "’"))

(defun ar-leftrightsinglequote-bracketed-atpt ()
  "Leftrightsinglequote BRACKETED at point."
  (interactive "*")
  (ar-th-delimit--intern 'bracketed "‘" "’"))

(defun ar-leftrightsinglequote-lesserangled-atpt ()
  "Leftrightsinglequote LESSERANGLED at point."
  (interactive "*")
  (ar-th-delimit--intern 'lesserangled "‘" "’"))

(defun ar-leftrightsinglequote-greaterangled-atpt ()
  "Leftrightsinglequote GREATERANGLED at point."
  (interactive "*")
  (ar-th-delimit--intern 'greaterangled "‘" "’"))

(defun ar-leftrightsinglequote-leftrightsinglequoted-atpt ()
  "Leftrightsinglequote LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  (ar-th-delimit--intern 'leftrightsinglequoted "‘" "’"))

(defun ar-leftrightsinglequote-leftrightdoublequoted-atpt ()
  "Leftrightsinglequote LEFTRIGHTDOUBLEQUOTED at point."
  (interactive "*")
  (ar-th-delimit--intern 'leftrightdoublequoted "‘" "’"))

(defun ar-leftrightsinglequote-parentized-atpt ()
  "Leftrightsinglequote PARENTIZED at point."
  (interactive "*")
  (ar-th-delimit--intern 'parentized "‘" "’"))

(defun ar-leftrightdoublequote-braced-atpt ()
  "Leftrightdoublequote BRACED at point."
  (interactive "*")
  (ar-th-delimit--intern 'braced "“" "”"))

(defun ar-leftrightdoublequote-bracketed-atpt ()
  "Leftrightdoublequote BRACKETED at point."
  (interactive "*")
  (ar-th-delimit--intern 'bracketed "“" "”"))

(defun ar-leftrightdoublequote-lesserangled-atpt ()
  "Leftrightdoublequote LESSERANGLED at point."
  (interactive "*")
  (ar-th-delimit--intern 'lesserangled "“" "”"))

(defun ar-leftrightdoublequote-greaterangled-atpt ()
  "Leftrightdoublequote GREATERANGLED at point."
  (interactive "*")
  (ar-th-delimit--intern 'greaterangled "“" "”"))

(defun ar-leftrightdoublequote-leftrightsinglequoted-atpt ()
  "Leftrightdoublequote LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  (ar-th-delimit--intern 'leftrightsinglequoted "“" "”"))

(defun ar-leftrightdoublequote-leftrightdoublequoted-atpt ()
  "Leftrightdoublequote LEFTRIGHTDOUBLEQUOTED at point."
  (interactive "*")
  (ar-th-delimit--intern 'leftrightdoublequoted "“" "”"))

(defun ar-leftrightdoublequote-parentized-atpt ()
  "Leftrightdoublequote PARENTIZED at point."
  (interactive "*")
  (ar-th-delimit--intern 'parentized "“" "”"))

(defun ar-parentize-braced-atpt ()
  "Parentize BRACED at point."
  (interactive "*")
  (ar-th-delimit--intern 'braced "(" ")"))

(defun ar-parentize-bracketed-atpt ()
  "Parentize BRACKETED at point."
  (interactive "*")
  (ar-th-delimit--intern 'bracketed "(" ")"))

(defun ar-parentize-lesserangled-atpt ()
  "Parentize LESSERANGLED at point."
  (interactive "*")
  (ar-th-delimit--intern 'lesserangled "(" ")"))

(defun ar-parentize-greaterangled-atpt ()
  "Parentize GREATERANGLED at point."
  (interactive "*")
  (ar-th-delimit--intern 'greaterangled "(" ")"))

(defun ar-parentize-leftrightsinglequoted-atpt ()
  "Parentize LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  (ar-th-delimit--intern 'leftrightsinglequoted "(" ")"))

(defun ar-parentize-leftrightdoublequoted-atpt ()
  "Parentize LEFTRIGHTDOUBLEQUOTED at point."
  (interactive "*")
  (ar-th-delimit--intern 'leftrightdoublequoted "(" ")"))

(defun ar-parentize-parentized-atpt ()
  "Parentize PARENTIZED at point."
  (interactive "*")
  (ar-th-delimit--intern 'parentized "(" ")"))

;; ar-thing-at-point-utils-activ-passiv ar-paired-delimited-passiv-raw: end
;; ar-thing-at-point-utils-nodelim-core ar-atpt-classes: start

(defun ar-alnum-atpt () 
  "Returns alnum at point if any, nil otherwise. "
  (interactive)
  (ar-th 'alnum nil (called-interactively-p 'any)))

(defun ar-bounds-of-alnum-atpt ()
  "Returns a list, borders of alnum if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'alnum nil (called-interactively-p 'any)))

(defun ar-alnum-beginning-position-atpt ()
  "Returns a number, beginning position ALNUM at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'alnum nil (called-interactively-p 'any)))

(defun ar-alnum-end-position-atpt ()
  "Returns a number, end position of ALNUM at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'alnum nil (called-interactively-p 'any)))

(defun ar-alnum-beginning-atpt ()
  "Goto beginning of symbol or char-class ALNUM at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'alnum nil (called-interactively-p 'any)))

(defun ar-alnum-end-atpt ()
  "Goto end of symbol or char-class ALNUM at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'alnum nil (called-interactively-p 'any)))

(defun ar-in-alnum-p-atpt ()
  "Returns bounds of ALNUM at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'alnum nil (called-interactively-p 'any)))

(defun ar-length-of-alnum-atpt ()
  "Returns beginning of symbol or char-class ALNUM at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'alnum nil (called-interactively-p 'any)))

(defun ar-copy-alnum-atpt ()
  "Returns a copy of ALNUM at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'alnum nil (called-interactively-p 'any)))

(defun ar-delete-alnum-atpt (&optional arg)
  "Deletes alnum at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'alnum arg (called-interactively-p 'any)))

(defun ar-kill-alnum-atpt (&optional arg)
  "Deletes alnum at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'alnum arg (called-interactively-p 'any)))

(defun ar-forward-alnum-atpt (&optional arg)
  "Deletes alnum at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'alnum arg (called-interactively-p 'any)))

(defun ar-backward-alnum-atpt (&optional arg)
  "Deletes alnum at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'alnum arg (called-interactively-p 'any)))

(defun ar-triplequotedq-alnum-atpt (&optional arg)
  "Deletes alnum at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'alnum arg (called-interactively-p 'any)))

(defun ar-triplequotesq-alnum-atpt (&optional arg)
  "Deletes alnum at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'alnum arg (called-interactively-p 'any)))

(defun ar-delete-alnum-in-region (beg end)
  "Deletes ALNUM at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'alnum beg end (called-interactively-p 'any)))

(defun ar-alpha-atpt () 
  "Returns alpha at point if any, nil otherwise. "
  (interactive)
  (ar-th 'alpha nil (called-interactively-p 'any)))

(defun ar-bounds-of-alpha-atpt ()
  "Returns a list, borders of alpha if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'alpha nil (called-interactively-p 'any)))

(defun ar-alpha-beginning-position-atpt ()
  "Returns a number, beginning position ALPHA at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'alpha nil (called-interactively-p 'any)))

(defun ar-alpha-end-position-atpt ()
  "Returns a number, end position of ALPHA at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'alpha nil (called-interactively-p 'any)))

(defun ar-alpha-beginning-atpt ()
  "Goto beginning of symbol or char-class ALPHA at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'alpha nil (called-interactively-p 'any)))

(defun ar-alpha-end-atpt ()
  "Goto end of symbol or char-class ALPHA at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'alpha nil (called-interactively-p 'any)))

(defun ar-in-alpha-p-atpt ()
  "Returns bounds of ALPHA at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'alpha nil (called-interactively-p 'any)))

(defun ar-length-of-alpha-atpt ()
  "Returns beginning of symbol or char-class ALPHA at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'alpha nil (called-interactively-p 'any)))

(defun ar-copy-alpha-atpt ()
  "Returns a copy of ALPHA at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'alpha nil (called-interactively-p 'any)))

(defun ar-delete-alpha-atpt (&optional arg)
  "Deletes alpha at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'alpha arg (called-interactively-p 'any)))

(defun ar-kill-alpha-atpt (&optional arg)
  "Deletes alpha at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'alpha arg (called-interactively-p 'any)))

(defun ar-forward-alpha-atpt (&optional arg)
  "Deletes alpha at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'alpha arg (called-interactively-p 'any)))

(defun ar-backward-alpha-atpt (&optional arg)
  "Deletes alpha at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'alpha arg (called-interactively-p 'any)))

(defun ar-triplequotedq-alpha-atpt (&optional arg)
  "Deletes alpha at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'alpha arg (called-interactively-p 'any)))

(defun ar-triplequotesq-alpha-atpt (&optional arg)
  "Deletes alpha at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'alpha arg (called-interactively-p 'any)))

(defun ar-delete-alpha-in-region (beg end)
  "Deletes ALPHA at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'alpha beg end (called-interactively-p 'any)))

(defun ar-ascii-atpt () 
  "Returns ascii at point if any, nil otherwise. "
  (interactive)
  (ar-th 'ascii nil (called-interactively-p 'any)))

(defun ar-bounds-of-ascii-atpt ()
  "Returns a list, borders of ascii if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'ascii nil (called-interactively-p 'any)))

(defun ar-ascii-beginning-position-atpt ()
  "Returns a number, beginning position ASCII at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'ascii nil (called-interactively-p 'any)))

(defun ar-ascii-end-position-atpt ()
  "Returns a number, end position of ASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'ascii nil (called-interactively-p 'any)))

(defun ar-ascii-beginning-atpt ()
  "Goto beginning of symbol or char-class ASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'ascii nil (called-interactively-p 'any)))

(defun ar-ascii-end-atpt ()
  "Goto end of symbol or char-class ASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'ascii nil (called-interactively-p 'any)))

(defun ar-in-ascii-p-atpt ()
  "Returns bounds of ASCII at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'ascii nil (called-interactively-p 'any)))

(defun ar-length-of-ascii-atpt ()
  "Returns beginning of symbol or char-class ASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'ascii nil (called-interactively-p 'any)))

(defun ar-copy-ascii-atpt ()
  "Returns a copy of ASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'ascii nil (called-interactively-p 'any)))

(defun ar-delete-ascii-atpt (&optional arg)
  "Deletes ascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'ascii arg (called-interactively-p 'any)))

(defun ar-kill-ascii-atpt (&optional arg)
  "Deletes ascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'ascii arg (called-interactively-p 'any)))

(defun ar-forward-ascii-atpt (&optional arg)
  "Deletes ascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'ascii arg (called-interactively-p 'any)))

(defun ar-backward-ascii-atpt (&optional arg)
  "Deletes ascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'ascii arg (called-interactively-p 'any)))

(defun ar-triplequotedq-ascii-atpt (&optional arg)
  "Deletes ascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'ascii arg (called-interactively-p 'any)))

(defun ar-triplequotesq-ascii-atpt (&optional arg)
  "Deletes ascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'ascii arg (called-interactively-p 'any)))

(defun ar-delete-ascii-in-region (beg end)
  "Deletes ASCII at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'ascii beg end (called-interactively-p 'any)))

(defun ar-blank-atpt () 
  "Returns blank at point if any, nil otherwise. "
  (interactive)
  (ar-th 'blank nil (called-interactively-p 'any)))

(defun ar-bounds-of-blank-atpt ()
  "Returns a list, borders of blank if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'blank nil (called-interactively-p 'any)))

(defun ar-blank-beginning-position-atpt ()
  "Returns a number, beginning position BLANK at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'blank nil (called-interactively-p 'any)))

(defun ar-blank-end-position-atpt ()
  "Returns a number, end position of BLANK at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'blank nil (called-interactively-p 'any)))

(defun ar-blank-beginning-atpt ()
  "Goto beginning of symbol or char-class BLANK at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'blank nil (called-interactively-p 'any)))

(defun ar-blank-end-atpt ()
  "Goto end of symbol or char-class BLANK at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'blank nil (called-interactively-p 'any)))

(defun ar-in-blank-p-atpt ()
  "Returns bounds of BLANK at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'blank nil (called-interactively-p 'any)))

(defun ar-length-of-blank-atpt ()
  "Returns beginning of symbol or char-class BLANK at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'blank nil (called-interactively-p 'any)))

(defun ar-copy-blank-atpt ()
  "Returns a copy of BLANK at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'blank nil (called-interactively-p 'any)))

(defun ar-delete-blank-atpt (&optional arg)
  "Deletes blank at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'blank arg (called-interactively-p 'any)))

(defun ar-kill-blank-atpt (&optional arg)
  "Deletes blank at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'blank arg (called-interactively-p 'any)))

(defun ar-forward-blank-atpt (&optional arg)
  "Deletes blank at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'blank arg (called-interactively-p 'any)))

(defun ar-backward-blank-atpt (&optional arg)
  "Deletes blank at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'blank arg (called-interactively-p 'any)))

(defun ar-triplequotedq-blank-atpt (&optional arg)
  "Deletes blank at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'blank arg (called-interactively-p 'any)))

(defun ar-triplequotesq-blank-atpt (&optional arg)
  "Deletes blank at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'blank arg (called-interactively-p 'any)))

(defun ar-delete-blank-in-region (beg end)
  "Deletes BLANK at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'blank beg end (called-interactively-p 'any)))

(defun ar-cntrl-atpt () 
  "Returns cntrl at point if any, nil otherwise. "
  (interactive)
  (ar-th 'cntrl nil (called-interactively-p 'any)))

(defun ar-bounds-of-cntrl-atpt ()
  "Returns a list, borders of cntrl if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'cntrl nil (called-interactively-p 'any)))

(defun ar-cntrl-beginning-position-atpt ()
  "Returns a number, beginning position CNTRL at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'cntrl nil (called-interactively-p 'any)))

(defun ar-cntrl-end-position-atpt ()
  "Returns a number, end position of CNTRL at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'cntrl nil (called-interactively-p 'any)))

(defun ar-cntrl-beginning-atpt ()
  "Goto beginning of symbol or char-class CNTRL at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'cntrl nil (called-interactively-p 'any)))

(defun ar-cntrl-end-atpt ()
  "Goto end of symbol or char-class CNTRL at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'cntrl nil (called-interactively-p 'any)))

(defun ar-in-cntrl-p-atpt ()
  "Returns bounds of CNTRL at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'cntrl nil (called-interactively-p 'any)))

(defun ar-length-of-cntrl-atpt ()
  "Returns beginning of symbol or char-class CNTRL at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'cntrl nil (called-interactively-p 'any)))

(defun ar-copy-cntrl-atpt ()
  "Returns a copy of CNTRL at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'cntrl nil (called-interactively-p 'any)))

(defun ar-delete-cntrl-atpt (&optional arg)
  "Deletes cntrl at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'cntrl arg (called-interactively-p 'any)))

(defun ar-kill-cntrl-atpt (&optional arg)
  "Deletes cntrl at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'cntrl arg (called-interactively-p 'any)))

(defun ar-forward-cntrl-atpt (&optional arg)
  "Deletes cntrl at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'cntrl arg (called-interactively-p 'any)))

(defun ar-backward-cntrl-atpt (&optional arg)
  "Deletes cntrl at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'cntrl arg (called-interactively-p 'any)))

(defun ar-triplequotedq-cntrl-atpt (&optional arg)
  "Deletes cntrl at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'cntrl arg (called-interactively-p 'any)))

(defun ar-triplequotesq-cntrl-atpt (&optional arg)
  "Deletes cntrl at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'cntrl arg (called-interactively-p 'any)))

(defun ar-delete-cntrl-in-region (beg end)
  "Deletes CNTRL at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'cntrl beg end (called-interactively-p 'any)))

(defun ar-digit-atpt () 
  "Returns digit at point if any, nil otherwise. "
  (interactive)
  (ar-th 'digit nil (called-interactively-p 'any)))

(defun ar-bounds-of-digit-atpt ()
  "Returns a list, borders of digit if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'digit nil (called-interactively-p 'any)))

(defun ar-digit-beginning-position-atpt ()
  "Returns a number, beginning position DIGIT at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'digit nil (called-interactively-p 'any)))

(defun ar-digit-end-position-atpt ()
  "Returns a number, end position of DIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'digit nil (called-interactively-p 'any)))

(defun ar-digit-beginning-atpt ()
  "Goto beginning of symbol or char-class DIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'digit nil (called-interactively-p 'any)))

(defun ar-digit-end-atpt ()
  "Goto end of symbol or char-class DIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'digit nil (called-interactively-p 'any)))

(defun ar-in-digit-p-atpt ()
  "Returns bounds of DIGIT at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'digit nil (called-interactively-p 'any)))

(defun ar-length-of-digit-atpt ()
  "Returns beginning of symbol or char-class DIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'digit nil (called-interactively-p 'any)))

(defun ar-copy-digit-atpt ()
  "Returns a copy of DIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'digit nil (called-interactively-p 'any)))

(defun ar-delete-digit-atpt (&optional arg)
  "Deletes digit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'digit arg (called-interactively-p 'any)))

(defun ar-kill-digit-atpt (&optional arg)
  "Deletes digit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'digit arg (called-interactively-p 'any)))

(defun ar-forward-digit-atpt (&optional arg)
  "Deletes digit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'digit arg (called-interactively-p 'any)))

(defun ar-backward-digit-atpt (&optional arg)
  "Deletes digit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'digit arg (called-interactively-p 'any)))

(defun ar-triplequotedq-digit-atpt (&optional arg)
  "Deletes digit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'digit arg (called-interactively-p 'any)))

(defun ar-triplequotesq-digit-atpt (&optional arg)
  "Deletes digit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'digit arg (called-interactively-p 'any)))

(defun ar-delete-digit-in-region (beg end)
  "Deletes DIGIT at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'digit beg end (called-interactively-p 'any)))

(defun ar-graph-atpt () 
  "Returns graph at point if any, nil otherwise. "
  (interactive)
  (ar-th 'graph nil (called-interactively-p 'any)))

(defun ar-bounds-of-graph-atpt ()
  "Returns a list, borders of graph if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'graph nil (called-interactively-p 'any)))

(defun ar-graph-beginning-position-atpt ()
  "Returns a number, beginning position GRAPH at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'graph nil (called-interactively-p 'any)))

(defun ar-graph-end-position-atpt ()
  "Returns a number, end position of GRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'graph nil (called-interactively-p 'any)))

(defun ar-graph-beginning-atpt ()
  "Goto beginning of symbol or char-class GRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'graph nil (called-interactively-p 'any)))

(defun ar-graph-end-atpt ()
  "Goto end of symbol or char-class GRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'graph nil (called-interactively-p 'any)))

(defun ar-in-graph-p-atpt ()
  "Returns bounds of GRAPH at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'graph nil (called-interactively-p 'any)))

(defun ar-length-of-graph-atpt ()
  "Returns beginning of symbol or char-class GRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'graph nil (called-interactively-p 'any)))

(defun ar-copy-graph-atpt ()
  "Returns a copy of GRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'graph nil (called-interactively-p 'any)))

(defun ar-delete-graph-atpt (&optional arg)
  "Deletes graph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'graph arg (called-interactively-p 'any)))

(defun ar-kill-graph-atpt (&optional arg)
  "Deletes graph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'graph arg (called-interactively-p 'any)))

(defun ar-forward-graph-atpt (&optional arg)
  "Deletes graph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'graph arg (called-interactively-p 'any)))

(defun ar-backward-graph-atpt (&optional arg)
  "Deletes graph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'graph arg (called-interactively-p 'any)))

(defun ar-triplequotedq-graph-atpt (&optional arg)
  "Deletes graph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'graph arg (called-interactively-p 'any)))

(defun ar-triplequotesq-graph-atpt (&optional arg)
  "Deletes graph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'graph arg (called-interactively-p 'any)))

(defun ar-delete-graph-in-region (beg end)
  "Deletes GRAPH at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'graph beg end (called-interactively-p 'any)))

(defun ar-lower-atpt () 
  "Returns lower at point if any, nil otherwise. "
  (interactive)
  (ar-th 'lower nil (called-interactively-p 'any)))

(defun ar-bounds-of-lower-atpt ()
  "Returns a list, borders of lower if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'lower nil (called-interactively-p 'any)))

(defun ar-lower-beginning-position-atpt ()
  "Returns a number, beginning position LOWER at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'lower nil (called-interactively-p 'any)))

(defun ar-lower-end-position-atpt ()
  "Returns a number, end position of LOWER at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'lower nil (called-interactively-p 'any)))

(defun ar-lower-beginning-atpt ()
  "Goto beginning of symbol or char-class LOWER at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'lower nil (called-interactively-p 'any)))

(defun ar-lower-end-atpt ()
  "Goto end of symbol or char-class LOWER at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'lower nil (called-interactively-p 'any)))

(defun ar-in-lower-p-atpt ()
  "Returns bounds of LOWER at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'lower nil (called-interactively-p 'any)))

(defun ar-length-of-lower-atpt ()
  "Returns beginning of symbol or char-class LOWER at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'lower nil (called-interactively-p 'any)))

(defun ar-copy-lower-atpt ()
  "Returns a copy of LOWER at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'lower nil (called-interactively-p 'any)))

(defun ar-delete-lower-atpt (&optional arg)
  "Deletes lower at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'lower arg (called-interactively-p 'any)))

(defun ar-kill-lower-atpt (&optional arg)
  "Deletes lower at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'lower arg (called-interactively-p 'any)))

(defun ar-forward-lower-atpt (&optional arg)
  "Deletes lower at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'lower arg (called-interactively-p 'any)))

(defun ar-backward-lower-atpt (&optional arg)
  "Deletes lower at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'lower arg (called-interactively-p 'any)))

(defun ar-triplequotedq-lower-atpt (&optional arg)
  "Deletes lower at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'lower arg (called-interactively-p 'any)))

(defun ar-triplequotesq-lower-atpt (&optional arg)
  "Deletes lower at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'lower arg (called-interactively-p 'any)))

(defun ar-delete-lower-in-region (beg end)
  "Deletes LOWER at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'lower beg end (called-interactively-p 'any)))

(defun ar-nonascii-atpt () 
  "Returns nonascii at point if any, nil otherwise. "
  (interactive)
  (ar-th 'nonascii nil (called-interactively-p 'any)))

(defun ar-bounds-of-nonascii-atpt ()
  "Returns a list, borders of nonascii if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'nonascii nil (called-interactively-p 'any)))

(defun ar-nonascii-beginning-position-atpt ()
  "Returns a number, beginning position NONASCII at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'nonascii nil (called-interactively-p 'any)))

(defun ar-nonascii-end-position-atpt ()
  "Returns a number, end position of NONASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'nonascii nil (called-interactively-p 'any)))

(defun ar-nonascii-beginning-atpt ()
  "Goto beginning of symbol or char-class NONASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'nonascii nil (called-interactively-p 'any)))

(defun ar-nonascii-end-atpt ()
  "Goto end of symbol or char-class NONASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'nonascii nil (called-interactively-p 'any)))

(defun ar-in-nonascii-p-atpt ()
  "Returns bounds of NONASCII at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'nonascii nil (called-interactively-p 'any)))

(defun ar-length-of-nonascii-atpt ()
  "Returns beginning of symbol or char-class NONASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'nonascii nil (called-interactively-p 'any)))

(defun ar-copy-nonascii-atpt ()
  "Returns a copy of NONASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'nonascii nil (called-interactively-p 'any)))

(defun ar-delete-nonascii-atpt (&optional arg)
  "Deletes nonascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'nonascii arg (called-interactively-p 'any)))

(defun ar-kill-nonascii-atpt (&optional arg)
  "Deletes nonascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'nonascii arg (called-interactively-p 'any)))

(defun ar-forward-nonascii-atpt (&optional arg)
  "Deletes nonascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'nonascii arg (called-interactively-p 'any)))

(defun ar-backward-nonascii-atpt (&optional arg)
  "Deletes nonascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'nonascii arg (called-interactively-p 'any)))

(defun ar-triplequotedq-nonascii-atpt (&optional arg)
  "Deletes nonascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'nonascii arg (called-interactively-p 'any)))

(defun ar-triplequotesq-nonascii-atpt (&optional arg)
  "Deletes nonascii at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'nonascii arg (called-interactively-p 'any)))

(defun ar-delete-nonascii-in-region (beg end)
  "Deletes NONASCII at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'nonascii beg end (called-interactively-p 'any)))

(defun ar-print-atpt () 
  "Returns print at point if any, nil otherwise. "
  (interactive)
  (ar-th 'print nil (called-interactively-p 'any)))

(defun ar-bounds-of-print-atpt ()
  "Returns a list, borders of print if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'print nil (called-interactively-p 'any)))

(defun ar-print-beginning-position-atpt ()
  "Returns a number, beginning position PRINT at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'print nil (called-interactively-p 'any)))

(defun ar-print-end-position-atpt ()
  "Returns a number, end position of PRINT at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'print nil (called-interactively-p 'any)))

(defun ar-print-beginning-atpt ()
  "Goto beginning of symbol or char-class PRINT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'print nil (called-interactively-p 'any)))

(defun ar-print-end-atpt ()
  "Goto end of symbol or char-class PRINT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'print nil (called-interactively-p 'any)))

(defun ar-in-print-p-atpt ()
  "Returns bounds of PRINT at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'print nil (called-interactively-p 'any)))

(defun ar-length-of-print-atpt ()
  "Returns beginning of symbol or char-class PRINT at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'print nil (called-interactively-p 'any)))

(defun ar-copy-print-atpt ()
  "Returns a copy of PRINT at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'print nil (called-interactively-p 'any)))

(defun ar-delete-print-atpt (&optional arg)
  "Deletes print at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'print arg (called-interactively-p 'any)))

(defun ar-kill-print-atpt (&optional arg)
  "Deletes print at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'print arg (called-interactively-p 'any)))

(defun ar-forward-print-atpt (&optional arg)
  "Deletes print at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'print arg (called-interactively-p 'any)))

(defun ar-backward-print-atpt (&optional arg)
  "Deletes print at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'print arg (called-interactively-p 'any)))

(defun ar-triplequotedq-print-atpt (&optional arg)
  "Deletes print at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'print arg (called-interactively-p 'any)))

(defun ar-triplequotesq-print-atpt (&optional arg)
  "Deletes print at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'print arg (called-interactively-p 'any)))

(defun ar-delete-print-in-region (beg end)
  "Deletes PRINT at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'print beg end (called-interactively-p 'any)))

(defun ar-punct-atpt () 
  "Returns punct at point if any, nil otherwise. "
  (interactive)
  (ar-th 'punct nil (called-interactively-p 'any)))

(defun ar-bounds-of-punct-atpt ()
  "Returns a list, borders of punct if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'punct nil (called-interactively-p 'any)))

(defun ar-punct-beginning-position-atpt ()
  "Returns a number, beginning position PUNCT at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'punct nil (called-interactively-p 'any)))

(defun ar-punct-end-position-atpt ()
  "Returns a number, end position of PUNCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'punct nil (called-interactively-p 'any)))

(defun ar-punct-beginning-atpt ()
  "Goto beginning of symbol or char-class PUNCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'punct nil (called-interactively-p 'any)))

(defun ar-punct-end-atpt ()
  "Goto end of symbol or char-class PUNCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'punct nil (called-interactively-p 'any)))

(defun ar-in-punct-p-atpt ()
  "Returns bounds of PUNCT at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'punct nil (called-interactively-p 'any)))

(defun ar-length-of-punct-atpt ()
  "Returns beginning of symbol or char-class PUNCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'punct nil (called-interactively-p 'any)))

(defun ar-copy-punct-atpt ()
  "Returns a copy of PUNCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'punct nil (called-interactively-p 'any)))

(defun ar-delete-punct-atpt (&optional arg)
  "Deletes punct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'punct arg (called-interactively-p 'any)))

(defun ar-kill-punct-atpt (&optional arg)
  "Deletes punct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'punct arg (called-interactively-p 'any)))

(defun ar-forward-punct-atpt (&optional arg)
  "Deletes punct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'punct arg (called-interactively-p 'any)))

(defun ar-backward-punct-atpt (&optional arg)
  "Deletes punct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'punct arg (called-interactively-p 'any)))

(defun ar-triplequotedq-punct-atpt (&optional arg)
  "Deletes punct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'punct arg (called-interactively-p 'any)))

(defun ar-triplequotesq-punct-atpt (&optional arg)
  "Deletes punct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'punct arg (called-interactively-p 'any)))

(defun ar-delete-punct-in-region (beg end)
  "Deletes PUNCT at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'punct beg end (called-interactively-p 'any)))

(defun ar-space-atpt () 
  "Returns space at point if any, nil otherwise. "
  (interactive)
  (ar-th 'space nil (called-interactively-p 'any)))

(defun ar-bounds-of-space-atpt ()
  "Returns a list, borders of space if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'space nil (called-interactively-p 'any)))

(defun ar-space-beginning-position-atpt ()
  "Returns a number, beginning position SPACE at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'space nil (called-interactively-p 'any)))

(defun ar-space-end-position-atpt ()
  "Returns a number, end position of SPACE at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'space nil (called-interactively-p 'any)))

(defun ar-space-beginning-atpt ()
  "Goto beginning of symbol or char-class SPACE at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'space nil (called-interactively-p 'any)))

(defun ar-space-end-atpt ()
  "Goto end of symbol or char-class SPACE at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'space nil (called-interactively-p 'any)))

(defun ar-in-space-p-atpt ()
  "Returns bounds of SPACE at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'space nil (called-interactively-p 'any)))

(defun ar-length-of-space-atpt ()
  "Returns beginning of symbol or char-class SPACE at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'space nil (called-interactively-p 'any)))

(defun ar-copy-space-atpt ()
  "Returns a copy of SPACE at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'space nil (called-interactively-p 'any)))

(defun ar-delete-space-atpt (&optional arg)
  "Deletes space at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'space arg (called-interactively-p 'any)))

(defun ar-kill-space-atpt (&optional arg)
  "Deletes space at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'space arg (called-interactively-p 'any)))

(defun ar-forward-space-atpt (&optional arg)
  "Deletes space at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'space arg (called-interactively-p 'any)))

(defun ar-backward-space-atpt (&optional arg)
  "Deletes space at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'space arg (called-interactively-p 'any)))

(defun ar-triplequotedq-space-atpt (&optional arg)
  "Deletes space at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'space arg (called-interactively-p 'any)))

(defun ar-triplequotesq-space-atpt (&optional arg)
  "Deletes space at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'space arg (called-interactively-p 'any)))

(defun ar-delete-space-in-region (beg end)
  "Deletes SPACE at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'space beg end (called-interactively-p 'any)))

(defun ar-upper-atpt () 
  "Returns upper at point if any, nil otherwise. "
  (interactive)
  (ar-th 'upper nil (called-interactively-p 'any)))

(defun ar-bounds-of-upper-atpt ()
  "Returns a list, borders of upper if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'upper nil (called-interactively-p 'any)))

(defun ar-upper-beginning-position-atpt ()
  "Returns a number, beginning position UPPER at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'upper nil (called-interactively-p 'any)))

(defun ar-upper-end-position-atpt ()
  "Returns a number, end position of UPPER at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'upper nil (called-interactively-p 'any)))

(defun ar-upper-beginning-atpt ()
  "Goto beginning of symbol or char-class UPPER at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'upper nil (called-interactively-p 'any)))

(defun ar-upper-end-atpt ()
  "Goto end of symbol or char-class UPPER at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'upper nil (called-interactively-p 'any)))

(defun ar-in-upper-p-atpt ()
  "Returns bounds of UPPER at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'upper nil (called-interactively-p 'any)))

(defun ar-length-of-upper-atpt ()
  "Returns beginning of symbol or char-class UPPER at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'upper nil (called-interactively-p 'any)))

(defun ar-copy-upper-atpt ()
  "Returns a copy of UPPER at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'upper nil (called-interactively-p 'any)))

(defun ar-delete-upper-atpt (&optional arg)
  "Deletes upper at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'upper arg (called-interactively-p 'any)))

(defun ar-kill-upper-atpt (&optional arg)
  "Deletes upper at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'upper arg (called-interactively-p 'any)))

(defun ar-forward-upper-atpt (&optional arg)
  "Deletes upper at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'upper arg (called-interactively-p 'any)))

(defun ar-backward-upper-atpt (&optional arg)
  "Deletes upper at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'upper arg (called-interactively-p 'any)))

(defun ar-triplequotedq-upper-atpt (&optional arg)
  "Deletes upper at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'upper arg (called-interactively-p 'any)))

(defun ar-triplequotesq-upper-atpt (&optional arg)
  "Deletes upper at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'upper arg (called-interactively-p 'any)))

(defun ar-delete-upper-in-region (beg end)
  "Deletes UPPER at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'upper beg end (called-interactively-p 'any)))

(defun ar-xdigit-atpt () 
  "Returns xdigit at point if any, nil otherwise. "
  (interactive)
  (ar-th 'xdigit nil (called-interactively-p 'any)))

(defun ar-bounds-of-xdigit-atpt ()
  "Returns a list, borders of xdigit if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'xdigit nil (called-interactively-p 'any)))

(defun ar-xdigit-beginning-position-atpt ()
  "Returns a number, beginning position XDIGIT at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'xdigit nil (called-interactively-p 'any)))

(defun ar-xdigit-end-position-atpt ()
  "Returns a number, end position of XDIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'xdigit nil (called-interactively-p 'any)))

(defun ar-xdigit-beginning-atpt ()
  "Goto beginning of symbol or char-class XDIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'xdigit nil (called-interactively-p 'any)))

(defun ar-xdigit-end-atpt ()
  "Goto end of symbol or char-class XDIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'xdigit nil (called-interactively-p 'any)))

(defun ar-in-xdigit-p-atpt ()
  "Returns bounds of XDIGIT at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'xdigit nil (called-interactively-p 'any)))

(defun ar-length-of-xdigit-atpt ()
  "Returns beginning of symbol or char-class XDIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'xdigit nil (called-interactively-p 'any)))

(defun ar-copy-xdigit-atpt ()
  "Returns a copy of XDIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'xdigit nil (called-interactively-p 'any)))

(defun ar-delete-xdigit-atpt (&optional arg)
  "Deletes xdigit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'xdigit arg (called-interactively-p 'any)))

(defun ar-kill-xdigit-atpt (&optional arg)
  "Deletes xdigit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'xdigit arg (called-interactively-p 'any)))

(defun ar-forward-xdigit-atpt (&optional arg)
  "Deletes xdigit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'xdigit arg (called-interactively-p 'any)))

(defun ar-backward-xdigit-atpt (&optional arg)
  "Deletes xdigit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'xdigit arg (called-interactively-p 'any)))

(defun ar-triplequotedq-xdigit-atpt (&optional arg)
  "Deletes xdigit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'xdigit arg (called-interactively-p 'any)))

(defun ar-triplequotesq-xdigit-atpt (&optional arg)
  "Deletes xdigit at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'xdigit arg (called-interactively-p 'any)))

(defun ar-delete-xdigit-in-region (beg end)
  "Deletes XDIGIT at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'xdigit beg end (called-interactively-p 'any)))
;; ar-thing-at-point-utils-nodelim-core ar-atpt-classes: end
;; ar-thing-at-point-utils-nodelim-core ar-atpt-rest-list: start

(defun ar-greateranglednested-atpt () 
  "Returns greateranglednested at point if any, nil otherwise. "
  (interactive)
  (ar-th 'greateranglednested nil (called-interactively-p 'any)))

(defun ar-bounds-of-greateranglednested-atpt ()
  "Returns a list, borders of greateranglednested if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'greateranglednested nil (called-interactively-p 'any)))

(defun ar-greateranglednested-beginning-position-atpt ()
  "Returns a number, beginning position GREATERANGLEDNESTED at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'greateranglednested nil (called-interactively-p 'any)))

(defun ar-greateranglednested-end-position-atpt ()
  "Returns a number, end position of GREATERANGLEDNESTED at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'greateranglednested nil (called-interactively-p 'any)))

(defun ar-greateranglednested-beginning-atpt ()
  "Goto beginning of symbol or char-class GREATERANGLEDNESTED at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'greateranglednested nil (called-interactively-p 'any)))

(defun ar-greateranglednested-end-atpt ()
  "Goto end of symbol or char-class GREATERANGLEDNESTED at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'greateranglednested nil (called-interactively-p 'any)))

(defun ar-in-greateranglednested-p-atpt ()
  "Returns bounds of GREATERANGLEDNESTED at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'greateranglednested nil (called-interactively-p 'any)))

(defun ar-length-of-greateranglednested-atpt ()
  "Returns beginning of symbol or char-class GREATERANGLEDNESTED at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'greateranglednested nil (called-interactively-p 'any)))

(defun ar-copy-greateranglednested-atpt ()
  "Returns a copy of GREATERANGLEDNESTED at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'greateranglednested nil (called-interactively-p 'any)))

(defun ar-delete-greateranglednested-atpt (&optional arg)
  "Deletes greateranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'greateranglednested arg (called-interactively-p 'any)))

(defun ar-kill-greateranglednested-atpt (&optional arg)
  "Deletes greateranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'greateranglednested arg (called-interactively-p 'any)))

(defun ar-forward-greateranglednested-atpt (&optional arg)
  "Deletes greateranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'greateranglednested arg (called-interactively-p 'any)))

(defun ar-backward-greateranglednested-atpt (&optional arg)
  "Deletes greateranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'greateranglednested arg (called-interactively-p 'any)))

(defun ar-triplequotedq-greateranglednested-atpt (&optional arg)
  "Deletes greateranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'greateranglednested arg (called-interactively-p 'any)))

(defun ar-triplequotesq-greateranglednested-atpt (&optional arg)
  "Deletes greateranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'greateranglednested arg (called-interactively-p 'any)))

(defun ar-delete-greateranglednested-in-region (beg end)
  "Deletes GREATERANGLEDNESTED at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'greateranglednested beg end (called-interactively-p 'any)))

(defun ar-lesseranglednested-atpt () 
  "Returns lesseranglednested at point if any, nil otherwise. "
  (interactive)
  (ar-th 'lesseranglednested nil (called-interactively-p 'any)))

(defun ar-bounds-of-lesseranglednested-atpt ()
  "Returns a list, borders of lesseranglednested if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'lesseranglednested nil (called-interactively-p 'any)))

(defun ar-lesseranglednested-beginning-position-atpt ()
  "Returns a number, beginning position LESSERANGLEDNESTED at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'lesseranglednested nil (called-interactively-p 'any)))

(defun ar-lesseranglednested-end-position-atpt ()
  "Returns a number, end position of LESSERANGLEDNESTED at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'lesseranglednested nil (called-interactively-p 'any)))

(defun ar-lesseranglednested-beginning-atpt ()
  "Goto beginning of symbol or char-class LESSERANGLEDNESTED at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'lesseranglednested nil (called-interactively-p 'any)))

(defun ar-lesseranglednested-end-atpt ()
  "Goto end of symbol or char-class LESSERANGLEDNESTED at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'lesseranglednested nil (called-interactively-p 'any)))

(defun ar-in-lesseranglednested-p-atpt ()
  "Returns bounds of LESSERANGLEDNESTED at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'lesseranglednested nil (called-interactively-p 'any)))

(defun ar-length-of-lesseranglednested-atpt ()
  "Returns beginning of symbol or char-class LESSERANGLEDNESTED at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'lesseranglednested nil (called-interactively-p 'any)))

(defun ar-copy-lesseranglednested-atpt ()
  "Returns a copy of LESSERANGLEDNESTED at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'lesseranglednested nil (called-interactively-p 'any)))

(defun ar-delete-lesseranglednested-atpt (&optional arg)
  "Deletes lesseranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'lesseranglednested arg (called-interactively-p 'any)))

(defun ar-kill-lesseranglednested-atpt (&optional arg)
  "Deletes lesseranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'lesseranglednested arg (called-interactively-p 'any)))

(defun ar-forward-lesseranglednested-atpt (&optional arg)
  "Deletes lesseranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'lesseranglednested arg (called-interactively-p 'any)))

(defun ar-backward-lesseranglednested-atpt (&optional arg)
  "Deletes lesseranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'lesseranglednested arg (called-interactively-p 'any)))

(defun ar-triplequotedq-lesseranglednested-atpt (&optional arg)
  "Deletes lesseranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'lesseranglednested arg (called-interactively-p 'any)))

(defun ar-triplequotesq-lesseranglednested-atpt (&optional arg)
  "Deletes lesseranglednested at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'lesseranglednested arg (called-interactively-p 'any)))

(defun ar-delete-lesseranglednested-in-region (beg end)
  "Deletes LESSERANGLEDNESTED at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'lesseranglednested beg end (called-interactively-p 'any)))

(defun ar-buffer-atpt () 
  "Returns buffer at point if any, nil otherwise. "
  (interactive)
  (ar-th 'buffer nil (called-interactively-p 'any)))

(defun ar-bounds-of-buffer-atpt ()
  "Returns a list, borders of buffer if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'buffer nil (called-interactively-p 'any)))

(defun ar-buffer-beginning-position-atpt ()
  "Returns a number, beginning position BUFFER at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'buffer nil (called-interactively-p 'any)))

(defun ar-buffer-end-position-atpt ()
  "Returns a number, end position of BUFFER at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'buffer nil (called-interactively-p 'any)))

(defun ar-buffer-beginning-atpt ()
  "Goto beginning of symbol or char-class BUFFER at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'buffer nil (called-interactively-p 'any)))

(defun ar-buffer-end-atpt ()
  "Goto end of symbol or char-class BUFFER at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'buffer nil (called-interactively-p 'any)))

(defun ar-in-buffer-p-atpt ()
  "Returns bounds of BUFFER at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'buffer nil (called-interactively-p 'any)))

(defun ar-length-of-buffer-atpt ()
  "Returns beginning of symbol or char-class BUFFER at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'buffer nil (called-interactively-p 'any)))

(defun ar-copy-buffer-atpt ()
  "Returns a copy of BUFFER at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'buffer nil (called-interactively-p 'any)))

(defun ar-delete-buffer-atpt (&optional arg)
  "Deletes buffer at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'buffer arg (called-interactively-p 'any)))

(defun ar-kill-buffer-atpt (&optional arg)
  "Deletes buffer at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'buffer arg (called-interactively-p 'any)))

(defun ar-forward-buffer-atpt (&optional arg)
  "Deletes buffer at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'buffer arg (called-interactively-p 'any)))

(defun ar-backward-buffer-atpt (&optional arg)
  "Deletes buffer at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'buffer arg (called-interactively-p 'any)))

(defun ar-triplequotedq-buffer-atpt (&optional arg)
  "Deletes buffer at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'buffer arg (called-interactively-p 'any)))

(defun ar-triplequotesq-buffer-atpt (&optional arg)
  "Deletes buffer at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'buffer arg (called-interactively-p 'any)))

(defun ar-delete-buffer-in-region (beg end)
  "Deletes BUFFER at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'buffer beg end (called-interactively-p 'any)))

(defun ar-comment-atpt () 
  "Returns comment at point if any, nil otherwise. "
  (interactive)
  (ar-th 'comment nil (called-interactively-p 'any)))

(defun ar-bounds-of-comment-atpt ()
  "Returns a list, borders of comment if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'comment nil (called-interactively-p 'any)))

(defun ar-comment-beginning-position-atpt ()
  "Returns a number, beginning position COMMENT at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'comment nil (called-interactively-p 'any)))

(defun ar-comment-end-position-atpt ()
  "Returns a number, end position of COMMENT at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'comment nil (called-interactively-p 'any)))

(defun ar-comment-beginning-atpt ()
  "Goto beginning of symbol or char-class COMMENT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'comment nil (called-interactively-p 'any)))

(defun ar-comment-end-atpt ()
  "Goto end of symbol or char-class COMMENT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'comment nil (called-interactively-p 'any)))

(defun ar-in-comment-p-atpt ()
  "Returns bounds of COMMENT at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'comment nil (called-interactively-p 'any)))

(defun ar-length-of-comment-atpt ()
  "Returns beginning of symbol or char-class COMMENT at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'comment nil (called-interactively-p 'any)))

(defun ar-copy-comment-atpt ()
  "Returns a copy of COMMENT at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'comment nil (called-interactively-p 'any)))

(defun ar-delete-comment-atpt (&optional arg)
  "Deletes comment at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'comment arg (called-interactively-p 'any)))

(defun ar-kill-comment-atpt (&optional arg)
  "Deletes comment at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'comment arg (called-interactively-p 'any)))

(defun ar-forward-comment-atpt (&optional arg)
  "Deletes comment at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'comment arg (called-interactively-p 'any)))

(defun ar-backward-comment-atpt (&optional arg)
  "Deletes comment at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'comment arg (called-interactively-p 'any)))

(defun ar-triplequotedq-comment-atpt (&optional arg)
  "Deletes comment at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'comment arg (called-interactively-p 'any)))

(defun ar-triplequotesq-comment-atpt (&optional arg)
  "Deletes comment at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'comment arg (called-interactively-p 'any)))

(defun ar-delete-comment-in-region (beg end)
  "Deletes COMMENT at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'comment beg end (called-interactively-p 'any)))

(defun ar-csv-atpt () 
  "Returns csv at point if any, nil otherwise. "
  (interactive)
  (ar-th 'csv nil (called-interactively-p 'any)))

(defun ar-bounds-of-csv-atpt ()
  "Returns a list, borders of csv if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'csv nil (called-interactively-p 'any)))

(defun ar-csv-beginning-position-atpt ()
  "Returns a number, beginning position CSV at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'csv nil (called-interactively-p 'any)))

(defun ar-csv-end-position-atpt ()
  "Returns a number, end position of CSV at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'csv nil (called-interactively-p 'any)))

(defun ar-csv-beginning-atpt ()
  "Goto beginning of symbol or char-class CSV at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'csv nil (called-interactively-p 'any)))

(defun ar-csv-end-atpt ()
  "Goto end of symbol or char-class CSV at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'csv nil (called-interactively-p 'any)))

(defun ar-in-csv-p-atpt ()
  "Returns bounds of CSV at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'csv nil (called-interactively-p 'any)))

(defun ar-length-of-csv-atpt ()
  "Returns beginning of symbol or char-class CSV at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'csv nil (called-interactively-p 'any)))

(defun ar-copy-csv-atpt ()
  "Returns a copy of CSV at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'csv nil (called-interactively-p 'any)))

(defun ar-delete-csv-atpt (&optional arg)
  "Deletes csv at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'csv arg (called-interactively-p 'any)))

(defun ar-kill-csv-atpt (&optional arg)
  "Deletes csv at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'csv arg (called-interactively-p 'any)))

(defun ar-forward-csv-atpt (&optional arg)
  "Deletes csv at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'csv arg (called-interactively-p 'any)))

(defun ar-backward-csv-atpt (&optional arg)
  "Deletes csv at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'csv arg (called-interactively-p 'any)))

(defun ar-triplequotedq-csv-atpt (&optional arg)
  "Deletes csv at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'csv arg (called-interactively-p 'any)))

(defun ar-triplequotesq-csv-atpt (&optional arg)
  "Deletes csv at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'csv arg (called-interactively-p 'any)))

(defun ar-delete-csv-in-region (beg end)
  "Deletes CSV at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'csv beg end (called-interactively-p 'any)))

(defun ar-date-atpt () 
  "Returns date at point if any, nil otherwise. "
  (interactive)
  (ar-th 'date nil (called-interactively-p 'any)))

(defun ar-bounds-of-date-atpt ()
  "Returns a list, borders of date if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'date nil (called-interactively-p 'any)))

(defun ar-date-beginning-position-atpt ()
  "Returns a number, beginning position DATE at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'date nil (called-interactively-p 'any)))

(defun ar-date-end-position-atpt ()
  "Returns a number, end position of DATE at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'date nil (called-interactively-p 'any)))

(defun ar-date-beginning-atpt ()
  "Goto beginning of symbol or char-class DATE at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'date nil (called-interactively-p 'any)))

(defun ar-date-end-atpt ()
  "Goto end of symbol or char-class DATE at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'date nil (called-interactively-p 'any)))

(defun ar-in-date-p-atpt ()
  "Returns bounds of DATE at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'date nil (called-interactively-p 'any)))

(defun ar-length-of-date-atpt ()
  "Returns beginning of symbol or char-class DATE at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'date nil (called-interactively-p 'any)))

(defun ar-copy-date-atpt ()
  "Returns a copy of DATE at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'date nil (called-interactively-p 'any)))

(defun ar-delete-date-atpt (&optional arg)
  "Deletes date at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'date arg (called-interactively-p 'any)))

(defun ar-kill-date-atpt (&optional arg)
  "Deletes date at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'date arg (called-interactively-p 'any)))

(defun ar-forward-date-atpt (&optional arg)
  "Deletes date at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'date arg (called-interactively-p 'any)))

(defun ar-backward-date-atpt (&optional arg)
  "Deletes date at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'date arg (called-interactively-p 'any)))

(defun ar-triplequotedq-date-atpt (&optional arg)
  "Deletes date at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'date arg (called-interactively-p 'any)))

(defun ar-triplequotesq-date-atpt (&optional arg)
  "Deletes date at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'date arg (called-interactively-p 'any)))

(defun ar-delete-date-in-region (beg end)
  "Deletes DATE at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'date beg end (called-interactively-p 'any)))

(defun ar-email-atpt () 
  "Returns email at point if any, nil otherwise. "
  (interactive)
  (ar-th 'email nil (called-interactively-p 'any)))

(defun ar-bounds-of-email-atpt ()
  "Returns a list, borders of email if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'email nil (called-interactively-p 'any)))

(defun ar-email-beginning-position-atpt ()
  "Returns a number, beginning position EMAIL at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'email nil (called-interactively-p 'any)))

(defun ar-email-end-position-atpt ()
  "Returns a number, end position of EMAIL at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'email nil (called-interactively-p 'any)))

(defun ar-email-beginning-atpt ()
  "Goto beginning of symbol or char-class EMAIL at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'email nil (called-interactively-p 'any)))

(defun ar-email-end-atpt ()
  "Goto end of symbol or char-class EMAIL at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'email nil (called-interactively-p 'any)))

(defun ar-in-email-p-atpt ()
  "Returns bounds of EMAIL at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'email nil (called-interactively-p 'any)))

(defun ar-length-of-email-atpt ()
  "Returns beginning of symbol or char-class EMAIL at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'email nil (called-interactively-p 'any)))

(defun ar-copy-email-atpt ()
  "Returns a copy of EMAIL at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'email nil (called-interactively-p 'any)))

(defun ar-delete-email-atpt (&optional arg)
  "Deletes email at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'email arg (called-interactively-p 'any)))

(defun ar-kill-email-atpt (&optional arg)
  "Deletes email at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'email arg (called-interactively-p 'any)))

(defun ar-forward-email-atpt (&optional arg)
  "Deletes email at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'email arg (called-interactively-p 'any)))

(defun ar-backward-email-atpt (&optional arg)
  "Deletes email at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'email arg (called-interactively-p 'any)))

(defun ar-triplequotedq-email-atpt (&optional arg)
  "Deletes email at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'email arg (called-interactively-p 'any)))

(defun ar-triplequotesq-email-atpt (&optional arg)
  "Deletes email at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'email arg (called-interactively-p 'any)))

(defun ar-delete-email-in-region (beg end)
  "Deletes EMAIL at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'email beg end (called-interactively-p 'any)))

(defun ar-filename-atpt () 
  "Returns filename at point if any, nil otherwise. "
  (interactive)
  (ar-th 'filename nil (called-interactively-p 'any)))

(defun ar-bounds-of-filename-atpt ()
  "Returns a list, borders of filename if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'filename nil (called-interactively-p 'any)))

(defun ar-filename-beginning-position-atpt ()
  "Returns a number, beginning position FILENAME at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'filename nil (called-interactively-p 'any)))

(defun ar-filename-end-position-atpt ()
  "Returns a number, end position of FILENAME at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'filename nil (called-interactively-p 'any)))

(defun ar-filename-beginning-atpt ()
  "Goto beginning of symbol or char-class FILENAME at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'filename nil (called-interactively-p 'any)))

(defun ar-filename-end-atpt ()
  "Goto end of symbol or char-class FILENAME at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'filename nil (called-interactively-p 'any)))

(defun ar-in-filename-p-atpt ()
  "Returns bounds of FILENAME at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'filename nil (called-interactively-p 'any)))

(defun ar-length-of-filename-atpt ()
  "Returns beginning of symbol or char-class FILENAME at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'filename nil (called-interactively-p 'any)))

(defun ar-copy-filename-atpt ()
  "Returns a copy of FILENAME at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'filename nil (called-interactively-p 'any)))

(defun ar-delete-filename-atpt (&optional arg)
  "Deletes filename at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'filename arg (called-interactively-p 'any)))

(defun ar-kill-filename-atpt (&optional arg)
  "Deletes filename at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'filename arg (called-interactively-p 'any)))

(defun ar-forward-filename-atpt (&optional arg)
  "Deletes filename at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'filename arg (called-interactively-p 'any)))

(defun ar-backward-filename-atpt (&optional arg)
  "Deletes filename at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'filename arg (called-interactively-p 'any)))

(defun ar-triplequotedq-filename-atpt (&optional arg)
  "Deletes filename at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'filename arg (called-interactively-p 'any)))

(defun ar-triplequotesq-filename-atpt (&optional arg)
  "Deletes filename at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'filename arg (called-interactively-p 'any)))

(defun ar-delete-filename-in-region (beg end)
  "Deletes FILENAME at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'filename beg end (called-interactively-p 'any)))

(defun ar-filenamenondirectory-atpt () 
  "Returns filenamenondirectory at point if any, nil otherwise. "
  (interactive)
  (ar-th 'filenamenondirectory nil (called-interactively-p 'any)))

(defun ar-bounds-of-filenamenondirectory-atpt ()
  "Returns a list, borders of filenamenondirectory if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'filenamenondirectory nil (called-interactively-p 'any)))

(defun ar-filenamenondirectory-beginning-position-atpt ()
  "Returns a number, beginning position FILENAMENONDIRECTORY at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'filenamenondirectory nil (called-interactively-p 'any)))

(defun ar-filenamenondirectory-end-position-atpt ()
  "Returns a number, end position of FILENAMENONDIRECTORY at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'filenamenondirectory nil (called-interactively-p 'any)))

(defun ar-filenamenondirectory-beginning-atpt ()
  "Goto beginning of symbol or char-class FILENAMENONDIRECTORY at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'filenamenondirectory nil (called-interactively-p 'any)))

(defun ar-filenamenondirectory-end-atpt ()
  "Goto end of symbol or char-class FILENAMENONDIRECTORY at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'filenamenondirectory nil (called-interactively-p 'any)))

(defun ar-in-filenamenondirectory-p-atpt ()
  "Returns bounds of FILENAMENONDIRECTORY at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'filenamenondirectory nil (called-interactively-p 'any)))

(defun ar-length-of-filenamenondirectory-atpt ()
  "Returns beginning of symbol or char-class FILENAMENONDIRECTORY at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'filenamenondirectory nil (called-interactively-p 'any)))

(defun ar-copy-filenamenondirectory-atpt ()
  "Returns a copy of FILENAMENONDIRECTORY at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'filenamenondirectory nil (called-interactively-p 'any)))

(defun ar-delete-filenamenondirectory-atpt (&optional arg)
  "Deletes filenamenondirectory at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'filenamenondirectory arg (called-interactively-p 'any)))

(defun ar-kill-filenamenondirectory-atpt (&optional arg)
  "Deletes filenamenondirectory at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'filenamenondirectory arg (called-interactively-p 'any)))

(defun ar-forward-filenamenondirectory-atpt (&optional arg)
  "Deletes filenamenondirectory at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'filenamenondirectory arg (called-interactively-p 'any)))

(defun ar-backward-filenamenondirectory-atpt (&optional arg)
  "Deletes filenamenondirectory at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'filenamenondirectory arg (called-interactively-p 'any)))

(defun ar-triplequotedq-filenamenondirectory-atpt (&optional arg)
  "Deletes filenamenondirectory at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'filenamenondirectory arg (called-interactively-p 'any)))

(defun ar-triplequotesq-filenamenondirectory-atpt (&optional arg)
  "Deletes filenamenondirectory at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'filenamenondirectory arg (called-interactively-p 'any)))

(defun ar-delete-filenamenondirectory-in-region (beg end)
  "Deletes FILENAMENONDIRECTORY at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'filenamenondirectory beg end (called-interactively-p 'any)))

(defun ar-float-atpt () 
  "Returns float at point if any, nil otherwise. "
  (interactive)
  (ar-th 'float nil (called-interactively-p 'any)))

(defun ar-bounds-of-float-atpt ()
  "Returns a list, borders of float if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'float nil (called-interactively-p 'any)))

(defun ar-float-beginning-position-atpt ()
  "Returns a number, beginning position FLOAT at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'float nil (called-interactively-p 'any)))

(defun ar-float-end-position-atpt ()
  "Returns a number, end position of FLOAT at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'float nil (called-interactively-p 'any)))

(defun ar-float-beginning-atpt ()
  "Goto beginning of symbol or char-class FLOAT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'float nil (called-interactively-p 'any)))

(defun ar-float-end-atpt ()
  "Goto end of symbol or char-class FLOAT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'float nil (called-interactively-p 'any)))

(defun ar-in-float-p-atpt ()
  "Returns bounds of FLOAT at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'float nil (called-interactively-p 'any)))

(defun ar-length-of-float-atpt ()
  "Returns beginning of symbol or char-class FLOAT at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'float nil (called-interactively-p 'any)))

(defun ar-copy-float-atpt ()
  "Returns a copy of FLOAT at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'float nil (called-interactively-p 'any)))

(defun ar-delete-float-atpt (&optional arg)
  "Deletes float at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'float arg (called-interactively-p 'any)))

(defun ar-kill-float-atpt (&optional arg)
  "Deletes float at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'float arg (called-interactively-p 'any)))

(defun ar-forward-float-atpt (&optional arg)
  "Deletes float at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'float arg (called-interactively-p 'any)))

(defun ar-backward-float-atpt (&optional arg)
  "Deletes float at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'float arg (called-interactively-p 'any)))

(defun ar-triplequotedq-float-atpt (&optional arg)
  "Deletes float at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'float arg (called-interactively-p 'any)))

(defun ar-triplequotesq-float-atpt (&optional arg)
  "Deletes float at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'float arg (called-interactively-p 'any)))

(defun ar-delete-float-in-region (beg end)
  "Deletes FLOAT at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'float beg end (called-interactively-p 'any)))

(defun ar-function-atpt () 
  "Returns function at point if any, nil otherwise. "
  (interactive)
  (ar-th 'function nil (called-interactively-p 'any)))

(defun ar-bounds-of-function-atpt ()
  "Returns a list, borders of function if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'function nil (called-interactively-p 'any)))

(defun ar-function-beginning-position-atpt ()
  "Returns a number, beginning position FUNCTION at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'function nil (called-interactively-p 'any)))

(defun ar-function-end-position-atpt ()
  "Returns a number, end position of FUNCTION at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'function nil (called-interactively-p 'any)))

(defun ar-function-beginning-atpt ()
  "Goto beginning of symbol or char-class FUNCTION at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'function nil (called-interactively-p 'any)))

(defun ar-function-end-atpt ()
  "Goto end of symbol or char-class FUNCTION at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'function nil (called-interactively-p 'any)))

(defun ar-in-function-p-atpt ()
  "Returns bounds of FUNCTION at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'function nil (called-interactively-p 'any)))

(defun ar-length-of-function-atpt ()
  "Returns beginning of symbol or char-class FUNCTION at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'function nil (called-interactively-p 'any)))

(defun ar-copy-function-atpt ()
  "Returns a copy of FUNCTION at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'function nil (called-interactively-p 'any)))

(defun ar-delete-function-atpt (&optional arg)
  "Deletes function at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'function arg (called-interactively-p 'any)))

(defun ar-kill-function-atpt (&optional arg)
  "Deletes function at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'function arg (called-interactively-p 'any)))

(defun ar-forward-function-atpt (&optional arg)
  "Deletes function at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'function arg (called-interactively-p 'any)))

(defun ar-backward-function-atpt (&optional arg)
  "Deletes function at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'function arg (called-interactively-p 'any)))

(defun ar-triplequotedq-function-atpt (&optional arg)
  "Deletes function at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'function arg (called-interactively-p 'any)))

(defun ar-triplequotesq-function-atpt (&optional arg)
  "Deletes function at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'function arg (called-interactively-p 'any)))

(defun ar-delete-function-in-region (beg end)
  "Deletes FUNCTION at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'function beg end (called-interactively-p 'any)))

(defun ar-ip-atpt () 
  "Returns ip at point if any, nil otherwise. "
  (interactive)
  (ar-th 'ip nil (called-interactively-p 'any)))

(defun ar-bounds-of-ip-atpt ()
  "Returns a list, borders of ip if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'ip nil (called-interactively-p 'any)))

(defun ar-ip-beginning-position-atpt ()
  "Returns a number, beginning position IP at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'ip nil (called-interactively-p 'any)))

(defun ar-ip-end-position-atpt ()
  "Returns a number, end position of IP at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'ip nil (called-interactively-p 'any)))

(defun ar-ip-beginning-atpt ()
  "Goto beginning of symbol or char-class IP at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'ip nil (called-interactively-p 'any)))

(defun ar-ip-end-atpt ()
  "Goto end of symbol or char-class IP at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'ip nil (called-interactively-p 'any)))

(defun ar-in-ip-p-atpt ()
  "Returns bounds of IP at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'ip nil (called-interactively-p 'any)))

(defun ar-length-of-ip-atpt ()
  "Returns beginning of symbol or char-class IP at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'ip nil (called-interactively-p 'any)))

(defun ar-copy-ip-atpt ()
  "Returns a copy of IP at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'ip nil (called-interactively-p 'any)))

(defun ar-delete-ip-atpt (&optional arg)
  "Deletes ip at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'ip arg (called-interactively-p 'any)))

(defun ar-kill-ip-atpt (&optional arg)
  "Deletes ip at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'ip arg (called-interactively-p 'any)))

(defun ar-forward-ip-atpt (&optional arg)
  "Deletes ip at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'ip arg (called-interactively-p 'any)))

(defun ar-backward-ip-atpt (&optional arg)
  "Deletes ip at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'ip arg (called-interactively-p 'any)))

(defun ar-triplequotedq-ip-atpt (&optional arg)
  "Deletes ip at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'ip arg (called-interactively-p 'any)))

(defun ar-triplequotesq-ip-atpt (&optional arg)
  "Deletes ip at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'ip arg (called-interactively-p 'any)))

(defun ar-delete-ip-in-region (beg end)
  "Deletes IP at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'ip beg end (called-interactively-p 'any)))

(defun ar-isbn-atpt () 
  "Returns isbn at point if any, nil otherwise. "
  (interactive)
  (ar-th 'isbn nil (called-interactively-p 'any)))

(defun ar-bounds-of-isbn-atpt ()
  "Returns a list, borders of isbn if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'isbn nil (called-interactively-p 'any)))

(defun ar-isbn-beginning-position-atpt ()
  "Returns a number, beginning position ISBN at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'isbn nil (called-interactively-p 'any)))

(defun ar-isbn-end-position-atpt ()
  "Returns a number, end position of ISBN at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'isbn nil (called-interactively-p 'any)))

(defun ar-isbn-beginning-atpt ()
  "Goto beginning of symbol or char-class ISBN at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'isbn nil (called-interactively-p 'any)))

(defun ar-isbn-end-atpt ()
  "Goto end of symbol or char-class ISBN at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'isbn nil (called-interactively-p 'any)))

(defun ar-in-isbn-p-atpt ()
  "Returns bounds of ISBN at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'isbn nil (called-interactively-p 'any)))

(defun ar-length-of-isbn-atpt ()
  "Returns beginning of symbol or char-class ISBN at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'isbn nil (called-interactively-p 'any)))

(defun ar-copy-isbn-atpt ()
  "Returns a copy of ISBN at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'isbn nil (called-interactively-p 'any)))

(defun ar-delete-isbn-atpt (&optional arg)
  "Deletes isbn at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'isbn arg (called-interactively-p 'any)))

(defun ar-kill-isbn-atpt (&optional arg)
  "Deletes isbn at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'isbn arg (called-interactively-p 'any)))

(defun ar-forward-isbn-atpt (&optional arg)
  "Deletes isbn at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'isbn arg (called-interactively-p 'any)))

(defun ar-backward-isbn-atpt (&optional arg)
  "Deletes isbn at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'isbn arg (called-interactively-p 'any)))

(defun ar-triplequotedq-isbn-atpt (&optional arg)
  "Deletes isbn at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'isbn arg (called-interactively-p 'any)))

(defun ar-triplequotesq-isbn-atpt (&optional arg)
  "Deletes isbn at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'isbn arg (called-interactively-p 'any)))

(defun ar-delete-isbn-in-region (beg end)
  "Deletes ISBN at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'isbn beg end (called-interactively-p 'any)))

(defun ar-line-atpt () 
  "Returns line at point if any, nil otherwise. "
  (interactive)
  (ar-th 'line nil (called-interactively-p 'any)))

(defun ar-bounds-of-line-atpt ()
  "Returns a list, borders of line if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'line nil (called-interactively-p 'any)))

(defun ar-line-beginning-position-atpt ()
  "Returns a number, beginning position LINE at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'line nil (called-interactively-p 'any)))

(defun ar-line-end-position-atpt ()
  "Returns a number, end position of LINE at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'line nil (called-interactively-p 'any)))

(defun ar-line-beginning-atpt ()
  "Goto beginning of symbol or char-class LINE at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'line nil (called-interactively-p 'any)))

(defun ar-line-end-atpt ()
  "Goto end of symbol or char-class LINE at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'line nil (called-interactively-p 'any)))

(defun ar-in-line-p-atpt ()
  "Returns bounds of LINE at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'line nil (called-interactively-p 'any)))

(defun ar-length-of-line-atpt ()
  "Returns beginning of symbol or char-class LINE at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'line nil (called-interactively-p 'any)))

(defun ar-copy-line-atpt ()
  "Returns a copy of LINE at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'line nil (called-interactively-p 'any)))

(defun ar-delete-line-atpt (&optional arg)
  "Deletes line at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'line arg (called-interactively-p 'any)))

(defun ar-kill-line-atpt (&optional arg)
  "Deletes line at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'line arg (called-interactively-p 'any)))

(defun ar-forward-line-atpt (&optional arg)
  "Deletes line at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'line arg (called-interactively-p 'any)))

(defun ar-backward-line-atpt (&optional arg)
  "Deletes line at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'line arg (called-interactively-p 'any)))

(defun ar-triplequotedq-line-atpt (&optional arg)
  "Deletes line at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'line arg (called-interactively-p 'any)))

(defun ar-triplequotesq-line-atpt (&optional arg)
  "Deletes line at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'line arg (called-interactively-p 'any)))

(defun ar-delete-line-in-region (beg end)
  "Deletes LINE at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'line beg end (called-interactively-p 'any)))

(defun ar-list-atpt () 
  "Returns list at point if any, nil otherwise. "
  (interactive)
  (ar-th 'list nil (called-interactively-p 'any)))

(defun ar-bounds-of-list-atpt ()
  "Returns a list, borders of list if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'list nil (called-interactively-p 'any)))

(defun ar-list-beginning-position-atpt ()
  "Returns a number, beginning position LIST at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'list nil (called-interactively-p 'any)))

(defun ar-list-end-position-atpt ()
  "Returns a number, end position of LIST at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'list nil (called-interactively-p 'any)))

(defun ar-list-beginning-atpt ()
  "Goto beginning of symbol or char-class LIST at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'list nil (called-interactively-p 'any)))

(defun ar-list-end-atpt ()
  "Goto end of symbol or char-class LIST at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'list nil (called-interactively-p 'any)))

(defun ar-in-list-p-atpt ()
  "Returns bounds of LIST at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'list nil (called-interactively-p 'any)))

(defun ar-length-of-list-atpt ()
  "Returns beginning of symbol or char-class LIST at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'list nil (called-interactively-p 'any)))

(defun ar-copy-list-atpt ()
  "Returns a copy of LIST at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'list nil (called-interactively-p 'any)))

(defun ar-delete-list-atpt (&optional arg)
  "Deletes list at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'list arg (called-interactively-p 'any)))

(defun ar-kill-list-atpt (&optional arg)
  "Deletes list at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'list arg (called-interactively-p 'any)))

(defun ar-forward-list-atpt (&optional arg)
  "Deletes list at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'list arg (called-interactively-p 'any)))

(defun ar-backward-list-atpt (&optional arg)
  "Deletes list at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'list arg (called-interactively-p 'any)))

(defun ar-triplequotedq-list-atpt (&optional arg)
  "Deletes list at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'list arg (called-interactively-p 'any)))

(defun ar-triplequotesq-list-atpt (&optional arg)
  "Deletes list at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'list arg (called-interactively-p 'any)))

(defun ar-delete-list-in-region (beg end)
  "Deletes LIST at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'list beg end (called-interactively-p 'any)))

(defun ar-name-atpt () 
  "Returns name at point if any, nil otherwise. "
  (interactive)
  (ar-th 'name nil (called-interactively-p 'any)))

(defun ar-bounds-of-name-atpt ()
  "Returns a list, borders of name if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'name nil (called-interactively-p 'any)))

(defun ar-name-beginning-position-atpt ()
  "Returns a number, beginning position NAME at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'name nil (called-interactively-p 'any)))

(defun ar-name-end-position-atpt ()
  "Returns a number, end position of NAME at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'name nil (called-interactively-p 'any)))

(defun ar-name-beginning-atpt ()
  "Goto beginning of symbol or char-class NAME at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'name nil (called-interactively-p 'any)))

(defun ar-name-end-atpt ()
  "Goto end of symbol or char-class NAME at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'name nil (called-interactively-p 'any)))

(defun ar-in-name-p-atpt ()
  "Returns bounds of NAME at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'name nil (called-interactively-p 'any)))

(defun ar-length-of-name-atpt ()
  "Returns beginning of symbol or char-class NAME at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'name nil (called-interactively-p 'any)))

(defun ar-copy-name-atpt ()
  "Returns a copy of NAME at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'name nil (called-interactively-p 'any)))

(defun ar-delete-name-atpt (&optional arg)
  "Deletes name at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'name arg (called-interactively-p 'any)))

(defun ar-kill-name-atpt (&optional arg)
  "Deletes name at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'name arg (called-interactively-p 'any)))

(defun ar-forward-name-atpt (&optional arg)
  "Deletes name at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'name arg (called-interactively-p 'any)))

(defun ar-backward-name-atpt (&optional arg)
  "Deletes name at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'name arg (called-interactively-p 'any)))

(defun ar-triplequotedq-name-atpt (&optional arg)
  "Deletes name at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'name arg (called-interactively-p 'any)))

(defun ar-triplequotesq-name-atpt (&optional arg)
  "Deletes name at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'name arg (called-interactively-p 'any)))

(defun ar-delete-name-in-region (beg end)
  "Deletes NAME at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'name beg end (called-interactively-p 'any)))

(defun ar-number-atpt () 
  "Returns number at point if any, nil otherwise. "
  (interactive)
  (ar-th 'number nil (called-interactively-p 'any)))

(defun ar-bounds-of-number-atpt ()
  "Returns a list, borders of number if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'number nil (called-interactively-p 'any)))

(defun ar-number-beginning-position-atpt ()
  "Returns a number, beginning position NUMBER at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'number nil (called-interactively-p 'any)))

(defun ar-number-end-position-atpt ()
  "Returns a number, end position of NUMBER at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'number nil (called-interactively-p 'any)))

(defun ar-number-beginning-atpt ()
  "Goto beginning of symbol or char-class NUMBER at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'number nil (called-interactively-p 'any)))

(defun ar-number-end-atpt ()
  "Goto end of symbol or char-class NUMBER at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'number nil (called-interactively-p 'any)))

(defun ar-in-number-p-atpt ()
  "Returns bounds of NUMBER at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'number nil (called-interactively-p 'any)))

(defun ar-length-of-number-atpt ()
  "Returns beginning of symbol or char-class NUMBER at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'number nil (called-interactively-p 'any)))

(defun ar-copy-number-atpt ()
  "Returns a copy of NUMBER at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'number nil (called-interactively-p 'any)))

(defun ar-delete-number-atpt (&optional arg)
  "Deletes number at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'number arg (called-interactively-p 'any)))

(defun ar-kill-number-atpt (&optional arg)
  "Deletes number at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'number arg (called-interactively-p 'any)))

(defun ar-forward-number-atpt (&optional arg)
  "Deletes number at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'number arg (called-interactively-p 'any)))

(defun ar-backward-number-atpt (&optional arg)
  "Deletes number at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'number arg (called-interactively-p 'any)))

(defun ar-triplequotedq-number-atpt (&optional arg)
  "Deletes number at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'number arg (called-interactively-p 'any)))

(defun ar-triplequotesq-number-atpt (&optional arg)
  "Deletes number at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'number arg (called-interactively-p 'any)))

(defun ar-delete-number-in-region (beg end)
  "Deletes NUMBER at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'number beg end (called-interactively-p 'any)))

(defun ar-page-atpt () 
  "Returns page at point if any, nil otherwise. "
  (interactive)
  (ar-th 'page nil (called-interactively-p 'any)))

(defun ar-bounds-of-page-atpt ()
  "Returns a list, borders of page if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'page nil (called-interactively-p 'any)))

(defun ar-page-beginning-position-atpt ()
  "Returns a number, beginning position PAGE at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'page nil (called-interactively-p 'any)))

(defun ar-page-end-position-atpt ()
  "Returns a number, end position of PAGE at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'page nil (called-interactively-p 'any)))

(defun ar-page-beginning-atpt ()
  "Goto beginning of symbol or char-class PAGE at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'page nil (called-interactively-p 'any)))

(defun ar-page-end-atpt ()
  "Goto end of symbol or char-class PAGE at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'page nil (called-interactively-p 'any)))

(defun ar-in-page-p-atpt ()
  "Returns bounds of PAGE at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'page nil (called-interactively-p 'any)))

(defun ar-length-of-page-atpt ()
  "Returns beginning of symbol or char-class PAGE at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'page nil (called-interactively-p 'any)))

(defun ar-copy-page-atpt ()
  "Returns a copy of PAGE at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'page nil (called-interactively-p 'any)))

(defun ar-delete-page-atpt (&optional arg)
  "Deletes page at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'page arg (called-interactively-p 'any)))

(defun ar-kill-page-atpt (&optional arg)
  "Deletes page at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'page arg (called-interactively-p 'any)))

(defun ar-forward-page-atpt (&optional arg)
  "Deletes page at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'page arg (called-interactively-p 'any)))

(defun ar-backward-page-atpt (&optional arg)
  "Deletes page at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'page arg (called-interactively-p 'any)))

(defun ar-triplequotedq-page-atpt (&optional arg)
  "Deletes page at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'page arg (called-interactively-p 'any)))

(defun ar-triplequotesq-page-atpt (&optional arg)
  "Deletes page at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'page arg (called-interactively-p 'any)))

(defun ar-delete-page-in-region (beg end)
  "Deletes PAGE at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'page beg end (called-interactively-p 'any)))

(defun ar-paragraph-atpt () 
  "Returns paragraph at point if any, nil otherwise. "
  (interactive)
  (ar-th 'paragraph nil (called-interactively-p 'any)))

(defun ar-bounds-of-paragraph-atpt ()
  "Returns a list, borders of paragraph if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'paragraph nil (called-interactively-p 'any)))

(defun ar-paragraph-beginning-position-atpt ()
  "Returns a number, beginning position PARAGRAPH at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'paragraph nil (called-interactively-p 'any)))

(defun ar-paragraph-end-position-atpt ()
  "Returns a number, end position of PARAGRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'paragraph nil (called-interactively-p 'any)))

(defun ar-paragraph-beginning-atpt ()
  "Goto beginning of symbol or char-class PARAGRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'paragraph nil (called-interactively-p 'any)))

(defun ar-paragraph-end-atpt ()
  "Goto end of symbol or char-class PARAGRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'paragraph nil (called-interactively-p 'any)))

(defun ar-in-paragraph-p-atpt ()
  "Returns bounds of PARAGRAPH at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'paragraph nil (called-interactively-p 'any)))

(defun ar-length-of-paragraph-atpt ()
  "Returns beginning of symbol or char-class PARAGRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'paragraph nil (called-interactively-p 'any)))

(defun ar-copy-paragraph-atpt ()
  "Returns a copy of PARAGRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'paragraph nil (called-interactively-p 'any)))

(defun ar-delete-paragraph-atpt (&optional arg)
  "Deletes paragraph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'paragraph arg (called-interactively-p 'any)))

(defun ar-kill-paragraph-atpt (&optional arg)
  "Deletes paragraph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'paragraph arg (called-interactively-p 'any)))

(defun ar-forward-paragraph-atpt (&optional arg)
  "Deletes paragraph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'paragraph arg (called-interactively-p 'any)))

(defun ar-backward-paragraph-atpt (&optional arg)
  "Deletes paragraph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'paragraph arg (called-interactively-p 'any)))

(defun ar-triplequotedq-paragraph-atpt (&optional arg)
  "Deletes paragraph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'paragraph arg (called-interactively-p 'any)))

(defun ar-triplequotesq-paragraph-atpt (&optional arg)
  "Deletes paragraph at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'paragraph arg (called-interactively-p 'any)))

(defun ar-delete-paragraph-in-region (beg end)
  "Deletes PARAGRAPH at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'paragraph beg end (called-interactively-p 'any)))

(defun ar-phone-atpt () 
  "Returns phone at point if any, nil otherwise. "
  (interactive)
  (ar-th 'phone nil (called-interactively-p 'any)))

(defun ar-bounds-of-phone-atpt ()
  "Returns a list, borders of phone if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'phone nil (called-interactively-p 'any)))

(defun ar-phone-beginning-position-atpt ()
  "Returns a number, beginning position PHONE at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'phone nil (called-interactively-p 'any)))

(defun ar-phone-end-position-atpt ()
  "Returns a number, end position of PHONE at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'phone nil (called-interactively-p 'any)))

(defun ar-phone-beginning-atpt ()
  "Goto beginning of symbol or char-class PHONE at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'phone nil (called-interactively-p 'any)))

(defun ar-phone-end-atpt ()
  "Goto end of symbol or char-class PHONE at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'phone nil (called-interactively-p 'any)))

(defun ar-in-phone-p-atpt ()
  "Returns bounds of PHONE at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'phone nil (called-interactively-p 'any)))

(defun ar-length-of-phone-atpt ()
  "Returns beginning of symbol or char-class PHONE at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'phone nil (called-interactively-p 'any)))

(defun ar-copy-phone-atpt ()
  "Returns a copy of PHONE at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'phone nil (called-interactively-p 'any)))

(defun ar-delete-phone-atpt (&optional arg)
  "Deletes phone at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'phone arg (called-interactively-p 'any)))

(defun ar-kill-phone-atpt (&optional arg)
  "Deletes phone at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'phone arg (called-interactively-p 'any)))

(defun ar-forward-phone-atpt (&optional arg)
  "Deletes phone at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'phone arg (called-interactively-p 'any)))

(defun ar-backward-phone-atpt (&optional arg)
  "Deletes phone at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'phone arg (called-interactively-p 'any)))

(defun ar-triplequotedq-phone-atpt (&optional arg)
  "Deletes phone at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'phone arg (called-interactively-p 'any)))

(defun ar-triplequotesq-phone-atpt (&optional arg)
  "Deletes phone at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'phone arg (called-interactively-p 'any)))

(defun ar-delete-phone-in-region (beg end)
  "Deletes PHONE at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'phone beg end (called-interactively-p 'any)))

(defun ar-region-atpt () 
  "Returns region at point if any, nil otherwise. "
  (interactive)
  (ar-th 'region nil (called-interactively-p 'any)))

(defun ar-bounds-of-region-atpt ()
  "Returns a list, borders of region if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'region nil (called-interactively-p 'any)))

(defun ar-region-beginning-position-atpt ()
  "Returns a number, beginning position REGION at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'region nil (called-interactively-p 'any)))

(defun ar-region-end-position-atpt ()
  "Returns a number, end position of REGION at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'region nil (called-interactively-p 'any)))

(defun ar-region-beginning-atpt ()
  "Goto beginning of symbol or char-class REGION at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'region nil (called-interactively-p 'any)))

(defun ar-region-end-atpt ()
  "Goto end of symbol or char-class REGION at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'region nil (called-interactively-p 'any)))

(defun ar-in-region-p-atpt ()
  "Returns bounds of REGION at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'region nil (called-interactively-p 'any)))

(defun ar-length-of-region-atpt ()
  "Returns beginning of symbol or char-class REGION at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'region nil (called-interactively-p 'any)))

(defun ar-copy-region-atpt ()
  "Returns a copy of REGION at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'region nil (called-interactively-p 'any)))

(defun ar-delete-region-atpt (&optional arg)
  "Deletes region at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'region arg (called-interactively-p 'any)))

(defun ar-kill-region-atpt (&optional arg)
  "Deletes region at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'region arg (called-interactively-p 'any)))

(defun ar-forward-region-atpt (&optional arg)
  "Deletes region at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'region arg (called-interactively-p 'any)))

(defun ar-backward-region-atpt (&optional arg)
  "Deletes region at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'region arg (called-interactively-p 'any)))

(defun ar-triplequotedq-region-atpt (&optional arg)
  "Deletes region at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'region arg (called-interactively-p 'any)))

(defun ar-triplequotesq-region-atpt (&optional arg)
  "Deletes region at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'region arg (called-interactively-p 'any)))

(defun ar-delete-region-in-region (beg end)
  "Deletes REGION at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'region beg end (called-interactively-p 'any)))

(defun ar-sentence-atpt () 
  "Returns sentence at point if any, nil otherwise. "
  (interactive)
  (ar-th 'sentence nil (called-interactively-p 'any)))

(defun ar-bounds-of-sentence-atpt ()
  "Returns a list, borders of sentence if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'sentence nil (called-interactively-p 'any)))

(defun ar-sentence-beginning-position-atpt ()
  "Returns a number, beginning position SENTENCE at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'sentence nil (called-interactively-p 'any)))

(defun ar-sentence-end-position-atpt ()
  "Returns a number, end position of SENTENCE at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'sentence nil (called-interactively-p 'any)))

(defun ar-sentence-beginning-atpt ()
  "Goto beginning of symbol or char-class SENTENCE at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'sentence nil (called-interactively-p 'any)))

(defun ar-sentence-end-atpt ()
  "Goto end of symbol or char-class SENTENCE at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'sentence nil (called-interactively-p 'any)))

(defun ar-in-sentence-p-atpt ()
  "Returns bounds of SENTENCE at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'sentence nil (called-interactively-p 'any)))

(defun ar-length-of-sentence-atpt ()
  "Returns beginning of symbol or char-class SENTENCE at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'sentence nil (called-interactively-p 'any)))

(defun ar-copy-sentence-atpt ()
  "Returns a copy of SENTENCE at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'sentence nil (called-interactively-p 'any)))

(defun ar-delete-sentence-atpt (&optional arg)
  "Deletes sentence at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'sentence arg (called-interactively-p 'any)))

(defun ar-kill-sentence-atpt (&optional arg)
  "Deletes sentence at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'sentence arg (called-interactively-p 'any)))

(defun ar-forward-sentence-atpt (&optional arg)
  "Deletes sentence at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'sentence arg (called-interactively-p 'any)))

(defun ar-backward-sentence-atpt (&optional arg)
  "Deletes sentence at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'sentence arg (called-interactively-p 'any)))

(defun ar-triplequotedq-sentence-atpt (&optional arg)
  "Deletes sentence at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'sentence arg (called-interactively-p 'any)))

(defun ar-triplequotesq-sentence-atpt (&optional arg)
  "Deletes sentence at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'sentence arg (called-interactively-p 'any)))

(defun ar-delete-sentence-in-region (beg end)
  "Deletes SENTENCE at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'sentence beg end (called-interactively-p 'any)))

(defun ar-sexp-atpt () 
  "Returns sexp at point if any, nil otherwise. "
  (interactive)
  (ar-th 'sexp nil (called-interactively-p 'any)))

(defun ar-bounds-of-sexp-atpt ()
  "Returns a list, borders of sexp if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'sexp nil (called-interactively-p 'any)))

(defun ar-sexp-beginning-position-atpt ()
  "Returns a number, beginning position SEXP at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'sexp nil (called-interactively-p 'any)))

(defun ar-sexp-end-position-atpt ()
  "Returns a number, end position of SEXP at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'sexp nil (called-interactively-p 'any)))

(defun ar-sexp-beginning-atpt ()
  "Goto beginning of symbol or char-class SEXP at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'sexp nil (called-interactively-p 'any)))

(defun ar-sexp-end-atpt ()
  "Goto end of symbol or char-class SEXP at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'sexp nil (called-interactively-p 'any)))

(defun ar-in-sexp-p-atpt ()
  "Returns bounds of SEXP at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'sexp nil (called-interactively-p 'any)))

(defun ar-length-of-sexp-atpt ()
  "Returns beginning of symbol or char-class SEXP at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'sexp nil (called-interactively-p 'any)))

(defun ar-copy-sexp-atpt ()
  "Returns a copy of SEXP at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'sexp nil (called-interactively-p 'any)))

(defun ar-delete-sexp-atpt (&optional arg)
  "Deletes sexp at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'sexp arg (called-interactively-p 'any)))

(defun ar-kill-sexp-atpt (&optional arg)
  "Deletes sexp at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'sexp arg (called-interactively-p 'any)))

(defun ar-forward-sexp-atpt (&optional arg)
  "Deletes sexp at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'sexp arg (called-interactively-p 'any)))

(defun ar-backward-sexp-atpt (&optional arg)
  "Deletes sexp at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'sexp arg (called-interactively-p 'any)))

(defun ar-triplequotedq-sexp-atpt (&optional arg)
  "Deletes sexp at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'sexp arg (called-interactively-p 'any)))

(defun ar-triplequotesq-sexp-atpt (&optional arg)
  "Deletes sexp at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'sexp arg (called-interactively-p 'any)))

(defun ar-delete-sexp-in-region (beg end)
  "Deletes SEXP at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'sexp beg end (called-interactively-p 'any)))

(defun ar-shstruct-atpt () 
  "Returns shstruct at point if any, nil otherwise. "
  (interactive)
  (ar-th 'shstruct nil (called-interactively-p 'any)))

(defun ar-bounds-of-shstruct-atpt ()
  "Returns a list, borders of shstruct if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'shstruct nil (called-interactively-p 'any)))

(defun ar-shstruct-beginning-position-atpt ()
  "Returns a number, beginning position SHSTRUCT at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'shstruct nil (called-interactively-p 'any)))

(defun ar-shstruct-end-position-atpt ()
  "Returns a number, end position of SHSTRUCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'shstruct nil (called-interactively-p 'any)))

(defun ar-shstruct-beginning-atpt ()
  "Goto beginning of symbol or char-class SHSTRUCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'shstruct nil (called-interactively-p 'any)))

(defun ar-shstruct-end-atpt ()
  "Goto end of symbol or char-class SHSTRUCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'shstruct nil (called-interactively-p 'any)))

(defun ar-in-shstruct-p-atpt ()
  "Returns bounds of SHSTRUCT at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'shstruct nil (called-interactively-p 'any)))

(defun ar-length-of-shstruct-atpt ()
  "Returns beginning of symbol or char-class SHSTRUCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'shstruct nil (called-interactively-p 'any)))

(defun ar-copy-shstruct-atpt ()
  "Returns a copy of SHSTRUCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'shstruct nil (called-interactively-p 'any)))

(defun ar-delete-shstruct-atpt (&optional arg)
  "Deletes shstruct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'shstruct arg (called-interactively-p 'any)))

(defun ar-kill-shstruct-atpt (&optional arg)
  "Deletes shstruct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'shstruct arg (called-interactively-p 'any)))

(defun ar-forward-shstruct-atpt (&optional arg)
  "Deletes shstruct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'shstruct arg (called-interactively-p 'any)))

(defun ar-backward-shstruct-atpt (&optional arg)
  "Deletes shstruct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'shstruct arg (called-interactively-p 'any)))

(defun ar-triplequotedq-shstruct-atpt (&optional arg)
  "Deletes shstruct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'shstruct arg (called-interactively-p 'any)))

(defun ar-triplequotesq-shstruct-atpt (&optional arg)
  "Deletes shstruct at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'shstruct arg (called-interactively-p 'any)))

(defun ar-delete-shstruct-in-region (beg end)
  "Deletes SHSTRUCT at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'shstruct beg end (called-interactively-p 'any)))

(defun ar-symbol-atpt () 
  "Returns symbol at point if any, nil otherwise. "
  (interactive)
  (ar-th 'symbol nil (called-interactively-p 'any)))

(defun ar-bounds-of-symbol-atpt ()
  "Returns a list, borders of symbol if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'symbol nil (called-interactively-p 'any)))

(defun ar-symbol-beginning-position-atpt ()
  "Returns a number, beginning position SYMBOL at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'symbol nil (called-interactively-p 'any)))

(defun ar-symbol-end-position-atpt ()
  "Returns a number, end position of SYMBOL at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'symbol nil (called-interactively-p 'any)))

(defun ar-symbol-beginning-atpt ()
  "Goto beginning of symbol or char-class SYMBOL at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'symbol nil (called-interactively-p 'any)))

(defun ar-symbol-end-atpt ()
  "Goto end of symbol or char-class SYMBOL at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'symbol nil (called-interactively-p 'any)))

(defun ar-in-symbol-p-atpt ()
  "Returns bounds of SYMBOL at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'symbol nil (called-interactively-p 'any)))

(defun ar-length-of-symbol-atpt ()
  "Returns beginning of symbol or char-class SYMBOL at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'symbol nil (called-interactively-p 'any)))

(defun ar-copy-symbol-atpt ()
  "Returns a copy of SYMBOL at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'symbol nil (called-interactively-p 'any)))

(defun ar-delete-symbol-atpt (&optional arg)
  "Deletes symbol at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'symbol arg (called-interactively-p 'any)))

(defun ar-kill-symbol-atpt (&optional arg)
  "Deletes symbol at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'symbol arg (called-interactively-p 'any)))

(defun ar-forward-symbol-atpt (&optional arg)
  "Deletes symbol at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'symbol arg (called-interactively-p 'any)))

(defun ar-backward-symbol-atpt (&optional arg)
  "Deletes symbol at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'symbol arg (called-interactively-p 'any)))

(defun ar-triplequotedq-symbol-atpt (&optional arg)
  "Deletes symbol at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'symbol arg (called-interactively-p 'any)))

(defun ar-triplequotesq-symbol-atpt (&optional arg)
  "Deletes symbol at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'symbol arg (called-interactively-p 'any)))

(defun ar-delete-symbol-in-region (beg end)
  "Deletes SYMBOL at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'symbol beg end (called-interactively-p 'any)))

(defun ar-url-atpt () 
  "Returns url at point if any, nil otherwise. "
  (interactive)
  (ar-th 'url nil (called-interactively-p 'any)))

(defun ar-bounds-of-url-atpt ()
  "Returns a list, borders of url if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'url nil (called-interactively-p 'any)))

(defun ar-url-beginning-position-atpt ()
  "Returns a number, beginning position URL at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'url nil (called-interactively-p 'any)))

(defun ar-url-end-position-atpt ()
  "Returns a number, end position of URL at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'url nil (called-interactively-p 'any)))

(defun ar-url-beginning-atpt ()
  "Goto beginning of symbol or char-class URL at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'url nil (called-interactively-p 'any)))

(defun ar-url-end-atpt ()
  "Goto end of symbol or char-class URL at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'url nil (called-interactively-p 'any)))

(defun ar-in-url-p-atpt ()
  "Returns bounds of URL at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'url nil (called-interactively-p 'any)))

(defun ar-length-of-url-atpt ()
  "Returns beginning of symbol or char-class URL at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'url nil (called-interactively-p 'any)))

(defun ar-copy-url-atpt ()
  "Returns a copy of URL at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'url nil (called-interactively-p 'any)))

(defun ar-delete-url-atpt (&optional arg)
  "Deletes url at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'url arg (called-interactively-p 'any)))

(defun ar-kill-url-atpt (&optional arg)
  "Deletes url at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'url arg (called-interactively-p 'any)))

(defun ar-forward-url-atpt (&optional arg)
  "Deletes url at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'url arg (called-interactively-p 'any)))

(defun ar-backward-url-atpt (&optional arg)
  "Deletes url at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'url arg (called-interactively-p 'any)))

(defun ar-triplequotedq-url-atpt (&optional arg)
  "Deletes url at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'url arg (called-interactively-p 'any)))

(defun ar-triplequotesq-url-atpt (&optional arg)
  "Deletes url at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'url arg (called-interactively-p 'any)))

(defun ar-delete-url-in-region (beg end)
  "Deletes URL at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'url beg end (called-interactively-p 'any)))

(defun ar-word-atpt () 
  "Returns word at point if any, nil otherwise. "
  (interactive)
  (ar-th 'word nil (called-interactively-p 'any)))

(defun ar-bounds-of-word-atpt ()
  "Returns a list, borders of word if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'word nil (called-interactively-p 'any)))

(defun ar-word-beginning-position-atpt ()
  "Returns a number, beginning position WORD at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'word nil (called-interactively-p 'any)))

(defun ar-word-end-position-atpt ()
  "Returns a number, end position of WORD at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'word nil (called-interactively-p 'any)))

(defun ar-word-beginning-atpt ()
  "Goto beginning of symbol or char-class WORD at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'word nil (called-interactively-p 'any)))

(defun ar-word-end-atpt ()
  "Goto end of symbol or char-class WORD at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'word nil (called-interactively-p 'any)))

(defun ar-in-word-p-atpt ()
  "Returns bounds of WORD at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'word nil (called-interactively-p 'any)))

(defun ar-length-of-word-atpt ()
  "Returns beginning of symbol or char-class WORD at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'word nil (called-interactively-p 'any)))

(defun ar-copy-word-atpt ()
  "Returns a copy of WORD at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'word nil (called-interactively-p 'any)))

(defun ar-delete-word-atpt (&optional arg)
  "Deletes word at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'word arg (called-interactively-p 'any)))

(defun ar-kill-word-atpt (&optional arg)
  "Deletes word at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'word arg (called-interactively-p 'any)))

(defun ar-forward-word-atpt (&optional arg)
  "Deletes word at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'word arg (called-interactively-p 'any)))

(defun ar-backward-word-atpt (&optional arg)
  "Deletes word at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'word arg (called-interactively-p 'any)))

(defun ar-triplequotedq-word-atpt (&optional arg)
  "Deletes word at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'word arg (called-interactively-p 'any)))

(defun ar-triplequotesq-word-atpt (&optional arg)
  "Deletes word at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'word arg (called-interactively-p 'any)))

(defun ar-delete-word-in-region (beg end)
  "Deletes WORD at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'word beg end (called-interactively-p 'any)))

(defun ar-wordalphaonly-atpt () 
  "Returns wordalphaonly at point if any, nil otherwise. "
  (interactive)
  (ar-th 'wordalphaonly nil (called-interactively-p 'any)))

(defun ar-bounds-of-wordalphaonly-atpt ()
  "Returns a list, borders of wordalphaonly if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'wordalphaonly nil (called-interactively-p 'any)))

(defun ar-wordalphaonly-beginning-position-atpt ()
  "Returns a number, beginning position WORDALPHAONLY at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'wordalphaonly nil (called-interactively-p 'any)))

(defun ar-wordalphaonly-end-position-atpt ()
  "Returns a number, end position of WORDALPHAONLY at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'wordalphaonly nil (called-interactively-p 'any)))

(defun ar-wordalphaonly-beginning-atpt ()
  "Goto beginning of symbol or char-class WORDALPHAONLY at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'wordalphaonly nil (called-interactively-p 'any)))

(defun ar-wordalphaonly-end-atpt ()
  "Goto end of symbol or char-class WORDALPHAONLY at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'wordalphaonly nil (called-interactively-p 'any)))

(defun ar-in-wordalphaonly-p-atpt ()
  "Returns bounds of WORDALPHAONLY at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'wordalphaonly nil (called-interactively-p 'any)))

(defun ar-length-of-wordalphaonly-atpt ()
  "Returns beginning of symbol or char-class WORDALPHAONLY at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'wordalphaonly nil (called-interactively-p 'any)))

(defun ar-copy-wordalphaonly-atpt ()
  "Returns a copy of WORDALPHAONLY at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'wordalphaonly nil (called-interactively-p 'any)))

(defun ar-delete-wordalphaonly-atpt (&optional arg)
  "Deletes wordalphaonly at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'wordalphaonly arg (called-interactively-p 'any)))

(defun ar-kill-wordalphaonly-atpt (&optional arg)
  "Deletes wordalphaonly at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-kill 'wordalphaonly arg (called-interactively-p 'any)))

(defun ar-forward-wordalphaonly-atpt (&optional arg)
  "Deletes wordalphaonly at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-forward 'wordalphaonly arg (called-interactively-p 'any)))

(defun ar-backward-wordalphaonly-atpt (&optional arg)
  "Deletes wordalphaonly at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-backward 'wordalphaonly arg (called-interactively-p 'any)))

(defun ar-triplequotedq-wordalphaonly-atpt (&optional arg)
  "Deletes wordalphaonly at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotedq 'wordalphaonly arg (called-interactively-p 'any)))

(defun ar-triplequotesq-wordalphaonly-atpt (&optional arg)
  "Deletes wordalphaonly at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-triplequotesq 'wordalphaonly arg (called-interactively-p 'any)))

(defun ar-delete-wordalphaonly-in-region (beg end)
  "Deletes WORDALPHAONLY at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'wordalphaonly beg end (called-interactively-p 'any)))
;; ar-thing-at-point-utils-nodelim-core ar-atpt-rest-list: end

;; ar-thing-at-point-utils-unpaired-delim-aktiv ar-unpaired-delimit-aktiv ar-atpt-rest-list: start


(defalias 'ar-backslash-alnum-atpt 'ar-alnum-backslash-atpt)
(defun ar-alnum-backslash-atpt ()
  "Returns regexp-quoted ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'alnum nil))

(defalias 'ar-backtick-alnum-atpt 'ar-alnum-backtick-atpt)
(defun ar-alnum-backtick-atpt ()
  "Returns regexp-quoted ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'alnum nil))

(defalias 'ar-colon-alnum-atpt 'ar-alnum-colon-atpt)
(defun ar-alnum-colon-atpt ()
  "Returns regexp-quoted ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'alnum nil))

(defalias 'ar-cross-alnum-atpt 'ar-alnum-cross-atpt)
(defun ar-alnum-cross-atpt ()
  "Returns regexp-quoted ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'alnum nil))

(defalias 'ar-dollar-alnum-atpt 'ar-alnum-dollar-atpt)
(defun ar-alnum-dollar-atpt ()
  "Returns regexp-quoted ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'alnum nil))

(defalias 'ar-doublequote-alnum-atpt 'ar-alnum-doublequote-atpt)
(defun ar-alnum-doublequote-atpt ()
  "Returns regexp-quoted ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'alnum nil))

(defalias 'ar-equalize-alnum-atpt 'ar-alnum-equalize-atpt)
(defun ar-alnum-equalize-atpt ()
  "Returns regexp-quoted ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'alnum nil))

(defalias 'ar-escape-alnum-atpt 'ar-alnum-escape-atpt)
(defun ar-alnum-escape-atpt ()
  "Returns regexp-quoted ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'alnum nil))

(defalias 'ar-hash-alnum-atpt 'ar-alnum-hash-atpt)
(defun ar-alnum-hash-atpt ()
  "Returns regexp-quoted ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'alnum nil))

(defalias 'ar-hyphen-alnum-atpt 'ar-alnum-hyphen-atpt)
(defun ar-alnum-hyphen-atpt ()
  "Returns regexp-quoted ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'alnum nil))

(defalias 'ar-singlequote-alnum-atpt 'ar-alnum-singlequote-atpt)
(defun ar-alnum-singlequote-atpt ()
  "Returns regexp-quoted ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'alnum nil))

(defalias 'ar-slash-alnum-atpt 'ar-alnum-slash-atpt)
(defun ar-alnum-slash-atpt ()
  "Returns regexp-quoted ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'alnum nil))

(defalias 'ar-star-alnum-atpt 'ar-alnum-star-atpt)
(defun ar-alnum-star-atpt ()
  "Returns regexp-quoted ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'alnum nil))

(defalias 'ar-tild-alnum-atpt 'ar-alnum-tild-atpt)
(defun ar-alnum-tild-atpt ()
  "Returns regexp-quoted ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'alnum nil))

(defalias 'ar-underscore-alnum-atpt 'ar-alnum-underscore-atpt)
(defun ar-alnum-underscore-atpt ()
  "Returns regexp-quoted ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'alnum nil))

(defalias 'ar-whitespace-alnum-atpt 'ar-alnum-whitespace-atpt)
(defun ar-alnum-whitespace-atpt ()
  "Returns regexp-quoted ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'alnum nil))

(defalias 'ar-doubleslash-alnum-atpt 'ar-alnum-doubleslash-atpt)
(defun ar-alnum-doubleslash-atpt ()
  "Returns regexp-quoted ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'alnum nil))

(defalias 'ar-backslash-alpha-atpt 'ar-alpha-backslash-atpt)
(defun ar-alpha-backslash-atpt ()
  "Returns regexp-quoted ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'alpha nil))

(defalias 'ar-backtick-alpha-atpt 'ar-alpha-backtick-atpt)
(defun ar-alpha-backtick-atpt ()
  "Returns regexp-quoted ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'alpha nil))

(defalias 'ar-colon-alpha-atpt 'ar-alpha-colon-atpt)
(defun ar-alpha-colon-atpt ()
  "Returns regexp-quoted ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'alpha nil))

(defalias 'ar-cross-alpha-atpt 'ar-alpha-cross-atpt)
(defun ar-alpha-cross-atpt ()
  "Returns regexp-quoted ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'alpha nil))

(defalias 'ar-dollar-alpha-atpt 'ar-alpha-dollar-atpt)
(defun ar-alpha-dollar-atpt ()
  "Returns regexp-quoted ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'alpha nil))

(defalias 'ar-doublequote-alpha-atpt 'ar-alpha-doublequote-atpt)
(defun ar-alpha-doublequote-atpt ()
  "Returns regexp-quoted ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'alpha nil))

(defalias 'ar-equalize-alpha-atpt 'ar-alpha-equalize-atpt)
(defun ar-alpha-equalize-atpt ()
  "Returns regexp-quoted ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'alpha nil))

(defalias 'ar-escape-alpha-atpt 'ar-alpha-escape-atpt)
(defun ar-alpha-escape-atpt ()
  "Returns regexp-quoted ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'alpha nil))

(defalias 'ar-hash-alpha-atpt 'ar-alpha-hash-atpt)
(defun ar-alpha-hash-atpt ()
  "Returns regexp-quoted ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'alpha nil))

(defalias 'ar-hyphen-alpha-atpt 'ar-alpha-hyphen-atpt)
(defun ar-alpha-hyphen-atpt ()
  "Returns regexp-quoted ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'alpha nil))

(defalias 'ar-singlequote-alpha-atpt 'ar-alpha-singlequote-atpt)
(defun ar-alpha-singlequote-atpt ()
  "Returns regexp-quoted ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'alpha nil))

(defalias 'ar-slash-alpha-atpt 'ar-alpha-slash-atpt)
(defun ar-alpha-slash-atpt ()
  "Returns regexp-quoted ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'alpha nil))

(defalias 'ar-star-alpha-atpt 'ar-alpha-star-atpt)
(defun ar-alpha-star-atpt ()
  "Returns regexp-quoted ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'alpha nil))

(defalias 'ar-tild-alpha-atpt 'ar-alpha-tild-atpt)
(defun ar-alpha-tild-atpt ()
  "Returns regexp-quoted ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'alpha nil))

(defalias 'ar-underscore-alpha-atpt 'ar-alpha-underscore-atpt)
(defun ar-alpha-underscore-atpt ()
  "Returns regexp-quoted ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'alpha nil))

(defalias 'ar-whitespace-alpha-atpt 'ar-alpha-whitespace-atpt)
(defun ar-alpha-whitespace-atpt ()
  "Returns regexp-quoted ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'alpha nil))

(defalias 'ar-doubleslash-alpha-atpt 'ar-alpha-doubleslash-atpt)
(defun ar-alpha-doubleslash-atpt ()
  "Returns regexp-quoted ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'alpha nil))

(defalias 'ar-backslash-ascii-atpt 'ar-ascii-backslash-atpt)
(defun ar-ascii-backslash-atpt ()
  "Returns regexp-quoted ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'ascii nil))

(defalias 'ar-backtick-ascii-atpt 'ar-ascii-backtick-atpt)
(defun ar-ascii-backtick-atpt ()
  "Returns regexp-quoted ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'ascii nil))

(defalias 'ar-colon-ascii-atpt 'ar-ascii-colon-atpt)
(defun ar-ascii-colon-atpt ()
  "Returns regexp-quoted ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'ascii nil))

(defalias 'ar-cross-ascii-atpt 'ar-ascii-cross-atpt)
(defun ar-ascii-cross-atpt ()
  "Returns regexp-quoted ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'ascii nil))

(defalias 'ar-dollar-ascii-atpt 'ar-ascii-dollar-atpt)
(defun ar-ascii-dollar-atpt ()
  "Returns regexp-quoted ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'ascii nil))

(defalias 'ar-doublequote-ascii-atpt 'ar-ascii-doublequote-atpt)
(defun ar-ascii-doublequote-atpt ()
  "Returns regexp-quoted ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'ascii nil))

(defalias 'ar-equalize-ascii-atpt 'ar-ascii-equalize-atpt)
(defun ar-ascii-equalize-atpt ()
  "Returns regexp-quoted ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'ascii nil))

(defalias 'ar-escape-ascii-atpt 'ar-ascii-escape-atpt)
(defun ar-ascii-escape-atpt ()
  "Returns regexp-quoted ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'ascii nil))

(defalias 'ar-hash-ascii-atpt 'ar-ascii-hash-atpt)
(defun ar-ascii-hash-atpt ()
  "Returns regexp-quoted ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'ascii nil))

(defalias 'ar-hyphen-ascii-atpt 'ar-ascii-hyphen-atpt)
(defun ar-ascii-hyphen-atpt ()
  "Returns regexp-quoted ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'ascii nil))

(defalias 'ar-singlequote-ascii-atpt 'ar-ascii-singlequote-atpt)
(defun ar-ascii-singlequote-atpt ()
  "Returns regexp-quoted ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'ascii nil))

(defalias 'ar-slash-ascii-atpt 'ar-ascii-slash-atpt)
(defun ar-ascii-slash-atpt ()
  "Returns regexp-quoted ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'ascii nil))

(defalias 'ar-star-ascii-atpt 'ar-ascii-star-atpt)
(defun ar-ascii-star-atpt ()
  "Returns regexp-quoted ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'ascii nil))

(defalias 'ar-tild-ascii-atpt 'ar-ascii-tild-atpt)
(defun ar-ascii-tild-atpt ()
  "Returns regexp-quoted ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'ascii nil))

(defalias 'ar-underscore-ascii-atpt 'ar-ascii-underscore-atpt)
(defun ar-ascii-underscore-atpt ()
  "Returns regexp-quoted ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'ascii nil))

(defalias 'ar-whitespace-ascii-atpt 'ar-ascii-whitespace-atpt)
(defun ar-ascii-whitespace-atpt ()
  "Returns regexp-quoted ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'ascii nil))

(defalias 'ar-doubleslash-ascii-atpt 'ar-ascii-doubleslash-atpt)
(defun ar-ascii-doubleslash-atpt ()
  "Returns regexp-quoted ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'ascii nil))

(defalias 'ar-backslash-blank-atpt 'ar-blank-backslash-atpt)
(defun ar-blank-backslash-atpt ()
  "Returns regexp-quoted BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'blank nil))

(defalias 'ar-backtick-blank-atpt 'ar-blank-backtick-atpt)
(defun ar-blank-backtick-atpt ()
  "Returns regexp-quoted BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'blank nil))

(defalias 'ar-colon-blank-atpt 'ar-blank-colon-atpt)
(defun ar-blank-colon-atpt ()
  "Returns regexp-quoted BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'blank nil))

(defalias 'ar-cross-blank-atpt 'ar-blank-cross-atpt)
(defun ar-blank-cross-atpt ()
  "Returns regexp-quoted BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'blank nil))

(defalias 'ar-dollar-blank-atpt 'ar-blank-dollar-atpt)
(defun ar-blank-dollar-atpt ()
  "Returns regexp-quoted BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'blank nil))

(defalias 'ar-doublequote-blank-atpt 'ar-blank-doublequote-atpt)
(defun ar-blank-doublequote-atpt ()
  "Returns regexp-quoted BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'blank nil))

(defalias 'ar-equalize-blank-atpt 'ar-blank-equalize-atpt)
(defun ar-blank-equalize-atpt ()
  "Returns regexp-quoted BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'blank nil))

(defalias 'ar-escape-blank-atpt 'ar-blank-escape-atpt)
(defun ar-blank-escape-atpt ()
  "Returns regexp-quoted BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'blank nil))

(defalias 'ar-hash-blank-atpt 'ar-blank-hash-atpt)
(defun ar-blank-hash-atpt ()
  "Returns regexp-quoted BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'blank nil))

(defalias 'ar-hyphen-blank-atpt 'ar-blank-hyphen-atpt)
(defun ar-blank-hyphen-atpt ()
  "Returns regexp-quoted BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'blank nil))

(defalias 'ar-singlequote-blank-atpt 'ar-blank-singlequote-atpt)
(defun ar-blank-singlequote-atpt ()
  "Returns regexp-quoted BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'blank nil))

(defalias 'ar-slash-blank-atpt 'ar-blank-slash-atpt)
(defun ar-blank-slash-atpt ()
  "Returns regexp-quoted BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'blank nil))

(defalias 'ar-star-blank-atpt 'ar-blank-star-atpt)
(defun ar-blank-star-atpt ()
  "Returns regexp-quoted BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'blank nil))

(defalias 'ar-tild-blank-atpt 'ar-blank-tild-atpt)
(defun ar-blank-tild-atpt ()
  "Returns regexp-quoted BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'blank nil))

(defalias 'ar-underscore-blank-atpt 'ar-blank-underscore-atpt)
(defun ar-blank-underscore-atpt ()
  "Returns regexp-quoted BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'blank nil))

(defalias 'ar-whitespace-blank-atpt 'ar-blank-whitespace-atpt)
(defun ar-blank-whitespace-atpt ()
  "Returns regexp-quoted BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'blank nil))

(defalias 'ar-doubleslash-blank-atpt 'ar-blank-doubleslash-atpt)
(defun ar-blank-doubleslash-atpt ()
  "Returns regexp-quoted BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'blank nil))

(defalias 'ar-backslash-cntrl-atpt 'ar-cntrl-backslash-atpt)
(defun ar-cntrl-backslash-atpt ()
  "Returns regexp-quoted CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'cntrl nil))

(defalias 'ar-backtick-cntrl-atpt 'ar-cntrl-backtick-atpt)
(defun ar-cntrl-backtick-atpt ()
  "Returns regexp-quoted CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'cntrl nil))

(defalias 'ar-colon-cntrl-atpt 'ar-cntrl-colon-atpt)
(defun ar-cntrl-colon-atpt ()
  "Returns regexp-quoted CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'cntrl nil))

(defalias 'ar-cross-cntrl-atpt 'ar-cntrl-cross-atpt)
(defun ar-cntrl-cross-atpt ()
  "Returns regexp-quoted CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'cntrl nil))

(defalias 'ar-dollar-cntrl-atpt 'ar-cntrl-dollar-atpt)
(defun ar-cntrl-dollar-atpt ()
  "Returns regexp-quoted CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'cntrl nil))

(defalias 'ar-doublequote-cntrl-atpt 'ar-cntrl-doublequote-atpt)
(defun ar-cntrl-doublequote-atpt ()
  "Returns regexp-quoted CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'cntrl nil))

(defalias 'ar-equalize-cntrl-atpt 'ar-cntrl-equalize-atpt)
(defun ar-cntrl-equalize-atpt ()
  "Returns regexp-quoted CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'cntrl nil))

(defalias 'ar-escape-cntrl-atpt 'ar-cntrl-escape-atpt)
(defun ar-cntrl-escape-atpt ()
  "Returns regexp-quoted CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'cntrl nil))

(defalias 'ar-hash-cntrl-atpt 'ar-cntrl-hash-atpt)
(defun ar-cntrl-hash-atpt ()
  "Returns regexp-quoted CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'cntrl nil))

(defalias 'ar-hyphen-cntrl-atpt 'ar-cntrl-hyphen-atpt)
(defun ar-cntrl-hyphen-atpt ()
  "Returns regexp-quoted CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'cntrl nil))

(defalias 'ar-singlequote-cntrl-atpt 'ar-cntrl-singlequote-atpt)
(defun ar-cntrl-singlequote-atpt ()
  "Returns regexp-quoted CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'cntrl nil))

(defalias 'ar-slash-cntrl-atpt 'ar-cntrl-slash-atpt)
(defun ar-cntrl-slash-atpt ()
  "Returns regexp-quoted CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'cntrl nil))

(defalias 'ar-star-cntrl-atpt 'ar-cntrl-star-atpt)
(defun ar-cntrl-star-atpt ()
  "Returns regexp-quoted CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'cntrl nil))

(defalias 'ar-tild-cntrl-atpt 'ar-cntrl-tild-atpt)
(defun ar-cntrl-tild-atpt ()
  "Returns regexp-quoted CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'cntrl nil))

(defalias 'ar-underscore-cntrl-atpt 'ar-cntrl-underscore-atpt)
(defun ar-cntrl-underscore-atpt ()
  "Returns regexp-quoted CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'cntrl nil))

(defalias 'ar-whitespace-cntrl-atpt 'ar-cntrl-whitespace-atpt)
(defun ar-cntrl-whitespace-atpt ()
  "Returns regexp-quoted CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'cntrl nil))

(defalias 'ar-doubleslash-cntrl-atpt 'ar-cntrl-doubleslash-atpt)
(defun ar-cntrl-doubleslash-atpt ()
  "Returns regexp-quoted CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'cntrl nil))

(defalias 'ar-backslash-digit-atpt 'ar-digit-backslash-atpt)
(defun ar-digit-backslash-atpt ()
  "Returns regexp-quoted DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'digit nil))

(defalias 'ar-backtick-digit-atpt 'ar-digit-backtick-atpt)
(defun ar-digit-backtick-atpt ()
  "Returns regexp-quoted DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'digit nil))

(defalias 'ar-colon-digit-atpt 'ar-digit-colon-atpt)
(defun ar-digit-colon-atpt ()
  "Returns regexp-quoted DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'digit nil))

(defalias 'ar-cross-digit-atpt 'ar-digit-cross-atpt)
(defun ar-digit-cross-atpt ()
  "Returns regexp-quoted DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'digit nil))

(defalias 'ar-dollar-digit-atpt 'ar-digit-dollar-atpt)
(defun ar-digit-dollar-atpt ()
  "Returns regexp-quoted DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'digit nil))

(defalias 'ar-doublequote-digit-atpt 'ar-digit-doublequote-atpt)
(defun ar-digit-doublequote-atpt ()
  "Returns regexp-quoted DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'digit nil))

(defalias 'ar-equalize-digit-atpt 'ar-digit-equalize-atpt)
(defun ar-digit-equalize-atpt ()
  "Returns regexp-quoted DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'digit nil))

(defalias 'ar-escape-digit-atpt 'ar-digit-escape-atpt)
(defun ar-digit-escape-atpt ()
  "Returns regexp-quoted DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'digit nil))

(defalias 'ar-hash-digit-atpt 'ar-digit-hash-atpt)
(defun ar-digit-hash-atpt ()
  "Returns regexp-quoted DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'digit nil))

(defalias 'ar-hyphen-digit-atpt 'ar-digit-hyphen-atpt)
(defun ar-digit-hyphen-atpt ()
  "Returns regexp-quoted DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'digit nil))

(defalias 'ar-singlequote-digit-atpt 'ar-digit-singlequote-atpt)
(defun ar-digit-singlequote-atpt ()
  "Returns regexp-quoted DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'digit nil))

(defalias 'ar-slash-digit-atpt 'ar-digit-slash-atpt)
(defun ar-digit-slash-atpt ()
  "Returns regexp-quoted DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'digit nil))

(defalias 'ar-star-digit-atpt 'ar-digit-star-atpt)
(defun ar-digit-star-atpt ()
  "Returns regexp-quoted DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'digit nil))

(defalias 'ar-tild-digit-atpt 'ar-digit-tild-atpt)
(defun ar-digit-tild-atpt ()
  "Returns regexp-quoted DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'digit nil))

(defalias 'ar-underscore-digit-atpt 'ar-digit-underscore-atpt)
(defun ar-digit-underscore-atpt ()
  "Returns regexp-quoted DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'digit nil))

(defalias 'ar-whitespace-digit-atpt 'ar-digit-whitespace-atpt)
(defun ar-digit-whitespace-atpt ()
  "Returns regexp-quoted DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'digit nil))

(defalias 'ar-doubleslash-digit-atpt 'ar-digit-doubleslash-atpt)
(defun ar-digit-doubleslash-atpt ()
  "Returns regexp-quoted DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'digit nil))

(defalias 'ar-backslash-graph-atpt 'ar-graph-backslash-atpt)
(defun ar-graph-backslash-atpt ()
  "Returns regexp-quoted GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'graph nil))

(defalias 'ar-backtick-graph-atpt 'ar-graph-backtick-atpt)
(defun ar-graph-backtick-atpt ()
  "Returns regexp-quoted GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'graph nil))

(defalias 'ar-colon-graph-atpt 'ar-graph-colon-atpt)
(defun ar-graph-colon-atpt ()
  "Returns regexp-quoted GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'graph nil))

(defalias 'ar-cross-graph-atpt 'ar-graph-cross-atpt)
(defun ar-graph-cross-atpt ()
  "Returns regexp-quoted GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'graph nil))

(defalias 'ar-dollar-graph-atpt 'ar-graph-dollar-atpt)
(defun ar-graph-dollar-atpt ()
  "Returns regexp-quoted GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'graph nil))

(defalias 'ar-doublequote-graph-atpt 'ar-graph-doublequote-atpt)
(defun ar-graph-doublequote-atpt ()
  "Returns regexp-quoted GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'graph nil))

(defalias 'ar-equalize-graph-atpt 'ar-graph-equalize-atpt)
(defun ar-graph-equalize-atpt ()
  "Returns regexp-quoted GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'graph nil))

(defalias 'ar-escape-graph-atpt 'ar-graph-escape-atpt)
(defun ar-graph-escape-atpt ()
  "Returns regexp-quoted GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'graph nil))

(defalias 'ar-hash-graph-atpt 'ar-graph-hash-atpt)
(defun ar-graph-hash-atpt ()
  "Returns regexp-quoted GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'graph nil))

(defalias 'ar-hyphen-graph-atpt 'ar-graph-hyphen-atpt)
(defun ar-graph-hyphen-atpt ()
  "Returns regexp-quoted GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'graph nil))

(defalias 'ar-singlequote-graph-atpt 'ar-graph-singlequote-atpt)
(defun ar-graph-singlequote-atpt ()
  "Returns regexp-quoted GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'graph nil))

(defalias 'ar-slash-graph-atpt 'ar-graph-slash-atpt)
(defun ar-graph-slash-atpt ()
  "Returns regexp-quoted GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'graph nil))

(defalias 'ar-star-graph-atpt 'ar-graph-star-atpt)
(defun ar-graph-star-atpt ()
  "Returns regexp-quoted GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'graph nil))

(defalias 'ar-tild-graph-atpt 'ar-graph-tild-atpt)
(defun ar-graph-tild-atpt ()
  "Returns regexp-quoted GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'graph nil))

(defalias 'ar-underscore-graph-atpt 'ar-graph-underscore-atpt)
(defun ar-graph-underscore-atpt ()
  "Returns regexp-quoted GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'graph nil))

(defalias 'ar-whitespace-graph-atpt 'ar-graph-whitespace-atpt)
(defun ar-graph-whitespace-atpt ()
  "Returns regexp-quoted GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'graph nil))

(defalias 'ar-doubleslash-graph-atpt 'ar-graph-doubleslash-atpt)
(defun ar-graph-doubleslash-atpt ()
  "Returns regexp-quoted GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'graph nil))

(defalias 'ar-backslash-lower-atpt 'ar-lower-backslash-atpt)
(defun ar-lower-backslash-atpt ()
  "Returns regexp-quoted LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'lower nil))

(defalias 'ar-backtick-lower-atpt 'ar-lower-backtick-atpt)
(defun ar-lower-backtick-atpt ()
  "Returns regexp-quoted LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'lower nil))

(defalias 'ar-colon-lower-atpt 'ar-lower-colon-atpt)
(defun ar-lower-colon-atpt ()
  "Returns regexp-quoted LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'lower nil))

(defalias 'ar-cross-lower-atpt 'ar-lower-cross-atpt)
(defun ar-lower-cross-atpt ()
  "Returns regexp-quoted LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'lower nil))

(defalias 'ar-dollar-lower-atpt 'ar-lower-dollar-atpt)
(defun ar-lower-dollar-atpt ()
  "Returns regexp-quoted LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'lower nil))

(defalias 'ar-doublequote-lower-atpt 'ar-lower-doublequote-atpt)
(defun ar-lower-doublequote-atpt ()
  "Returns regexp-quoted LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'lower nil))

(defalias 'ar-equalize-lower-atpt 'ar-lower-equalize-atpt)
(defun ar-lower-equalize-atpt ()
  "Returns regexp-quoted LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'lower nil))

(defalias 'ar-escape-lower-atpt 'ar-lower-escape-atpt)
(defun ar-lower-escape-atpt ()
  "Returns regexp-quoted LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'lower nil))

(defalias 'ar-hash-lower-atpt 'ar-lower-hash-atpt)
(defun ar-lower-hash-atpt ()
  "Returns regexp-quoted LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'lower nil))

(defalias 'ar-hyphen-lower-atpt 'ar-lower-hyphen-atpt)
(defun ar-lower-hyphen-atpt ()
  "Returns regexp-quoted LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'lower nil))

(defalias 'ar-singlequote-lower-atpt 'ar-lower-singlequote-atpt)
(defun ar-lower-singlequote-atpt ()
  "Returns regexp-quoted LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'lower nil))

(defalias 'ar-slash-lower-atpt 'ar-lower-slash-atpt)
(defun ar-lower-slash-atpt ()
  "Returns regexp-quoted LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'lower nil))

(defalias 'ar-star-lower-atpt 'ar-lower-star-atpt)
(defun ar-lower-star-atpt ()
  "Returns regexp-quoted LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'lower nil))

(defalias 'ar-tild-lower-atpt 'ar-lower-tild-atpt)
(defun ar-lower-tild-atpt ()
  "Returns regexp-quoted LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'lower nil))

(defalias 'ar-underscore-lower-atpt 'ar-lower-underscore-atpt)
(defun ar-lower-underscore-atpt ()
  "Returns regexp-quoted LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'lower nil))

(defalias 'ar-whitespace-lower-atpt 'ar-lower-whitespace-atpt)
(defun ar-lower-whitespace-atpt ()
  "Returns regexp-quoted LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'lower nil))

(defalias 'ar-doubleslash-lower-atpt 'ar-lower-doubleslash-atpt)
(defun ar-lower-doubleslash-atpt ()
  "Returns regexp-quoted LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'lower nil))

(defalias 'ar-backslash-nonascii-atpt 'ar-nonascii-backslash-atpt)
(defun ar-nonascii-backslash-atpt ()
  "Returns regexp-quoted NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'nonascii nil))

(defalias 'ar-backtick-nonascii-atpt 'ar-nonascii-backtick-atpt)
(defun ar-nonascii-backtick-atpt ()
  "Returns regexp-quoted NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'nonascii nil))

(defalias 'ar-colon-nonascii-atpt 'ar-nonascii-colon-atpt)
(defun ar-nonascii-colon-atpt ()
  "Returns regexp-quoted NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'nonascii nil))

(defalias 'ar-cross-nonascii-atpt 'ar-nonascii-cross-atpt)
(defun ar-nonascii-cross-atpt ()
  "Returns regexp-quoted NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'nonascii nil))

(defalias 'ar-dollar-nonascii-atpt 'ar-nonascii-dollar-atpt)
(defun ar-nonascii-dollar-atpt ()
  "Returns regexp-quoted NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'nonascii nil))

(defalias 'ar-doublequote-nonascii-atpt 'ar-nonascii-doublequote-atpt)
(defun ar-nonascii-doublequote-atpt ()
  "Returns regexp-quoted NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'nonascii nil))

(defalias 'ar-equalize-nonascii-atpt 'ar-nonascii-equalize-atpt)
(defun ar-nonascii-equalize-atpt ()
  "Returns regexp-quoted NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'nonascii nil))

(defalias 'ar-escape-nonascii-atpt 'ar-nonascii-escape-atpt)
(defun ar-nonascii-escape-atpt ()
  "Returns regexp-quoted NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'nonascii nil))

(defalias 'ar-hash-nonascii-atpt 'ar-nonascii-hash-atpt)
(defun ar-nonascii-hash-atpt ()
  "Returns regexp-quoted NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'nonascii nil))

(defalias 'ar-hyphen-nonascii-atpt 'ar-nonascii-hyphen-atpt)
(defun ar-nonascii-hyphen-atpt ()
  "Returns regexp-quoted NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'nonascii nil))

(defalias 'ar-singlequote-nonascii-atpt 'ar-nonascii-singlequote-atpt)
(defun ar-nonascii-singlequote-atpt ()
  "Returns regexp-quoted NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'nonascii nil))

(defalias 'ar-slash-nonascii-atpt 'ar-nonascii-slash-atpt)
(defun ar-nonascii-slash-atpt ()
  "Returns regexp-quoted NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'nonascii nil))

(defalias 'ar-star-nonascii-atpt 'ar-nonascii-star-atpt)
(defun ar-nonascii-star-atpt ()
  "Returns regexp-quoted NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'nonascii nil))

(defalias 'ar-tild-nonascii-atpt 'ar-nonascii-tild-atpt)
(defun ar-nonascii-tild-atpt ()
  "Returns regexp-quoted NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'nonascii nil))

(defalias 'ar-underscore-nonascii-atpt 'ar-nonascii-underscore-atpt)
(defun ar-nonascii-underscore-atpt ()
  "Returns regexp-quoted NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'nonascii nil))

(defalias 'ar-whitespace-nonascii-atpt 'ar-nonascii-whitespace-atpt)
(defun ar-nonascii-whitespace-atpt ()
  "Returns regexp-quoted NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'nonascii nil))

(defalias 'ar-doubleslash-nonascii-atpt 'ar-nonascii-doubleslash-atpt)
(defun ar-nonascii-doubleslash-atpt ()
  "Returns regexp-quoted NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'nonascii nil))

(defalias 'ar-backslash-print-atpt 'ar-print-backslash-atpt)
(defun ar-print-backslash-atpt ()
  "Returns regexp-quoted PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'print nil))

(defalias 'ar-backtick-print-atpt 'ar-print-backtick-atpt)
(defun ar-print-backtick-atpt ()
  "Returns regexp-quoted PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'print nil))

(defalias 'ar-colon-print-atpt 'ar-print-colon-atpt)
(defun ar-print-colon-atpt ()
  "Returns regexp-quoted PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'print nil))

(defalias 'ar-cross-print-atpt 'ar-print-cross-atpt)
(defun ar-print-cross-atpt ()
  "Returns regexp-quoted PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'print nil))

(defalias 'ar-dollar-print-atpt 'ar-print-dollar-atpt)
(defun ar-print-dollar-atpt ()
  "Returns regexp-quoted PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'print nil))

(defalias 'ar-doublequote-print-atpt 'ar-print-doublequote-atpt)
(defun ar-print-doublequote-atpt ()
  "Returns regexp-quoted PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'print nil))

(defalias 'ar-equalize-print-atpt 'ar-print-equalize-atpt)
(defun ar-print-equalize-atpt ()
  "Returns regexp-quoted PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'print nil))

(defalias 'ar-escape-print-atpt 'ar-print-escape-atpt)
(defun ar-print-escape-atpt ()
  "Returns regexp-quoted PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'print nil))

(defalias 'ar-hash-print-atpt 'ar-print-hash-atpt)
(defun ar-print-hash-atpt ()
  "Returns regexp-quoted PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'print nil))

(defalias 'ar-hyphen-print-atpt 'ar-print-hyphen-atpt)
(defun ar-print-hyphen-atpt ()
  "Returns regexp-quoted PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'print nil))

(defalias 'ar-singlequote-print-atpt 'ar-print-singlequote-atpt)
(defun ar-print-singlequote-atpt ()
  "Returns regexp-quoted PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'print nil))

(defalias 'ar-slash-print-atpt 'ar-print-slash-atpt)
(defun ar-print-slash-atpt ()
  "Returns regexp-quoted PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'print nil))

(defalias 'ar-star-print-atpt 'ar-print-star-atpt)
(defun ar-print-star-atpt ()
  "Returns regexp-quoted PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'print nil))

(defalias 'ar-tild-print-atpt 'ar-print-tild-atpt)
(defun ar-print-tild-atpt ()
  "Returns regexp-quoted PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'print nil))

(defalias 'ar-underscore-print-atpt 'ar-print-underscore-atpt)
(defun ar-print-underscore-atpt ()
  "Returns regexp-quoted PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'print nil))

(defalias 'ar-whitespace-print-atpt 'ar-print-whitespace-atpt)
(defun ar-print-whitespace-atpt ()
  "Returns regexp-quoted PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'print nil))

(defalias 'ar-doubleslash-print-atpt 'ar-print-doubleslash-atpt)
(defun ar-print-doubleslash-atpt ()
  "Returns regexp-quoted PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'print nil))

(defalias 'ar-backslash-punct-atpt 'ar-punct-backslash-atpt)
(defun ar-punct-backslash-atpt ()
  "Returns regexp-quoted PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'punct nil))

(defalias 'ar-backtick-punct-atpt 'ar-punct-backtick-atpt)
(defun ar-punct-backtick-atpt ()
  "Returns regexp-quoted PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'punct nil))

(defalias 'ar-colon-punct-atpt 'ar-punct-colon-atpt)
(defun ar-punct-colon-atpt ()
  "Returns regexp-quoted PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'punct nil))

(defalias 'ar-cross-punct-atpt 'ar-punct-cross-atpt)
(defun ar-punct-cross-atpt ()
  "Returns regexp-quoted PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'punct nil))

(defalias 'ar-dollar-punct-atpt 'ar-punct-dollar-atpt)
(defun ar-punct-dollar-atpt ()
  "Returns regexp-quoted PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'punct nil))

(defalias 'ar-doublequote-punct-atpt 'ar-punct-doublequote-atpt)
(defun ar-punct-doublequote-atpt ()
  "Returns regexp-quoted PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'punct nil))

(defalias 'ar-equalize-punct-atpt 'ar-punct-equalize-atpt)
(defun ar-punct-equalize-atpt ()
  "Returns regexp-quoted PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'punct nil))

(defalias 'ar-escape-punct-atpt 'ar-punct-escape-atpt)
(defun ar-punct-escape-atpt ()
  "Returns regexp-quoted PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'punct nil))

(defalias 'ar-hash-punct-atpt 'ar-punct-hash-atpt)
(defun ar-punct-hash-atpt ()
  "Returns regexp-quoted PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'punct nil))

(defalias 'ar-hyphen-punct-atpt 'ar-punct-hyphen-atpt)
(defun ar-punct-hyphen-atpt ()
  "Returns regexp-quoted PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'punct nil))

(defalias 'ar-singlequote-punct-atpt 'ar-punct-singlequote-atpt)
(defun ar-punct-singlequote-atpt ()
  "Returns regexp-quoted PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'punct nil))

(defalias 'ar-slash-punct-atpt 'ar-punct-slash-atpt)
(defun ar-punct-slash-atpt ()
  "Returns regexp-quoted PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'punct nil))

(defalias 'ar-star-punct-atpt 'ar-punct-star-atpt)
(defun ar-punct-star-atpt ()
  "Returns regexp-quoted PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'punct nil))

(defalias 'ar-tild-punct-atpt 'ar-punct-tild-atpt)
(defun ar-punct-tild-atpt ()
  "Returns regexp-quoted PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'punct nil))

(defalias 'ar-underscore-punct-atpt 'ar-punct-underscore-atpt)
(defun ar-punct-underscore-atpt ()
  "Returns regexp-quoted PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'punct nil))

(defalias 'ar-whitespace-punct-atpt 'ar-punct-whitespace-atpt)
(defun ar-punct-whitespace-atpt ()
  "Returns regexp-quoted PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'punct nil))

(defalias 'ar-doubleslash-punct-atpt 'ar-punct-doubleslash-atpt)
(defun ar-punct-doubleslash-atpt ()
  "Returns regexp-quoted PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'punct nil))

(defalias 'ar-backslash-space-atpt 'ar-space-backslash-atpt)
(defun ar-space-backslash-atpt ()
  "Returns regexp-quoted SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'space nil))

(defalias 'ar-backtick-space-atpt 'ar-space-backtick-atpt)
(defun ar-space-backtick-atpt ()
  "Returns regexp-quoted SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'space nil))

(defalias 'ar-colon-space-atpt 'ar-space-colon-atpt)
(defun ar-space-colon-atpt ()
  "Returns regexp-quoted SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'space nil))

(defalias 'ar-cross-space-atpt 'ar-space-cross-atpt)
(defun ar-space-cross-atpt ()
  "Returns regexp-quoted SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'space nil))

(defalias 'ar-dollar-space-atpt 'ar-space-dollar-atpt)
(defun ar-space-dollar-atpt ()
  "Returns regexp-quoted SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'space nil))

(defalias 'ar-doublequote-space-atpt 'ar-space-doublequote-atpt)
(defun ar-space-doublequote-atpt ()
  "Returns regexp-quoted SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'space nil))

(defalias 'ar-equalize-space-atpt 'ar-space-equalize-atpt)
(defun ar-space-equalize-atpt ()
  "Returns regexp-quoted SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'space nil))

(defalias 'ar-escape-space-atpt 'ar-space-escape-atpt)
(defun ar-space-escape-atpt ()
  "Returns regexp-quoted SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'space nil))

(defalias 'ar-hash-space-atpt 'ar-space-hash-atpt)
(defun ar-space-hash-atpt ()
  "Returns regexp-quoted SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'space nil))

(defalias 'ar-hyphen-space-atpt 'ar-space-hyphen-atpt)
(defun ar-space-hyphen-atpt ()
  "Returns regexp-quoted SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'space nil))

(defalias 'ar-singlequote-space-atpt 'ar-space-singlequote-atpt)
(defun ar-space-singlequote-atpt ()
  "Returns regexp-quoted SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'space nil))

(defalias 'ar-slash-space-atpt 'ar-space-slash-atpt)
(defun ar-space-slash-atpt ()
  "Returns regexp-quoted SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'space nil))

(defalias 'ar-star-space-atpt 'ar-space-star-atpt)
(defun ar-space-star-atpt ()
  "Returns regexp-quoted SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'space nil))

(defalias 'ar-tild-space-atpt 'ar-space-tild-atpt)
(defun ar-space-tild-atpt ()
  "Returns regexp-quoted SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'space nil))

(defalias 'ar-underscore-space-atpt 'ar-space-underscore-atpt)
(defun ar-space-underscore-atpt ()
  "Returns regexp-quoted SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'space nil))

(defalias 'ar-whitespace-space-atpt 'ar-space-whitespace-atpt)
(defun ar-space-whitespace-atpt ()
  "Returns regexp-quoted SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'space nil))

(defalias 'ar-doubleslash-space-atpt 'ar-space-doubleslash-atpt)
(defun ar-space-doubleslash-atpt ()
  "Returns regexp-quoted SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'space nil))

(defalias 'ar-backslash-upper-atpt 'ar-upper-backslash-atpt)
(defun ar-upper-backslash-atpt ()
  "Returns regexp-quoted UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'upper nil))

(defalias 'ar-backtick-upper-atpt 'ar-upper-backtick-atpt)
(defun ar-upper-backtick-atpt ()
  "Returns regexp-quoted UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'upper nil))

(defalias 'ar-colon-upper-atpt 'ar-upper-colon-atpt)
(defun ar-upper-colon-atpt ()
  "Returns regexp-quoted UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'upper nil))

(defalias 'ar-cross-upper-atpt 'ar-upper-cross-atpt)
(defun ar-upper-cross-atpt ()
  "Returns regexp-quoted UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'upper nil))

(defalias 'ar-dollar-upper-atpt 'ar-upper-dollar-atpt)
(defun ar-upper-dollar-atpt ()
  "Returns regexp-quoted UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'upper nil))

(defalias 'ar-doublequote-upper-atpt 'ar-upper-doublequote-atpt)
(defun ar-upper-doublequote-atpt ()
  "Returns regexp-quoted UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'upper nil))

(defalias 'ar-equalize-upper-atpt 'ar-upper-equalize-atpt)
(defun ar-upper-equalize-atpt ()
  "Returns regexp-quoted UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'upper nil))

(defalias 'ar-escape-upper-atpt 'ar-upper-escape-atpt)
(defun ar-upper-escape-atpt ()
  "Returns regexp-quoted UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'upper nil))

(defalias 'ar-hash-upper-atpt 'ar-upper-hash-atpt)
(defun ar-upper-hash-atpt ()
  "Returns regexp-quoted UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'upper nil))

(defalias 'ar-hyphen-upper-atpt 'ar-upper-hyphen-atpt)
(defun ar-upper-hyphen-atpt ()
  "Returns regexp-quoted UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'upper nil))

(defalias 'ar-singlequote-upper-atpt 'ar-upper-singlequote-atpt)
(defun ar-upper-singlequote-atpt ()
  "Returns regexp-quoted UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'upper nil))

(defalias 'ar-slash-upper-atpt 'ar-upper-slash-atpt)
(defun ar-upper-slash-atpt ()
  "Returns regexp-quoted UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'upper nil))

(defalias 'ar-star-upper-atpt 'ar-upper-star-atpt)
(defun ar-upper-star-atpt ()
  "Returns regexp-quoted UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'upper nil))

(defalias 'ar-tild-upper-atpt 'ar-upper-tild-atpt)
(defun ar-upper-tild-atpt ()
  "Returns regexp-quoted UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'upper nil))

(defalias 'ar-underscore-upper-atpt 'ar-upper-underscore-atpt)
(defun ar-upper-underscore-atpt ()
  "Returns regexp-quoted UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'upper nil))

(defalias 'ar-whitespace-upper-atpt 'ar-upper-whitespace-atpt)
(defun ar-upper-whitespace-atpt ()
  "Returns regexp-quoted UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'upper nil))

(defalias 'ar-doubleslash-upper-atpt 'ar-upper-doubleslash-atpt)
(defun ar-upper-doubleslash-atpt ()
  "Returns regexp-quoted UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'upper nil))

(defalias 'ar-backslash-xdigit-atpt 'ar-xdigit-backslash-atpt)
(defun ar-xdigit-backslash-atpt ()
  "Returns regexp-quoted XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'xdigit nil))

(defalias 'ar-backtick-xdigit-atpt 'ar-xdigit-backtick-atpt)
(defun ar-xdigit-backtick-atpt ()
  "Returns regexp-quoted XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'xdigit nil))

(defalias 'ar-colon-xdigit-atpt 'ar-xdigit-colon-atpt)
(defun ar-xdigit-colon-atpt ()
  "Returns regexp-quoted XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'xdigit nil))

(defalias 'ar-cross-xdigit-atpt 'ar-xdigit-cross-atpt)
(defun ar-xdigit-cross-atpt ()
  "Returns regexp-quoted XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'xdigit nil))

(defalias 'ar-dollar-xdigit-atpt 'ar-xdigit-dollar-atpt)
(defun ar-xdigit-dollar-atpt ()
  "Returns regexp-quoted XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'xdigit nil))

(defalias 'ar-doublequote-xdigit-atpt 'ar-xdigit-doublequote-atpt)
(defun ar-xdigit-doublequote-atpt ()
  "Returns regexp-quoted XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'xdigit nil))

(defalias 'ar-equalize-xdigit-atpt 'ar-xdigit-equalize-atpt)
(defun ar-xdigit-equalize-atpt ()
  "Returns regexp-quoted XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'xdigit nil))

(defalias 'ar-escape-xdigit-atpt 'ar-xdigit-escape-atpt)
(defun ar-xdigit-escape-atpt ()
  "Returns regexp-quoted XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'xdigit nil))

(defalias 'ar-hash-xdigit-atpt 'ar-xdigit-hash-atpt)
(defun ar-xdigit-hash-atpt ()
  "Returns regexp-quoted XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'xdigit nil))

(defalias 'ar-hyphen-xdigit-atpt 'ar-xdigit-hyphen-atpt)
(defun ar-xdigit-hyphen-atpt ()
  "Returns regexp-quoted XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'xdigit nil))

(defalias 'ar-singlequote-xdigit-atpt 'ar-xdigit-singlequote-atpt)
(defun ar-xdigit-singlequote-atpt ()
  "Returns regexp-quoted XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'xdigit nil))

(defalias 'ar-slash-xdigit-atpt 'ar-xdigit-slash-atpt)
(defun ar-xdigit-slash-atpt ()
  "Returns regexp-quoted XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'xdigit nil))

(defalias 'ar-star-xdigit-atpt 'ar-xdigit-star-atpt)
(defun ar-xdigit-star-atpt ()
  "Returns regexp-quoted XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'xdigit nil))

(defalias 'ar-tild-xdigit-atpt 'ar-xdigit-tild-atpt)
(defun ar-xdigit-tild-atpt ()
  "Returns regexp-quoted XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'xdigit nil))

(defalias 'ar-underscore-xdigit-atpt 'ar-xdigit-underscore-atpt)
(defun ar-xdigit-underscore-atpt ()
  "Returns regexp-quoted XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'xdigit nil))

(defalias 'ar-whitespace-xdigit-atpt 'ar-xdigit-whitespace-atpt)
(defun ar-xdigit-whitespace-atpt ()
  "Returns regexp-quoted XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'xdigit nil))

(defalias 'ar-doubleslash-xdigit-atpt 'ar-xdigit-doubleslash-atpt)
(defun ar-xdigit-doubleslash-atpt ()
  "Returns regexp-quoted XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'xdigit nil));; ar-thing-at-point-utils-unpaired-delim-aktiv ar-unpaired-delimit-aktiv ar-atpt-rest-list: end

;; ar-thing-at-point-utils-unpaired-paired: start


(defalias 'ar-backslash-braced-atpt 'ar-braced-backslash-atpt)
(defun ar-braced-backslash-atpt ()
  "Returns regexp-quoted BRACED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'braced nil))

(defalias 'ar-backtick-braced-atpt 'ar-braced-backtick-atpt)
(defun ar-braced-backtick-atpt ()
  "Returns regexp-quoted BRACED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'braced nil))

(defalias 'ar-colon-braced-atpt 'ar-braced-colon-atpt)
(defun ar-braced-colon-atpt ()
  "Returns regexp-quoted BRACED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'braced nil))

(defalias 'ar-cross-braced-atpt 'ar-braced-cross-atpt)
(defun ar-braced-cross-atpt ()
  "Returns regexp-quoted BRACED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'braced nil))

(defalias 'ar-dollar-braced-atpt 'ar-braced-dollar-atpt)
(defun ar-braced-dollar-atpt ()
  "Returns regexp-quoted BRACED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'braced nil))

(defalias 'ar-doublequote-braced-atpt 'ar-braced-doublequote-atpt)
(defun ar-braced-doublequote-atpt ()
  "Returns regexp-quoted BRACED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'braced nil))

(defalias 'ar-equalize-braced-atpt 'ar-braced-equalize-atpt)
(defun ar-braced-equalize-atpt ()
  "Returns regexp-quoted BRACED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'braced nil))

(defalias 'ar-escape-braced-atpt 'ar-braced-escape-atpt)
(defun ar-braced-escape-atpt ()
  "Returns regexp-quoted BRACED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'braced nil))

(defalias 'ar-hash-braced-atpt 'ar-braced-hash-atpt)
(defun ar-braced-hash-atpt ()
  "Returns regexp-quoted BRACED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'braced nil))

(defalias 'ar-hyphen-braced-atpt 'ar-braced-hyphen-atpt)
(defun ar-braced-hyphen-atpt ()
  "Returns regexp-quoted BRACED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'braced nil))

(defalias 'ar-singlequote-braced-atpt 'ar-braced-singlequote-atpt)
(defun ar-braced-singlequote-atpt ()
  "Returns regexp-quoted BRACED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'braced nil))

(defalias 'ar-slash-braced-atpt 'ar-braced-slash-atpt)
(defun ar-braced-slash-atpt ()
  "Returns regexp-quoted BRACED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'braced nil))

(defalias 'ar-star-braced-atpt 'ar-braced-star-atpt)
(defun ar-braced-star-atpt ()
  "Returns regexp-quoted BRACED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'braced nil))

(defalias 'ar-tild-braced-atpt 'ar-braced-tild-atpt)
(defun ar-braced-tild-atpt ()
  "Returns regexp-quoted BRACED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'braced nil))

(defalias 'ar-underscore-braced-atpt 'ar-braced-underscore-atpt)
(defun ar-braced-underscore-atpt ()
  "Returns regexp-quoted BRACED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'braced nil))

(defalias 'ar-whitespace-braced-atpt 'ar-braced-whitespace-atpt)
(defun ar-braced-whitespace-atpt ()
  "Returns regexp-quoted BRACED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'braced nil))

(defalias 'ar-doubleslash-braced-atpt 'ar-braced-doubleslash-atpt)
(defun ar-braced-doubleslash-atpt ()
  "Returns regexp-quoted BRACED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'braced nil))

(defalias 'ar-backslash-bracketed-atpt 'ar-bracketed-backslash-atpt)
(defun ar-bracketed-backslash-atpt ()
  "Returns regexp-quoted BRACKETED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'bracketed nil))

(defalias 'ar-backtick-bracketed-atpt 'ar-bracketed-backtick-atpt)
(defun ar-bracketed-backtick-atpt ()
  "Returns regexp-quoted BRACKETED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'bracketed nil))

(defalias 'ar-colon-bracketed-atpt 'ar-bracketed-colon-atpt)
(defun ar-bracketed-colon-atpt ()
  "Returns regexp-quoted BRACKETED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'bracketed nil))

(defalias 'ar-cross-bracketed-atpt 'ar-bracketed-cross-atpt)
(defun ar-bracketed-cross-atpt ()
  "Returns regexp-quoted BRACKETED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'bracketed nil))

(defalias 'ar-dollar-bracketed-atpt 'ar-bracketed-dollar-atpt)
(defun ar-bracketed-dollar-atpt ()
  "Returns regexp-quoted BRACKETED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'bracketed nil))

(defalias 'ar-doublequote-bracketed-atpt 'ar-bracketed-doublequote-atpt)
(defun ar-bracketed-doublequote-atpt ()
  "Returns regexp-quoted BRACKETED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'bracketed nil))

(defalias 'ar-equalize-bracketed-atpt 'ar-bracketed-equalize-atpt)
(defun ar-bracketed-equalize-atpt ()
  "Returns regexp-quoted BRACKETED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'bracketed nil))

(defalias 'ar-escape-bracketed-atpt 'ar-bracketed-escape-atpt)
(defun ar-bracketed-escape-atpt ()
  "Returns regexp-quoted BRACKETED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'bracketed nil))

(defalias 'ar-hash-bracketed-atpt 'ar-bracketed-hash-atpt)
(defun ar-bracketed-hash-atpt ()
  "Returns regexp-quoted BRACKETED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'bracketed nil))

(defalias 'ar-hyphen-bracketed-atpt 'ar-bracketed-hyphen-atpt)
(defun ar-bracketed-hyphen-atpt ()
  "Returns regexp-quoted BRACKETED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'bracketed nil))

(defalias 'ar-singlequote-bracketed-atpt 'ar-bracketed-singlequote-atpt)
(defun ar-bracketed-singlequote-atpt ()
  "Returns regexp-quoted BRACKETED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'bracketed nil))

(defalias 'ar-slash-bracketed-atpt 'ar-bracketed-slash-atpt)
(defun ar-bracketed-slash-atpt ()
  "Returns regexp-quoted BRACKETED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'bracketed nil))

(defalias 'ar-star-bracketed-atpt 'ar-bracketed-star-atpt)
(defun ar-bracketed-star-atpt ()
  "Returns regexp-quoted BRACKETED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'bracketed nil))

(defalias 'ar-tild-bracketed-atpt 'ar-bracketed-tild-atpt)
(defun ar-bracketed-tild-atpt ()
  "Returns regexp-quoted BRACKETED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'bracketed nil))

(defalias 'ar-underscore-bracketed-atpt 'ar-bracketed-underscore-atpt)
(defun ar-bracketed-underscore-atpt ()
  "Returns regexp-quoted BRACKETED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'bracketed nil))

(defalias 'ar-whitespace-bracketed-atpt 'ar-bracketed-whitespace-atpt)
(defun ar-bracketed-whitespace-atpt ()
  "Returns regexp-quoted BRACKETED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'bracketed nil))

(defalias 'ar-doubleslash-bracketed-atpt 'ar-bracketed-doubleslash-atpt)
(defun ar-bracketed-doubleslash-atpt ()
  "Returns regexp-quoted BRACKETED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'bracketed nil))

(defalias 'ar-backslash-lesserangled-atpt 'ar-lesserangled-backslash-atpt)
(defun ar-lesserangled-backslash-atpt ()
  "Returns regexp-quoted LESSERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'lesserangled nil))

(defalias 'ar-backtick-lesserangled-atpt 'ar-lesserangled-backtick-atpt)
(defun ar-lesserangled-backtick-atpt ()
  "Returns regexp-quoted LESSERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'lesserangled nil))

(defalias 'ar-colon-lesserangled-atpt 'ar-lesserangled-colon-atpt)
(defun ar-lesserangled-colon-atpt ()
  "Returns regexp-quoted LESSERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'lesserangled nil))

(defalias 'ar-cross-lesserangled-atpt 'ar-lesserangled-cross-atpt)
(defun ar-lesserangled-cross-atpt ()
  "Returns regexp-quoted LESSERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'lesserangled nil))

(defalias 'ar-dollar-lesserangled-atpt 'ar-lesserangled-dollar-atpt)
(defun ar-lesserangled-dollar-atpt ()
  "Returns regexp-quoted LESSERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'lesserangled nil))

(defalias 'ar-doublequote-lesserangled-atpt 'ar-lesserangled-doublequote-atpt)
(defun ar-lesserangled-doublequote-atpt ()
  "Returns regexp-quoted LESSERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'lesserangled nil))

(defalias 'ar-equalize-lesserangled-atpt 'ar-lesserangled-equalize-atpt)
(defun ar-lesserangled-equalize-atpt ()
  "Returns regexp-quoted LESSERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'lesserangled nil))

(defalias 'ar-escape-lesserangled-atpt 'ar-lesserangled-escape-atpt)
(defun ar-lesserangled-escape-atpt ()
  "Returns regexp-quoted LESSERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'lesserangled nil))

(defalias 'ar-hash-lesserangled-atpt 'ar-lesserangled-hash-atpt)
(defun ar-lesserangled-hash-atpt ()
  "Returns regexp-quoted LESSERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'lesserangled nil))

(defalias 'ar-hyphen-lesserangled-atpt 'ar-lesserangled-hyphen-atpt)
(defun ar-lesserangled-hyphen-atpt ()
  "Returns regexp-quoted LESSERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'lesserangled nil))

(defalias 'ar-singlequote-lesserangled-atpt 'ar-lesserangled-singlequote-atpt)
(defun ar-lesserangled-singlequote-atpt ()
  "Returns regexp-quoted LESSERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'lesserangled nil))

(defalias 'ar-slash-lesserangled-atpt 'ar-lesserangled-slash-atpt)
(defun ar-lesserangled-slash-atpt ()
  "Returns regexp-quoted LESSERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'lesserangled nil))

(defalias 'ar-star-lesserangled-atpt 'ar-lesserangled-star-atpt)
(defun ar-lesserangled-star-atpt ()
  "Returns regexp-quoted LESSERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'lesserangled nil))

(defalias 'ar-tild-lesserangled-atpt 'ar-lesserangled-tild-atpt)
(defun ar-lesserangled-tild-atpt ()
  "Returns regexp-quoted LESSERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'lesserangled nil))

(defalias 'ar-underscore-lesserangled-atpt 'ar-lesserangled-underscore-atpt)
(defun ar-lesserangled-underscore-atpt ()
  "Returns regexp-quoted LESSERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'lesserangled nil))

(defalias 'ar-whitespace-lesserangled-atpt 'ar-lesserangled-whitespace-atpt)
(defun ar-lesserangled-whitespace-atpt ()
  "Returns regexp-quoted LESSERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'lesserangled nil))

(defalias 'ar-doubleslash-lesserangled-atpt 'ar-lesserangled-doubleslash-atpt)
(defun ar-lesserangled-doubleslash-atpt ()
  "Returns regexp-quoted LESSERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'lesserangled nil))

(defalias 'ar-backslash-greaterangled-atpt 'ar-greaterangled-backslash-atpt)
(defun ar-greaterangled-backslash-atpt ()
  "Returns regexp-quoted GREATERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'greaterangled nil))

(defalias 'ar-backtick-greaterangled-atpt 'ar-greaterangled-backtick-atpt)
(defun ar-greaterangled-backtick-atpt ()
  "Returns regexp-quoted GREATERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'greaterangled nil))

(defalias 'ar-colon-greaterangled-atpt 'ar-greaterangled-colon-atpt)
(defun ar-greaterangled-colon-atpt ()
  "Returns regexp-quoted GREATERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'greaterangled nil))

(defalias 'ar-cross-greaterangled-atpt 'ar-greaterangled-cross-atpt)
(defun ar-greaterangled-cross-atpt ()
  "Returns regexp-quoted GREATERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'greaterangled nil))

(defalias 'ar-dollar-greaterangled-atpt 'ar-greaterangled-dollar-atpt)
(defun ar-greaterangled-dollar-atpt ()
  "Returns regexp-quoted GREATERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'greaterangled nil))

(defalias 'ar-doublequote-greaterangled-atpt 'ar-greaterangled-doublequote-atpt)
(defun ar-greaterangled-doublequote-atpt ()
  "Returns regexp-quoted GREATERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'greaterangled nil))

(defalias 'ar-equalize-greaterangled-atpt 'ar-greaterangled-equalize-atpt)
(defun ar-greaterangled-equalize-atpt ()
  "Returns regexp-quoted GREATERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'greaterangled nil))

(defalias 'ar-escape-greaterangled-atpt 'ar-greaterangled-escape-atpt)
(defun ar-greaterangled-escape-atpt ()
  "Returns regexp-quoted GREATERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'greaterangled nil))

(defalias 'ar-hash-greaterangled-atpt 'ar-greaterangled-hash-atpt)
(defun ar-greaterangled-hash-atpt ()
  "Returns regexp-quoted GREATERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'greaterangled nil))

(defalias 'ar-hyphen-greaterangled-atpt 'ar-greaterangled-hyphen-atpt)
(defun ar-greaterangled-hyphen-atpt ()
  "Returns regexp-quoted GREATERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'greaterangled nil))

(defalias 'ar-singlequote-greaterangled-atpt 'ar-greaterangled-singlequote-atpt)
(defun ar-greaterangled-singlequote-atpt ()
  "Returns regexp-quoted GREATERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'greaterangled nil))

(defalias 'ar-slash-greaterangled-atpt 'ar-greaterangled-slash-atpt)
(defun ar-greaterangled-slash-atpt ()
  "Returns regexp-quoted GREATERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'greaterangled nil))

(defalias 'ar-star-greaterangled-atpt 'ar-greaterangled-star-atpt)
(defun ar-greaterangled-star-atpt ()
  "Returns regexp-quoted GREATERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'greaterangled nil))

(defalias 'ar-tild-greaterangled-atpt 'ar-greaterangled-tild-atpt)
(defun ar-greaterangled-tild-atpt ()
  "Returns regexp-quoted GREATERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'greaterangled nil))

(defalias 'ar-underscore-greaterangled-atpt 'ar-greaterangled-underscore-atpt)
(defun ar-greaterangled-underscore-atpt ()
  "Returns regexp-quoted GREATERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'greaterangled nil))

(defalias 'ar-whitespace-greaterangled-atpt 'ar-greaterangled-whitespace-atpt)
(defun ar-greaterangled-whitespace-atpt ()
  "Returns regexp-quoted GREATERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'greaterangled nil))

(defalias 'ar-doubleslash-greaterangled-atpt 'ar-greaterangled-doubleslash-atpt)
(defun ar-greaterangled-doubleslash-atpt ()
  "Returns regexp-quoted GREATERANGLED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'greaterangled nil))

(defalias 'ar-backslash-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-backslash-atpt)
(defun ar-leftrightsinglequoted-backslash-atpt ()
  "Returns regexp-quoted LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'leftrightsinglequoted nil))

(defalias 'ar-backtick-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-backtick-atpt)
(defun ar-leftrightsinglequoted-backtick-atpt ()
  "Returns regexp-quoted LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'leftrightsinglequoted nil))

(defalias 'ar-colon-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-colon-atpt)
(defun ar-leftrightsinglequoted-colon-atpt ()
  "Returns regexp-quoted LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'leftrightsinglequoted nil))

(defalias 'ar-cross-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-cross-atpt)
(defun ar-leftrightsinglequoted-cross-atpt ()
  "Returns regexp-quoted LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'leftrightsinglequoted nil))

(defalias 'ar-dollar-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-dollar-atpt)
(defun ar-leftrightsinglequoted-dollar-atpt ()
  "Returns regexp-quoted LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'leftrightsinglequoted nil))

(defalias 'ar-doublequote-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-doublequote-atpt)
(defun ar-leftrightsinglequoted-doublequote-atpt ()
  "Returns regexp-quoted LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'leftrightsinglequoted nil))

(defalias 'ar-equalize-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-equalize-atpt)
(defun ar-leftrightsinglequoted-equalize-atpt ()
  "Returns regexp-quoted LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'leftrightsinglequoted nil))

(defalias 'ar-escape-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-escape-atpt)
(defun ar-leftrightsinglequoted-escape-atpt ()
  "Returns regexp-quoted LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'leftrightsinglequoted nil))

(defalias 'ar-hash-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-hash-atpt)
(defun ar-leftrightsinglequoted-hash-atpt ()
  "Returns regexp-quoted LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'leftrightsinglequoted nil))

(defalias 'ar-hyphen-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-hyphen-atpt)
(defun ar-leftrightsinglequoted-hyphen-atpt ()
  "Returns regexp-quoted LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'leftrightsinglequoted nil))

(defalias 'ar-singlequote-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-singlequote-atpt)
(defun ar-leftrightsinglequoted-singlequote-atpt ()
  "Returns regexp-quoted LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'leftrightsinglequoted nil))

(defalias 'ar-slash-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-slash-atpt)
(defun ar-leftrightsinglequoted-slash-atpt ()
  "Returns regexp-quoted LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'leftrightsinglequoted nil))

(defalias 'ar-star-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-star-atpt)
(defun ar-leftrightsinglequoted-star-atpt ()
  "Returns regexp-quoted LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'leftrightsinglequoted nil))

(defalias 'ar-tild-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-tild-atpt)
(defun ar-leftrightsinglequoted-tild-atpt ()
  "Returns regexp-quoted LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'leftrightsinglequoted nil))

(defalias 'ar-underscore-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-underscore-atpt)
(defun ar-leftrightsinglequoted-underscore-atpt ()
  "Returns regexp-quoted LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'leftrightsinglequoted nil))

(defalias 'ar-whitespace-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-whitespace-atpt)
(defun ar-leftrightsinglequoted-whitespace-atpt ()
  "Returns regexp-quoted LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'leftrightsinglequoted nil))

(defalias 'ar-doubleslash-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-doubleslash-atpt)
(defun ar-leftrightsinglequoted-doubleslash-atpt ()
  "Returns regexp-quoted LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'leftrightsinglequoted nil))

(defalias 'ar-backslash-leftrightdoublequoted-atpt 'ar-leftrightdoublequoted-backslash-atpt)
(defun ar-leftrightdoublequoted-backslash-atpt ()
  "Returns regexp-quoted LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'leftrightdoublequoted nil))

(defalias 'ar-backtick-leftrightdoublequoted-atpt 'ar-leftrightdoublequoted-backtick-atpt)
(defun ar-leftrightdoublequoted-backtick-atpt ()
  "Returns regexp-quoted LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'leftrightdoublequoted nil))

(defalias 'ar-colon-leftrightdoublequoted-atpt 'ar-leftrightdoublequoted-colon-atpt)
(defun ar-leftrightdoublequoted-colon-atpt ()
  "Returns regexp-quoted LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'leftrightdoublequoted nil))

(defalias 'ar-cross-leftrightdoublequoted-atpt 'ar-leftrightdoublequoted-cross-atpt)
(defun ar-leftrightdoublequoted-cross-atpt ()
  "Returns regexp-quoted LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'leftrightdoublequoted nil))

(defalias 'ar-dollar-leftrightdoublequoted-atpt 'ar-leftrightdoublequoted-dollar-atpt)
(defun ar-leftrightdoublequoted-dollar-atpt ()
  "Returns regexp-quoted LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'leftrightdoublequoted nil))

(defalias 'ar-doublequote-leftrightdoublequoted-atpt 'ar-leftrightdoublequoted-doublequote-atpt)
(defun ar-leftrightdoublequoted-doublequote-atpt ()
  "Returns regexp-quoted LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'leftrightdoublequoted nil))

(defalias 'ar-equalize-leftrightdoublequoted-atpt 'ar-leftrightdoublequoted-equalize-atpt)
(defun ar-leftrightdoublequoted-equalize-atpt ()
  "Returns regexp-quoted LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'leftrightdoublequoted nil))

(defalias 'ar-escape-leftrightdoublequoted-atpt 'ar-leftrightdoublequoted-escape-atpt)
(defun ar-leftrightdoublequoted-escape-atpt ()
  "Returns regexp-quoted LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'leftrightdoublequoted nil))

(defalias 'ar-hash-leftrightdoublequoted-atpt 'ar-leftrightdoublequoted-hash-atpt)
(defun ar-leftrightdoublequoted-hash-atpt ()
  "Returns regexp-quoted LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'leftrightdoublequoted nil))

(defalias 'ar-hyphen-leftrightdoublequoted-atpt 'ar-leftrightdoublequoted-hyphen-atpt)
(defun ar-leftrightdoublequoted-hyphen-atpt ()
  "Returns regexp-quoted LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'leftrightdoublequoted nil))

(defalias 'ar-singlequote-leftrightdoublequoted-atpt 'ar-leftrightdoublequoted-singlequote-atpt)
(defun ar-leftrightdoublequoted-singlequote-atpt ()
  "Returns regexp-quoted LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'leftrightdoublequoted nil))

(defalias 'ar-slash-leftrightdoublequoted-atpt 'ar-leftrightdoublequoted-slash-atpt)
(defun ar-leftrightdoublequoted-slash-atpt ()
  "Returns regexp-quoted LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'leftrightdoublequoted nil))

(defalias 'ar-star-leftrightdoublequoted-atpt 'ar-leftrightdoublequoted-star-atpt)
(defun ar-leftrightdoublequoted-star-atpt ()
  "Returns regexp-quoted LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'leftrightdoublequoted nil))

(defalias 'ar-tild-leftrightdoublequoted-atpt 'ar-leftrightdoublequoted-tild-atpt)
(defun ar-leftrightdoublequoted-tild-atpt ()
  "Returns regexp-quoted LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'leftrightdoublequoted nil))

(defalias 'ar-underscore-leftrightdoublequoted-atpt 'ar-leftrightdoublequoted-underscore-atpt)
(defun ar-leftrightdoublequoted-underscore-atpt ()
  "Returns regexp-quoted LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'leftrightdoublequoted nil))

(defalias 'ar-whitespace-leftrightdoublequoted-atpt 'ar-leftrightdoublequoted-whitespace-atpt)
(defun ar-leftrightdoublequoted-whitespace-atpt ()
  "Returns regexp-quoted LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'leftrightdoublequoted nil))

(defalias 'ar-doubleslash-leftrightdoublequoted-atpt 'ar-leftrightdoublequoted-doubleslash-atpt)
(defun ar-leftrightdoublequoted-doubleslash-atpt ()
  "Returns regexp-quoted LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'leftrightdoublequoted nil))

(defalias 'ar-backslash-parentized-atpt 'ar-parentized-backslash-atpt)
(defun ar-parentized-backslash-atpt ()
  "Returns regexp-quoted PARENTIZED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'parentized nil))

(defalias 'ar-backtick-parentized-atpt 'ar-parentized-backtick-atpt)
(defun ar-parentized-backtick-atpt ()
  "Returns regexp-quoted PARENTIZED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'parentized nil))

(defalias 'ar-colon-parentized-atpt 'ar-parentized-colon-atpt)
(defun ar-parentized-colon-atpt ()
  "Returns regexp-quoted PARENTIZED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'parentized nil))

(defalias 'ar-cross-parentized-atpt 'ar-parentized-cross-atpt)
(defun ar-parentized-cross-atpt ()
  "Returns regexp-quoted PARENTIZED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'parentized nil))

(defalias 'ar-dollar-parentized-atpt 'ar-parentized-dollar-atpt)
(defun ar-parentized-dollar-atpt ()
  "Returns regexp-quoted PARENTIZED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'parentized nil))

(defalias 'ar-doublequote-parentized-atpt 'ar-parentized-doublequote-atpt)
(defun ar-parentized-doublequote-atpt ()
  "Returns regexp-quoted PARENTIZED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'parentized nil))

(defalias 'ar-equalize-parentized-atpt 'ar-parentized-equalize-atpt)
(defun ar-parentized-equalize-atpt ()
  "Returns regexp-quoted PARENTIZED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'parentized nil))

(defalias 'ar-escape-parentized-atpt 'ar-parentized-escape-atpt)
(defun ar-parentized-escape-atpt ()
  "Returns regexp-quoted PARENTIZED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'parentized nil))

(defalias 'ar-hash-parentized-atpt 'ar-parentized-hash-atpt)
(defun ar-parentized-hash-atpt ()
  "Returns regexp-quoted PARENTIZED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'parentized nil))

(defalias 'ar-hyphen-parentized-atpt 'ar-parentized-hyphen-atpt)
(defun ar-parentized-hyphen-atpt ()
  "Returns regexp-quoted PARENTIZED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'parentized nil))

(defalias 'ar-singlequote-parentized-atpt 'ar-parentized-singlequote-atpt)
(defun ar-parentized-singlequote-atpt ()
  "Returns regexp-quoted PARENTIZED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'parentized nil))

(defalias 'ar-slash-parentized-atpt 'ar-parentized-slash-atpt)
(defun ar-parentized-slash-atpt ()
  "Returns regexp-quoted PARENTIZED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'parentized nil))

(defalias 'ar-star-parentized-atpt 'ar-parentized-star-atpt)
(defun ar-parentized-star-atpt ()
  "Returns regexp-quoted PARENTIZED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'parentized nil))

(defalias 'ar-tild-parentized-atpt 'ar-parentized-tild-atpt)
(defun ar-parentized-tild-atpt ()
  "Returns regexp-quoted PARENTIZED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'parentized nil))

(defalias 'ar-underscore-parentized-atpt 'ar-parentized-underscore-atpt)
(defun ar-parentized-underscore-atpt ()
  "Returns regexp-quoted PARENTIZED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'parentized nil))

(defalias 'ar-whitespace-parentized-atpt 'ar-parentized-whitespace-atpt)
(defun ar-parentized-whitespace-atpt ()
  "Returns regexp-quoted PARENTIZED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'parentized nil))

(defalias 'ar-doubleslash-parentized-atpt 'ar-parentized-doubleslash-atpt)
(defun ar-parentized-doubleslash-atpt ()
  "Returns regexp-quoted PARENTIZED at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'parentized nil));; ar-thing-at-point-utils-unpaired-paired: end

;; ar-thing-at-point-utils-delim-classes-paired ar-atpt-classes ar-paired-delimit-aktiv: start


(defalias 'ar-brace-alnum-atpt 'ar-alnum-brace-atpt)
(defun ar-alnum-brace-atpt ()
  "Brace ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'alnum nil))

(defalias 'ar-bracket-alnum-atpt 'ar-alnum-bracket-atpt)
(defun ar-alnum-bracket-atpt ()
  "Bracket ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'alnum nil))

(defalias 'ar-lesserangle-alnum-atpt 'ar-alnum-lesserangle-atpt)
(defun ar-alnum-lesserangle-atpt ()
  "Lesserangle ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'alnum nil))

(defalias 'ar-greaterangle-alnum-atpt 'ar-alnum-greaterangle-atpt)
(defun ar-alnum-greaterangle-atpt ()
  "Greaterangle ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'alnum nil))

(defalias 'ar-leftrightsinglequote-alnum-atpt 'ar-alnum-leftrightsinglequote-atpt)
(defun ar-alnum-leftrightsinglequote-atpt ()
  "Leftrightsinglequote ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'alnum nil))

(defalias 'ar-leftrightdoublequote-alnum-atpt 'ar-alnum-leftrightdoublequote-atpt)
(defun ar-alnum-leftrightdoublequote-atpt ()
  "Leftrightdoublequote ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightdoublequote 'alnum nil))

(defalias 'ar-parentize-alnum-atpt 'ar-alnum-parentize-atpt)
(defun ar-alnum-parentize-atpt ()
  "Parentize ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'alnum nil))

(defalias 'ar-brace-alpha-atpt 'ar-alpha-brace-atpt)
(defun ar-alpha-brace-atpt ()
  "Brace ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'alpha nil))

(defalias 'ar-bracket-alpha-atpt 'ar-alpha-bracket-atpt)
(defun ar-alpha-bracket-atpt ()
  "Bracket ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'alpha nil))

(defalias 'ar-lesserangle-alpha-atpt 'ar-alpha-lesserangle-atpt)
(defun ar-alpha-lesserangle-atpt ()
  "Lesserangle ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'alpha nil))

(defalias 'ar-greaterangle-alpha-atpt 'ar-alpha-greaterangle-atpt)
(defun ar-alpha-greaterangle-atpt ()
  "Greaterangle ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'alpha nil))

(defalias 'ar-leftrightsinglequote-alpha-atpt 'ar-alpha-leftrightsinglequote-atpt)
(defun ar-alpha-leftrightsinglequote-atpt ()
  "Leftrightsinglequote ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'alpha nil))

(defalias 'ar-leftrightdoublequote-alpha-atpt 'ar-alpha-leftrightdoublequote-atpt)
(defun ar-alpha-leftrightdoublequote-atpt ()
  "Leftrightdoublequote ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightdoublequote 'alpha nil))

(defalias 'ar-parentize-alpha-atpt 'ar-alpha-parentize-atpt)
(defun ar-alpha-parentize-atpt ()
  "Parentize ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'alpha nil))

(defalias 'ar-brace-ascii-atpt 'ar-ascii-brace-atpt)
(defun ar-ascii-brace-atpt ()
  "Brace ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'ascii nil))

(defalias 'ar-bracket-ascii-atpt 'ar-ascii-bracket-atpt)
(defun ar-ascii-bracket-atpt ()
  "Bracket ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'ascii nil))

(defalias 'ar-lesserangle-ascii-atpt 'ar-ascii-lesserangle-atpt)
(defun ar-ascii-lesserangle-atpt ()
  "Lesserangle ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'ascii nil))

(defalias 'ar-greaterangle-ascii-atpt 'ar-ascii-greaterangle-atpt)
(defun ar-ascii-greaterangle-atpt ()
  "Greaterangle ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'ascii nil))

(defalias 'ar-leftrightsinglequote-ascii-atpt 'ar-ascii-leftrightsinglequote-atpt)
(defun ar-ascii-leftrightsinglequote-atpt ()
  "Leftrightsinglequote ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'ascii nil))

(defalias 'ar-leftrightdoublequote-ascii-atpt 'ar-ascii-leftrightdoublequote-atpt)
(defun ar-ascii-leftrightdoublequote-atpt ()
  "Leftrightdoublequote ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightdoublequote 'ascii nil))

(defalias 'ar-parentize-ascii-atpt 'ar-ascii-parentize-atpt)
(defun ar-ascii-parentize-atpt ()
  "Parentize ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'ascii nil))

(defalias 'ar-brace-blank-atpt 'ar-blank-brace-atpt)
(defun ar-blank-brace-atpt ()
  "Brace BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'blank nil))

(defalias 'ar-bracket-blank-atpt 'ar-blank-bracket-atpt)
(defun ar-blank-bracket-atpt ()
  "Bracket BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'blank nil))

(defalias 'ar-lesserangle-blank-atpt 'ar-blank-lesserangle-atpt)
(defun ar-blank-lesserangle-atpt ()
  "Lesserangle BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'blank nil))

(defalias 'ar-greaterangle-blank-atpt 'ar-blank-greaterangle-atpt)
(defun ar-blank-greaterangle-atpt ()
  "Greaterangle BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'blank nil))

(defalias 'ar-leftrightsinglequote-blank-atpt 'ar-blank-leftrightsinglequote-atpt)
(defun ar-blank-leftrightsinglequote-atpt ()
  "Leftrightsinglequote BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'blank nil))

(defalias 'ar-leftrightdoublequote-blank-atpt 'ar-blank-leftrightdoublequote-atpt)
(defun ar-blank-leftrightdoublequote-atpt ()
  "Leftrightdoublequote BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightdoublequote 'blank nil))

(defalias 'ar-parentize-blank-atpt 'ar-blank-parentize-atpt)
(defun ar-blank-parentize-atpt ()
  "Parentize BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'blank nil))

(defalias 'ar-brace-cntrl-atpt 'ar-cntrl-brace-atpt)
(defun ar-cntrl-brace-atpt ()
  "Brace CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'cntrl nil))

(defalias 'ar-bracket-cntrl-atpt 'ar-cntrl-bracket-atpt)
(defun ar-cntrl-bracket-atpt ()
  "Bracket CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'cntrl nil))

(defalias 'ar-lesserangle-cntrl-atpt 'ar-cntrl-lesserangle-atpt)
(defun ar-cntrl-lesserangle-atpt ()
  "Lesserangle CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'cntrl nil))

(defalias 'ar-greaterangle-cntrl-atpt 'ar-cntrl-greaterangle-atpt)
(defun ar-cntrl-greaterangle-atpt ()
  "Greaterangle CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'cntrl nil))

(defalias 'ar-leftrightsinglequote-cntrl-atpt 'ar-cntrl-leftrightsinglequote-atpt)
(defun ar-cntrl-leftrightsinglequote-atpt ()
  "Leftrightsinglequote CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'cntrl nil))

(defalias 'ar-leftrightdoublequote-cntrl-atpt 'ar-cntrl-leftrightdoublequote-atpt)
(defun ar-cntrl-leftrightdoublequote-atpt ()
  "Leftrightdoublequote CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightdoublequote 'cntrl nil))

(defalias 'ar-parentize-cntrl-atpt 'ar-cntrl-parentize-atpt)
(defun ar-cntrl-parentize-atpt ()
  "Parentize CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'cntrl nil))

(defalias 'ar-brace-digit-atpt 'ar-digit-brace-atpt)
(defun ar-digit-brace-atpt ()
  "Brace DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'digit nil))

(defalias 'ar-bracket-digit-atpt 'ar-digit-bracket-atpt)
(defun ar-digit-bracket-atpt ()
  "Bracket DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'digit nil))

(defalias 'ar-lesserangle-digit-atpt 'ar-digit-lesserangle-atpt)
(defun ar-digit-lesserangle-atpt ()
  "Lesserangle DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'digit nil))

(defalias 'ar-greaterangle-digit-atpt 'ar-digit-greaterangle-atpt)
(defun ar-digit-greaterangle-atpt ()
  "Greaterangle DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'digit nil))

(defalias 'ar-leftrightsinglequote-digit-atpt 'ar-digit-leftrightsinglequote-atpt)
(defun ar-digit-leftrightsinglequote-atpt ()
  "Leftrightsinglequote DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'digit nil))

(defalias 'ar-leftrightdoublequote-digit-atpt 'ar-digit-leftrightdoublequote-atpt)
(defun ar-digit-leftrightdoublequote-atpt ()
  "Leftrightdoublequote DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightdoublequote 'digit nil))

(defalias 'ar-parentize-digit-atpt 'ar-digit-parentize-atpt)
(defun ar-digit-parentize-atpt ()
  "Parentize DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'digit nil))

(defalias 'ar-brace-graph-atpt 'ar-graph-brace-atpt)
(defun ar-graph-brace-atpt ()
  "Brace GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'graph nil))

(defalias 'ar-bracket-graph-atpt 'ar-graph-bracket-atpt)
(defun ar-graph-bracket-atpt ()
  "Bracket GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'graph nil))

(defalias 'ar-lesserangle-graph-atpt 'ar-graph-lesserangle-atpt)
(defun ar-graph-lesserangle-atpt ()
  "Lesserangle GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'graph nil))

(defalias 'ar-greaterangle-graph-atpt 'ar-graph-greaterangle-atpt)
(defun ar-graph-greaterangle-atpt ()
  "Greaterangle GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'graph nil))

(defalias 'ar-leftrightsinglequote-graph-atpt 'ar-graph-leftrightsinglequote-atpt)
(defun ar-graph-leftrightsinglequote-atpt ()
  "Leftrightsinglequote GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'graph nil))

(defalias 'ar-leftrightdoublequote-graph-atpt 'ar-graph-leftrightdoublequote-atpt)
(defun ar-graph-leftrightdoublequote-atpt ()
  "Leftrightdoublequote GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightdoublequote 'graph nil))

(defalias 'ar-parentize-graph-atpt 'ar-graph-parentize-atpt)
(defun ar-graph-parentize-atpt ()
  "Parentize GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'graph nil))

(defalias 'ar-brace-lower-atpt 'ar-lower-brace-atpt)
(defun ar-lower-brace-atpt ()
  "Brace LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'lower nil))

(defalias 'ar-bracket-lower-atpt 'ar-lower-bracket-atpt)
(defun ar-lower-bracket-atpt ()
  "Bracket LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'lower nil))

(defalias 'ar-lesserangle-lower-atpt 'ar-lower-lesserangle-atpt)
(defun ar-lower-lesserangle-atpt ()
  "Lesserangle LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'lower nil))

(defalias 'ar-greaterangle-lower-atpt 'ar-lower-greaterangle-atpt)
(defun ar-lower-greaterangle-atpt ()
  "Greaterangle LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'lower nil))

(defalias 'ar-leftrightsinglequote-lower-atpt 'ar-lower-leftrightsinglequote-atpt)
(defun ar-lower-leftrightsinglequote-atpt ()
  "Leftrightsinglequote LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'lower nil))

(defalias 'ar-leftrightdoublequote-lower-atpt 'ar-lower-leftrightdoublequote-atpt)
(defun ar-lower-leftrightdoublequote-atpt ()
  "Leftrightdoublequote LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightdoublequote 'lower nil))

(defalias 'ar-parentize-lower-atpt 'ar-lower-parentize-atpt)
(defun ar-lower-parentize-atpt ()
  "Parentize LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'lower nil))

(defalias 'ar-brace-nonascii-atpt 'ar-nonascii-brace-atpt)
(defun ar-nonascii-brace-atpt ()
  "Brace NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'nonascii nil))

(defalias 'ar-bracket-nonascii-atpt 'ar-nonascii-bracket-atpt)
(defun ar-nonascii-bracket-atpt ()
  "Bracket NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'nonascii nil))

(defalias 'ar-lesserangle-nonascii-atpt 'ar-nonascii-lesserangle-atpt)
(defun ar-nonascii-lesserangle-atpt ()
  "Lesserangle NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'nonascii nil))

(defalias 'ar-greaterangle-nonascii-atpt 'ar-nonascii-greaterangle-atpt)
(defun ar-nonascii-greaterangle-atpt ()
  "Greaterangle NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'nonascii nil))

(defalias 'ar-leftrightsinglequote-nonascii-atpt 'ar-nonascii-leftrightsinglequote-atpt)
(defun ar-nonascii-leftrightsinglequote-atpt ()
  "Leftrightsinglequote NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'nonascii nil))

(defalias 'ar-leftrightdoublequote-nonascii-atpt 'ar-nonascii-leftrightdoublequote-atpt)
(defun ar-nonascii-leftrightdoublequote-atpt ()
  "Leftrightdoublequote NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightdoublequote 'nonascii nil))

(defalias 'ar-parentize-nonascii-atpt 'ar-nonascii-parentize-atpt)
(defun ar-nonascii-parentize-atpt ()
  "Parentize NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'nonascii nil))

(defalias 'ar-brace-print-atpt 'ar-print-brace-atpt)
(defun ar-print-brace-atpt ()
  "Brace PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'print nil))

(defalias 'ar-bracket-print-atpt 'ar-print-bracket-atpt)
(defun ar-print-bracket-atpt ()
  "Bracket PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'print nil))

(defalias 'ar-lesserangle-print-atpt 'ar-print-lesserangle-atpt)
(defun ar-print-lesserangle-atpt ()
  "Lesserangle PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'print nil))

(defalias 'ar-greaterangle-print-atpt 'ar-print-greaterangle-atpt)
(defun ar-print-greaterangle-atpt ()
  "Greaterangle PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'print nil))

(defalias 'ar-leftrightsinglequote-print-atpt 'ar-print-leftrightsinglequote-atpt)
(defun ar-print-leftrightsinglequote-atpt ()
  "Leftrightsinglequote PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'print nil))

(defalias 'ar-leftrightdoublequote-print-atpt 'ar-print-leftrightdoublequote-atpt)
(defun ar-print-leftrightdoublequote-atpt ()
  "Leftrightdoublequote PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightdoublequote 'print nil))

(defalias 'ar-parentize-print-atpt 'ar-print-parentize-atpt)
(defun ar-print-parentize-atpt ()
  "Parentize PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'print nil))

(defalias 'ar-brace-punct-atpt 'ar-punct-brace-atpt)
(defun ar-punct-brace-atpt ()
  "Brace PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'punct nil))

(defalias 'ar-bracket-punct-atpt 'ar-punct-bracket-atpt)
(defun ar-punct-bracket-atpt ()
  "Bracket PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'punct nil))

(defalias 'ar-lesserangle-punct-atpt 'ar-punct-lesserangle-atpt)
(defun ar-punct-lesserangle-atpt ()
  "Lesserangle PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'punct nil))

(defalias 'ar-greaterangle-punct-atpt 'ar-punct-greaterangle-atpt)
(defun ar-punct-greaterangle-atpt ()
  "Greaterangle PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'punct nil))

(defalias 'ar-leftrightsinglequote-punct-atpt 'ar-punct-leftrightsinglequote-atpt)
(defun ar-punct-leftrightsinglequote-atpt ()
  "Leftrightsinglequote PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'punct nil))

(defalias 'ar-leftrightdoublequote-punct-atpt 'ar-punct-leftrightdoublequote-atpt)
(defun ar-punct-leftrightdoublequote-atpt ()
  "Leftrightdoublequote PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightdoublequote 'punct nil))

(defalias 'ar-parentize-punct-atpt 'ar-punct-parentize-atpt)
(defun ar-punct-parentize-atpt ()
  "Parentize PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'punct nil))

(defalias 'ar-brace-space-atpt 'ar-space-brace-atpt)
(defun ar-space-brace-atpt ()
  "Brace SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'space nil))

(defalias 'ar-bracket-space-atpt 'ar-space-bracket-atpt)
(defun ar-space-bracket-atpt ()
  "Bracket SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'space nil))

(defalias 'ar-lesserangle-space-atpt 'ar-space-lesserangle-atpt)
(defun ar-space-lesserangle-atpt ()
  "Lesserangle SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'space nil))

(defalias 'ar-greaterangle-space-atpt 'ar-space-greaterangle-atpt)
(defun ar-space-greaterangle-atpt ()
  "Greaterangle SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'space nil))

(defalias 'ar-leftrightsinglequote-space-atpt 'ar-space-leftrightsinglequote-atpt)
(defun ar-space-leftrightsinglequote-atpt ()
  "Leftrightsinglequote SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'space nil))

(defalias 'ar-leftrightdoublequote-space-atpt 'ar-space-leftrightdoublequote-atpt)
(defun ar-space-leftrightdoublequote-atpt ()
  "Leftrightdoublequote SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightdoublequote 'space nil))

(defalias 'ar-parentize-space-atpt 'ar-space-parentize-atpt)
(defun ar-space-parentize-atpt ()
  "Parentize SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'space nil))

(defalias 'ar-brace-upper-atpt 'ar-upper-brace-atpt)
(defun ar-upper-brace-atpt ()
  "Brace UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'upper nil))

(defalias 'ar-bracket-upper-atpt 'ar-upper-bracket-atpt)
(defun ar-upper-bracket-atpt ()
  "Bracket UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'upper nil))

(defalias 'ar-lesserangle-upper-atpt 'ar-upper-lesserangle-atpt)
(defun ar-upper-lesserangle-atpt ()
  "Lesserangle UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'upper nil))

(defalias 'ar-greaterangle-upper-atpt 'ar-upper-greaterangle-atpt)
(defun ar-upper-greaterangle-atpt ()
  "Greaterangle UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'upper nil))

(defalias 'ar-leftrightsinglequote-upper-atpt 'ar-upper-leftrightsinglequote-atpt)
(defun ar-upper-leftrightsinglequote-atpt ()
  "Leftrightsinglequote UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'upper nil))

(defalias 'ar-leftrightdoublequote-upper-atpt 'ar-upper-leftrightdoublequote-atpt)
(defun ar-upper-leftrightdoublequote-atpt ()
  "Leftrightdoublequote UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightdoublequote 'upper nil))

(defalias 'ar-parentize-upper-atpt 'ar-upper-parentize-atpt)
(defun ar-upper-parentize-atpt ()
  "Parentize UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'upper nil))

(defalias 'ar-brace-xdigit-atpt 'ar-xdigit-brace-atpt)
(defun ar-xdigit-brace-atpt ()
  "Brace XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'xdigit nil))

(defalias 'ar-bracket-xdigit-atpt 'ar-xdigit-bracket-atpt)
(defun ar-xdigit-bracket-atpt ()
  "Bracket XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'xdigit nil))

(defalias 'ar-lesserangle-xdigit-atpt 'ar-xdigit-lesserangle-atpt)
(defun ar-xdigit-lesserangle-atpt ()
  "Lesserangle XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'xdigit nil))

(defalias 'ar-greaterangle-xdigit-atpt 'ar-xdigit-greaterangle-atpt)
(defun ar-xdigit-greaterangle-atpt ()
  "Greaterangle XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'xdigit nil))

(defalias 'ar-leftrightsinglequote-xdigit-atpt 'ar-xdigit-leftrightsinglequote-atpt)
(defun ar-xdigit-leftrightsinglequote-atpt ()
  "Leftrightsinglequote XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'xdigit nil))

(defalias 'ar-leftrightdoublequote-xdigit-atpt 'ar-xdigit-leftrightdoublequote-atpt)
(defun ar-xdigit-leftrightdoublequote-atpt ()
  "Leftrightdoublequote XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightdoublequote 'xdigit nil))

(defalias 'ar-parentize-xdigit-atpt 'ar-xdigit-parentize-atpt)
(defun ar-xdigit-parentize-atpt ()
  "Parentize XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'xdigit nil));; ar-thing-at-point-utils-delim-classes-paired ar-atpt-classes ar-paired-delimit-aktiv: end

;; ar-thing-at-point-utils-nodelim-einzeln: start

(defun ar-blok-alnum-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around alnum.
  Returns blok or nil if no ALNUM at cursor-position. "
  (interactive "*")
  (ar-th-blok 'alnum nil (called-interactively-p 'any)))

(defun ar-comment-alnum-atpt ()
  "Comments ALNUM at point if any. "
  (interactive "*")
  (ar-th-comment 'alnum nil (called-interactively-p 'any)))

(defun ar-commatize-alnum-atpt ()
  "Put a comma after ALNUM at point if any. "
  (interactive "*")
  (ar-th-commatize 'alnum nil (called-interactively-p 'any)))

(defun ar-mark-alnum-atpt ()
  "Marks ALNUM at point if any. "
  (interactive)
  (ar-th-mark 'alnum))

(defun ar-hide-alnum-atpt ()
  "Hides ALNUM at point. "
  (interactive)
  (ar-th-hide 'alnum))

(defun ar-show-alnum-atpt ()
  "Shows hidden ALNUM at point. "
  (interactive)
  (ar-th-show 'alnum))

(defun ar-hide-show-alnum-atpt ()
  "Alternatively hides or shows ALNUM at point. "
  (interactive)
  (ar-th-hide-show 'alnum))

(defun ar-highlight-alnum-atpt-mode ()
  "Toggles alnum-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'alnum nil (called-interactively-p 'any)))

(defun ar-kill-alnum-atpt ()
  "Kills ALNUM at point if any. "
  (interactive "*")
  (ar-th-kill 'alnum nil (called-interactively-p 'any)))

(defun ar-separate-alnum-atpt ()
  "Separates ALNUM at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'alnum nil (called-interactively-p 'any)))

(defun ar-triplequotedq-alnum-atpt ()
  "Put triplequotes composed of doublequotes around alnum. "
  (interactive "*")
  (ar-th-triplequotedq 'alnum nil (called-interactively-p 'any)))

(defun ar-triplequotesq-alnum-atpt ()
  "Put triplequotes composed of singlequotes around alnum. "
  (interactive "*")
  (ar-th-triplequotesq 'alnum nil (called-interactively-p 'any)))

(defun ar-forward-alnum-atpt (&optional arg)
  "Moves forward over ALNUM at point if any, does nothing otherwise.
Returns end position of ALNUM "
  (interactive "p")
  (ar-th-forward 'alnum arg (called-interactively-p 'any)))

(defun ar-backward-alnum-atpt (&optional arg)
  "Moves backward over ALNUM before point if any, does nothing otherwise.
Returns beginning position of ALNUM "
  (interactive "p")
  (ar-th-backward 'alnum arg (called-interactively-p 'any)))

(defun ar-transpose-alnum-atpt (&optional arg)
  "Transposes ALNUM with ALNUM before point if any. "
  (interactive "*P")
  (ar-th-transpose 'alnum arg (called-interactively-p 'any)))

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

(defun ar-check-alnum-atpt ()
  "Return t if a ALNUM at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-alnum-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-alnum-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-blok-alpha-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around alpha.
  Returns blok or nil if no ALPHA at cursor-position. "
  (interactive "*")
  (ar-th-blok 'alpha nil (called-interactively-p 'any)))

(defun ar-comment-alpha-atpt ()
  "Comments ALPHA at point if any. "
  (interactive "*")
  (ar-th-comment 'alpha nil (called-interactively-p 'any)))

(defun ar-commatize-alpha-atpt ()
  "Put a comma after ALPHA at point if any. "
  (interactive "*")
  (ar-th-commatize 'alpha nil (called-interactively-p 'any)))

(defun ar-mark-alpha-atpt ()
  "Marks ALPHA at point if any. "
  (interactive)
  (ar-th-mark 'alpha))

(defun ar-hide-alpha-atpt ()
  "Hides ALPHA at point. "
  (interactive)
  (ar-th-hide 'alpha))

(defun ar-show-alpha-atpt ()
  "Shows hidden ALPHA at point. "
  (interactive)
  (ar-th-show 'alpha))

(defun ar-hide-show-alpha-atpt ()
  "Alternatively hides or shows ALPHA at point. "
  (interactive)
  (ar-th-hide-show 'alpha))

(defun ar-highlight-alpha-atpt-mode ()
  "Toggles alpha-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'alpha nil (called-interactively-p 'any)))

(defun ar-kill-alpha-atpt ()
  "Kills ALPHA at point if any. "
  (interactive "*")
  (ar-th-kill 'alpha nil (called-interactively-p 'any)))

(defun ar-separate-alpha-atpt ()
  "Separates ALPHA at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'alpha nil (called-interactively-p 'any)))

(defun ar-triplequotedq-alpha-atpt ()
  "Put triplequotes composed of doublequotes around alpha. "
  (interactive "*")
  (ar-th-triplequotedq 'alpha nil (called-interactively-p 'any)))

(defun ar-triplequotesq-alpha-atpt ()
  "Put triplequotes composed of singlequotes around alpha. "
  (interactive "*")
  (ar-th-triplequotesq 'alpha nil (called-interactively-p 'any)))

(defun ar-forward-alpha-atpt (&optional arg)
  "Moves forward over ALPHA at point if any, does nothing otherwise.
Returns end position of ALPHA "
  (interactive "p")
  (ar-th-forward 'alpha arg (called-interactively-p 'any)))

(defun ar-backward-alpha-atpt (&optional arg)
  "Moves backward over ALPHA before point if any, does nothing otherwise.
Returns beginning position of ALPHA "
  (interactive "p")
  (ar-th-backward 'alpha arg (called-interactively-p 'any)))

(defun ar-transpose-alpha-atpt (&optional arg)
  "Transposes ALPHA with ALPHA before point if any. "
  (interactive "*P")
  (ar-th-transpose 'alpha arg (called-interactively-p 'any)))

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

(defun ar-check-alpha-atpt ()
  "Return t if a ALPHA at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-alpha-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-alpha-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-blok-ascii-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around ascii.
  Returns blok or nil if no ASCII at cursor-position. "
  (interactive "*")
  (ar-th-blok 'ascii nil (called-interactively-p 'any)))

(defun ar-comment-ascii-atpt ()
  "Comments ASCII at point if any. "
  (interactive "*")
  (ar-th-comment 'ascii nil (called-interactively-p 'any)))

(defun ar-commatize-ascii-atpt ()
  "Put a comma after ASCII at point if any. "
  (interactive "*")
  (ar-th-commatize 'ascii nil (called-interactively-p 'any)))

(defun ar-mark-ascii-atpt ()
  "Marks ASCII at point if any. "
  (interactive)
  (ar-th-mark 'ascii))

(defun ar-hide-ascii-atpt ()
  "Hides ASCII at point. "
  (interactive)
  (ar-th-hide 'ascii))

(defun ar-show-ascii-atpt ()
  "Shows hidden ASCII at point. "
  (interactive)
  (ar-th-show 'ascii))

(defun ar-hide-show-ascii-atpt ()
  "Alternatively hides or shows ASCII at point. "
  (interactive)
  (ar-th-hide-show 'ascii))

(defun ar-highlight-ascii-atpt-mode ()
  "Toggles ascii-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'ascii nil (called-interactively-p 'any)))

(defun ar-kill-ascii-atpt ()
  "Kills ASCII at point if any. "
  (interactive "*")
  (ar-th-kill 'ascii nil (called-interactively-p 'any)))

(defun ar-separate-ascii-atpt ()
  "Separates ASCII at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'ascii nil (called-interactively-p 'any)))

(defun ar-triplequotedq-ascii-atpt ()
  "Put triplequotes composed of doublequotes around ascii. "
  (interactive "*")
  (ar-th-triplequotedq 'ascii nil (called-interactively-p 'any)))

(defun ar-triplequotesq-ascii-atpt ()
  "Put triplequotes composed of singlequotes around ascii. "
  (interactive "*")
  (ar-th-triplequotesq 'ascii nil (called-interactively-p 'any)))

(defun ar-forward-ascii-atpt (&optional arg)
  "Moves forward over ASCII at point if any, does nothing otherwise.
Returns end position of ASCII "
  (interactive "p")
  (ar-th-forward 'ascii arg (called-interactively-p 'any)))

(defun ar-backward-ascii-atpt (&optional arg)
  "Moves backward over ASCII before point if any, does nothing otherwise.
Returns beginning position of ASCII "
  (interactive "p")
  (ar-th-backward 'ascii arg (called-interactively-p 'any)))

(defun ar-transpose-ascii-atpt (&optional arg)
  "Transposes ASCII with ASCII before point if any. "
  (interactive "*P")
  (ar-th-transpose 'ascii arg (called-interactively-p 'any)))

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

(defun ar-check-ascii-atpt ()
  "Return t if a ASCII at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-ascii-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-ascii-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-blok-blank-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around blank.
  Returns blok or nil if no BLANK at cursor-position. "
  (interactive "*")
  (ar-th-blok 'blank nil (called-interactively-p 'any)))

(defun ar-comment-blank-atpt ()
  "Comments BLANK at point if any. "
  (interactive "*")
  (ar-th-comment 'blank nil (called-interactively-p 'any)))

(defun ar-commatize-blank-atpt ()
  "Put a comma after BLANK at point if any. "
  (interactive "*")
  (ar-th-commatize 'blank nil (called-interactively-p 'any)))

(defun ar-mark-blank-atpt ()
  "Marks BLANK at point if any. "
  (interactive)
  (ar-th-mark 'blank))

(defun ar-hide-blank-atpt ()
  "Hides BLANK at point. "
  (interactive)
  (ar-th-hide 'blank))

(defun ar-show-blank-atpt ()
  "Shows hidden BLANK at point. "
  (interactive)
  (ar-th-show 'blank))

(defun ar-hide-show-blank-atpt ()
  "Alternatively hides or shows BLANK at point. "
  (interactive)
  (ar-th-hide-show 'blank))

(defun ar-highlight-blank-atpt-mode ()
  "Toggles blank-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'blank nil (called-interactively-p 'any)))

(defun ar-kill-blank-atpt ()
  "Kills BLANK at point if any. "
  (interactive "*")
  (ar-th-kill 'blank nil (called-interactively-p 'any)))

(defun ar-separate-blank-atpt ()
  "Separates BLANK at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'blank nil (called-interactively-p 'any)))

(defun ar-triplequotedq-blank-atpt ()
  "Put triplequotes composed of doublequotes around blank. "
  (interactive "*")
  (ar-th-triplequotedq 'blank nil (called-interactively-p 'any)))

(defun ar-triplequotesq-blank-atpt ()
  "Put triplequotes composed of singlequotes around blank. "
  (interactive "*")
  (ar-th-triplequotesq 'blank nil (called-interactively-p 'any)))

(defun ar-forward-blank-atpt (&optional arg)
  "Moves forward over BLANK at point if any, does nothing otherwise.
Returns end position of BLANK "
  (interactive "p")
  (ar-th-forward 'blank arg (called-interactively-p 'any)))

(defun ar-backward-blank-atpt (&optional arg)
  "Moves backward over BLANK before point if any, does nothing otherwise.
Returns beginning position of BLANK "
  (interactive "p")
  (ar-th-backward 'blank arg (called-interactively-p 'any)))

(defun ar-transpose-blank-atpt (&optional arg)
  "Transposes BLANK with BLANK before point if any. "
  (interactive "*P")
  (ar-th-transpose 'blank arg (called-interactively-p 'any)))

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

(defun ar-check-blank-atpt ()
  "Return t if a BLANK at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-blank-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-blank-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-blok-cntrl-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around cntrl.
  Returns blok or nil if no CNTRL at cursor-position. "
  (interactive "*")
  (ar-th-blok 'cntrl nil (called-interactively-p 'any)))

(defun ar-comment-cntrl-atpt ()
  "Comments CNTRL at point if any. "
  (interactive "*")
  (ar-th-comment 'cntrl nil (called-interactively-p 'any)))

(defun ar-commatize-cntrl-atpt ()
  "Put a comma after CNTRL at point if any. "
  (interactive "*")
  (ar-th-commatize 'cntrl nil (called-interactively-p 'any)))

(defun ar-mark-cntrl-atpt ()
  "Marks CNTRL at point if any. "
  (interactive)
  (ar-th-mark 'cntrl))

(defun ar-hide-cntrl-atpt ()
  "Hides CNTRL at point. "
  (interactive)
  (ar-th-hide 'cntrl))

(defun ar-show-cntrl-atpt ()
  "Shows hidden CNTRL at point. "
  (interactive)
  (ar-th-show 'cntrl))

(defun ar-hide-show-cntrl-atpt ()
  "Alternatively hides or shows CNTRL at point. "
  (interactive)
  (ar-th-hide-show 'cntrl))

(defun ar-highlight-cntrl-atpt-mode ()
  "Toggles cntrl-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'cntrl nil (called-interactively-p 'any)))

(defun ar-kill-cntrl-atpt ()
  "Kills CNTRL at point if any. "
  (interactive "*")
  (ar-th-kill 'cntrl nil (called-interactively-p 'any)))

(defun ar-separate-cntrl-atpt ()
  "Separates CNTRL at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'cntrl nil (called-interactively-p 'any)))

(defun ar-triplequotedq-cntrl-atpt ()
  "Put triplequotes composed of doublequotes around cntrl. "
  (interactive "*")
  (ar-th-triplequotedq 'cntrl nil (called-interactively-p 'any)))

(defun ar-triplequotesq-cntrl-atpt ()
  "Put triplequotes composed of singlequotes around cntrl. "
  (interactive "*")
  (ar-th-triplequotesq 'cntrl nil (called-interactively-p 'any)))

(defun ar-forward-cntrl-atpt (&optional arg)
  "Moves forward over CNTRL at point if any, does nothing otherwise.
Returns end position of CNTRL "
  (interactive "p")
  (ar-th-forward 'cntrl arg (called-interactively-p 'any)))

(defun ar-backward-cntrl-atpt (&optional arg)
  "Moves backward over CNTRL before point if any, does nothing otherwise.
Returns beginning position of CNTRL "
  (interactive "p")
  (ar-th-backward 'cntrl arg (called-interactively-p 'any)))

(defun ar-transpose-cntrl-atpt (&optional arg)
  "Transposes CNTRL with CNTRL before point if any. "
  (interactive "*P")
  (ar-th-transpose 'cntrl arg (called-interactively-p 'any)))

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

(defun ar-check-cntrl-atpt ()
  "Return t if a CNTRL at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-cntrl-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-cntrl-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-blok-digit-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around digit.
  Returns blok or nil if no DIGIT at cursor-position. "
  (interactive "*")
  (ar-th-blok 'digit nil (called-interactively-p 'any)))

(defun ar-comment-digit-atpt ()
  "Comments DIGIT at point if any. "
  (interactive "*")
  (ar-th-comment 'digit nil (called-interactively-p 'any)))

(defun ar-commatize-digit-atpt ()
  "Put a comma after DIGIT at point if any. "
  (interactive "*")
  (ar-th-commatize 'digit nil (called-interactively-p 'any)))

(defun ar-mark-digit-atpt ()
  "Marks DIGIT at point if any. "
  (interactive)
  (ar-th-mark 'digit))

(defun ar-hide-digit-atpt ()
  "Hides DIGIT at point. "
  (interactive)
  (ar-th-hide 'digit))

(defun ar-show-digit-atpt ()
  "Shows hidden DIGIT at point. "
  (interactive)
  (ar-th-show 'digit))

(defun ar-hide-show-digit-atpt ()
  "Alternatively hides or shows DIGIT at point. "
  (interactive)
  (ar-th-hide-show 'digit))

(defun ar-highlight-digit-atpt-mode ()
  "Toggles digit-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'digit nil (called-interactively-p 'any)))

(defun ar-kill-digit-atpt ()
  "Kills DIGIT at point if any. "
  (interactive "*")
  (ar-th-kill 'digit nil (called-interactively-p 'any)))

(defun ar-separate-digit-atpt ()
  "Separates DIGIT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'digit nil (called-interactively-p 'any)))

(defun ar-triplequotedq-digit-atpt ()
  "Put triplequotes composed of doublequotes around digit. "
  (interactive "*")
  (ar-th-triplequotedq 'digit nil (called-interactively-p 'any)))

(defun ar-triplequotesq-digit-atpt ()
  "Put triplequotes composed of singlequotes around digit. "
  (interactive "*")
  (ar-th-triplequotesq 'digit nil (called-interactively-p 'any)))

(defun ar-forward-digit-atpt (&optional arg)
  "Moves forward over DIGIT at point if any, does nothing otherwise.
Returns end position of DIGIT "
  (interactive "p")
  (ar-th-forward 'digit arg (called-interactively-p 'any)))

(defun ar-backward-digit-atpt (&optional arg)
  "Moves backward over DIGIT before point if any, does nothing otherwise.
Returns beginning position of DIGIT "
  (interactive "p")
  (ar-th-backward 'digit arg (called-interactively-p 'any)))

(defun ar-transpose-digit-atpt (&optional arg)
  "Transposes DIGIT with DIGIT before point if any. "
  (interactive "*P")
  (ar-th-transpose 'digit arg (called-interactively-p 'any)))

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

(defun ar-check-digit-atpt ()
  "Return t if a DIGIT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-digit-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-digit-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-blok-graph-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around graph.
  Returns blok or nil if no GRAPH at cursor-position. "
  (interactive "*")
  (ar-th-blok 'graph nil (called-interactively-p 'any)))

(defun ar-comment-graph-atpt ()
  "Comments GRAPH at point if any. "
  (interactive "*")
  (ar-th-comment 'graph nil (called-interactively-p 'any)))

(defun ar-commatize-graph-atpt ()
  "Put a comma after GRAPH at point if any. "
  (interactive "*")
  (ar-th-commatize 'graph nil (called-interactively-p 'any)))

(defun ar-mark-graph-atpt ()
  "Marks GRAPH at point if any. "
  (interactive)
  (ar-th-mark 'graph))

(defun ar-hide-graph-atpt ()
  "Hides GRAPH at point. "
  (interactive)
  (ar-th-hide 'graph))

(defun ar-show-graph-atpt ()
  "Shows hidden GRAPH at point. "
  (interactive)
  (ar-th-show 'graph))

(defun ar-hide-show-graph-atpt ()
  "Alternatively hides or shows GRAPH at point. "
  (interactive)
  (ar-th-hide-show 'graph))

(defun ar-highlight-graph-atpt-mode ()
  "Toggles graph-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'graph nil (called-interactively-p 'any)))

(defun ar-kill-graph-atpt ()
  "Kills GRAPH at point if any. "
  (interactive "*")
  (ar-th-kill 'graph nil (called-interactively-p 'any)))

(defun ar-separate-graph-atpt ()
  "Separates GRAPH at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'graph nil (called-interactively-p 'any)))

(defun ar-triplequotedq-graph-atpt ()
  "Put triplequotes composed of doublequotes around graph. "
  (interactive "*")
  (ar-th-triplequotedq 'graph nil (called-interactively-p 'any)))

(defun ar-triplequotesq-graph-atpt ()
  "Put triplequotes composed of singlequotes around graph. "
  (interactive "*")
  (ar-th-triplequotesq 'graph nil (called-interactively-p 'any)))

(defun ar-forward-graph-atpt (&optional arg)
  "Moves forward over GRAPH at point if any, does nothing otherwise.
Returns end position of GRAPH "
  (interactive "p")
  (ar-th-forward 'graph arg (called-interactively-p 'any)))

(defun ar-backward-graph-atpt (&optional arg)
  "Moves backward over GRAPH before point if any, does nothing otherwise.
Returns beginning position of GRAPH "
  (interactive "p")
  (ar-th-backward 'graph arg (called-interactively-p 'any)))

(defun ar-transpose-graph-atpt (&optional arg)
  "Transposes GRAPH with GRAPH before point if any. "
  (interactive "*P")
  (ar-th-transpose 'graph arg (called-interactively-p 'any)))

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

(defun ar-check-graph-atpt ()
  "Return t if a GRAPH at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-graph-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-graph-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-blok-lower-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around lower.
  Returns blok or nil if no LOWER at cursor-position. "
  (interactive "*")
  (ar-th-blok 'lower nil (called-interactively-p 'any)))

(defun ar-comment-lower-atpt ()
  "Comments LOWER at point if any. "
  (interactive "*")
  (ar-th-comment 'lower nil (called-interactively-p 'any)))

(defun ar-commatize-lower-atpt ()
  "Put a comma after LOWER at point if any. "
  (interactive "*")
  (ar-th-commatize 'lower nil (called-interactively-p 'any)))

(defun ar-mark-lower-atpt ()
  "Marks LOWER at point if any. "
  (interactive)
  (ar-th-mark 'lower))

(defun ar-hide-lower-atpt ()
  "Hides LOWER at point. "
  (interactive)
  (ar-th-hide 'lower))

(defun ar-show-lower-atpt ()
  "Shows hidden LOWER at point. "
  (interactive)
  (ar-th-show 'lower))

(defun ar-hide-show-lower-atpt ()
  "Alternatively hides or shows LOWER at point. "
  (interactive)
  (ar-th-hide-show 'lower))

(defun ar-highlight-lower-atpt-mode ()
  "Toggles lower-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'lower nil (called-interactively-p 'any)))

(defun ar-kill-lower-atpt ()
  "Kills LOWER at point if any. "
  (interactive "*")
  (ar-th-kill 'lower nil (called-interactively-p 'any)))

(defun ar-separate-lower-atpt ()
  "Separates LOWER at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'lower nil (called-interactively-p 'any)))

(defun ar-triplequotedq-lower-atpt ()
  "Put triplequotes composed of doublequotes around lower. "
  (interactive "*")
  (ar-th-triplequotedq 'lower nil (called-interactively-p 'any)))

(defun ar-triplequotesq-lower-atpt ()
  "Put triplequotes composed of singlequotes around lower. "
  (interactive "*")
  (ar-th-triplequotesq 'lower nil (called-interactively-p 'any)))

(defun ar-forward-lower-atpt (&optional arg)
  "Moves forward over LOWER at point if any, does nothing otherwise.
Returns end position of LOWER "
  (interactive "p")
  (ar-th-forward 'lower arg (called-interactively-p 'any)))

(defun ar-backward-lower-atpt (&optional arg)
  "Moves backward over LOWER before point if any, does nothing otherwise.
Returns beginning position of LOWER "
  (interactive "p")
  (ar-th-backward 'lower arg (called-interactively-p 'any)))

(defun ar-transpose-lower-atpt (&optional arg)
  "Transposes LOWER with LOWER before point if any. "
  (interactive "*P")
  (ar-th-transpose 'lower arg (called-interactively-p 'any)))

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

(defun ar-check-lower-atpt ()
  "Return t if a LOWER at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-lower-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-lower-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-blok-nonascii-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around nonascii.
  Returns blok or nil if no NONASCII at cursor-position. "
  (interactive "*")
  (ar-th-blok 'nonascii nil (called-interactively-p 'any)))

(defun ar-comment-nonascii-atpt ()
  "Comments NONASCII at point if any. "
  (interactive "*")
  (ar-th-comment 'nonascii nil (called-interactively-p 'any)))

(defun ar-commatize-nonascii-atpt ()
  "Put a comma after NONASCII at point if any. "
  (interactive "*")
  (ar-th-commatize 'nonascii nil (called-interactively-p 'any)))

(defun ar-mark-nonascii-atpt ()
  "Marks NONASCII at point if any. "
  (interactive)
  (ar-th-mark 'nonascii))

(defun ar-hide-nonascii-atpt ()
  "Hides NONASCII at point. "
  (interactive)
  (ar-th-hide 'nonascii))

(defun ar-show-nonascii-atpt ()
  "Shows hidden NONASCII at point. "
  (interactive)
  (ar-th-show 'nonascii))

(defun ar-hide-show-nonascii-atpt ()
  "Alternatively hides or shows NONASCII at point. "
  (interactive)
  (ar-th-hide-show 'nonascii))

(defun ar-highlight-nonascii-atpt-mode ()
  "Toggles nonascii-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'nonascii nil (called-interactively-p 'any)))

(defun ar-kill-nonascii-atpt ()
  "Kills NONASCII at point if any. "
  (interactive "*")
  (ar-th-kill 'nonascii nil (called-interactively-p 'any)))

(defun ar-separate-nonascii-atpt ()
  "Separates NONASCII at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'nonascii nil (called-interactively-p 'any)))

(defun ar-triplequotedq-nonascii-atpt ()
  "Put triplequotes composed of doublequotes around nonascii. "
  (interactive "*")
  (ar-th-triplequotedq 'nonascii nil (called-interactively-p 'any)))

(defun ar-triplequotesq-nonascii-atpt ()
  "Put triplequotes composed of singlequotes around nonascii. "
  (interactive "*")
  (ar-th-triplequotesq 'nonascii nil (called-interactively-p 'any)))

(defun ar-forward-nonascii-atpt (&optional arg)
  "Moves forward over NONASCII at point if any, does nothing otherwise.
Returns end position of NONASCII "
  (interactive "p")
  (ar-th-forward 'nonascii arg (called-interactively-p 'any)))

(defun ar-backward-nonascii-atpt (&optional arg)
  "Moves backward over NONASCII before point if any, does nothing otherwise.
Returns beginning position of NONASCII "
  (interactive "p")
  (ar-th-backward 'nonascii arg (called-interactively-p 'any)))

(defun ar-transpose-nonascii-atpt (&optional arg)
  "Transposes NONASCII with NONASCII before point if any. "
  (interactive "*P")
  (ar-th-transpose 'nonascii arg (called-interactively-p 'any)))

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

(defun ar-check-nonascii-atpt ()
  "Return t if a NONASCII at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-nonascii-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-nonascii-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-blok-print-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around print.
  Returns blok or nil if no PRINT at cursor-position. "
  (interactive "*")
  (ar-th-blok 'print nil (called-interactively-p 'any)))

(defun ar-comment-print-atpt ()
  "Comments PRINT at point if any. "
  (interactive "*")
  (ar-th-comment 'print nil (called-interactively-p 'any)))

(defun ar-commatize-print-atpt ()
  "Put a comma after PRINT at point if any. "
  (interactive "*")
  (ar-th-commatize 'print nil (called-interactively-p 'any)))

(defun ar-mark-print-atpt ()
  "Marks PRINT at point if any. "
  (interactive)
  (ar-th-mark 'print))

(defun ar-hide-print-atpt ()
  "Hides PRINT at point. "
  (interactive)
  (ar-th-hide 'print))

(defun ar-show-print-atpt ()
  "Shows hidden PRINT at point. "
  (interactive)
  (ar-th-show 'print))

(defun ar-hide-show-print-atpt ()
  "Alternatively hides or shows PRINT at point. "
  (interactive)
  (ar-th-hide-show 'print))

(defun ar-highlight-print-atpt-mode ()
  "Toggles print-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'print nil (called-interactively-p 'any)))

(defun ar-kill-print-atpt ()
  "Kills PRINT at point if any. "
  (interactive "*")
  (ar-th-kill 'print nil (called-interactively-p 'any)))

(defun ar-separate-print-atpt ()
  "Separates PRINT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'print nil (called-interactively-p 'any)))

(defun ar-triplequotedq-print-atpt ()
  "Put triplequotes composed of doublequotes around print. "
  (interactive "*")
  (ar-th-triplequotedq 'print nil (called-interactively-p 'any)))

(defun ar-triplequotesq-print-atpt ()
  "Put triplequotes composed of singlequotes around print. "
  (interactive "*")
  (ar-th-triplequotesq 'print nil (called-interactively-p 'any)))

(defun ar-forward-print-atpt (&optional arg)
  "Moves forward over PRINT at point if any, does nothing otherwise.
Returns end position of PRINT "
  (interactive "p")
  (ar-th-forward 'print arg (called-interactively-p 'any)))

(defun ar-backward-print-atpt (&optional arg)
  "Moves backward over PRINT before point if any, does nothing otherwise.
Returns beginning position of PRINT "
  (interactive "p")
  (ar-th-backward 'print arg (called-interactively-p 'any)))

(defun ar-transpose-print-atpt (&optional arg)
  "Transposes PRINT with PRINT before point if any. "
  (interactive "*P")
  (ar-th-transpose 'print arg (called-interactively-p 'any)))

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

(defun ar-check-print-atpt ()
  "Return t if a PRINT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-print-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-print-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-blok-punct-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around punct.
  Returns blok or nil if no PUNCT at cursor-position. "
  (interactive "*")
  (ar-th-blok 'punct nil (called-interactively-p 'any)))

(defun ar-comment-punct-atpt ()
  "Comments PUNCT at point if any. "
  (interactive "*")
  (ar-th-comment 'punct nil (called-interactively-p 'any)))

(defun ar-commatize-punct-atpt ()
  "Put a comma after PUNCT at point if any. "
  (interactive "*")
  (ar-th-commatize 'punct nil (called-interactively-p 'any)))

(defun ar-mark-punct-atpt ()
  "Marks PUNCT at point if any. "
  (interactive)
  (ar-th-mark 'punct))

(defun ar-hide-punct-atpt ()
  "Hides PUNCT at point. "
  (interactive)
  (ar-th-hide 'punct))

(defun ar-show-punct-atpt ()
  "Shows hidden PUNCT at point. "
  (interactive)
  (ar-th-show 'punct))

(defun ar-hide-show-punct-atpt ()
  "Alternatively hides or shows PUNCT at point. "
  (interactive)
  (ar-th-hide-show 'punct))

(defun ar-highlight-punct-atpt-mode ()
  "Toggles punct-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'punct nil (called-interactively-p 'any)))

(defun ar-kill-punct-atpt ()
  "Kills PUNCT at point if any. "
  (interactive "*")
  (ar-th-kill 'punct nil (called-interactively-p 'any)))

(defun ar-separate-punct-atpt ()
  "Separates PUNCT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'punct nil (called-interactively-p 'any)))

(defun ar-triplequotedq-punct-atpt ()
  "Put triplequotes composed of doublequotes around punct. "
  (interactive "*")
  (ar-th-triplequotedq 'punct nil (called-interactively-p 'any)))

(defun ar-triplequotesq-punct-atpt ()
  "Put triplequotes composed of singlequotes around punct. "
  (interactive "*")
  (ar-th-triplequotesq 'punct nil (called-interactively-p 'any)))

(defun ar-forward-punct-atpt (&optional arg)
  "Moves forward over PUNCT at point if any, does nothing otherwise.
Returns end position of PUNCT "
  (interactive "p")
  (ar-th-forward 'punct arg (called-interactively-p 'any)))

(defun ar-backward-punct-atpt (&optional arg)
  "Moves backward over PUNCT before point if any, does nothing otherwise.
Returns beginning position of PUNCT "
  (interactive "p")
  (ar-th-backward 'punct arg (called-interactively-p 'any)))

(defun ar-transpose-punct-atpt (&optional arg)
  "Transposes PUNCT with PUNCT before point if any. "
  (interactive "*P")
  (ar-th-transpose 'punct arg (called-interactively-p 'any)))

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

(defun ar-check-punct-atpt ()
  "Return t if a PUNCT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-punct-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-punct-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-blok-space-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around space.
  Returns blok or nil if no SPACE at cursor-position. "
  (interactive "*")
  (ar-th-blok 'space nil (called-interactively-p 'any)))

(defun ar-comment-space-atpt ()
  "Comments SPACE at point if any. "
  (interactive "*")
  (ar-th-comment 'space nil (called-interactively-p 'any)))

(defun ar-commatize-space-atpt ()
  "Put a comma after SPACE at point if any. "
  (interactive "*")
  (ar-th-commatize 'space nil (called-interactively-p 'any)))

(defun ar-mark-space-atpt ()
  "Marks SPACE at point if any. "
  (interactive)
  (ar-th-mark 'space))

(defun ar-hide-space-atpt ()
  "Hides SPACE at point. "
  (interactive)
  (ar-th-hide 'space))

(defun ar-show-space-atpt ()
  "Shows hidden SPACE at point. "
  (interactive)
  (ar-th-show 'space))

(defun ar-hide-show-space-atpt ()
  "Alternatively hides or shows SPACE at point. "
  (interactive)
  (ar-th-hide-show 'space))

(defun ar-highlight-space-atpt-mode ()
  "Toggles space-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'space nil (called-interactively-p 'any)))

(defun ar-kill-space-atpt ()
  "Kills SPACE at point if any. "
  (interactive "*")
  (ar-th-kill 'space nil (called-interactively-p 'any)))

(defun ar-separate-space-atpt ()
  "Separates SPACE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'space nil (called-interactively-p 'any)))

(defun ar-triplequotedq-space-atpt ()
  "Put triplequotes composed of doublequotes around space. "
  (interactive "*")
  (ar-th-triplequotedq 'space nil (called-interactively-p 'any)))

(defun ar-triplequotesq-space-atpt ()
  "Put triplequotes composed of singlequotes around space. "
  (interactive "*")
  (ar-th-triplequotesq 'space nil (called-interactively-p 'any)))

(defun ar-forward-space-atpt (&optional arg)
  "Moves forward over SPACE at point if any, does nothing otherwise.
Returns end position of SPACE "
  (interactive "p")
  (ar-th-forward 'space arg (called-interactively-p 'any)))

(defun ar-backward-space-atpt (&optional arg)
  "Moves backward over SPACE before point if any, does nothing otherwise.
Returns beginning position of SPACE "
  (interactive "p")
  (ar-th-backward 'space arg (called-interactively-p 'any)))

(defun ar-transpose-space-atpt (&optional arg)
  "Transposes SPACE with SPACE before point if any. "
  (interactive "*P")
  (ar-th-transpose 'space arg (called-interactively-p 'any)))

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

(defun ar-check-space-atpt ()
  "Return t if a SPACE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-space-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-space-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-blok-upper-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around upper.
  Returns blok or nil if no UPPER at cursor-position. "
  (interactive "*")
  (ar-th-blok 'upper nil (called-interactively-p 'any)))

(defun ar-comment-upper-atpt ()
  "Comments UPPER at point if any. "
  (interactive "*")
  (ar-th-comment 'upper nil (called-interactively-p 'any)))

(defun ar-commatize-upper-atpt ()
  "Put a comma after UPPER at point if any. "
  (interactive "*")
  (ar-th-commatize 'upper nil (called-interactively-p 'any)))

(defun ar-mark-upper-atpt ()
  "Marks UPPER at point if any. "
  (interactive)
  (ar-th-mark 'upper))

(defun ar-hide-upper-atpt ()
  "Hides UPPER at point. "
  (interactive)
  (ar-th-hide 'upper))

(defun ar-show-upper-atpt ()
  "Shows hidden UPPER at point. "
  (interactive)
  (ar-th-show 'upper))

(defun ar-hide-show-upper-atpt ()
  "Alternatively hides or shows UPPER at point. "
  (interactive)
  (ar-th-hide-show 'upper))

(defun ar-highlight-upper-atpt-mode ()
  "Toggles upper-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'upper nil (called-interactively-p 'any)))

(defun ar-kill-upper-atpt ()
  "Kills UPPER at point if any. "
  (interactive "*")
  (ar-th-kill 'upper nil (called-interactively-p 'any)))

(defun ar-separate-upper-atpt ()
  "Separates UPPER at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'upper nil (called-interactively-p 'any)))

(defun ar-triplequotedq-upper-atpt ()
  "Put triplequotes composed of doublequotes around upper. "
  (interactive "*")
  (ar-th-triplequotedq 'upper nil (called-interactively-p 'any)))

(defun ar-triplequotesq-upper-atpt ()
  "Put triplequotes composed of singlequotes around upper. "
  (interactive "*")
  (ar-th-triplequotesq 'upper nil (called-interactively-p 'any)))

(defun ar-forward-upper-atpt (&optional arg)
  "Moves forward over UPPER at point if any, does nothing otherwise.
Returns end position of UPPER "
  (interactive "p")
  (ar-th-forward 'upper arg (called-interactively-p 'any)))

(defun ar-backward-upper-atpt (&optional arg)
  "Moves backward over UPPER before point if any, does nothing otherwise.
Returns beginning position of UPPER "
  (interactive "p")
  (ar-th-backward 'upper arg (called-interactively-p 'any)))

(defun ar-transpose-upper-atpt (&optional arg)
  "Transposes UPPER with UPPER before point if any. "
  (interactive "*P")
  (ar-th-transpose 'upper arg (called-interactively-p 'any)))

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

(defun ar-check-upper-atpt ()
  "Return t if a UPPER at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-upper-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-upper-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-blok-xdigit-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around xdigit.
  Returns blok or nil if no XDIGIT at cursor-position. "
  (interactive "*")
  (ar-th-blok 'xdigit nil (called-interactively-p 'any)))

(defun ar-comment-xdigit-atpt ()
  "Comments XDIGIT at point if any. "
  (interactive "*")
  (ar-th-comment 'xdigit nil (called-interactively-p 'any)))

(defun ar-commatize-xdigit-atpt ()
  "Put a comma after XDIGIT at point if any. "
  (interactive "*")
  (ar-th-commatize 'xdigit nil (called-interactively-p 'any)))

(defun ar-mark-xdigit-atpt ()
  "Marks XDIGIT at point if any. "
  (interactive)
  (ar-th-mark 'xdigit))

(defun ar-hide-xdigit-atpt ()
  "Hides XDIGIT at point. "
  (interactive)
  (ar-th-hide 'xdigit))

(defun ar-show-xdigit-atpt ()
  "Shows hidden XDIGIT at point. "
  (interactive)
  (ar-th-show 'xdigit))

(defun ar-hide-show-xdigit-atpt ()
  "Alternatively hides or shows XDIGIT at point. "
  (interactive)
  (ar-th-hide-show 'xdigit))

(defun ar-highlight-xdigit-atpt-mode ()
  "Toggles xdigit-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'xdigit nil (called-interactively-p 'any)))

(defun ar-kill-xdigit-atpt ()
  "Kills XDIGIT at point if any. "
  (interactive "*")
  (ar-th-kill 'xdigit nil (called-interactively-p 'any)))

(defun ar-separate-xdigit-atpt ()
  "Separates XDIGIT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'xdigit nil (called-interactively-p 'any)))

(defun ar-triplequotedq-xdigit-atpt ()
  "Put triplequotes composed of doublequotes around xdigit. "
  (interactive "*")
  (ar-th-triplequotedq 'xdigit nil (called-interactively-p 'any)))

(defun ar-triplequotesq-xdigit-atpt ()
  "Put triplequotes composed of singlequotes around xdigit. "
  (interactive "*")
  (ar-th-triplequotesq 'xdigit nil (called-interactively-p 'any)))

(defun ar-forward-xdigit-atpt (&optional arg)
  "Moves forward over XDIGIT at point if any, does nothing otherwise.
Returns end position of XDIGIT "
  (interactive "p")
  (ar-th-forward 'xdigit arg (called-interactively-p 'any)))

(defun ar-backward-xdigit-atpt (&optional arg)
  "Moves backward over XDIGIT before point if any, does nothing otherwise.
Returns beginning position of XDIGIT "
  (interactive "p")
  (ar-th-backward 'xdigit arg (called-interactively-p 'any)))

(defun ar-transpose-xdigit-atpt (&optional arg)
  "Transposes XDIGIT with XDIGIT before point if any. "
  (interactive "*P")
  (ar-th-transpose 'xdigit arg (called-interactively-p 'any)))

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

(defun ar-check-xdigit-atpt ()
  "Return t if a XDIGIT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-xdigit-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-xdigit-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))
;; ar-thing-at-point-utils-nodelim-einzeln: end

;; ar-thing-at-point-utils-delimited-intern: ar-unpaired-delimited-raw: start

(defun ar-backslashed-atpt (&optional no-delimiters nested)
  "Returns backslashed at point if any, nil otherwise.

With numerical ARG 2 `ar-thing-no-nest' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'backslashed no-delimiters (called-interactively-p 'any) (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-backslashed-atpt (&optional no-delimiters check)
  "Returns a list, borders of backslashed if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'backslashed no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-backslashed-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position backslashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'backslashed no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-backslashed-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of backslashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'backslashed no-delimiters (called-interactively-p 'any) check))

(defun ar-beginning-of-backslashed-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class backslashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'backslashed no-delimiters (called-interactively-p 'any) check))

(defun ar-end-of-backslashed-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class backslashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'backslashed no-delimiters (called-interactively-p 'any) check))

(defun ar-length-of-backslashed-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class backslashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'backslashed no-delimiters (called-interactively-p 'any) check))

(defun ar-copy-backslashed-atpt (&optional no-delimiters check)
  "Returns a copy of backslashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'backslashed no-delimiters (called-interactively-p 'any)))

(defun ar-delete-backslashed-atpt (&optional arg)
  "Deletes backslashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'backslashed arg (called-interactively-p 'any)))

(defun ar-delete-backslashed-in-region (beg end)
  "Deletes backslashed at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'backslashed beg end (called-interactively-p 'any)))

(defun ar-blok-backslashed-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around backslashed.

If region is active, do that for all elements \"backslashed\" in region.
  Returns blok or nil if no backslashed at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'backslashed no-delimiters (called-interactively-p 'any)))

(defun ar-doubleslash-backslashed-atpt (&optional no-delimiters check)
  "Puts doubled slashes around backslashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'backslashed no-delimiters (called-interactively-p 'any)))

(defun ar-backslashparen-backslashed-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around backslashed at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'backslashed no-delimiters (called-interactively-p 'any)))

(defun ar-comment-backslashed-atpt (&optional no-delimiters check)
  "Comments backslashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'backslashed no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-backslashed-atpt (&optional no-delimiters check)
  "Put a comma after backslashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'backslashed no-delimiters (called-interactively-p 'any)))

(defun ar-mark-backslashed-atpt (&optional no-delimiters check)
  "Marks backslashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'backslashed))

(defun ar-hide-backslashed-atpt ()
  "Hides backslashed at point. "
  (interactive)
  (ar-th-hide 'backslashed))

(defun ar-show-backslashed-atpt ()
  "Shows hidden backslashed at point. "
  (interactive)
  (ar-th-show 'backslashed))

(defun ar-hide-show-backslashed-atpt ()
  "Alternatively hides or shows backslashed at point. "
  (interactive)
  (ar-th-hide-show 'backslashed))

(defun ar-highlight-backslashed-atpt-mode (&optional no-delimiters check)
  "Toggles backslashed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'backslashed no-delimiters (called-interactively-p 'any)))

(defun ar-kill-backslashed-atpt (&optional no-delimiters check)
  "Kills backslashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'backslashed no-delimiters (called-interactively-p 'any)))

(defun ar-separate-backslashed-atpt (&optional no-delimiters check)
  "Separates backslashed at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'backslashed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any) check))

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
  (ar-th-forward 'backslashed arg (called-interactively-p 'any)))

(defun ar-backward-backslashed-atpt (&optional arg)
  "Moves backward over backslashed before point if any, does nothing otherwise.
Returns beginning position of backslashed "
  (interactive "p")
  (ar-th-backward 'backslashed arg (called-interactively-p 'any)))

(defun ar-transpose-backslashed-atpt (&optional arg)
  "Transposes backslashed with backslashed before point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'backslashed arg (called-interactively-p 'any)))

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

(defun ar-check-backslashed-atpt ()
  "Return t if a backslashed at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-backslashed-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-backslashed-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-backticked-atpt (&optional no-delimiters nested)
  "Returns backticked at point if any, nil otherwise.

With numerical ARG 2 `ar-thing-no-nest' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'backticked no-delimiters (called-interactively-p 'any) (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-backticked-atpt (&optional no-delimiters check)
  "Returns a list, borders of backticked if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'backticked no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-backticked-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position backticked at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'backticked no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-backticked-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of backticked at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'backticked no-delimiters (called-interactively-p 'any) check))

(defun ar-beginning-of-backticked-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class backticked at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'backticked no-delimiters (called-interactively-p 'any) check))

(defun ar-end-of-backticked-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class backticked at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'backticked no-delimiters (called-interactively-p 'any) check))

(defun ar-length-of-backticked-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class backticked at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'backticked no-delimiters (called-interactively-p 'any) check))

(defun ar-copy-backticked-atpt (&optional no-delimiters check)
  "Returns a copy of backticked at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'backticked no-delimiters (called-interactively-p 'any)))

(defun ar-delete-backticked-atpt (&optional arg)
  "Deletes backticked at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'backticked arg (called-interactively-p 'any)))

(defun ar-delete-backticked-in-region (beg end)
  "Deletes backticked at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'backticked beg end (called-interactively-p 'any)))

(defun ar-blok-backticked-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around backticked.

If region is active, do that for all elements \"backticked\" in region.
  Returns blok or nil if no backticked at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'backticked no-delimiters (called-interactively-p 'any)))

(defun ar-doubleslash-backticked-atpt (&optional no-delimiters check)
  "Puts doubled slashes around backticked at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'backticked no-delimiters (called-interactively-p 'any)))

(defun ar-backslashparen-backticked-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around backticked at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'backticked no-delimiters (called-interactively-p 'any)))

(defun ar-comment-backticked-atpt (&optional no-delimiters check)
  "Comments backticked at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'backticked no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-backticked-atpt (&optional no-delimiters check)
  "Put a comma after backticked at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'backticked no-delimiters (called-interactively-p 'any)))

(defun ar-mark-backticked-atpt (&optional no-delimiters check)
  "Marks backticked at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'backticked))

(defun ar-hide-backticked-atpt ()
  "Hides backticked at point. "
  (interactive)
  (ar-th-hide 'backticked))

(defun ar-show-backticked-atpt ()
  "Shows hidden backticked at point. "
  (interactive)
  (ar-th-show 'backticked))

(defun ar-hide-show-backticked-atpt ()
  "Alternatively hides or shows backticked at point. "
  (interactive)
  (ar-th-hide-show 'backticked))

(defun ar-highlight-backticked-atpt-mode (&optional no-delimiters check)
  "Toggles backticked-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'backticked no-delimiters (called-interactively-p 'any)))

(defun ar-kill-backticked-atpt (&optional no-delimiters check)
  "Kills backticked at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'backticked no-delimiters (called-interactively-p 'any)))

(defun ar-separate-backticked-atpt (&optional no-delimiters check)
  "Separates backticked at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'backticked (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any) check))

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
  (ar-th-forward 'backticked arg (called-interactively-p 'any)))

(defun ar-backward-backticked-atpt (&optional arg)
  "Moves backward over backticked before point if any, does nothing otherwise.
Returns beginning position of backticked "
  (interactive "p")
  (ar-th-backward 'backticked arg (called-interactively-p 'any)))

(defun ar-transpose-backticked-atpt (&optional arg)
  "Transposes backticked with backticked before point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'backticked arg (called-interactively-p 'any)))

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

(defun ar-check-backticked-atpt ()
  "Return t if a backticked at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-backticked-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-backticked-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-coloned-atpt (&optional no-delimiters nested)
  "Returns coloned at point if any, nil otherwise.

With numerical ARG 2 `ar-thing-no-nest' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'coloned no-delimiters (called-interactively-p 'any) (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-coloned-atpt (&optional no-delimiters check)
  "Returns a list, borders of coloned if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'coloned no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-coloned-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position coloned at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'coloned no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-coloned-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of coloned at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'coloned no-delimiters (called-interactively-p 'any) check))

(defun ar-beginning-of-coloned-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class coloned at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'coloned no-delimiters (called-interactively-p 'any) check))

(defun ar-end-of-coloned-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class coloned at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'coloned no-delimiters (called-interactively-p 'any) check))

(defun ar-length-of-coloned-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class coloned at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'coloned no-delimiters (called-interactively-p 'any) check))

(defun ar-copy-coloned-atpt (&optional no-delimiters check)
  "Returns a copy of coloned at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'coloned no-delimiters (called-interactively-p 'any)))

(defun ar-delete-coloned-atpt (&optional arg)
  "Deletes coloned at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'coloned arg (called-interactively-p 'any)))

(defun ar-delete-coloned-in-region (beg end)
  "Deletes coloned at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'coloned beg end (called-interactively-p 'any)))

(defun ar-blok-coloned-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around coloned.

If region is active, do that for all elements \"coloned\" in region.
  Returns blok or nil if no coloned at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'coloned no-delimiters (called-interactively-p 'any)))

(defun ar-doubleslash-coloned-atpt (&optional no-delimiters check)
  "Puts doubled slashes around coloned at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'coloned no-delimiters (called-interactively-p 'any)))

(defun ar-backslashparen-coloned-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around coloned at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'coloned no-delimiters (called-interactively-p 'any)))

(defun ar-comment-coloned-atpt (&optional no-delimiters check)
  "Comments coloned at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'coloned no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-coloned-atpt (&optional no-delimiters check)
  "Put a comma after coloned at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'coloned no-delimiters (called-interactively-p 'any)))

(defun ar-mark-coloned-atpt (&optional no-delimiters check)
  "Marks coloned at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'coloned))

(defun ar-hide-coloned-atpt ()
  "Hides coloned at point. "
  (interactive)
  (ar-th-hide 'coloned))

(defun ar-show-coloned-atpt ()
  "Shows hidden coloned at point. "
  (interactive)
  (ar-th-show 'coloned))

(defun ar-hide-show-coloned-atpt ()
  "Alternatively hides or shows coloned at point. "
  (interactive)
  (ar-th-hide-show 'coloned))

(defun ar-highlight-coloned-atpt-mode (&optional no-delimiters check)
  "Toggles coloned-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'coloned no-delimiters (called-interactively-p 'any)))

(defun ar-kill-coloned-atpt (&optional no-delimiters check)
  "Kills coloned at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'coloned no-delimiters (called-interactively-p 'any)))

(defun ar-separate-coloned-atpt (&optional no-delimiters check)
  "Separates coloned at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'coloned (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any) check))

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
  (ar-th-forward 'coloned arg (called-interactively-p 'any)))

(defun ar-backward-coloned-atpt (&optional arg)
  "Moves backward over coloned before point if any, does nothing otherwise.
Returns beginning position of coloned "
  (interactive "p")
  (ar-th-backward 'coloned arg (called-interactively-p 'any)))

(defun ar-transpose-coloned-atpt (&optional arg)
  "Transposes coloned with coloned before point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'coloned arg (called-interactively-p 'any)))

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

(defun ar-check-coloned-atpt ()
  "Return t if a coloned at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-coloned-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-coloned-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-dollared-atpt (&optional no-delimiters nested)
  "Returns dollared at point if any, nil otherwise.

With numerical ARG 2 `ar-thing-no-nest' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'dollared no-delimiters (called-interactively-p 'any) (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-dollared-atpt (&optional no-delimiters check)
  "Returns a list, borders of dollared if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'dollared no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-dollared-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position dollared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'dollared no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-dollared-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of dollared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'dollared no-delimiters (called-interactively-p 'any) check))

(defun ar-beginning-of-dollared-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class dollared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'dollared no-delimiters (called-interactively-p 'any) check))

(defun ar-end-of-dollared-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class dollared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'dollared no-delimiters (called-interactively-p 'any) check))

(defun ar-length-of-dollared-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class dollared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'dollared no-delimiters (called-interactively-p 'any) check))

(defun ar-copy-dollared-atpt (&optional no-delimiters check)
  "Returns a copy of dollared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'dollared no-delimiters (called-interactively-p 'any)))

(defun ar-delete-dollared-atpt (&optional arg)
  "Deletes dollared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'dollared arg (called-interactively-p 'any)))

(defun ar-delete-dollared-in-region (beg end)
  "Deletes dollared at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'dollared beg end (called-interactively-p 'any)))

(defun ar-blok-dollared-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around dollared.

If region is active, do that for all elements \"dollared\" in region.
  Returns blok or nil if no dollared at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'dollared no-delimiters (called-interactively-p 'any)))

(defun ar-doubleslash-dollared-atpt (&optional no-delimiters check)
  "Puts doubled slashes around dollared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'dollared no-delimiters (called-interactively-p 'any)))

(defun ar-backslashparen-dollared-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around dollared at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'dollared no-delimiters (called-interactively-p 'any)))

(defun ar-comment-dollared-atpt (&optional no-delimiters check)
  "Comments dollared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'dollared no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-dollared-atpt (&optional no-delimiters check)
  "Put a comma after dollared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'dollared no-delimiters (called-interactively-p 'any)))

(defun ar-mark-dollared-atpt (&optional no-delimiters check)
  "Marks dollared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'dollared))

(defun ar-hide-dollared-atpt ()
  "Hides dollared at point. "
  (interactive)
  (ar-th-hide 'dollared))

(defun ar-show-dollared-atpt ()
  "Shows hidden dollared at point. "
  (interactive)
  (ar-th-show 'dollared))

(defun ar-hide-show-dollared-atpt ()
  "Alternatively hides or shows dollared at point. "
  (interactive)
  (ar-th-hide-show 'dollared))

(defun ar-highlight-dollared-atpt-mode (&optional no-delimiters check)
  "Toggles dollared-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'dollared no-delimiters (called-interactively-p 'any)))

(defun ar-kill-dollared-atpt (&optional no-delimiters check)
  "Kills dollared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'dollared no-delimiters (called-interactively-p 'any)))

(defun ar-separate-dollared-atpt (&optional no-delimiters check)
  "Separates dollared at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'dollared (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any) check))

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
  (ar-th-forward 'dollared arg (called-interactively-p 'any)))

(defun ar-backward-dollared-atpt (&optional arg)
  "Moves backward over dollared before point if any, does nothing otherwise.
Returns beginning position of dollared "
  (interactive "p")
  (ar-th-backward 'dollared arg (called-interactively-p 'any)))

(defun ar-transpose-dollared-atpt (&optional arg)
  "Transposes dollared with dollared before point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'dollared arg (called-interactively-p 'any)))

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

(defun ar-check-dollared-atpt ()
  "Return t if a dollared at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-dollared-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-dollared-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-doublequoted-atpt (&optional no-delimiters nested)
  "Returns doublequoted at point if any, nil otherwise.

With numerical ARG 2 `ar-thing-no-nest' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'doublequoted no-delimiters (called-interactively-p 'any) (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-doublequoted-atpt (&optional no-delimiters check)
  "Returns a list, borders of doublequoted if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'doublequoted no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-doublequoted-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position doublequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'doublequoted no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-doublequoted-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of doublequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'doublequoted no-delimiters (called-interactively-p 'any) check))

(defun ar-beginning-of-doublequoted-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class doublequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'doublequoted no-delimiters (called-interactively-p 'any) check))

(defun ar-end-of-doublequoted-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class doublequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'doublequoted no-delimiters (called-interactively-p 'any) check))

(defun ar-length-of-doublequoted-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class doublequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'doublequoted no-delimiters (called-interactively-p 'any) check))

(defun ar-copy-doublequoted-atpt (&optional no-delimiters check)
  "Returns a copy of doublequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'doublequoted no-delimiters (called-interactively-p 'any)))

(defun ar-delete-doublequoted-atpt (&optional arg)
  "Deletes doublequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'doublequoted arg (called-interactively-p 'any)))

(defun ar-delete-doublequoted-in-region (beg end)
  "Deletes doublequoted at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'doublequoted beg end (called-interactively-p 'any)))

(defun ar-blok-doublequoted-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around doublequoted.

If region is active, do that for all elements \"doublequoted\" in region.
  Returns blok or nil if no doublequoted at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'doublequoted no-delimiters (called-interactively-p 'any)))

(defun ar-doubleslash-doublequoted-atpt (&optional no-delimiters check)
  "Puts doubled slashes around doublequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'doublequoted no-delimiters (called-interactively-p 'any)))

(defun ar-backslashparen-doublequoted-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around doublequoted at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'doublequoted no-delimiters (called-interactively-p 'any)))

(defun ar-comment-doublequoted-atpt (&optional no-delimiters check)
  "Comments doublequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'doublequoted no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-doublequoted-atpt (&optional no-delimiters check)
  "Put a comma after doublequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'doublequoted no-delimiters (called-interactively-p 'any)))

(defun ar-mark-doublequoted-atpt (&optional no-delimiters check)
  "Marks doublequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'doublequoted))

(defun ar-hide-doublequoted-atpt ()
  "Hides doublequoted at point. "
  (interactive)
  (ar-th-hide 'doublequoted))

(defun ar-show-doublequoted-atpt ()
  "Shows hidden doublequoted at point. "
  (interactive)
  (ar-th-show 'doublequoted))

(defun ar-hide-show-doublequoted-atpt ()
  "Alternatively hides or shows doublequoted at point. "
  (interactive)
  (ar-th-hide-show 'doublequoted))

(defun ar-highlight-doublequoted-atpt-mode (&optional no-delimiters check)
  "Toggles doublequoted-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'doublequoted no-delimiters (called-interactively-p 'any)))

(defun ar-kill-doublequoted-atpt (&optional no-delimiters check)
  "Kills doublequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'doublequoted no-delimiters (called-interactively-p 'any)))

(defun ar-separate-doublequoted-atpt (&optional no-delimiters check)
  "Separates doublequoted at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'doublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any) check))

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
  (ar-th-forward 'doublequoted arg (called-interactively-p 'any)))

(defun ar-backward-doublequoted-atpt (&optional arg)
  "Moves backward over doublequoted before point if any, does nothing otherwise.
Returns beginning position of doublequoted "
  (interactive "p")
  (ar-th-backward 'doublequoted arg (called-interactively-p 'any)))

(defun ar-transpose-doublequoted-atpt (&optional arg)
  "Transposes doublequoted with doublequoted before point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'doublequoted arg (called-interactively-p 'any)))

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

(defun ar-check-doublequoted-atpt ()
  "Return t if a doublequoted at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-doublequoted-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-doublequoted-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-equalized-atpt (&optional no-delimiters nested)
  "Returns equalized at point if any, nil otherwise.

With numerical ARG 2 `ar-thing-no-nest' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'equalized no-delimiters (called-interactively-p 'any) (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-equalized-atpt (&optional no-delimiters check)
  "Returns a list, borders of equalized if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'equalized no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-equalized-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position equalized at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'equalized no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-equalized-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of equalized at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'equalized no-delimiters (called-interactively-p 'any) check))

(defun ar-beginning-of-equalized-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class equalized at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'equalized no-delimiters (called-interactively-p 'any) check))

(defun ar-end-of-equalized-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class equalized at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'equalized no-delimiters (called-interactively-p 'any) check))

(defun ar-length-of-equalized-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class equalized at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'equalized no-delimiters (called-interactively-p 'any) check))

(defun ar-copy-equalized-atpt (&optional no-delimiters check)
  "Returns a copy of equalized at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'equalized no-delimiters (called-interactively-p 'any)))

(defun ar-delete-equalized-atpt (&optional arg)
  "Deletes equalized at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'equalized arg (called-interactively-p 'any)))

(defun ar-delete-equalized-in-region (beg end)
  "Deletes equalized at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'equalized beg end (called-interactively-p 'any)))

(defun ar-blok-equalized-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around equalized.

If region is active, do that for all elements \"equalized\" in region.
  Returns blok or nil if no equalized at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'equalized no-delimiters (called-interactively-p 'any)))

(defun ar-doubleslash-equalized-atpt (&optional no-delimiters check)
  "Puts doubled slashes around equalized at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'equalized no-delimiters (called-interactively-p 'any)))

(defun ar-backslashparen-equalized-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around equalized at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'equalized no-delimiters (called-interactively-p 'any)))

(defun ar-comment-equalized-atpt (&optional no-delimiters check)
  "Comments equalized at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'equalized no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-equalized-atpt (&optional no-delimiters check)
  "Put a comma after equalized at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'equalized no-delimiters (called-interactively-p 'any)))

(defun ar-mark-equalized-atpt (&optional no-delimiters check)
  "Marks equalized at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'equalized))

(defun ar-hide-equalized-atpt ()
  "Hides equalized at point. "
  (interactive)
  (ar-th-hide 'equalized))

(defun ar-show-equalized-atpt ()
  "Shows hidden equalized at point. "
  (interactive)
  (ar-th-show 'equalized))

(defun ar-hide-show-equalized-atpt ()
  "Alternatively hides or shows equalized at point. "
  (interactive)
  (ar-th-hide-show 'equalized))

(defun ar-highlight-equalized-atpt-mode (&optional no-delimiters check)
  "Toggles equalized-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'equalized no-delimiters (called-interactively-p 'any)))

(defun ar-kill-equalized-atpt (&optional no-delimiters check)
  "Kills equalized at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'equalized no-delimiters (called-interactively-p 'any)))

(defun ar-separate-equalized-atpt (&optional no-delimiters check)
  "Separates equalized at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'equalized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any) check))

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
  (ar-th-forward 'equalized arg (called-interactively-p 'any)))

(defun ar-backward-equalized-atpt (&optional arg)
  "Moves backward over equalized before point if any, does nothing otherwise.
Returns beginning position of equalized "
  (interactive "p")
  (ar-th-backward 'equalized arg (called-interactively-p 'any)))

(defun ar-transpose-equalized-atpt (&optional arg)
  "Transposes equalized with equalized before point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'equalized arg (called-interactively-p 'any)))

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

(defun ar-check-equalized-atpt ()
  "Return t if a equalized at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-equalized-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-equalized-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-hyphened-atpt (&optional no-delimiters nested)
  "Returns hyphened at point if any, nil otherwise.

With numerical ARG 2 `ar-thing-no-nest' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'hyphened no-delimiters (called-interactively-p 'any) (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-hyphened-atpt (&optional no-delimiters check)
  "Returns a list, borders of hyphened if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'hyphened no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-hyphened-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position hyphened at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'hyphened no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-hyphened-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of hyphened at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'hyphened no-delimiters (called-interactively-p 'any) check))

(defun ar-beginning-of-hyphened-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class hyphened at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'hyphened no-delimiters (called-interactively-p 'any) check))

(defun ar-end-of-hyphened-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class hyphened at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'hyphened no-delimiters (called-interactively-p 'any) check))

(defun ar-length-of-hyphened-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class hyphened at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'hyphened no-delimiters (called-interactively-p 'any) check))

(defun ar-copy-hyphened-atpt (&optional no-delimiters check)
  "Returns a copy of hyphened at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'hyphened no-delimiters (called-interactively-p 'any)))

(defun ar-delete-hyphened-atpt (&optional arg)
  "Deletes hyphened at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'hyphened arg (called-interactively-p 'any)))

(defun ar-delete-hyphened-in-region (beg end)
  "Deletes hyphened at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'hyphened beg end (called-interactively-p 'any)))

(defun ar-blok-hyphened-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around hyphened.

If region is active, do that for all elements \"hyphened\" in region.
  Returns blok or nil if no hyphened at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'hyphened no-delimiters (called-interactively-p 'any)))

(defun ar-doubleslash-hyphened-atpt (&optional no-delimiters check)
  "Puts doubled slashes around hyphened at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'hyphened no-delimiters (called-interactively-p 'any)))

(defun ar-backslashparen-hyphened-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around hyphened at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'hyphened no-delimiters (called-interactively-p 'any)))

(defun ar-comment-hyphened-atpt (&optional no-delimiters check)
  "Comments hyphened at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'hyphened no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-hyphened-atpt (&optional no-delimiters check)
  "Put a comma after hyphened at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'hyphened no-delimiters (called-interactively-p 'any)))

(defun ar-mark-hyphened-atpt (&optional no-delimiters check)
  "Marks hyphened at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'hyphened))

(defun ar-hide-hyphened-atpt ()
  "Hides hyphened at point. "
  (interactive)
  (ar-th-hide 'hyphened))

(defun ar-show-hyphened-atpt ()
  "Shows hidden hyphened at point. "
  (interactive)
  (ar-th-show 'hyphened))

(defun ar-hide-show-hyphened-atpt ()
  "Alternatively hides or shows hyphened at point. "
  (interactive)
  (ar-th-hide-show 'hyphened))

(defun ar-highlight-hyphened-atpt-mode (&optional no-delimiters check)
  "Toggles hyphened-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'hyphened no-delimiters (called-interactively-p 'any)))

(defun ar-kill-hyphened-atpt (&optional no-delimiters check)
  "Kills hyphened at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'hyphened no-delimiters (called-interactively-p 'any)))

(defun ar-separate-hyphened-atpt (&optional no-delimiters check)
  "Separates hyphened at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'hyphened (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any) check))

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
  (ar-th-forward 'hyphened arg (called-interactively-p 'any)))

(defun ar-backward-hyphened-atpt (&optional arg)
  "Moves backward over hyphened before point if any, does nothing otherwise.
Returns beginning position of hyphened "
  (interactive "p")
  (ar-th-backward 'hyphened arg (called-interactively-p 'any)))

(defun ar-transpose-hyphened-atpt (&optional arg)
  "Transposes hyphened with hyphened before point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'hyphened arg (called-interactively-p 'any)))

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

(defun ar-check-hyphened-atpt ()
  "Return t if a hyphened at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-hyphened-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-hyphened-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-singlequoted-atpt (&optional no-delimiters nested)
  "Returns singlequoted at point if any, nil otherwise.

With numerical ARG 2 `ar-thing-no-nest' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'singlequoted no-delimiters (called-interactively-p 'any) (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-singlequoted-atpt (&optional no-delimiters check)
  "Returns a list, borders of singlequoted if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'singlequoted no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-singlequoted-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position singlequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'singlequoted no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-singlequoted-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of singlequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'singlequoted no-delimiters (called-interactively-p 'any) check))

(defun ar-beginning-of-singlequoted-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class singlequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'singlequoted no-delimiters (called-interactively-p 'any) check))

(defun ar-end-of-singlequoted-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class singlequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'singlequoted no-delimiters (called-interactively-p 'any) check))

(defun ar-length-of-singlequoted-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class singlequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'singlequoted no-delimiters (called-interactively-p 'any) check))

(defun ar-copy-singlequoted-atpt (&optional no-delimiters check)
  "Returns a copy of singlequoted at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'singlequoted no-delimiters (called-interactively-p 'any)))

(defun ar-delete-singlequoted-atpt (&optional arg)
  "Deletes singlequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'singlequoted arg (called-interactively-p 'any)))

(defun ar-delete-singlequoted-in-region (beg end)
  "Deletes singlequoted at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'singlequoted beg end (called-interactively-p 'any)))

(defun ar-blok-singlequoted-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around singlequoted.

If region is active, do that for all elements \"singlequoted\" in region.
  Returns blok or nil if no singlequoted at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'singlequoted no-delimiters (called-interactively-p 'any)))

(defun ar-doubleslash-singlequoted-atpt (&optional no-delimiters check)
  "Puts doubled slashes around singlequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'singlequoted no-delimiters (called-interactively-p 'any)))

(defun ar-backslashparen-singlequoted-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around singlequoted at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'singlequoted no-delimiters (called-interactively-p 'any)))

(defun ar-comment-singlequoted-atpt (&optional no-delimiters check)
  "Comments singlequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'singlequoted no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-singlequoted-atpt (&optional no-delimiters check)
  "Put a comma after singlequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'singlequoted no-delimiters (called-interactively-p 'any)))

(defun ar-mark-singlequoted-atpt (&optional no-delimiters check)
  "Marks singlequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'singlequoted))

(defun ar-hide-singlequoted-atpt ()
  "Hides singlequoted at point. "
  (interactive)
  (ar-th-hide 'singlequoted))

(defun ar-show-singlequoted-atpt ()
  "Shows hidden singlequoted at point. "
  (interactive)
  (ar-th-show 'singlequoted))

(defun ar-hide-show-singlequoted-atpt ()
  "Alternatively hides or shows singlequoted at point. "
  (interactive)
  (ar-th-hide-show 'singlequoted))

(defun ar-highlight-singlequoted-atpt-mode (&optional no-delimiters check)
  "Toggles singlequoted-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'singlequoted no-delimiters (called-interactively-p 'any)))

(defun ar-kill-singlequoted-atpt (&optional no-delimiters check)
  "Kills singlequoted at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'singlequoted no-delimiters (called-interactively-p 'any)))

(defun ar-separate-singlequoted-atpt (&optional no-delimiters check)
  "Separates singlequoted at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'singlequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any) check))

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
  (ar-th-forward 'singlequoted arg (called-interactively-p 'any)))

(defun ar-backward-singlequoted-atpt (&optional arg)
  "Moves backward over singlequoted before point if any, does nothing otherwise.
Returns beginning position of singlequoted "
  (interactive "p")
  (ar-th-backward 'singlequoted arg (called-interactively-p 'any)))

(defun ar-transpose-singlequoted-atpt (&optional arg)
  "Transposes singlequoted with singlequoted before point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'singlequoted arg (called-interactively-p 'any)))

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

(defun ar-check-singlequoted-atpt ()
  "Return t if a singlequoted at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-singlequoted-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-singlequoted-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-slashed-atpt (&optional no-delimiters nested)
  "Returns slashed at point if any, nil otherwise.

With numerical ARG 2 `ar-thing-no-nest' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'slashed no-delimiters (called-interactively-p 'any) (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-slashed-atpt (&optional no-delimiters check)
  "Returns a list, borders of slashed if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'slashed no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-slashed-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position slashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'slashed no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-slashed-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of slashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'slashed no-delimiters (called-interactively-p 'any) check))

(defun ar-beginning-of-slashed-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class slashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'slashed no-delimiters (called-interactively-p 'any) check))

(defun ar-end-of-slashed-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class slashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'slashed no-delimiters (called-interactively-p 'any) check))

(defun ar-length-of-slashed-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class slashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'slashed no-delimiters (called-interactively-p 'any) check))

(defun ar-copy-slashed-atpt (&optional no-delimiters check)
  "Returns a copy of slashed at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'slashed no-delimiters (called-interactively-p 'any)))

(defun ar-delete-slashed-atpt (&optional arg)
  "Deletes slashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'slashed arg (called-interactively-p 'any)))

(defun ar-delete-slashed-in-region (beg end)
  "Deletes slashed at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'slashed beg end (called-interactively-p 'any)))

(defun ar-blok-slashed-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around slashed.

If region is active, do that for all elements \"slashed\" in region.
  Returns blok or nil if no slashed at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'slashed no-delimiters (called-interactively-p 'any)))

(defun ar-doubleslash-slashed-atpt (&optional no-delimiters check)
  "Puts doubled slashes around slashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'slashed no-delimiters (called-interactively-p 'any)))

(defun ar-backslashparen-slashed-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around slashed at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'slashed no-delimiters (called-interactively-p 'any)))

(defun ar-comment-slashed-atpt (&optional no-delimiters check)
  "Comments slashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'slashed no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-slashed-atpt (&optional no-delimiters check)
  "Put a comma after slashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'slashed no-delimiters (called-interactively-p 'any)))

(defun ar-mark-slashed-atpt (&optional no-delimiters check)
  "Marks slashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'slashed))

(defun ar-hide-slashed-atpt ()
  "Hides slashed at point. "
  (interactive)
  (ar-th-hide 'slashed))

(defun ar-show-slashed-atpt ()
  "Shows hidden slashed at point. "
  (interactive)
  (ar-th-show 'slashed))

(defun ar-hide-show-slashed-atpt ()
  "Alternatively hides or shows slashed at point. "
  (interactive)
  (ar-th-hide-show 'slashed))

(defun ar-highlight-slashed-atpt-mode (&optional no-delimiters check)
  "Toggles slashed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'slashed no-delimiters (called-interactively-p 'any)))

(defun ar-kill-slashed-atpt (&optional no-delimiters check)
  "Kills slashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'slashed no-delimiters (called-interactively-p 'any)))

(defun ar-separate-slashed-atpt (&optional no-delimiters check)
  "Separates slashed at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'slashed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any) check))

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
  (ar-th-forward 'slashed arg (called-interactively-p 'any)))

(defun ar-backward-slashed-atpt (&optional arg)
  "Moves backward over slashed before point if any, does nothing otherwise.
Returns beginning position of slashed "
  (interactive "p")
  (ar-th-backward 'slashed arg (called-interactively-p 'any)))

(defun ar-transpose-slashed-atpt (&optional arg)
  "Transposes slashed with slashed before point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'slashed arg (called-interactively-p 'any)))

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

(defun ar-check-slashed-atpt ()
  "Return t if a slashed at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-slashed-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-slashed-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-stared-atpt (&optional no-delimiters nested)
  "Returns stared at point if any, nil otherwise.

With numerical ARG 2 `ar-thing-no-nest' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'stared no-delimiters (called-interactively-p 'any) (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-stared-atpt (&optional no-delimiters check)
  "Returns a list, borders of stared if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'stared no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-stared-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position stared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'stared no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-stared-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of stared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'stared no-delimiters (called-interactively-p 'any) check))

(defun ar-beginning-of-stared-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class stared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'stared no-delimiters (called-interactively-p 'any) check))

(defun ar-end-of-stared-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class stared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'stared no-delimiters (called-interactively-p 'any) check))

(defun ar-length-of-stared-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class stared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'stared no-delimiters (called-interactively-p 'any) check))

(defun ar-copy-stared-atpt (&optional no-delimiters check)
  "Returns a copy of stared at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'stared no-delimiters (called-interactively-p 'any)))

(defun ar-delete-stared-atpt (&optional arg)
  "Deletes stared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'stared arg (called-interactively-p 'any)))

(defun ar-delete-stared-in-region (beg end)
  "Deletes stared at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'stared beg end (called-interactively-p 'any)))

(defun ar-blok-stared-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around stared.

If region is active, do that for all elements \"stared\" in region.
  Returns blok or nil if no stared at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'stared no-delimiters (called-interactively-p 'any)))

(defun ar-doubleslash-stared-atpt (&optional no-delimiters check)
  "Puts doubled slashes around stared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'stared no-delimiters (called-interactively-p 'any)))

(defun ar-backslashparen-stared-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around stared at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'stared no-delimiters (called-interactively-p 'any)))

(defun ar-comment-stared-atpt (&optional no-delimiters check)
  "Comments stared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'stared no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-stared-atpt (&optional no-delimiters check)
  "Put a comma after stared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'stared no-delimiters (called-interactively-p 'any)))

(defun ar-mark-stared-atpt (&optional no-delimiters check)
  "Marks stared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'stared))

(defun ar-hide-stared-atpt ()
  "Hides stared at point. "
  (interactive)
  (ar-th-hide 'stared))

(defun ar-show-stared-atpt ()
  "Shows hidden stared at point. "
  (interactive)
  (ar-th-show 'stared))

(defun ar-hide-show-stared-atpt ()
  "Alternatively hides or shows stared at point. "
  (interactive)
  (ar-th-hide-show 'stared))

(defun ar-highlight-stared-atpt-mode (&optional no-delimiters check)
  "Toggles stared-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'stared no-delimiters (called-interactively-p 'any)))

(defun ar-kill-stared-atpt (&optional no-delimiters check)
  "Kills stared at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'stared no-delimiters (called-interactively-p 'any)))

(defun ar-separate-stared-atpt (&optional no-delimiters check)
  "Separates stared at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'stared (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any) check))

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
  (ar-th-forward 'stared arg (called-interactively-p 'any)))

(defun ar-backward-stared-atpt (&optional arg)
  "Moves backward over stared before point if any, does nothing otherwise.
Returns beginning position of stared "
  (interactive "p")
  (ar-th-backward 'stared arg (called-interactively-p 'any)))

(defun ar-transpose-stared-atpt (&optional arg)
  "Transposes stared with stared before point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'stared arg (called-interactively-p 'any)))

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

(defun ar-check-stared-atpt ()
  "Return t if a stared at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-stared-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-stared-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-underscored-atpt (&optional no-delimiters nested)
  "Returns underscored at point if any, nil otherwise.

With numerical ARG 2 `ar-thing-no-nest' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'underscored no-delimiters (called-interactively-p 'any) (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-underscored-atpt (&optional no-delimiters check)
  "Returns a list, borders of underscored if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'underscored no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-underscored-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position underscored at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'underscored no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-underscored-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of underscored at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'underscored no-delimiters (called-interactively-p 'any) check))

(defun ar-beginning-of-underscored-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class underscored at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'underscored no-delimiters (called-interactively-p 'any) check))

(defun ar-end-of-underscored-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class underscored at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'underscored no-delimiters (called-interactively-p 'any) check))

(defun ar-length-of-underscored-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class underscored at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'underscored no-delimiters (called-interactively-p 'any) check))

(defun ar-copy-underscored-atpt (&optional no-delimiters check)
  "Returns a copy of underscored at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'underscored no-delimiters (called-interactively-p 'any)))

(defun ar-delete-underscored-atpt (&optional arg)
  "Deletes underscored at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'underscored arg (called-interactively-p 'any)))

(defun ar-delete-underscored-in-region (beg end)
  "Deletes underscored at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'underscored beg end (called-interactively-p 'any)))

(defun ar-blok-underscored-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around underscored.

If region is active, do that for all elements \"underscored\" in region.
  Returns blok or nil if no underscored at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'underscored no-delimiters (called-interactively-p 'any)))

(defun ar-doubleslash-underscored-atpt (&optional no-delimiters check)
  "Puts doubled slashes around underscored at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'underscored no-delimiters (called-interactively-p 'any)))

(defun ar-backslashparen-underscored-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around underscored at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'underscored no-delimiters (called-interactively-p 'any)))

(defun ar-comment-underscored-atpt (&optional no-delimiters check)
  "Comments underscored at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'underscored no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-underscored-atpt (&optional no-delimiters check)
  "Put a comma after underscored at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'underscored no-delimiters (called-interactively-p 'any)))

(defun ar-mark-underscored-atpt (&optional no-delimiters check)
  "Marks underscored at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'underscored))

(defun ar-hide-underscored-atpt ()
  "Hides underscored at point. "
  (interactive)
  (ar-th-hide 'underscored))

(defun ar-show-underscored-atpt ()
  "Shows hidden underscored at point. "
  (interactive)
  (ar-th-show 'underscored))

(defun ar-hide-show-underscored-atpt ()
  "Alternatively hides or shows underscored at point. "
  (interactive)
  (ar-th-hide-show 'underscored))

(defun ar-highlight-underscored-atpt-mode (&optional no-delimiters check)
  "Toggles underscored-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'underscored no-delimiters (called-interactively-p 'any)))

(defun ar-kill-underscored-atpt (&optional no-delimiters check)
  "Kills underscored at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'underscored no-delimiters (called-interactively-p 'any)))

(defun ar-separate-underscored-atpt (&optional no-delimiters check)
  "Separates underscored at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'underscored (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any) check))

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
  (ar-th-forward 'underscored arg (called-interactively-p 'any)))

(defun ar-backward-underscored-atpt (&optional arg)
  "Moves backward over underscored before point if any, does nothing otherwise.
Returns beginning position of underscored "
  (interactive "p")
  (ar-th-backward 'underscored arg (called-interactively-p 'any)))

(defun ar-transpose-underscored-atpt (&optional arg)
  "Transposes underscored with underscored before point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'underscored arg (called-interactively-p 'any)))

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

(defun ar-check-underscored-atpt ()
  "Return t if a underscored at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-underscored-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-underscored-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-whitespaced-atpt (&optional no-delimiters nested)
  "Returns whitespaced at point if any, nil otherwise.

With numerical ARG 2 `ar-thing-no-nest' is non-nil, scan whole buffer 

With optional \\[universal-argument] NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'whitespaced no-delimiters (called-interactively-p 'any) (or nested (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-bounds-of-whitespaced-atpt (&optional no-delimiters check)
  "Returns a list, borders of whitespaced if any, nil otherwise.
With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'whitespaced no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-whitespaced-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position whitespaced at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected
"
  (interactive "P")
  (ar-th-beg 'whitespaced no-delimiters (called-interactively-p 'any) (or check (eq 2 (prefix-numeric-value no-delimiters)))))

(defun ar-whitespaced-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of whitespaced at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. 
With CHECK scan accessible part of buffer, otherwise next delimiter is selected "
  (interactive "P")
  (ar-th-end 'whitespaced no-delimiters (called-interactively-p 'any) check))

(defun ar-beginning-of-whitespaced-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class whitespaced at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'whitespaced no-delimiters (called-interactively-p 'any) check))

(defun ar-end-of-whitespaced-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class whitespaced at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'whitespaced no-delimiters (called-interactively-p 'any) check))

(defun ar-length-of-whitespaced-atpt (&optional no-delimiters check) 
  "Returns beginning of symbol or char-class whitespaced at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'whitespaced no-delimiters (called-interactively-p 'any) check))

(defun ar-copy-whitespaced-atpt (&optional no-delimiters check)
  "Returns a copy of whitespaced at point if any, nil otherwise.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'whitespaced no-delimiters (called-interactively-p 'any)))

(defun ar-delete-whitespaced-atpt (&optional arg)
  "Deletes whitespaced at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*")
  (ar-th-delete 'whitespaced arg (called-interactively-p 'any)))

(defun ar-delete-whitespaced-in-region (beg end)
  "Deletes whitespaced at point if any. "
  (interactive "*")
  (ar-th-delete-in-region 'whitespaced beg end (called-interactively-p 'any)))

(defun ar-blok-whitespaced-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around whitespaced.

If region is active, do that for all elements \"whitespaced\" in region.
  Returns blok or nil if no whitespaced at cursor-position.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-blok 'whitespaced no-delimiters (called-interactively-p 'any)))

(defun ar-doubleslash-whitespaced-atpt (&optional no-delimiters check)
  "Puts doubled slashes around whitespaced at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-doubleslash 'whitespaced no-delimiters (called-interactively-p 'any)))

(defun ar-backslashparen-whitespaced-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around whitespaced at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'whitespaced no-delimiters (called-interactively-p 'any)))

(defun ar-comment-whitespaced-atpt (&optional no-delimiters check)
  "Comments whitespaced at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-comment 'whitespaced no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-whitespaced-atpt (&optional no-delimiters check)
  "Put a comma after whitespaced at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-commatize 'whitespaced no-delimiters (called-interactively-p 'any)))

(defun ar-mark-whitespaced-atpt (&optional no-delimiters check)
  "Marks whitespaced at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'whitespaced))

(defun ar-hide-whitespaced-atpt ()
  "Hides whitespaced at point. "
  (interactive)
  (ar-th-hide 'whitespaced))

(defun ar-show-whitespaced-atpt ()
  "Shows hidden whitespaced at point. "
  (interactive)
  (ar-th-show 'whitespaced))

(defun ar-hide-show-whitespaced-atpt ()
  "Alternatively hides or shows whitespaced at point. "
  (interactive)
  (ar-th-hide-show 'whitespaced))

(defun ar-highlight-whitespaced-atpt-mode (&optional no-delimiters check)
  "Toggles whitespaced-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'whitespaced no-delimiters (called-interactively-p 'any)))

(defun ar-kill-whitespaced-atpt (&optional no-delimiters check)
  "Kills whitespaced at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'whitespaced no-delimiters (called-interactively-p 'any)))

(defun ar-separate-whitespaced-atpt (&optional no-delimiters check)
  "Separates whitespaced at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'whitespaced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any) check))

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
  (ar-th-forward 'whitespaced arg (called-interactively-p 'any)))

(defun ar-backward-whitespaced-atpt (&optional arg)
  "Moves backward over whitespaced before point if any, does nothing otherwise.
Returns beginning position of whitespaced "
  (interactive "p")
  (ar-th-backward 'whitespaced arg (called-interactively-p 'any)))

(defun ar-transpose-whitespaced-atpt (&optional arg)
  "Transposes whitespaced with whitespaced before point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-transpose 'whitespaced arg (called-interactively-p 'any)))

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

(defun ar-check-whitespaced-atpt ()
  "Return t if a whitespaced at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-whitespaced-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-whitespaced-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

;; ar-thing-at-point-utils-delimited-intern: ar-unpaired-delimited-raw: end

;; ar-thing-at-point-utils-delimited-unpaired-anlegen: ar-unpaired-delimited-raw: start 

(defun ar-in-backslashed-p-atpt (&optional condition)
  "Returns beginning position of ` backslashed' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "\\\\" condition)))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun ar-in-backticked-p-atpt (&optional condition)
  "Returns beginning position of ` backticked' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "`" condition)))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun ar-in-coloned-p-atpt (&optional condition)
  "Returns beginning position of ` coloned' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base ":" condition)))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun ar-in-dollared-p-atpt (&optional condition)
  "Returns beginning position of ` dollared' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "\\$" condition)))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun ar-in-doublequoted-p-atpt (&optional condition)
  "Returns beginning position of ` doublequoted' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "\"" condition)))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun ar-in-equalized-p-atpt (&optional condition)
  "Returns beginning position of ` equalized' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "=" condition)))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun ar-in-hyphened-p-atpt (&optional condition)
  "Returns beginning position of ` hyphened' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "-" condition)))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun ar-in-singlequoted-p-atpt (&optional condition)
  "Returns beginning position of ` singlequoted' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "'" condition)))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun ar-in-slashed-p-atpt (&optional condition)
  "Returns beginning position of ` slashed' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "/" condition)))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun ar-in-stared-p-atpt (&optional condition)
  "Returns beginning position of ` stared' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "*" condition)))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun ar-in-underscored-p-atpt (&optional condition)
  "Returns beginning position of ` underscored' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "_" condition)))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun ar-in-whitespaced-p-atpt (&optional condition)
  "Returns beginning position of ` whitespaced' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base " " condition)))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

;; ar-thing-at-point-utils-delimited-unpaired-anlegen: ar-unpaired-delimited-raw: end

;; ar-thing-at-point-utils-delimiters-core: ar-paired-delimited-passiv: start

(defun ar-braced-atpt (&optional no-delimiters)
  "Returns braced at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-braced-atpt (&optional no-delimiters check)
  "Returns a list, borders of braced if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-braced-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position BRACED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-braced-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of BRACED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-braced-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class BRACED at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-braced-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class BRACED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-braced-p-atpt (&optional no-delimiters check)
  "Returns bounds of BRACED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-braced-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class BRACED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-braced-atpt (&optional no-delimiters check)
  "Returns a copy of BRACED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-braced-atpt (&optional arg)
  "Deletes BRACED at point if any. "
  (interactive "*P")
  (ar-th-delete 'braced arg (called-interactively-p 'any)))

(defun ar-delete-braced-in-region (beg end)
  "Deletes BRACED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'braced beg end (called-interactively-p 'any)))

(defun ar-blok-braced-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around braced.
  Returns blok or nil if no BRACED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-braced-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around braced at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-braced-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around BRACED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-braced-atpt (&optional no-delimiters check)
  "Puts doubled slashes around BRACED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-braced-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around BRACED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-braced-atpt (&optional no-delimiters check)
  "Provides double backticks around BRACED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-braced-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around BRACED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-braced-atpt (&optional no-delimiters check)
  "Comments BRACED at point if any. "
  (interactive "*P")
  (ar-th-comment 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-braced-atpt (&optional no-delimiters check)
  "Put a comma after BRACED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-braced-atpt (&optional no-delimiters check)
  "Put a singlequote before BRACED at point if any. "
  (interactive "*P")
  (ar-th-quote 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-braced-atpt ()
  "Marks BRACED at point if any. "
  (interactive)
  (ar-th-mark 'braced))

(defun ar-hide-braced-atpt ()
  "Hides BRACED at point. "
  (interactive)
  (ar-th-hide 'braced))

(defun ar-show-braced-atpt ()
  "Shows hidden BRACED at point. "
  (interactive)
  (ar-th-show 'braced))

(defun ar-hide-show-braced-atpt ()
  "Alternatively hides or shows BRACED at point. "
  (interactive)
  (ar-th-hide-show 'braced))

(defun ar-highlight-braced-atpt-mode (&optional no-delimiters check)
  "Toggles braced-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-braced-atpt (&optional no-delimiters check)
  "Kills BRACED at point if any. "
  (interactive "*P")
  (ar-th-kill 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-braced-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-braced-atpt (&optional no-delimiters check)
  "Separates BRACED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-braced-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around braced. "
  (interactive "*P")
  (ar-th-triplequotedq 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-braced-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around braced. "
  (interactive "*P")
  (ar-th-triplequotesq 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-braced-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'braced (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-braced-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'braced (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-braced-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'braced n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-braced-atpt (&optional no-delimiters check)
  "Put underscore char around BRACED. "
  (interactive "*P")
  (ar-th-underscore 'braced (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-braced-whitespace-atpt 'ar-whitespace-braced-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-braced-atpt (&optional no-delimiters check)
;;   "Put whitespace char around BRACED. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'braced nil t))

(defun ar-forward-braced-atpt (&optional arg)
  "Moves forward over BRACED at point if any, does nothing otherwise.
Returns end position of BRACED "
  (interactive "p")
  (ar-th-forward 'braced arg (called-interactively-p 'any)))

(defun ar-backward-braced-atpt (&optional arg)
  "Moves backward over BRACED before point if any, does nothing otherwise.
Returns beginning position of BRACED "
  (interactive "p")
  (ar-th-backward 'braced arg (called-interactively-p 'any)))

(defun ar-transpose-braced-atpt (&optional arg)
  "Transposes BRACED with BRACED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'braced arg (called-interactively-p 'any)))

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

(defun ar-check-braced-atpt ()
  "Return t if a BRACED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-braced-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-braced-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-bracketed-atpt (&optional no-delimiters)
  "Returns bracketed at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-bracketed-atpt (&optional no-delimiters check)
  "Returns a list, borders of bracketed if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bracketed-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position BRACKETED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bracketed-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of BRACKETED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-bracketed-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class BRACKETED at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-bracketed-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class BRACKETED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-bracketed-p-atpt (&optional no-delimiters check)
  "Returns bounds of BRACKETED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-bracketed-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class BRACKETED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-bracketed-atpt (&optional no-delimiters check)
  "Returns a copy of BRACKETED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-bracketed-atpt (&optional arg)
  "Deletes BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-delete 'bracketed arg (called-interactively-p 'any)))

(defun ar-delete-bracketed-in-region (beg end)
  "Deletes BRACKETED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'bracketed beg end (called-interactively-p 'any)))

(defun ar-blok-bracketed-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around bracketed.
  Returns blok or nil if no BRACKETED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-bracketed-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around bracketed at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-bracketed-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-bracketed-atpt (&optional no-delimiters check)
  "Puts doubled slashes around BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-bracketed-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-bracketed-atpt (&optional no-delimiters check)
  "Provides double backticks around BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-bracketed-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-bracketed-atpt (&optional no-delimiters check)
  "Comments BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-comment 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-bracketed-atpt (&optional no-delimiters check)
  "Put a comma after BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-bracketed-atpt (&optional no-delimiters check)
  "Put a singlequote before BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-quote 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-bracketed-atpt ()
  "Marks BRACKETED at point if any. "
  (interactive)
  (ar-th-mark 'bracketed))

(defun ar-hide-bracketed-atpt ()
  "Hides BRACKETED at point. "
  (interactive)
  (ar-th-hide 'bracketed))

(defun ar-show-bracketed-atpt ()
  "Shows hidden BRACKETED at point. "
  (interactive)
  (ar-th-show 'bracketed))

(defun ar-hide-show-bracketed-atpt ()
  "Alternatively hides or shows BRACKETED at point. "
  (interactive)
  (ar-th-hide-show 'bracketed))

(defun ar-highlight-bracketed-atpt-mode (&optional no-delimiters check)
  "Toggles bracketed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-bracketed-atpt (&optional no-delimiters check)
  "Kills BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-kill 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-bracketed-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-bracketed-atpt (&optional no-delimiters check)
  "Separates BRACKETED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-bracketed-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around bracketed. "
  (interactive "*P")
  (ar-th-triplequotedq 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-bracketed-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around bracketed. "
  (interactive "*P")
  (ar-th-triplequotesq 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-bracketed-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-bracketed-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-bracketed-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'bracketed n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-bracketed-atpt (&optional no-delimiters check)
  "Put underscore char around BRACKETED. "
  (interactive "*P")
  (ar-th-underscore 'bracketed (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-bracketed-whitespace-atpt 'ar-whitespace-bracketed-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-bracketed-atpt (&optional no-delimiters check)
;;   "Put whitespace char around BRACKETED. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'bracketed nil t))

(defun ar-forward-bracketed-atpt (&optional arg)
  "Moves forward over BRACKETED at point if any, does nothing otherwise.
Returns end position of BRACKETED "
  (interactive "p")
  (ar-th-forward 'bracketed arg (called-interactively-p 'any)))

(defun ar-backward-bracketed-atpt (&optional arg)
  "Moves backward over BRACKETED before point if any, does nothing otherwise.
Returns beginning position of BRACKETED "
  (interactive "p")
  (ar-th-backward 'bracketed arg (called-interactively-p 'any)))

(defun ar-transpose-bracketed-atpt (&optional arg)
  "Transposes BRACKETED with BRACKETED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'bracketed arg (called-interactively-p 'any)))

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

(defun ar-check-bracketed-atpt ()
  "Return t if a BRACKETED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-bracketed-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-bracketed-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-lesserangled-atpt (&optional no-delimiters)
  "Returns lesserangled at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-lesserangled-atpt (&optional no-delimiters check)
  "Returns a list, borders of lesserangled if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-lesserangled-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position LESSERANGLED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-lesserangled-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of LESSERANGLED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-lesserangled-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class LESSERANGLED at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-lesserangled-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class LESSERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-lesserangled-p-atpt (&optional no-delimiters check)
  "Returns bounds of LESSERANGLED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-lesserangled-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class LESSERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-lesserangled-atpt (&optional no-delimiters check)
  "Returns a copy of LESSERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-lesserangled-atpt (&optional arg)
  "Deletes LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-delete 'lesserangled arg (called-interactively-p 'any)))

(defun ar-delete-lesserangled-in-region (beg end)
  "Deletes LESSERANGLED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'lesserangled beg end (called-interactively-p 'any)))

(defun ar-blok-lesserangled-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around lesserangled.
  Returns blok or nil if no LESSERANGLED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-lesserangled-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around lesserangled at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-lesserangled-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-lesserangled-atpt (&optional no-delimiters check)
  "Puts doubled slashes around LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-lesserangled-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-lesserangled-atpt (&optional no-delimiters check)
  "Provides double backticks around LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-lesserangled-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-lesserangled-atpt (&optional no-delimiters check)
  "Comments LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-comment 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-lesserangled-atpt (&optional no-delimiters check)
  "Put a comma after LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-lesserangled-atpt (&optional no-delimiters check)
  "Put a singlequote before LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-quote 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-lesserangled-atpt ()
  "Marks LESSERANGLED at point if any. "
  (interactive)
  (ar-th-mark 'lesserangled))

(defun ar-hide-lesserangled-atpt ()
  "Hides LESSERANGLED at point. "
  (interactive)
  (ar-th-hide 'lesserangled))

(defun ar-show-lesserangled-atpt ()
  "Shows hidden LESSERANGLED at point. "
  (interactive)
  (ar-th-show 'lesserangled))

(defun ar-hide-show-lesserangled-atpt ()
  "Alternatively hides or shows LESSERANGLED at point. "
  (interactive)
  (ar-th-hide-show 'lesserangled))

(defun ar-highlight-lesserangled-atpt-mode (&optional no-delimiters check)
  "Toggles lesserangled-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-lesserangled-atpt (&optional no-delimiters check)
  "Kills LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-kill 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-lesserangled-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-lesserangled-atpt (&optional no-delimiters check)
  "Separates LESSERANGLED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-lesserangled-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around lesserangled. "
  (interactive "*P")
  (ar-th-triplequotedq 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-lesserangled-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around lesserangled. "
  (interactive "*P")
  (ar-th-triplequotesq 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-lesserangled-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-lesserangled-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-lesserangled-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'lesserangled n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-lesserangled-atpt (&optional no-delimiters check)
  "Put underscore char around LESSERANGLED. "
  (interactive "*P")
  (ar-th-underscore 'lesserangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-lesserangled-whitespace-atpt 'ar-whitespace-lesserangled-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-lesserangled-atpt (&optional no-delimiters check)
;;   "Put whitespace char around LESSERANGLED. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'lesserangled nil t))

(defun ar-forward-lesserangled-atpt (&optional arg)
  "Moves forward over LESSERANGLED at point if any, does nothing otherwise.
Returns end position of LESSERANGLED "
  (interactive "p")
  (ar-th-forward 'lesserangled arg (called-interactively-p 'any)))

(defun ar-backward-lesserangled-atpt (&optional arg)
  "Moves backward over LESSERANGLED before point if any, does nothing otherwise.
Returns beginning position of LESSERANGLED "
  (interactive "p")
  (ar-th-backward 'lesserangled arg (called-interactively-p 'any)))

(defun ar-transpose-lesserangled-atpt (&optional arg)
  "Transposes LESSERANGLED with LESSERANGLED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'lesserangled arg (called-interactively-p 'any)))

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

(defun ar-check-lesserangled-atpt ()
  "Return t if a LESSERANGLED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-lesserangled-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-lesserangled-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-greaterangled-atpt (&optional no-delimiters)
  "Returns greaterangled at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-greaterangled-atpt (&optional no-delimiters check)
  "Returns a list, borders of greaterangled if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-greaterangled-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position GREATERANGLED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-greaterangled-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of GREATERANGLED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-greaterangled-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class GREATERANGLED at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-greaterangled-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class GREATERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-greaterangled-p-atpt (&optional no-delimiters check)
  "Returns bounds of GREATERANGLED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-greaterangled-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class GREATERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-greaterangled-atpt (&optional no-delimiters check)
  "Returns a copy of GREATERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-greaterangled-atpt (&optional arg)
  "Deletes GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-delete 'greaterangled arg (called-interactively-p 'any)))

(defun ar-delete-greaterangled-in-region (beg end)
  "Deletes GREATERANGLED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'greaterangled beg end (called-interactively-p 'any)))

(defun ar-blok-greaterangled-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around greaterangled.
  Returns blok or nil if no GREATERANGLED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-greaterangled-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around greaterangled at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-greaterangled-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-greaterangled-atpt (&optional no-delimiters check)
  "Puts doubled slashes around GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-greaterangled-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-greaterangled-atpt (&optional no-delimiters check)
  "Provides double backticks around GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-greaterangled-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-greaterangled-atpt (&optional no-delimiters check)
  "Comments GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-comment 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-greaterangled-atpt (&optional no-delimiters check)
  "Put a comma after GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-greaterangled-atpt (&optional no-delimiters check)
  "Put a singlequote before GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-quote 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-greaterangled-atpt ()
  "Marks GREATERANGLED at point if any. "
  (interactive)
  (ar-th-mark 'greaterangled))

(defun ar-hide-greaterangled-atpt ()
  "Hides GREATERANGLED at point. "
  (interactive)
  (ar-th-hide 'greaterangled))

(defun ar-show-greaterangled-atpt ()
  "Shows hidden GREATERANGLED at point. "
  (interactive)
  (ar-th-show 'greaterangled))

(defun ar-hide-show-greaterangled-atpt ()
  "Alternatively hides or shows GREATERANGLED at point. "
  (interactive)
  (ar-th-hide-show 'greaterangled))

(defun ar-highlight-greaterangled-atpt-mode (&optional no-delimiters check)
  "Toggles greaterangled-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-greaterangled-atpt (&optional no-delimiters check)
  "Kills GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-kill 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-greaterangled-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-greaterangled-atpt (&optional no-delimiters check)
  "Separates GREATERANGLED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-greaterangled-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around greaterangled. "
  (interactive "*P")
  (ar-th-triplequotedq 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-greaterangled-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around greaterangled. "
  (interactive "*P")
  (ar-th-triplequotesq 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-greaterangled-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-greaterangled-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-greaterangled-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'greaterangled n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-greaterangled-atpt (&optional no-delimiters check)
  "Put underscore char around GREATERANGLED. "
  (interactive "*P")
  (ar-th-underscore 'greaterangled (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-greaterangled-whitespace-atpt 'ar-whitespace-greaterangled-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-greaterangled-atpt (&optional no-delimiters check)
;;   "Put whitespace char around GREATERANGLED. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'greaterangled nil t))

(defun ar-forward-greaterangled-atpt (&optional arg)
  "Moves forward over GREATERANGLED at point if any, does nothing otherwise.
Returns end position of GREATERANGLED "
  (interactive "p")
  (ar-th-forward 'greaterangled arg (called-interactively-p 'any)))

(defun ar-backward-greaterangled-atpt (&optional arg)
  "Moves backward over GREATERANGLED before point if any, does nothing otherwise.
Returns beginning position of GREATERANGLED "
  (interactive "p")
  (ar-th-backward 'greaterangled arg (called-interactively-p 'any)))

(defun ar-transpose-greaterangled-atpt (&optional arg)
  "Transposes GREATERANGLED with GREATERANGLED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'greaterangled arg (called-interactively-p 'any)))

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

(defun ar-check-greaterangled-atpt ()
  "Return t if a GREATERANGLED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-greaterangled-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-greaterangled-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Returns leftrightsinglequoted at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Returns a list, borders of leftrightsinglequoted if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequoted-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequoted-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-leftrightsinglequoted-p-atpt (&optional no-delimiters check)
  "Returns bounds of LEFTRIGHTSINGLEQUOTED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Returns a copy of LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-leftrightsinglequoted-atpt (&optional arg)
  "Deletes LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-delete 'leftrightsinglequoted arg (called-interactively-p 'any)))

(defun ar-delete-leftrightsinglequoted-in-region (beg end)
  "Deletes LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'leftrightsinglequoted beg end (called-interactively-p 'any)))

(defun ar-blok-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around leftrightsinglequoted.
  Returns blok or nil if no LEFTRIGHTSINGLEQUOTED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around leftrightsinglequoted at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Puts doubled slashes around LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Provides double backticks around LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Comments LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-comment 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Put a comma after LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Put a singlequote before LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-quote 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-leftrightsinglequoted-atpt ()
  "Marks LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive)
  (ar-th-mark 'leftrightsinglequoted))

(defun ar-hide-leftrightsinglequoted-atpt ()
  "Hides LEFTRIGHTSINGLEQUOTED at point. "
  (interactive)
  (ar-th-hide 'leftrightsinglequoted))

(defun ar-show-leftrightsinglequoted-atpt ()
  "Shows hidden LEFTRIGHTSINGLEQUOTED at point. "
  (interactive)
  (ar-th-show 'leftrightsinglequoted))

(defun ar-hide-show-leftrightsinglequoted-atpt ()
  "Alternatively hides or shows LEFTRIGHTSINGLEQUOTED at point. "
  (interactive)
  (ar-th-hide-show 'leftrightsinglequoted))

(defun ar-highlight-leftrightsinglequoted-atpt-mode (&optional no-delimiters check)
  "Toggles leftrightsinglequoted-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Kills LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-kill 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Separates LEFTRIGHTSINGLEQUOTED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around leftrightsinglequoted. "
  (interactive "*P")
  (ar-th-triplequotedq 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around leftrightsinglequoted. "
  (interactive "*P")
  (ar-th-triplequotesq 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-leftrightsinglequoted-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-leftrightsinglequoted-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-leftrightsinglequoted-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'leftrightsinglequoted n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-leftrightsinglequoted-atpt (&optional no-delimiters check)
  "Put underscore char around LEFTRIGHTSINGLEQUOTED. "
  (interactive "*P")
  (ar-th-underscore 'leftrightsinglequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-leftrightsinglequoted-whitespace-atpt 'ar-whitespace-leftrightsinglequoted-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-leftrightsinglequoted-atpt (&optional no-delimiters check)
;;   "Put whitespace char around LEFTRIGHTSINGLEQUOTED. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'leftrightsinglequoted nil t))

(defun ar-forward-leftrightsinglequoted-atpt (&optional arg)
  "Moves forward over LEFTRIGHTSINGLEQUOTED at point if any, does nothing otherwise.
Returns end position of LEFTRIGHTSINGLEQUOTED "
  (interactive "p")
  (ar-th-forward 'leftrightsinglequoted arg (called-interactively-p 'any)))

(defun ar-backward-leftrightsinglequoted-atpt (&optional arg)
  "Moves backward over LEFTRIGHTSINGLEQUOTED before point if any, does nothing otherwise.
Returns beginning position of LEFTRIGHTSINGLEQUOTED "
  (interactive "p")
  (ar-th-backward 'leftrightsinglequoted arg (called-interactively-p 'any)))

(defun ar-transpose-leftrightsinglequoted-atpt (&optional arg)
  "Transposes LEFTRIGHTSINGLEQUOTED with LEFTRIGHTSINGLEQUOTED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'leftrightsinglequoted arg (called-interactively-p 'any)))

(defun ar-sort-leftrightsinglequoted-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts leftrightsinglequoteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'leftrightsinglequoted reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-leftrightsinglequoted-atpt ()
  "Return t if a LEFTRIGHTSINGLEQUOTED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-leftrightsinglequoted-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-leftrightsinglequoted-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-leftrightdoublequoted-atpt (&optional no-delimiters)
  "Returns leftrightdoublequoted at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Returns a list, borders of leftrightdoublequoted if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightdoublequoted-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightdoublequoted-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-leftrightdoublequoted-p-atpt (&optional no-delimiters check)
  "Returns bounds of LEFTRIGHTDOUBLEQUOTED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Returns a copy of LEFTRIGHTDOUBLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-leftrightdoublequoted-atpt (&optional arg)
  "Deletes LEFTRIGHTDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-delete 'leftrightdoublequoted arg (called-interactively-p 'any)))

(defun ar-delete-leftrightdoublequoted-in-region (beg end)
  "Deletes LEFTRIGHTDOUBLEQUOTED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'leftrightdoublequoted beg end (called-interactively-p 'any)))

(defun ar-blok-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around leftrightdoublequoted.
  Returns blok or nil if no LEFTRIGHTDOUBLEQUOTED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around leftrightdoublequoted at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around LEFTRIGHTDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Puts doubled slashes around LEFTRIGHTDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around LEFTRIGHTDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Provides double backticks around LEFTRIGHTDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around LEFTRIGHTDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Comments LEFTRIGHTDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-comment 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Put a comma after LEFTRIGHTDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Put a singlequote before LEFTRIGHTDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-quote 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-leftrightdoublequoted-atpt ()
  "Marks LEFTRIGHTDOUBLEQUOTED at point if any. "
  (interactive)
  (ar-th-mark 'leftrightdoublequoted))

(defun ar-hide-leftrightdoublequoted-atpt ()
  "Hides LEFTRIGHTDOUBLEQUOTED at point. "
  (interactive)
  (ar-th-hide 'leftrightdoublequoted))

(defun ar-show-leftrightdoublequoted-atpt ()
  "Shows hidden LEFTRIGHTDOUBLEQUOTED at point. "
  (interactive)
  (ar-th-show 'leftrightdoublequoted))

(defun ar-hide-show-leftrightdoublequoted-atpt ()
  "Alternatively hides or shows LEFTRIGHTDOUBLEQUOTED at point. "
  (interactive)
  (ar-th-hide-show 'leftrightdoublequoted))

(defun ar-highlight-leftrightdoublequoted-atpt-mode (&optional no-delimiters check)
  "Toggles leftrightdoublequoted-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Kills LEFTRIGHTDOUBLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-kill 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Separates LEFTRIGHTDOUBLEQUOTED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around leftrightdoublequoted. "
  (interactive "*P")
  (ar-th-triplequotedq 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around leftrightdoublequoted. "
  (interactive "*P")
  (ar-th-triplequotesq 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-leftrightdoublequoted-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-leftrightdoublequoted-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-leftrightdoublequoted-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'leftrightdoublequoted n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-leftrightdoublequoted-atpt (&optional no-delimiters check)
  "Put underscore char around LEFTRIGHTDOUBLEQUOTED. "
  (interactive "*P")
  (ar-th-underscore 'leftrightdoublequoted (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-leftrightdoublequoted-whitespace-atpt 'ar-whitespace-leftrightdoublequoted-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-leftrightdoublequoted-atpt (&optional no-delimiters check)
;;   "Put whitespace char around LEFTRIGHTDOUBLEQUOTED. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'leftrightdoublequoted nil t))

(defun ar-forward-leftrightdoublequoted-atpt (&optional arg)
  "Moves forward over LEFTRIGHTDOUBLEQUOTED at point if any, does nothing otherwise.
Returns end position of LEFTRIGHTDOUBLEQUOTED "
  (interactive "p")
  (ar-th-forward 'leftrightdoublequoted arg (called-interactively-p 'any)))

(defun ar-backward-leftrightdoublequoted-atpt (&optional arg)
  "Moves backward over LEFTRIGHTDOUBLEQUOTED before point if any, does nothing otherwise.
Returns beginning position of LEFTRIGHTDOUBLEQUOTED "
  (interactive "p")
  (ar-th-backward 'leftrightdoublequoted arg (called-interactively-p 'any)))

(defun ar-transpose-leftrightdoublequoted-atpt (&optional arg)
  "Transposes LEFTRIGHTDOUBLEQUOTED with LEFTRIGHTDOUBLEQUOTED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'leftrightdoublequoted arg (called-interactively-p 'any)))

(defun ar-sort-leftrightdoublequoted-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts leftrightdoublequoteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'leftrightdoublequoted reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-leftrightdoublequoted-atpt ()
  "Return t if a LEFTRIGHTDOUBLEQUOTED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-leftrightdoublequoted-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-leftrightdoublequoted-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-parentized-atpt (&optional no-delimiters)
  "Returns parentized at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-parentized-atpt (&optional no-delimiters check)
  "Returns a list, borders of parentized if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-parentized-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position PARENTIZED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-parentized-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of PARENTIZED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-parentized-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class PARENTIZED at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-parentized-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class PARENTIZED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-parentized-p-atpt (&optional no-delimiters check)
  "Returns bounds of PARENTIZED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-parentized-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class PARENTIZED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-parentized-atpt (&optional no-delimiters check)
  "Returns a copy of PARENTIZED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-parentized-atpt (&optional arg)
  "Deletes PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-delete 'parentized arg (called-interactively-p 'any)))

(defun ar-delete-parentized-in-region (beg end)
  "Deletes PARENTIZED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'parentized beg end (called-interactively-p 'any)))

(defun ar-blok-parentized-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around parentized.
  Returns blok or nil if no PARENTIZED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-parentized-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around parentized at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-parentized-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-parentized-atpt (&optional no-delimiters check)
  "Puts doubled slashes around PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-parentized-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-parentized-atpt (&optional no-delimiters check)
  "Provides double backticks around PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-parentized-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-parentized-atpt (&optional no-delimiters check)
  "Comments PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-comment 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-parentized-atpt (&optional no-delimiters check)
  "Put a comma after PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-parentized-atpt (&optional no-delimiters check)
  "Put a singlequote before PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-quote 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-parentized-atpt ()
  "Marks PARENTIZED at point if any. "
  (interactive)
  (ar-th-mark 'parentized))

(defun ar-hide-parentized-atpt ()
  "Hides PARENTIZED at point. "
  (interactive)
  (ar-th-hide 'parentized))

(defun ar-show-parentized-atpt ()
  "Shows hidden PARENTIZED at point. "
  (interactive)
  (ar-th-show 'parentized))

(defun ar-hide-show-parentized-atpt ()
  "Alternatively hides or shows PARENTIZED at point. "
  (interactive)
  (ar-th-hide-show 'parentized))

(defun ar-highlight-parentized-atpt-mode (&optional no-delimiters check)
  "Toggles parentized-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-parentized-atpt (&optional no-delimiters check)
  "Kills PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-kill 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-parentized-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-parentized-atpt (&optional no-delimiters check)
  "Separates PARENTIZED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-parentized-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around parentized. "
  (interactive "*P")
  (ar-th-triplequotedq 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-parentized-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around parentized. "
  (interactive "*P")
  (ar-th-triplequotesq 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-parentized-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'parentized (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-parentized-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'parentized (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-parentized-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'parentized n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-parentized-atpt (&optional no-delimiters check)
  "Put underscore char around PARENTIZED. "
  (interactive "*P")
  (ar-th-underscore 'parentized (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-parentized-whitespace-atpt 'ar-whitespace-parentized-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-parentized-atpt (&optional no-delimiters check)
;;   "Put whitespace char around PARENTIZED. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'parentized nil t))

(defun ar-forward-parentized-atpt (&optional arg)
  "Moves forward over PARENTIZED at point if any, does nothing otherwise.
Returns end position of PARENTIZED "
  (interactive "p")
  (ar-th-forward 'parentized arg (called-interactively-p 'any)))

(defun ar-backward-parentized-atpt (&optional arg)
  "Moves backward over PARENTIZED before point if any, does nothing otherwise.
Returns beginning position of PARENTIZED "
  (interactive "p")
  (ar-th-backward 'parentized arg (called-interactively-p 'any)))

(defun ar-transpose-parentized-atpt (&optional arg)
  "Transposes PARENTIZED with PARENTIZED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'parentized arg (called-interactively-p 'any)))

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

(defun ar-check-parentized-atpt ()
  "Return t if a PARENTIZED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-parentized-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-parentized-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

;; ar-thing-at-point-utils-delimiters-core: ar-paired-delimited-passiv: end

;; ar-thing-at-point-utils-delimiters-core list: start

(defun ar-list-atpt (&optional no-delimiters)
  "Returns list at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-list-atpt (&optional no-delimiters check)
  "Returns a list, borders of list if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-list-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position LIST at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-list-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of LIST at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-list-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class LIST at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-list-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class LIST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-list-p-atpt (&optional no-delimiters check)
  "Returns bounds of LIST at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-list-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class LIST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-list-atpt (&optional no-delimiters check)
  "Returns a copy of LIST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-list-atpt (&optional arg)
  "Deletes LIST at point if any. "
  (interactive "*P")
  (ar-th-delete 'list arg (called-interactively-p 'any)))

(defun ar-delete-list-in-region (beg end)
  "Deletes LIST at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'list beg end (called-interactively-p 'any)))

(defun ar-blok-list-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around list.
  Returns blok or nil if no LIST at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-list-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around list at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-list-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around LIST at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-list-atpt (&optional no-delimiters check)
  "Puts doubled slashes around LIST at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-list-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around LIST at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-list-atpt (&optional no-delimiters check)
  "Provides double backticks around LIST at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-list-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around LIST at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-list-atpt (&optional no-delimiters check)
  "Comments LIST at point if any. "
  (interactive "*P")
  (ar-th-comment 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-list-atpt (&optional no-delimiters check)
  "Put a comma after LIST at point if any. "
  (interactive "*P")
  (ar-th-commatize 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-list-atpt (&optional no-delimiters check)
  "Put a singlequote before LIST at point if any. "
  (interactive "*P")
  (ar-th-quote 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-list-atpt ()
  "Marks LIST at point if any. "
  (interactive)
  (ar-th-mark 'list))

(defun ar-hide-list-atpt ()
  "Hides LIST at point. "
  (interactive)
  (ar-th-hide 'list))

(defun ar-show-list-atpt ()
  "Shows hidden LIST at point. "
  (interactive)
  (ar-th-show 'list))

(defun ar-hide-show-list-atpt ()
  "Alternatively hides or shows LIST at point. "
  (interactive)
  (ar-th-hide-show 'list))

(defun ar-highlight-list-atpt-mode (&optional no-delimiters check)
  "Toggles list-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-list-atpt (&optional no-delimiters check)
  "Kills LIST at point if any. "
  (interactive "*P")
  (ar-th-kill 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-list-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-list-atpt (&optional no-delimiters check)
  "Separates LIST at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-list-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around list. "
  (interactive "*P")
  (ar-th-triplequotedq 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-list-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around list. "
  (interactive "*P")
  (ar-th-triplequotesq 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-list-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'list (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-list-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'list (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-list-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'list n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-list-atpt (&optional no-delimiters check)
  "Put underscore char around LIST. "
  (interactive "*P")
  (ar-th-underscore 'list (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-list-whitespace-atpt 'ar-whitespace-list-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-list-atpt (&optional no-delimiters check)
;;   "Put whitespace char around LIST. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'list nil t))

(defun ar-forward-list-atpt (&optional arg)
  "Moves forward over LIST at point if any, does nothing otherwise.
Returns end position of LIST "
  (interactive "p")
  (ar-th-forward 'list arg (called-interactively-p 'any)))

(defun ar-backward-list-atpt (&optional arg)
  "Moves backward over LIST before point if any, does nothing otherwise.
Returns beginning position of LIST "
  (interactive "p")
  (ar-th-backward 'list arg (called-interactively-p 'any)))

(defun ar-transpose-list-atpt (&optional arg)
  "Transposes LIST with LIST before point if any. "
  (interactive "*P")
  (ar-th-transpose 'list arg (called-interactively-p 'any)))

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

(defun ar-check-list-atpt ()
  "Return t if a LIST at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-list-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-list-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

;; ar-thing-at-point-utils-delimiters-core list: end

;; ar-thing-at-point-utils-delimiters-core: ar-atpt-expression-list: start

(defun ar-block-atpt (&optional no-delimiters)
  "Returns block at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-block-atpt (&optional no-delimiters check)
  "Returns a list, borders of block if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-block-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position BLOCK at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-block-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of BLOCK at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-block-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class BLOCK at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-block-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class BLOCK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-block-p-atpt (&optional no-delimiters check)
  "Returns bounds of BLOCK at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-block-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class BLOCK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-block-atpt (&optional no-delimiters check)
  "Returns a copy of BLOCK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-block-atpt (&optional arg)
  "Deletes BLOCK at point if any. "
  (interactive "*P")
  (ar-th-delete 'block arg (called-interactively-p 'any)))

(defun ar-delete-block-in-region (beg end)
  "Deletes BLOCK at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'block beg end (called-interactively-p 'any)))

(defun ar-blok-block-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around block.
  Returns blok or nil if no BLOCK at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-block-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around block at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-block-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around BLOCK at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-block-atpt (&optional no-delimiters check)
  "Puts doubled slashes around BLOCK at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-block-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around BLOCK at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-block-atpt (&optional no-delimiters check)
  "Provides double backticks around BLOCK at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-block-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around BLOCK at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-block-atpt (&optional no-delimiters check)
  "Comments BLOCK at point if any. "
  (interactive "*P")
  (ar-th-comment 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-block-atpt (&optional no-delimiters check)
  "Put a comma after BLOCK at point if any. "
  (interactive "*P")
  (ar-th-commatize 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-block-atpt (&optional no-delimiters check)
  "Put a singlequote before BLOCK at point if any. "
  (interactive "*P")
  (ar-th-quote 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-block-atpt ()
  "Marks BLOCK at point if any. "
  (interactive)
  (ar-th-mark 'block))

(defun ar-hide-block-atpt ()
  "Hides BLOCK at point. "
  (interactive)
  (ar-th-hide 'block))

(defun ar-show-block-atpt ()
  "Shows hidden BLOCK at point. "
  (interactive)
  (ar-th-show 'block))

(defun ar-hide-show-block-atpt ()
  "Alternatively hides or shows BLOCK at point. "
  (interactive)
  (ar-th-hide-show 'block))

(defun ar-highlight-block-atpt-mode (&optional no-delimiters check)
  "Toggles block-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-block-atpt (&optional no-delimiters check)
  "Kills BLOCK at point if any. "
  (interactive "*P")
  (ar-th-kill 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-block-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-block-atpt (&optional no-delimiters check)
  "Separates BLOCK at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-block-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around block. "
  (interactive "*P")
  (ar-th-triplequotedq 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-block-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around block. "
  (interactive "*P")
  (ar-th-triplequotesq 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-block-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'block (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-block-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'block (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-block-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'block n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-block-atpt (&optional no-delimiters check)
  "Put underscore char around BLOCK. "
  (interactive "*P")
  (ar-th-underscore 'block (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-block-whitespace-atpt 'ar-whitespace-block-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-block-atpt (&optional no-delimiters check)
;;   "Put whitespace char around BLOCK. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'block nil t))

(defun ar-forward-block-atpt (&optional arg)
  "Moves forward over BLOCK at point if any, does nothing otherwise.
Returns end position of BLOCK "
  (interactive "p")
  (ar-th-forward 'block arg (called-interactively-p 'any)))

(defun ar-backward-block-atpt (&optional arg)
  "Moves backward over BLOCK before point if any, does nothing otherwise.
Returns beginning position of BLOCK "
  (interactive "p")
  (ar-th-backward 'block arg (called-interactively-p 'any)))

(defun ar-transpose-block-atpt (&optional arg)
  "Transposes BLOCK with BLOCK before point if any. "
  (interactive "*P")
  (ar-th-transpose 'block arg (called-interactively-p 'any)))

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

(defun ar-check-block-atpt ()
  "Return t if a BLOCK at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-block-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-block-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-block-or-clause-atpt (&optional no-delimiters)
  "Returns block-or-clause at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-block-or-clause-atpt (&optional no-delimiters check)
  "Returns a list, borders of block-or-clause if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-block-or-clause-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position BLOCK-OR-CLAUSE at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-block-or-clause-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of BLOCK-OR-CLAUSE at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-block-or-clause-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class BLOCK-OR-CLAUSE at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-block-or-clause-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class BLOCK-OR-CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-block-or-clause-p-atpt (&optional no-delimiters check)
  "Returns bounds of BLOCK-OR-CLAUSE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-block-or-clause-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class BLOCK-OR-CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-block-or-clause-atpt (&optional no-delimiters check)
  "Returns a copy of BLOCK-OR-CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-block-or-clause-atpt (&optional arg)
  "Deletes BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-delete 'block-or-clause arg (called-interactively-p 'any)))

(defun ar-delete-block-or-clause-in-region (beg end)
  "Deletes BLOCK-OR-CLAUSE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'block-or-clause beg end (called-interactively-p 'any)))

(defun ar-blok-block-or-clause-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around block-or-clause.
  Returns blok or nil if no BLOCK-OR-CLAUSE at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-block-or-clause-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around block-or-clause at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-block-or-clause-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-block-or-clause-atpt (&optional no-delimiters check)
  "Puts doubled slashes around BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-block-or-clause-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-block-or-clause-atpt (&optional no-delimiters check)
  "Provides double backticks around BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-block-or-clause-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-block-or-clause-atpt (&optional no-delimiters check)
  "Comments BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-comment 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-block-or-clause-atpt (&optional no-delimiters check)
  "Put a comma after BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-commatize 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-block-or-clause-atpt (&optional no-delimiters check)
  "Put a singlequote before BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-quote 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-block-or-clause-atpt ()
  "Marks BLOCK-OR-CLAUSE at point if any. "
  (interactive)
  (ar-th-mark 'block-or-clause))

(defun ar-hide-block-or-clause-atpt ()
  "Hides BLOCK-OR-CLAUSE at point. "
  (interactive)
  (ar-th-hide 'block-or-clause))

(defun ar-show-block-or-clause-atpt ()
  "Shows hidden BLOCK-OR-CLAUSE at point. "
  (interactive)
  (ar-th-show 'block-or-clause))

(defun ar-hide-show-block-or-clause-atpt ()
  "Alternatively hides or shows BLOCK-OR-CLAUSE at point. "
  (interactive)
  (ar-th-hide-show 'block-or-clause))

(defun ar-highlight-block-or-clause-atpt-mode (&optional no-delimiters check)
  "Toggles block-or-clause-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-block-or-clause-atpt (&optional no-delimiters check)
  "Kills BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-kill 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-block-or-clause-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-block-or-clause-atpt (&optional no-delimiters check)
  "Separates BLOCK-OR-CLAUSE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-block-or-clause-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around block-or-clause. "
  (interactive "*P")
  (ar-th-triplequotedq 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-block-or-clause-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around block-or-clause. "
  (interactive "*P")
  (ar-th-triplequotesq 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-block-or-clause-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-block-or-clause-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-block-or-clause-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'block-or-clause n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-block-or-clause-atpt (&optional no-delimiters check)
  "Put underscore char around BLOCK-OR-CLAUSE. "
  (interactive "*P")
  (ar-th-underscore 'block-or-clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-block-or-clause-whitespace-atpt 'ar-whitespace-block-or-clause-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-block-or-clause-atpt (&optional no-delimiters check)
;;   "Put whitespace char around BLOCK-OR-CLAUSE. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'block-or-clause nil t))

(defun ar-forward-block-or-clause-atpt (&optional arg)
  "Moves forward over BLOCK-OR-CLAUSE at point if any, does nothing otherwise.
Returns end position of BLOCK-OR-CLAUSE "
  (interactive "p")
  (ar-th-forward 'block-or-clause arg (called-interactively-p 'any)))

(defun ar-backward-block-or-clause-atpt (&optional arg)
  "Moves backward over BLOCK-OR-CLAUSE before point if any, does nothing otherwise.
Returns beginning position of BLOCK-OR-CLAUSE "
  (interactive "p")
  (ar-th-backward 'block-or-clause arg (called-interactively-p 'any)))

(defun ar-transpose-block-or-clause-atpt (&optional arg)
  "Transposes BLOCK-OR-CLAUSE with BLOCK-OR-CLAUSE before point if any. "
  (interactive "*P")
  (ar-th-transpose 'block-or-clause arg (called-interactively-p 'any)))

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

(defun ar-check-block-or-clause-atpt ()
  "Return t if a BLOCK-OR-CLAUSE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-block-or-clause-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-block-or-clause-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-class-atpt (&optional no-delimiters)
  "Returns class at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-class-atpt (&optional no-delimiters check)
  "Returns a list, borders of class if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-class-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position CLASS at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-class-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of CLASS at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-class-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class CLASS at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-class-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-class-p-atpt (&optional no-delimiters check)
  "Returns bounds of CLASS at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-class-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-class-atpt (&optional no-delimiters check)
  "Returns a copy of CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-class-atpt (&optional arg)
  "Deletes CLASS at point if any. "
  (interactive "*P")
  (ar-th-delete 'class arg (called-interactively-p 'any)))

(defun ar-delete-class-in-region (beg end)
  "Deletes CLASS at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'class beg end (called-interactively-p 'any)))

(defun ar-blok-class-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around class.
  Returns blok or nil if no CLASS at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-class-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around class at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-class-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around CLASS at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-class-atpt (&optional no-delimiters check)
  "Puts doubled slashes around CLASS at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-class-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around CLASS at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-class-atpt (&optional no-delimiters check)
  "Provides double backticks around CLASS at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-class-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around CLASS at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-class-atpt (&optional no-delimiters check)
  "Comments CLASS at point if any. "
  (interactive "*P")
  (ar-th-comment 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-class-atpt (&optional no-delimiters check)
  "Put a comma after CLASS at point if any. "
  (interactive "*P")
  (ar-th-commatize 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-class-atpt (&optional no-delimiters check)
  "Put a singlequote before CLASS at point if any. "
  (interactive "*P")
  (ar-th-quote 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-class-atpt ()
  "Marks CLASS at point if any. "
  (interactive)
  (ar-th-mark 'class))

(defun ar-hide-class-atpt ()
  "Hides CLASS at point. "
  (interactive)
  (ar-th-hide 'class))

(defun ar-show-class-atpt ()
  "Shows hidden CLASS at point. "
  (interactive)
  (ar-th-show 'class))

(defun ar-hide-show-class-atpt ()
  "Alternatively hides or shows CLASS at point. "
  (interactive)
  (ar-th-hide-show 'class))

(defun ar-highlight-class-atpt-mode (&optional no-delimiters check)
  "Toggles class-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-class-atpt (&optional no-delimiters check)
  "Kills CLASS at point if any. "
  (interactive "*P")
  (ar-th-kill 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-class-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-class-atpt (&optional no-delimiters check)
  "Separates CLASS at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-class-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around class. "
  (interactive "*P")
  (ar-th-triplequotedq 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-class-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around class. "
  (interactive "*P")
  (ar-th-triplequotesq 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-class-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'class (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-class-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'class (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-class-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'class n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-class-atpt (&optional no-delimiters check)
  "Put underscore char around CLASS. "
  (interactive "*P")
  (ar-th-underscore 'class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-class-whitespace-atpt 'ar-whitespace-class-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-class-atpt (&optional no-delimiters check)
;;   "Put whitespace char around CLASS. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'class nil t))

(defun ar-forward-class-atpt (&optional arg)
  "Moves forward over CLASS at point if any, does nothing otherwise.
Returns end position of CLASS "
  (interactive "p")
  (ar-th-forward 'class arg (called-interactively-p 'any)))

(defun ar-backward-class-atpt (&optional arg)
  "Moves backward over CLASS before point if any, does nothing otherwise.
Returns beginning position of CLASS "
  (interactive "p")
  (ar-th-backward 'class arg (called-interactively-p 'any)))

(defun ar-transpose-class-atpt (&optional arg)
  "Transposes CLASS with CLASS before point if any. "
  (interactive "*P")
  (ar-th-transpose 'class arg (called-interactively-p 'any)))

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

(defun ar-check-class-atpt ()
  "Return t if a CLASS at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-class-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-class-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-clause-atpt (&optional no-delimiters)
  "Returns clause at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-clause-atpt (&optional no-delimiters check)
  "Returns a list, borders of clause if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-clause-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position CLAUSE at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-clause-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of CLAUSE at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-clause-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class CLAUSE at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-clause-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-clause-p-atpt (&optional no-delimiters check)
  "Returns bounds of CLAUSE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-clause-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-clause-atpt (&optional no-delimiters check)
  "Returns a copy of CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-clause-atpt (&optional arg)
  "Deletes CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-delete 'clause arg (called-interactively-p 'any)))

(defun ar-delete-clause-in-region (beg end)
  "Deletes CLAUSE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'clause beg end (called-interactively-p 'any)))

(defun ar-blok-clause-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around clause.
  Returns blok or nil if no CLAUSE at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-clause-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around clause at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-clause-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-clause-atpt (&optional no-delimiters check)
  "Puts doubled slashes around CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-clause-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-clause-atpt (&optional no-delimiters check)
  "Provides double backticks around CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-clause-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-clause-atpt (&optional no-delimiters check)
  "Comments CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-comment 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-clause-atpt (&optional no-delimiters check)
  "Put a comma after CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-commatize 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-clause-atpt (&optional no-delimiters check)
  "Put a singlequote before CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-quote 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-clause-atpt ()
  "Marks CLAUSE at point if any. "
  (interactive)
  (ar-th-mark 'clause))

(defun ar-hide-clause-atpt ()
  "Hides CLAUSE at point. "
  (interactive)
  (ar-th-hide 'clause))

(defun ar-show-clause-atpt ()
  "Shows hidden CLAUSE at point. "
  (interactive)
  (ar-th-show 'clause))

(defun ar-hide-show-clause-atpt ()
  "Alternatively hides or shows CLAUSE at point. "
  (interactive)
  (ar-th-hide-show 'clause))

(defun ar-highlight-clause-atpt-mode (&optional no-delimiters check)
  "Toggles clause-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-clause-atpt (&optional no-delimiters check)
  "Kills CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-kill 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-clause-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-clause-atpt (&optional no-delimiters check)
  "Separates CLAUSE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-clause-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around clause. "
  (interactive "*P")
  (ar-th-triplequotedq 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-clause-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around clause. "
  (interactive "*P")
  (ar-th-triplequotesq 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-clause-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'clause (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-clause-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'clause (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-clause-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'clause n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-clause-atpt (&optional no-delimiters check)
  "Put underscore char around CLAUSE. "
  (interactive "*P")
  (ar-th-underscore 'clause (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-clause-whitespace-atpt 'ar-whitespace-clause-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-clause-atpt (&optional no-delimiters check)
;;   "Put whitespace char around CLAUSE. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'clause nil t))

(defun ar-forward-clause-atpt (&optional arg)
  "Moves forward over CLAUSE at point if any, does nothing otherwise.
Returns end position of CLAUSE "
  (interactive "p")
  (ar-th-forward 'clause arg (called-interactively-p 'any)))

(defun ar-backward-clause-atpt (&optional arg)
  "Moves backward over CLAUSE before point if any, does nothing otherwise.
Returns beginning position of CLAUSE "
  (interactive "p")
  (ar-th-backward 'clause arg (called-interactively-p 'any)))

(defun ar-transpose-clause-atpt (&optional arg)
  "Transposes CLAUSE with CLAUSE before point if any. "
  (interactive "*P")
  (ar-th-transpose 'clause arg (called-interactively-p 'any)))

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

(defun ar-check-clause-atpt ()
  "Return t if a CLAUSE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-clause-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-clause-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-def-or-class-atpt (&optional no-delimiters)
  "Returns def-or-class at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-def-or-class-atpt (&optional no-delimiters check)
  "Returns a list, borders of def-or-class if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-def-or-class-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position DEF-OR-CLASS at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-def-or-class-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of DEF-OR-CLASS at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-def-or-class-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class DEF-OR-CLASS at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-def-or-class-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class DEF-OR-CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-def-or-class-p-atpt (&optional no-delimiters check)
  "Returns bounds of DEF-OR-CLASS at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-def-or-class-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class DEF-OR-CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-def-or-class-atpt (&optional no-delimiters check)
  "Returns a copy of DEF-OR-CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-def-or-class-atpt (&optional arg)
  "Deletes DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-delete 'def-or-class arg (called-interactively-p 'any)))

(defun ar-delete-def-or-class-in-region (beg end)
  "Deletes DEF-OR-CLASS at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'def-or-class beg end (called-interactively-p 'any)))

(defun ar-blok-def-or-class-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around def-or-class.
  Returns blok or nil if no DEF-OR-CLASS at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-def-or-class-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around def-or-class at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-def-or-class-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-def-or-class-atpt (&optional no-delimiters check)
  "Puts doubled slashes around DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-def-or-class-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-def-or-class-atpt (&optional no-delimiters check)
  "Provides double backticks around DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-def-or-class-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-def-or-class-atpt (&optional no-delimiters check)
  "Comments DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-comment 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-def-or-class-atpt (&optional no-delimiters check)
  "Put a comma after DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-commatize 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-def-or-class-atpt (&optional no-delimiters check)
  "Put a singlequote before DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-quote 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-def-or-class-atpt ()
  "Marks DEF-OR-CLASS at point if any. "
  (interactive)
  (ar-th-mark 'def-or-class))

(defun ar-hide-def-or-class-atpt ()
  "Hides DEF-OR-CLASS at point. "
  (interactive)
  (ar-th-hide 'def-or-class))

(defun ar-show-def-or-class-atpt ()
  "Shows hidden DEF-OR-CLASS at point. "
  (interactive)
  (ar-th-show 'def-or-class))

(defun ar-hide-show-def-or-class-atpt ()
  "Alternatively hides or shows DEF-OR-CLASS at point. "
  (interactive)
  (ar-th-hide-show 'def-or-class))

(defun ar-highlight-def-or-class-atpt-mode (&optional no-delimiters check)
  "Toggles def-or-class-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-def-or-class-atpt (&optional no-delimiters check)
  "Kills DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-kill 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-def-or-class-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-def-or-class-atpt (&optional no-delimiters check)
  "Separates DEF-OR-CLASS at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-def-or-class-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around def-or-class. "
  (interactive "*P")
  (ar-th-triplequotedq 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-def-or-class-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around def-or-class. "
  (interactive "*P")
  (ar-th-triplequotesq 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-def-or-class-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-def-or-class-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-def-or-class-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'def-or-class n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-def-or-class-atpt (&optional no-delimiters check)
  "Put underscore char around DEF-OR-CLASS. "
  (interactive "*P")
  (ar-th-underscore 'def-or-class (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-def-or-class-whitespace-atpt 'ar-whitespace-def-or-class-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-def-or-class-atpt (&optional no-delimiters check)
;;   "Put whitespace char around DEF-OR-CLASS. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'def-or-class nil t))

(defun ar-forward-def-or-class-atpt (&optional arg)
  "Moves forward over DEF-OR-CLASS at point if any, does nothing otherwise.
Returns end position of DEF-OR-CLASS "
  (interactive "p")
  (ar-th-forward 'def-or-class arg (called-interactively-p 'any)))

(defun ar-backward-def-or-class-atpt (&optional arg)
  "Moves backward over DEF-OR-CLASS before point if any, does nothing otherwise.
Returns beginning position of DEF-OR-CLASS "
  (interactive "p")
  (ar-th-backward 'def-or-class arg (called-interactively-p 'any)))

(defun ar-transpose-def-or-class-atpt (&optional arg)
  "Transposes DEF-OR-CLASS with DEF-OR-CLASS before point if any. "
  (interactive "*P")
  (ar-th-transpose 'def-or-class arg (called-interactively-p 'any)))

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

(defun ar-check-def-or-class-atpt ()
  "Return t if a DEF-OR-CLASS at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-def-or-class-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-def-or-class-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-def-atpt (&optional no-delimiters)
  "Returns def at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-def-atpt (&optional no-delimiters check)
  "Returns a list, borders of def if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-def-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position DEF at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-def-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of DEF at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-def-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class DEF at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-def-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class DEF at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-def-p-atpt (&optional no-delimiters check)
  "Returns bounds of DEF at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-def-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class DEF at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-def-atpt (&optional no-delimiters check)
  "Returns a copy of DEF at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-def-atpt (&optional arg)
  "Deletes DEF at point if any. "
  (interactive "*P")
  (ar-th-delete 'def arg (called-interactively-p 'any)))

(defun ar-delete-def-in-region (beg end)
  "Deletes DEF at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'def beg end (called-interactively-p 'any)))

(defun ar-blok-def-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around def.
  Returns blok or nil if no DEF at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-def-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around def at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-def-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around DEF at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-def-atpt (&optional no-delimiters check)
  "Puts doubled slashes around DEF at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-def-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around DEF at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-def-atpt (&optional no-delimiters check)
  "Provides double backticks around DEF at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-def-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around DEF at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-def-atpt (&optional no-delimiters check)
  "Comments DEF at point if any. "
  (interactive "*P")
  (ar-th-comment 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-def-atpt (&optional no-delimiters check)
  "Put a comma after DEF at point if any. "
  (interactive "*P")
  (ar-th-commatize 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-def-atpt (&optional no-delimiters check)
  "Put a singlequote before DEF at point if any. "
  (interactive "*P")
  (ar-th-quote 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-def-atpt ()
  "Marks DEF at point if any. "
  (interactive)
  (ar-th-mark 'def))

(defun ar-hide-def-atpt ()
  "Hides DEF at point. "
  (interactive)
  (ar-th-hide 'def))

(defun ar-show-def-atpt ()
  "Shows hidden DEF at point. "
  (interactive)
  (ar-th-show 'def))

(defun ar-hide-show-def-atpt ()
  "Alternatively hides or shows DEF at point. "
  (interactive)
  (ar-th-hide-show 'def))

(defun ar-highlight-def-atpt-mode (&optional no-delimiters check)
  "Toggles def-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-def-atpt (&optional no-delimiters check)
  "Kills DEF at point if any. "
  (interactive "*P")
  (ar-th-kill 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-def-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-def-atpt (&optional no-delimiters check)
  "Separates DEF at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-def-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around def. "
  (interactive "*P")
  (ar-th-triplequotedq 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-def-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around def. "
  (interactive "*P")
  (ar-th-triplequotesq 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-def-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'def (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-def-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'def (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-def-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'def n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-def-atpt (&optional no-delimiters check)
  "Put underscore char around DEF. "
  (interactive "*P")
  (ar-th-underscore 'def (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-def-whitespace-atpt 'ar-whitespace-def-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-def-atpt (&optional no-delimiters check)
;;   "Put whitespace char around DEF. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'def nil t))

(defun ar-forward-def-atpt (&optional arg)
  "Moves forward over DEF at point if any, does nothing otherwise.
Returns end position of DEF "
  (interactive "p")
  (ar-th-forward 'def arg (called-interactively-p 'any)))

(defun ar-backward-def-atpt (&optional arg)
  "Moves backward over DEF before point if any, does nothing otherwise.
Returns beginning position of DEF "
  (interactive "p")
  (ar-th-backward 'def arg (called-interactively-p 'any)))

(defun ar-transpose-def-atpt (&optional arg)
  "Transposes DEF with DEF before point if any. "
  (interactive "*P")
  (ar-th-transpose 'def arg (called-interactively-p 'any)))

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

(defun ar-check-def-atpt ()
  "Return t if a DEF at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-def-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-def-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-delimited-atpt (&optional no-delimiters)
  "Returns delimited at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-delimited-atpt (&optional no-delimiters check)
  "Returns a list, borders of delimited if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delimited-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position DELIMITED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delimited-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of DELIMITED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-delimited-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class DELIMITED at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-delimited-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-delimited-p-atpt (&optional no-delimiters check)
  "Returns bounds of DELIMITED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-delimited-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-delimited-atpt (&optional no-delimiters check)
  "Returns a copy of DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-delimited-atpt (&optional arg)
  "Deletes DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-delete 'delimited arg (called-interactively-p 'any)))

(defun ar-delete-delimited-in-region (beg end)
  "Deletes DELIMITED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'delimited beg end (called-interactively-p 'any)))

(defun ar-blok-delimited-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around delimited.
  Returns blok or nil if no DELIMITED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-delimited-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around delimited at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-delimited-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-delimited-atpt (&optional no-delimiters check)
  "Puts doubled slashes around DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-delimited-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-delimited-atpt (&optional no-delimiters check)
  "Provides double backticks around DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-delimited-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-delimited-atpt (&optional no-delimiters check)
  "Comments DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-comment 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-delimited-atpt (&optional no-delimiters check)
  "Put a comma after DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-delimited-atpt (&optional no-delimiters check)
  "Put a singlequote before DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-quote 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-delimited-atpt ()
  "Marks DELIMITED at point if any. "
  (interactive)
  (ar-th-mark 'delimited))

(defun ar-hide-delimited-atpt ()
  "Hides DELIMITED at point. "
  (interactive)
  (ar-th-hide 'delimited))

(defun ar-show-delimited-atpt ()
  "Shows hidden DELIMITED at point. "
  (interactive)
  (ar-th-show 'delimited))

(defun ar-hide-show-delimited-atpt ()
  "Alternatively hides or shows DELIMITED at point. "
  (interactive)
  (ar-th-hide-show 'delimited))

(defun ar-highlight-delimited-atpt-mode (&optional no-delimiters check)
  "Toggles delimited-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-delimited-atpt (&optional no-delimiters check)
  "Kills DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-kill 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-delimited-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-delimited-atpt (&optional no-delimiters check)
  "Separates DELIMITED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-delimited-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around delimited. "
  (interactive "*P")
  (ar-th-triplequotedq 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-delimited-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around delimited. "
  (interactive "*P")
  (ar-th-triplequotesq 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-delimited-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'delimited (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-delimited-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'delimited (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-delimited-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'delimited n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-delimited-atpt (&optional no-delimiters check)
  "Put underscore char around DELIMITED. "
  (interactive "*P")
  (ar-th-underscore 'delimited (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-delimited-whitespace-atpt 'ar-whitespace-delimited-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-delimited-atpt (&optional no-delimiters check)
;;   "Put whitespace char around DELIMITED. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'delimited nil t))

(defun ar-forward-delimited-atpt (&optional arg)
  "Moves forward over DELIMITED at point if any, does nothing otherwise.
Returns end position of DELIMITED "
  (interactive "p")
  (ar-th-forward 'delimited arg (called-interactively-p 'any)))

(defun ar-backward-delimited-atpt (&optional arg)
  "Moves backward over DELIMITED before point if any, does nothing otherwise.
Returns beginning position of DELIMITED "
  (interactive "p")
  (ar-th-backward 'delimited arg (called-interactively-p 'any)))

(defun ar-transpose-delimited-atpt (&optional arg)
  "Transposes DELIMITED with DELIMITED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'delimited arg (called-interactively-p 'any)))

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

(defun ar-check-delimited-atpt ()
  "Return t if a DELIMITED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-delimited-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-delimited-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-expression-atpt (&optional no-delimiters)
  "Returns expression at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-expression-atpt (&optional no-delimiters check)
  "Returns a list, borders of expression if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-expression-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position EXPRESSION at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-expression-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of EXPRESSION at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-expression-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class EXPRESSION at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-expression-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-expression-p-atpt (&optional no-delimiters check)
  "Returns bounds of EXPRESSION at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-expression-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-expression-atpt (&optional no-delimiters check)
  "Returns a copy of EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-expression-atpt (&optional arg)
  "Deletes EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-delete 'expression arg (called-interactively-p 'any)))

(defun ar-delete-expression-in-region (beg end)
  "Deletes EXPRESSION at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'expression beg end (called-interactively-p 'any)))

(defun ar-blok-expression-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around expression.
  Returns blok or nil if no EXPRESSION at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-expression-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around expression at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-expression-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-expression-atpt (&optional no-delimiters check)
  "Puts doubled slashes around EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-expression-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-expression-atpt (&optional no-delimiters check)
  "Provides double backticks around EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-expression-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-expression-atpt (&optional no-delimiters check)
  "Comments EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-comment 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-expression-atpt (&optional no-delimiters check)
  "Put a comma after EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-commatize 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-expression-atpt (&optional no-delimiters check)
  "Put a singlequote before EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-quote 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-expression-atpt ()
  "Marks EXPRESSION at point if any. "
  (interactive)
  (ar-th-mark 'expression))

(defun ar-hide-expression-atpt ()
  "Hides EXPRESSION at point. "
  (interactive)
  (ar-th-hide 'expression))

(defun ar-show-expression-atpt ()
  "Shows hidden EXPRESSION at point. "
  (interactive)
  (ar-th-show 'expression))

(defun ar-hide-show-expression-atpt ()
  "Alternatively hides or shows EXPRESSION at point. "
  (interactive)
  (ar-th-hide-show 'expression))

(defun ar-highlight-expression-atpt-mode (&optional no-delimiters check)
  "Toggles expression-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-expression-atpt (&optional no-delimiters check)
  "Kills EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-kill 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-expression-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-expression-atpt (&optional no-delimiters check)
  "Separates EXPRESSION at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-expression-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around expression. "
  (interactive "*P")
  (ar-th-triplequotedq 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-expression-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around expression. "
  (interactive "*P")
  (ar-th-triplequotesq 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-expression-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'expression (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-expression-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'expression (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-expression-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'expression n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-expression-atpt (&optional no-delimiters check)
  "Put underscore char around EXPRESSION. "
  (interactive "*P")
  (ar-th-underscore 'expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-expression-whitespace-atpt 'ar-whitespace-expression-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-expression-atpt (&optional no-delimiters check)
;;   "Put whitespace char around EXPRESSION. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'expression nil t))

(defun ar-forward-expression-atpt (&optional arg)
  "Moves forward over EXPRESSION at point if any, does nothing otherwise.
Returns end position of EXPRESSION "
  (interactive "p")
  (ar-th-forward 'expression arg (called-interactively-p 'any)))

(defun ar-backward-expression-atpt (&optional arg)
  "Moves backward over EXPRESSION before point if any, does nothing otherwise.
Returns beginning position of EXPRESSION "
  (interactive "p")
  (ar-th-backward 'expression arg (called-interactively-p 'any)))

(defun ar-transpose-expression-atpt (&optional arg)
  "Transposes EXPRESSION with EXPRESSION before point if any. "
  (interactive "*P")
  (ar-th-transpose 'expression arg (called-interactively-p 'any)))

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

(defun ar-check-expression-atpt ()
  "Return t if a EXPRESSION at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-expression-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-expression-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-partial-expression-atpt (&optional no-delimiters)
  "Returns partial-expression at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-partial-expression-atpt (&optional no-delimiters check)
  "Returns a list, borders of partial-expression if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-partial-expression-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position PARTIAL-EXPRESSION at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-partial-expression-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of PARTIAL-EXPRESSION at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-partial-expression-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class PARTIAL-EXPRESSION at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-partial-expression-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class PARTIAL-EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-partial-expression-p-atpt (&optional no-delimiters check)
  "Returns bounds of PARTIAL-EXPRESSION at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-partial-expression-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class PARTIAL-EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-partial-expression-atpt (&optional no-delimiters check)
  "Returns a copy of PARTIAL-EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-partial-expression-atpt (&optional arg)
  "Deletes PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-delete 'partial-expression arg (called-interactively-p 'any)))

(defun ar-delete-partial-expression-in-region (beg end)
  "Deletes PARTIAL-EXPRESSION at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'partial-expression beg end (called-interactively-p 'any)))

(defun ar-blok-partial-expression-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around partial-expression.
  Returns blok or nil if no PARTIAL-EXPRESSION at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-partial-expression-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around partial-expression at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-partial-expression-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-partial-expression-atpt (&optional no-delimiters check)
  "Puts doubled slashes around PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-partial-expression-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-partial-expression-atpt (&optional no-delimiters check)
  "Provides double backticks around PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-partial-expression-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-partial-expression-atpt (&optional no-delimiters check)
  "Comments PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-comment 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-partial-expression-atpt (&optional no-delimiters check)
  "Put a comma after PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-commatize 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-partial-expression-atpt (&optional no-delimiters check)
  "Put a singlequote before PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-quote 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-partial-expression-atpt ()
  "Marks PARTIAL-EXPRESSION at point if any. "
  (interactive)
  (ar-th-mark 'partial-expression))

(defun ar-hide-partial-expression-atpt ()
  "Hides PARTIAL-EXPRESSION at point. "
  (interactive)
  (ar-th-hide 'partial-expression))

(defun ar-show-partial-expression-atpt ()
  "Shows hidden PARTIAL-EXPRESSION at point. "
  (interactive)
  (ar-th-show 'partial-expression))

(defun ar-hide-show-partial-expression-atpt ()
  "Alternatively hides or shows PARTIAL-EXPRESSION at point. "
  (interactive)
  (ar-th-hide-show 'partial-expression))

(defun ar-highlight-partial-expression-atpt-mode (&optional no-delimiters check)
  "Toggles partial-expression-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-partial-expression-atpt (&optional no-delimiters check)
  "Kills PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-kill 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-partial-expression-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-partial-expression-atpt (&optional no-delimiters check)
  "Separates PARTIAL-EXPRESSION at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-partial-expression-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around partial-expression. "
  (interactive "*P")
  (ar-th-triplequotedq 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-partial-expression-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around partial-expression. "
  (interactive "*P")
  (ar-th-triplequotesq 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-partial-expression-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-partial-expression-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-partial-expression-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'partial-expression n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-partial-expression-atpt (&optional no-delimiters check)
  "Put underscore char around PARTIAL-EXPRESSION. "
  (interactive "*P")
  (ar-th-underscore 'partial-expression (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-partial-expression-whitespace-atpt 'ar-whitespace-partial-expression-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-partial-expression-atpt (&optional no-delimiters check)
;;   "Put whitespace char around PARTIAL-EXPRESSION. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'partial-expression nil t))

(defun ar-forward-partial-expression-atpt (&optional arg)
  "Moves forward over PARTIAL-EXPRESSION at point if any, does nothing otherwise.
Returns end position of PARTIAL-EXPRESSION "
  (interactive "p")
  (ar-th-forward 'partial-expression arg (called-interactively-p 'any)))

(defun ar-backward-partial-expression-atpt (&optional arg)
  "Moves backward over PARTIAL-EXPRESSION before point if any, does nothing otherwise.
Returns beginning position of PARTIAL-EXPRESSION "
  (interactive "p")
  (ar-th-backward 'partial-expression arg (called-interactively-p 'any)))

(defun ar-transpose-partial-expression-atpt (&optional arg)
  "Transposes PARTIAL-EXPRESSION with PARTIAL-EXPRESSION before point if any. "
  (interactive "*P")
  (ar-th-transpose 'partial-expression arg (called-interactively-p 'any)))

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

(defun ar-check-partial-expression-atpt ()
  "Return t if a PARTIAL-EXPRESSION at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-partial-expression-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-partial-expression-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-statement-atpt (&optional no-delimiters)
  "Returns statement at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-statement-atpt (&optional no-delimiters check)
  "Returns a list, borders of statement if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-statement-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position STATEMENT at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-statement-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of STATEMENT at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-statement-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class STATEMENT at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-statement-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class STATEMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-statement-p-atpt (&optional no-delimiters check)
  "Returns bounds of STATEMENT at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-statement-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class STATEMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-statement-atpt (&optional no-delimiters check)
  "Returns a copy of STATEMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-statement-atpt (&optional arg)
  "Deletes STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-delete 'statement arg (called-interactively-p 'any)))

(defun ar-delete-statement-in-region (beg end)
  "Deletes STATEMENT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'statement beg end (called-interactively-p 'any)))

(defun ar-blok-statement-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around statement.
  Returns blok or nil if no STATEMENT at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-statement-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around statement at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-statement-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-statement-atpt (&optional no-delimiters check)
  "Puts doubled slashes around STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-statement-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-statement-atpt (&optional no-delimiters check)
  "Provides double backticks around STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-statement-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-statement-atpt (&optional no-delimiters check)
  "Comments STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-comment 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-statement-atpt (&optional no-delimiters check)
  "Put a comma after STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-commatize 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-statement-atpt (&optional no-delimiters check)
  "Put a singlequote before STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-quote 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-statement-atpt ()
  "Marks STATEMENT at point if any. "
  (interactive)
  (ar-th-mark 'statement))

(defun ar-hide-statement-atpt ()
  "Hides STATEMENT at point. "
  (interactive)
  (ar-th-hide 'statement))

(defun ar-show-statement-atpt ()
  "Shows hidden STATEMENT at point. "
  (interactive)
  (ar-th-show 'statement))

(defun ar-hide-show-statement-atpt ()
  "Alternatively hides or shows STATEMENT at point. "
  (interactive)
  (ar-th-hide-show 'statement))

(defun ar-highlight-statement-atpt-mode (&optional no-delimiters check)
  "Toggles statement-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-statement-atpt (&optional no-delimiters check)
  "Kills STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-kill 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-statement-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-statement-atpt (&optional no-delimiters check)
  "Separates STATEMENT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-statement-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around statement. "
  (interactive "*P")
  (ar-th-triplequotedq 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-statement-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around statement. "
  (interactive "*P")
  (ar-th-triplequotesq 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-statement-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'statement (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-statement-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'statement (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-statement-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'statement n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-statement-atpt (&optional no-delimiters check)
  "Put underscore char around STATEMENT. "
  (interactive "*P")
  (ar-th-underscore 'statement (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-statement-whitespace-atpt 'ar-whitespace-statement-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-statement-atpt (&optional no-delimiters check)
;;   "Put whitespace char around STATEMENT. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'statement nil t))

(defun ar-forward-statement-atpt (&optional arg)
  "Moves forward over STATEMENT at point if any, does nothing otherwise.
Returns end position of STATEMENT "
  (interactive "p")
  (ar-th-forward 'statement arg (called-interactively-p 'any)))

(defun ar-backward-statement-atpt (&optional arg)
  "Moves backward over STATEMENT before point if any, does nothing otherwise.
Returns beginning position of STATEMENT "
  (interactive "p")
  (ar-th-backward 'statement arg (called-interactively-p 'any)))

(defun ar-transpose-statement-atpt (&optional arg)
  "Transposes STATEMENT with STATEMENT before point if any. "
  (interactive "*P")
  (ar-th-transpose 'statement arg (called-interactively-p 'any)))

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

(defun ar-check-statement-atpt ()
  "Return t if a STATEMENT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-statement-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-statement-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-string-atpt (&optional no-delimiters)
  "Returns string at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "P")
  (ar-th 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-bounds-of-string-atpt (&optional no-delimiters check)
  "Returns a list, borders of string if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-string-beginning-position-atpt (&optional no-delimiters check)
  "Returns a number, beginning position STRING at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-string-end-position-atpt (&optional no-delimiters check)
  "Returns a number, end position of STRING at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-beginning-of-string-atpt (&optional no-delimiters check)
  "Goto beginning of symbol or char-class STRING at point if any, nil otherwise. 

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-end-of-string-atpt (&optional no-delimiters check)
  "Goto end of symbol or char-class STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-in-string-p-atpt (&optional no-delimiters check)
  "Returns bounds of STRING at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-length-of-string-atpt (&optional no-delimiters check)
  "Returns beginning of symbol or char-class STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-copy-string-atpt (&optional no-delimiters check)
  "Returns a copy of STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-delete-string-atpt (&optional arg)
  "Deletes STRING at point if any. "
  (interactive "*P")
  (ar-th-delete 'string arg (called-interactively-p 'any)))

(defun ar-delete-string-in-region (beg end)
  "Deletes STRING at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'string beg end (called-interactively-p 'any)))

(defun ar-blok-string-atpt (&optional no-delimiters check)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around string.
  Returns blok or nil if no STRING at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-backslashparen-string-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around string at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslash-string-atpt (&optional no-delimiters check)
  "Puts doubled backslashes around STRING at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doubleslash-string-atpt (&optional no-delimiters check)
  "Puts doubled slashes around STRING at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebackslashparen-string-atpt (&optional no-delimiters check)
  "Provides doubleslashed parentheses around STRING at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-doublebacktick-string-atpt (&optional no-delimiters check)
  "Provides double backticks around STRING at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-slashparen-string-atpt (&optional no-delimiters check)
  "Provides slashed parentheses around STRING at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-comment-string-atpt (&optional no-delimiters check)
  "Comments STRING at point if any. "
  (interactive "*P")
  (ar-th-comment 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-commatize-string-atpt (&optional no-delimiters check)
  "Put a comma after STRING at point if any. "
  (interactive "*P")
  (ar-th-commatize 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-quote-string-atpt (&optional no-delimiters check)
  "Put a singlequote before STRING at point if any. "
  (interactive "*P")
  (ar-th-quote 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))


(defun ar-mark-string-atpt ()
  "Marks STRING at point if any. "
  (interactive)
  (ar-th-mark 'string))

(defun ar-hide-string-atpt ()
  "Hides STRING at point. "
  (interactive)
  (ar-th-hide 'string))

(defun ar-show-string-atpt ()
  "Shows hidden STRING at point. "
  (interactive)
  (ar-th-show 'string))

(defun ar-hide-show-string-atpt ()
  "Alternatively hides or shows STRING at point. "
  (interactive)
  (ar-th-hide-show 'string))

(defun ar-highlight-string-atpt-mode (&optional no-delimiters check)
  "Toggles string-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-kill-string-atpt (&optional no-delimiters check)
  "Kills STRING at point if any. "
  (interactive "*P")
  (ar-th-kill 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-leftrightsinglequote-string-atpt (&optional no-delimiters check)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-leftrightsinglequote 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-separate-string-atpt (&optional no-delimiters check)
  "Separates STRING at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotedq-string-atpt (&optional no-delimiters check)
  "Put triplequotes composed of doublequotes around string. "
  (interactive "*P")
  (ar-th-triplequotedq 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-triplequotesq-string-atpt (&optional no-delimiters check)
  "Put triplequotes composed of singlequotes around string. "
  (interactive "*P")
  (ar-th-triplequotesq 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

(defun ar-trim-string-atpt (&optional no-delimiters iact check)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'string (eq 4  (prefix-numeric-value no-delimiters)) iact check t t))

(defun ar-left-trim-string-atpt (&optional no-delimiters iact check)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'string (eq 4  (prefix-numeric-value no-delimiters)) iact check t nil))

(defun ar-right-trim-string-atpt (&optional no-delimiters iact check)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'string n (eq 4  (prefix-numeric-value no-delimiters)) iact check nil t))

(defun ar-underscore-string-atpt (&optional no-delimiters check)
  "Put underscore char around STRING. "
  (interactive "*P")
  (ar-th-underscore 'string (eq 4  (prefix-numeric-value no-delimiters)) (called-interactively-p 'any)))

;; (defalias 'ar-string-whitespace-atpt 'ar-whitespace-string-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-string-atpt (&optional no-delimiters check)
;;   "Put whitespace char around STRING. "
;;   (interactive "*P")
;;   (ar-th-whitespace 'string nil t))

(defun ar-forward-string-atpt (&optional arg)
  "Moves forward over STRING at point if any, does nothing otherwise.
Returns end position of STRING "
  (interactive "p")
  (ar-th-forward 'string arg (called-interactively-p 'any)))

(defun ar-backward-string-atpt (&optional arg)
  "Moves backward over STRING before point if any, does nothing otherwise.
Returns beginning position of STRING "
  (interactive "p")
  (ar-th-backward 'string arg (called-interactively-p 'any)))

(defun ar-transpose-string-atpt (&optional arg)
  "Transposes STRING with STRING before point if any. "
  (interactive "*P")
  (ar-th-transpose 'string arg (called-interactively-p 'any)))

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

(defun ar-check-string-atpt ()
  "Return t if a STRING at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-string-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-string-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

;; ar-thing-at-point-utils-delimiters-core: ar-atpt-expression-list: end


(defun ar-backslash-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with backslash(s)

  otherwise copy backslash(ed) at point.
  With NO-DELIMITERS, copy backslash(ed) without delimiters.
  With negative argument kill backslash(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'backslash no-delimiters (called-interactively-p 'any) check))

(defun ar-backtick-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with backtick(s)

  otherwise copy backtick(ed) at point.
  With NO-DELIMITERS, copy backtick(ed) without delimiters.
  With negative argument kill backtick(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'backtick no-delimiters (called-interactively-p 'any) check))

(defun ar-colon-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with colon(s)

  otherwise copy colon(ed) at point.
  With NO-DELIMITERS, copy colon(ed) without delimiters.
  With negative argument kill colon(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'colon no-delimiters (called-interactively-p 'any) check))

(defun ar-cross-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with cross(s)

  otherwise copy cross(ed) at point.
  With NO-DELIMITERS, copy cross(ed) without delimiters.
  With negative argument kill cross(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'cross no-delimiters (called-interactively-p 'any) check))

(defun ar-dollar-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with dollar(s)

  otherwise copy dollar(ed) at point.
  With NO-DELIMITERS, copy dollar(ed) without delimiters.
  With negative argument kill dollar(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'dollar no-delimiters (called-interactively-p 'any) check))

(defun ar-doublequote-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with doublequote(s)

  otherwise copy doublequote(ed) at point.
  With NO-DELIMITERS, copy doublequote(ed) without delimiters.
  With negative argument kill doublequote(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'doublequote no-delimiters (called-interactively-p 'any) check))

(defun ar-equalize-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with equalize(s)

  otherwise copy equalize(ed) at point.
  With NO-DELIMITERS, copy equalize(ed) without delimiters.
  With negative argument kill equalize(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'equalize no-delimiters (called-interactively-p 'any) check))

(defun ar-escape-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with escape(s)

  otherwise copy escape(ed) at point.
  With NO-DELIMITERS, copy escape(ed) without delimiters.
  With negative argument kill escape(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'escape no-delimiters (called-interactively-p 'any) check))

(defun ar-hash-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with hash(s)

  otherwise copy hash(ed) at point.
  With NO-DELIMITERS, copy hash(ed) without delimiters.
  With negative argument kill hash(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'hash no-delimiters (called-interactively-p 'any) check))

(defun ar-hyphen-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with hyphen(s)

  otherwise copy hyphen(ed) at point.
  With NO-DELIMITERS, copy hyphen(ed) without delimiters.
  With negative argument kill hyphen(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'hyphen no-delimiters (called-interactively-p 'any) check))

(defun ar-singlequote-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with singlequote(s)

  otherwise copy singlequote(ed) at point.
  With NO-DELIMITERS, copy singlequote(ed) without delimiters.
  With negative argument kill singlequote(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'singlequote no-delimiters (called-interactively-p 'any) check))

(defun ar-slash-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with slash(s)

  otherwise copy slash(ed) at point.
  With NO-DELIMITERS, copy slash(ed) without delimiters.
  With negative argument kill slash(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'slash no-delimiters (called-interactively-p 'any) check))

(defun ar-star-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with star(s)

  otherwise copy star(ed) at point.
  With NO-DELIMITERS, copy star(ed) without delimiters.
  With negative argument kill star(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'star no-delimiters (called-interactively-p 'any) check))

(defun ar-tild-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with tild(s)

  otherwise copy tild(ed) at point.
  With NO-DELIMITERS, copy tild(ed) without delimiters.
  With negative argument kill tild(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'tild no-delimiters (called-interactively-p 'any) check))

(defun ar-underscore-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with underscore(s)

  otherwise copy underscore(ed) at point.
  With NO-DELIMITERS, copy underscore(ed) without delimiters.
  With negative argument kill underscore(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'underscore no-delimiters (called-interactively-p 'any) check))

(defun ar-whitespace-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with whitespace(s)

  otherwise copy whitespace(ed) at point.
  With NO-DELIMITERS, copy whitespace(ed) without delimiters.
  With negative argument kill whitespace(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'whitespace no-delimiters (called-interactively-p 'any) check))

(defun ar-doubleslash-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with doubleslash(s)

  otherwise copy doubleslash(ed) at point.
  With NO-DELIMITERS, copy doubleslash(ed) without delimiters.
  With negative argument kill doubleslash(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'doubleslash no-delimiters (called-interactively-p 'any) check))

(defun ar-backslash-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with backslash(s)

  otherwise copy backslash(ed) at point.
  With NO-DELIMITERS, copy backslash(ed) without delimiters.
  With negative argument kill backslash(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'backslash no-delimiters (called-interactively-p 'any) check))

(defun ar-backtick-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with backtick(s)

  otherwise copy backtick(ed) at point.
  With NO-DELIMITERS, copy backtick(ed) without delimiters.
  With negative argument kill backtick(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'backtick no-delimiters (called-interactively-p 'any) check))

(defun ar-colon-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with colon(s)

  otherwise copy colon(ed) at point.
  With NO-DELIMITERS, copy colon(ed) without delimiters.
  With negative argument kill colon(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'colon no-delimiters (called-interactively-p 'any) check))

(defun ar-cross-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with cross(s)

  otherwise copy cross(ed) at point.
  With NO-DELIMITERS, copy cross(ed) without delimiters.
  With negative argument kill cross(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'cross no-delimiters (called-interactively-p 'any) check))

(defun ar-dollar-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with dollar(s)

  otherwise copy dollar(ed) at point.
  With NO-DELIMITERS, copy dollar(ed) without delimiters.
  With negative argument kill dollar(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'dollar no-delimiters (called-interactively-p 'any) check))

(defun ar-doublequote-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with doublequote(s)

  otherwise copy doublequote(ed) at point.
  With NO-DELIMITERS, copy doublequote(ed) without delimiters.
  With negative argument kill doublequote(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'doublequote no-delimiters (called-interactively-p 'any) check))

(defun ar-equalize-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with equalize(s)

  otherwise copy equalize(ed) at point.
  With NO-DELIMITERS, copy equalize(ed) without delimiters.
  With negative argument kill equalize(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'equalize no-delimiters (called-interactively-p 'any) check))

(defun ar-escape-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with escape(s)

  otherwise copy escape(ed) at point.
  With NO-DELIMITERS, copy escape(ed) without delimiters.
  With negative argument kill escape(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'escape no-delimiters (called-interactively-p 'any) check))

(defun ar-hash-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with hash(s)

  otherwise copy hash(ed) at point.
  With NO-DELIMITERS, copy hash(ed) without delimiters.
  With negative argument kill hash(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'hash no-delimiters (called-interactively-p 'any) check))

(defun ar-hyphen-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with hyphen(s)

  otherwise copy hyphen(ed) at point.
  With NO-DELIMITERS, copy hyphen(ed) without delimiters.
  With negative argument kill hyphen(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'hyphen no-delimiters (called-interactively-p 'any) check))

(defun ar-singlequote-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with singlequote(s)

  otherwise copy singlequote(ed) at point.
  With NO-DELIMITERS, copy singlequote(ed) without delimiters.
  With negative argument kill singlequote(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'singlequote no-delimiters (called-interactively-p 'any) check))

(defun ar-slash-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with slash(s)

  otherwise copy slash(ed) at point.
  With NO-DELIMITERS, copy slash(ed) without delimiters.
  With negative argument kill slash(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'slash no-delimiters (called-interactively-p 'any) check))

(defun ar-star-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with star(s)

  otherwise copy star(ed) at point.
  With NO-DELIMITERS, copy star(ed) without delimiters.
  With negative argument kill star(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'star no-delimiters (called-interactively-p 'any) check))

(defun ar-tild-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with tild(s)

  otherwise copy tild(ed) at point.
  With NO-DELIMITERS, copy tild(ed) without delimiters.
  With negative argument kill tild(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'tild no-delimiters (called-interactively-p 'any) check))

(defun ar-underscore-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with underscore(s)

  otherwise copy underscore(ed) at point.
  With NO-DELIMITERS, copy underscore(ed) without delimiters.
  With negative argument kill underscore(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'underscore no-delimiters (called-interactively-p 'any) check))

(defun ar-whitespace-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with whitespace(s)

  otherwise copy whitespace(ed) at point.
  With NO-DELIMITERS, copy whitespace(ed) without delimiters.
  With negative argument kill whitespace(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'whitespace no-delimiters (called-interactively-p 'any) check))

(defun ar-doubleslash-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with doubleslash(s)

  otherwise copy doubleslash(ed) at point.
  With NO-DELIMITERS, copy doubleslash(ed) without delimiters.
  With negative argument kill doubleslash(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'doubleslash no-delimiters (called-interactively-p 'any) check))

(defun ar-brace-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with brace(s)

  otherwise copy brace(ed) at point.
  With NO-DELIMITERS, copy brace(ed) without delimiters.
  With negative argument kill brace(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'brace no-delimiters (called-interactively-p 'any) check))

(defun ar-bracket-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with bracket(s)

  otherwise copy bracket(ed) at point.
  With NO-DELIMITERS, copy bracket(ed) without delimiters.
  With negative argument kill bracket(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'bracket no-delimiters (called-interactively-p 'any) check))

(defun ar-lesserangle-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with lesserangle(s)

  otherwise copy lesserangle(ed) at point.
  With NO-DELIMITERS, copy lesserangle(ed) without delimiters.
  With negative argument kill lesserangle(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'lesserangle no-delimiters (called-interactively-p 'any) check))

(defun ar-greaterangle-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with greaterangle(s)

  otherwise copy greaterangle(ed) at point.
  With NO-DELIMITERS, copy greaterangle(ed) without delimiters.
  With negative argument kill greaterangle(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'greaterangle no-delimiters (called-interactively-p 'any) check))

(defun ar-leftrightsinglequote-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with leftrightsinglequote(s)

  otherwise copy leftrightsinglequote(ed) at point.
  With NO-DELIMITERS, copy leftrightsinglequote(ed) without delimiters.
  With negative argument kill leftrightsinglequote(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'leftrightsinglequote no-delimiters (called-interactively-p 'any) check))

(defun ar-leftrightdoublequote-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with leftrightdoublequote(s)

  otherwise copy leftrightdoublequote(ed) at point.
  With NO-DELIMITERS, copy leftrightdoublequote(ed) without delimiters.
  With negative argument kill leftrightdoublequote(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'leftrightdoublequote no-delimiters (called-interactively-p 'any) check))

(defun ar-parentize-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with parentize(s)

  otherwise copy parentize(ed) at point.
  With NO-DELIMITERS, copy parentize(ed) without delimiters.
  With negative argument kill parentize(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'parentize no-delimiters (called-interactively-p 'any) check))

(defun ar-greateranglednested-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with greateranglednested(s)

  otherwise copy greateranglednested(ed) at point.
  With NO-DELIMITERS, copy greateranglednested(ed) without delimiters.
  With negative argument kill greateranglednested(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'greateranglednested no-delimiters (called-interactively-p 'any) check))

(defun ar-lesseranglednested-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with lesseranglednested(s)

  otherwise copy lesseranglednested(ed) at point.
  With NO-DELIMITERS, copy lesseranglednested(ed) without delimiters.
  With negative argument kill lesseranglednested(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'lesseranglednested no-delimiters (called-interactively-p 'any) check))

(defun ar-buffer-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with buffer(s)

  otherwise copy buffer(ed) at point.
  With NO-DELIMITERS, copy buffer(ed) without delimiters.
  With negative argument kill buffer(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'buffer no-delimiters (called-interactively-p 'any) check))

(defun ar-comment-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with comment(s)

  otherwise copy comment(ed) at point.
  With NO-DELIMITERS, copy comment(ed) without delimiters.
  With negative argument kill comment(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'comment no-delimiters (called-interactively-p 'any) check))

(defun ar-csv-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with csv(s)

  otherwise copy csv(ed) at point.
  With NO-DELIMITERS, copy csv(ed) without delimiters.
  With negative argument kill csv(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'csv no-delimiters (called-interactively-p 'any) check))

(defun ar-date-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with date(s)

  otherwise copy date(ed) at point.
  With NO-DELIMITERS, copy date(ed) without delimiters.
  With negative argument kill date(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'date no-delimiters (called-interactively-p 'any) check))

(defun ar-email-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with email(s)

  otherwise copy email(ed) at point.
  With NO-DELIMITERS, copy email(ed) without delimiters.
  With negative argument kill email(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'email no-delimiters (called-interactively-p 'any) check))

(defun ar-filename-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with filename(s)

  otherwise copy filename(ed) at point.
  With NO-DELIMITERS, copy filename(ed) without delimiters.
  With negative argument kill filename(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'filename no-delimiters (called-interactively-p 'any) check))

(defun ar-filenamenondirectory-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with filenamenondirectory(s)

  otherwise copy filenamenondirectory(ed) at point.
  With NO-DELIMITERS, copy filenamenondirectory(ed) without delimiters.
  With negative argument kill filenamenondirectory(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'filenamenondirectory no-delimiters (called-interactively-p 'any) check))

(defun ar-float-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with float(s)

  otherwise copy float(ed) at point.
  With NO-DELIMITERS, copy float(ed) without delimiters.
  With negative argument kill float(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'float no-delimiters (called-interactively-p 'any) check))

(defun ar-function-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with function(s)

  otherwise copy function(ed) at point.
  With NO-DELIMITERS, copy function(ed) without delimiters.
  With negative argument kill function(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'function no-delimiters (called-interactively-p 'any) check))

(defun ar-ip-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with ip(s)

  otherwise copy ip(ed) at point.
  With NO-DELIMITERS, copy ip(ed) without delimiters.
  With negative argument kill ip(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'ip no-delimiters (called-interactively-p 'any) check))

(defun ar-isbn-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with isbn(s)

  otherwise copy isbn(ed) at point.
  With NO-DELIMITERS, copy isbn(ed) without delimiters.
  With negative argument kill isbn(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'isbn no-delimiters (called-interactively-p 'any) check))

(defun ar-line-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with line(s)

  otherwise copy line(ed) at point.
  With NO-DELIMITERS, copy line(ed) without delimiters.
  With negative argument kill line(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'line no-delimiters (called-interactively-p 'any) check))

(defun ar-list-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with list(s)

  otherwise copy list(ed) at point.
  With NO-DELIMITERS, copy list(ed) without delimiters.
  With negative argument kill list(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'list no-delimiters (called-interactively-p 'any) check))

(defun ar-name-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with name(s)

  otherwise copy name(ed) at point.
  With NO-DELIMITERS, copy name(ed) without delimiters.
  With negative argument kill name(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'name no-delimiters (called-interactively-p 'any) check))

(defun ar-number-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with number(s)

  otherwise copy number(ed) at point.
  With NO-DELIMITERS, copy number(ed) without delimiters.
  With negative argument kill number(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'number no-delimiters (called-interactively-p 'any) check))

(defun ar-page-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with page(s)

  otherwise copy page(ed) at point.
  With NO-DELIMITERS, copy page(ed) without delimiters.
  With negative argument kill page(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'page no-delimiters (called-interactively-p 'any) check))

(defun ar-paragraph-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with paragraph(s)

  otherwise copy paragraph(ed) at point.
  With NO-DELIMITERS, copy paragraph(ed) without delimiters.
  With negative argument kill paragraph(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'paragraph no-delimiters (called-interactively-p 'any) check))

(defun ar-phone-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with phone(s)

  otherwise copy phone(ed) at point.
  With NO-DELIMITERS, copy phone(ed) without delimiters.
  With negative argument kill phone(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'phone no-delimiters (called-interactively-p 'any) check))

(defun ar-region-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with region(s)

  otherwise copy region(ed) at point.
  With NO-DELIMITERS, copy region(ed) without delimiters.
  With negative argument kill region(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'region no-delimiters (called-interactively-p 'any) check))

(defun ar-sentence-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with sentence(s)

  otherwise copy sentence(ed) at point.
  With NO-DELIMITERS, copy sentence(ed) without delimiters.
  With negative argument kill sentence(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'sentence no-delimiters (called-interactively-p 'any) check))

(defun ar-sexp-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with sexp(s)

  otherwise copy sexp(ed) at point.
  With NO-DELIMITERS, copy sexp(ed) without delimiters.
  With negative argument kill sexp(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'sexp no-delimiters (called-interactively-p 'any) check))

(defun ar-shstruct-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with shstruct(s)

  otherwise copy shstruct(ed) at point.
  With NO-DELIMITERS, copy shstruct(ed) without delimiters.
  With negative argument kill shstruct(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'shstruct no-delimiters (called-interactively-p 'any) check))

(defun ar-symbol-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with symbol(s)

  otherwise copy symbol(ed) at point.
  With NO-DELIMITERS, copy symbol(ed) without delimiters.
  With negative argument kill symbol(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'symbol no-delimiters (called-interactively-p 'any) check))

(defun ar-url-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with url(s)

  otherwise copy url(ed) at point.
  With NO-DELIMITERS, copy url(ed) without delimiters.
  With negative argument kill url(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'url no-delimiters (called-interactively-p 'any) check))

(defun ar-word-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with word(s)

  otherwise copy word(ed) at point.
  With NO-DELIMITERS, copy word(ed) without delimiters.
  With negative argument kill word(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'word no-delimiters (called-interactively-p 'any) check))

(defun ar-wordalphaonly-or-copy-atpt (&optional no-delimiters check)
  "If region is highlighted, provide THING at point with wordalphaonly(s)

  otherwise copy wordalphaonly(ed) at point.
  With NO-DELIMITERS, copy wordalphaonly(ed) without delimiters.
  With negative argument kill wordalphaonly(ed) at point. "
  (interactive "P")
  (ar-th-base-copy-or 'wordalphaonly no-delimiters (called-interactively-p 'any) check))

(defun emacs-batch-expression ()
  "Copy and highlight an expression starting with \"eval\" or \"load\". "
  (interactive)
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
