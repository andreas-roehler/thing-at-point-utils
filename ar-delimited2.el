;;; ar-delimited2.el --- From generic delimited to specific


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

;;; Commentary: Replace delimiters by the one mentioned in command

;;; Code:

(require 'thingatpt-utils-core)

(require 'thing-at-point-utils)
(require 'thingatpt-transform-delimited)

(defun delimited2--interned (thing &optional no-delimiters)
  (let* ((bounds (ar-th-bounds 'delimited no-delimiters (interactive-p)))
	 (beg (caar bounds))
	 (end (copy-marker (cdr (cadr bounds)))))
    (ar-th-trim 'delimited t t)
    (goto-char beg)
    (push-mark)
    (goto-char end)
    (funcall (car (read-from-string (concat "ar-" (format "%s" thing) "-region-atpt"))))))

(defun ar-delimited2backslashed (&optional no-delimiters)
  "Replace delimiters at point by backslasheds. "
  (interactive "*")
  (delimited2--intern 'backslashed no-delimiters))

(defun ar-delimited2backticked (&optional no-delimiters)
  "Replace delimiters at point by backtickeds. "
  (interactive "*")
  (delimited2--intern 'backticked no-delimiters))

(defun ar-delimited2coloned (&optional no-delimiters)
  "Replace delimiters at point by coloneds. "
  (interactive "*")
  (delimited2--intern 'coloned no-delimiters))

(defun ar-delimited2crossed (&optional no-delimiters)
  "Replace delimiters at point by crosseds. "
  (interactive "*")
  (delimited2--intern 'crossed no-delimiters))

(defun ar-delimited2dollared (&optional no-delimiters)
  "Replace delimiters at point by dollareds. "
  (interactive "*")
  (delimited2--intern 'dollared no-delimiters))

(defun ar-delimited2doublequoted (&optional no-delimiters)
  "Replace delimiters at point by doublequoteds. "
  (interactive "*")
  (delimited2--intern 'doublequoted no-delimiters))

(defun ar-delimited2equalized (&optional no-delimiters)
  "Replace delimiters at point by equalizeds. "
  (interactive "*")
  (delimited2--intern 'equalized no-delimiters))

(defun ar-delimited2hashed (&optional no-delimiters)
  "Replace delimiters at point by hasheds. "
  (interactive "*")
  (delimited2--intern 'hashed no-delimiters))

(defun ar-delimited2hyphened (&optional no-delimiters)
  "Replace delimiters at point by hypheneds. "
  (interactive "*")
  (delimited2--intern 'hyphened no-delimiters))

(defun ar-delimited2singlequoted (&optional no-delimiters)
  "Replace delimiters at point by singlequoteds. "
  (interactive "*")
  (delimited2--intern 'singlequoted no-delimiters))

(defun ar-delimited2slashed (&optional no-delimiters)
  "Replace delimiters at point by slasheds. "
  (interactive "*")
  (delimited2--intern 'slashed no-delimiters))

(defun ar-delimited2stared (&optional no-delimiters)
  "Replace delimiters at point by stareds. "
  (interactive "*")
  (delimited2--intern 'stared no-delimiters))

(defun ar-delimited2tilded (&optional no-delimiters)
  "Replace delimiters at point by tildeds. "
  (interactive "*")
  (delimited2--intern 'tilded no-delimiters))

(defun ar-delimited2underscored (&optional no-delimiters)
  "Replace delimiters at point by underscoreds. "
  (interactive "*")
  (delimited2--intern 'underscored no-delimiters))

(defun ar-delimited2whitespaced (&optional no-delimiters)
  "Replace delimiters at point by whitespaceds. "
  (interactive "*")
  (delimited2--intern 'whitespaced no-delimiters))

(defun ar-delimited2brace (&optional no-delimiters)
  "Replace delimiters at point by braces. "
  (interactive "*")
  (delimited2--intern 'brace no-delimiters))

(defun ar-delimited2bracket (&optional no-delimiters)
  "Replace delimiters at point by brackets. "
  (interactive "*")
  (delimited2--intern 'bracket no-delimiters))

(defun ar-delimited2lesserangle (&optional no-delimiters)
  "Replace delimiters at point by lesserangles. "
  (interactive "*")
  (delimited2--intern 'lesserangle no-delimiters))

(defun ar-delimited2greaterangle (&optional no-delimiters)
  "Replace delimiters at point by greaterangles. "
  (interactive "*")
  (delimited2--intern 'greaterangle no-delimiters))

(defun ar-delimited2leftrightsinglequote (&optional no-delimiters)
  "Replace delimiters at point by leftrightsinglequotes. "
  (interactive "*")
  (delimited2--intern 'leftrightsinglequote no-delimiters))

(defun ar-delimited2leftrightsinglequote (&optional no-delimiters)
  "Replace delimiters at point by leftrightsinglequotes. "
  (interactive "*")
  (delimited2--intern 'leftrightsinglequote no-delimiters))

(defun ar-delimited2parentize (&optional no-delimiters)
  "Replace delimiters at point by parentizes. "
  (interactive "*")
  (delimited2--intern 'parentize no-delimiters))

(provide 'ar-delimited2)
;;; ar-delimited.el ends here
