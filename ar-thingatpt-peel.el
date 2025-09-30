;;; ar-thingatpt-peel.el --- Peel at point -*- lexical-binding: t; -*-


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

(require 'ar-thingatpt-utils-core)

;; Peel functions start
(defun ar--raise-inner-list (beg end)
  (let* ((bounds (ar-bounds-of-list-atpt))
	 (inner-beg (or (ignore-errors (caar bounds))(car-save bounds)))
	 (inner-end
	  (cond ((ignore-errors (listp bounds))
		 (if (ignore-errors (numberp (cdr bounds)))
		     (copy-marker (cdr bounds))
		   (copy-marker (or (ignore-errors (cdr (cadr bounds)))(ignore-errors (cadr bounds))))))))
	 ;; (inner-end (cadr bounds))
	 (erg (buffer-substring inner-beg inner-end)))
    ;; (delete-region inner-beg inner-end)
    (goto-char beg)
    (delete-region beg end)
    (insert erg)))

(defun ar--raise-inner-sexp (beg end)
  (skip-chars-forward (concat "^" th-beg-delimiter ar-delimiters-atpt))
  (let* ((bounds (ar-bounds-of-delimited-atpt))
	 (inner-beg (or (ignore-errors (caar bounds))(car-safe bounds)))
	 (inner-end
	  (cond ((ignore-errors (listp bounds))
		 (or (ignore-errors (cdr (cadr bounds)))(ignore-errors (cadr bounds))
		     (cdr bounds)))))
	 (erg (buffer-substring inner-beg inner-end)))
    (goto-char beg)
    (delete-region beg end)
    (insert erg)))

(defun ar-peel-list-atpt (&optional arg)  
 "Remove list at point, preserve inner lists. "
  (interactive "*p")
  (ar-th-peel 'list arg))



(provide 'ar-thingatpt-peel)
;;; ar-thingatpt-peel.el ends here
