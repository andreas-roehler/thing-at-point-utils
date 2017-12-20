;;; thingatpt-transform-generic-delimited.el --- Replace delimiters

;; Copyright (C) 2016  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun ar--transform-generic-delimited-atpt (replacement)
  (interactive "*")
  (let* ((bounds (ar-bounds-of-delimited-atpt))
	 ;; (startc (save-excursion (goto-char (car bounds))
	 ;; 			 (char-after)))
	 ;; (endc (save-excursion (goto-char (cdr bounds))
	 ;; 		       (char-before)))
	 (startnew (if (consp replacement)
		       (car replacement)
		     replacement))
	 (endnew (if (consp replacement)
		     (cdr replacement)
		   replacement)))
    (save-excursion
      (or (ignore-errors (goto-char (car bounds)))
	  (goto-char (caar bounds)))
      (delete-char 1)
      (insert startnew)
      (when (or
	     (ignore-errors (goto-char (cdr bounds)))
	     (goto-char (cdr (cadr bounds))))
	(delete-char -1)
	(insert endnew)))))

(defun ar-delimited2backslashed-atpt ()
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?\\))

(defun ar-delimited2backticked-atpt ()
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?`))

(defun ar-delimited2braced-atpt ()
  (interactive "*")
  (ar--transform-generic-delimited-atpt (cons ?{ ?})))

(defun ar-delimited2bracketed-atpt ()
  (interactive "*")
  (ar--transform-generic-delimited-atpt (cons ?\[ ?\])))

(defun ar-delimited2dollared-atpt ()
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?$))

(defun ar-delimited2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?\"))

(defun ar-delimited2equalized-atpt ()
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?=))

(defun ar-delimited2hyphened-atpt ()
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?-))

(defun ar-delimited2parentized-atpt ()
  (interactive "*")
  (ar--transform-generic-delimited-atpt (cons ?\( ?\))))

(defun ar-delimited2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?'))

(defun ar-delimited2slashed-atpt ()
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?\/))

(defun ar-delimited2underscored-atpt ()
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?_))

(defun ar-delimited2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?\ ))

;; (global-set-key [(control c) (control d) ( )] 'ar-delimited2whitespaced-atpt)
;; (global-set-key [(control c) (control d) (})] 'ar-delimited2braced-atpt)
(provide 'thingatpt-transform-generic-delimited)
;;; thingatpt-transform-generic-delimited.el ends here
