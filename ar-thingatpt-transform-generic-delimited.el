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

(defun ar-delimited2backslashed-atpt ()
  "Delimited2backslashed."
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?\\))

(defun ar-delimited2backticked-atpt ()
  "Delimited2backticked."
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?`))

(defun ar-delimited2braced-atpt ()
  "Delimited2braced."
  (interactive "*")
  (ar--transform-generic-delimited-atpt (cons ?{ ?})))

(defun ar-delimited2bracketed-atpt ()
  "Delimited2bracketed."
  (interactive "*")
  (ar--transform-generic-delimited-atpt (cons ?\[ ?\])))

(defun ar-delimited2dollared-atpt ()
  "Delimited2dollared."
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?$))

(defun ar-delimited2doublequoted-atpt ()
  "Delimited2doublequoted."
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?\"))

(defun ar-delimited2equalized-atpt ()
  "Delimited2equalized."
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?=))

(defun ar-delimited2hyphened-atpt ()
  "Delimited2hyphened."
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?-))

(defun ar-delimited2parentized-atpt ()
  "Delimited2parentized."
  (interactive "*")
  (ar--transform-generic-delimited-atpt (cons ?\( ?\))))

(defun ar-delimited2singlequoted-atpt ()
  "Delimited2singlequoted."
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?'))

(defun ar-delimited2slashed-atpt ()
  "Delimited2slashed."
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?\/))

(defun ar-delimited2underscored-atpt ()
  "Delimited2underscored."
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?_))

(defun ar-delimited2whitespaced-atpt ()
  "Delimited2whitespaced."
  (interactive "*")
  (ar--transform-generic-delimited-atpt ?\ ))

;; (global-set-key [(control c) (control d) ( )] 'ar-delimited2whitespaced-atpt)
;; (global-set-key [(control c) (control d) (})] 'ar-delimited2braced-atpt)

(provide 'ar-thingatpt-transform-generic-delimited)
;;; thingatpt-transform-generic-delimited.el ends here
