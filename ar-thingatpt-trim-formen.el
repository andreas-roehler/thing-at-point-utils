;;; ar-thingatpt-trim-formen.el --- Trim at point


;; Copyright (C) 2010-2024 Andreas Röhler, unless
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

;; ar-thingatpt-trim-formen-anlegen-intern ar-unpaired-delimited-passiv: start
(defun ar-trim-backslashed-atpt ()
  "Returns beginning position of ‘backslashed’ if succesful."
   (interactive "*") 
   (ar-th-trim 'backslashed))

(defun ar-trim-backticked-atpt ()
  "Returns beginning position of ‘backticked’ if succesful."
   (interactive "*") 
   (ar-th-trim 'backticked))

(defun ar-trim-coloned-atpt ()
  "Returns beginning position of ‘coloned’ if succesful."
   (interactive "*") 
   (ar-th-trim 'coloned))

(defun ar-trim-crossed-atpt ()
  "Returns beginning position of ‘crossed’ if succesful."
   (interactive "*") 
   (ar-th-trim 'crossed))

(defun ar-trim-dollared-atpt ()
  "Returns beginning position of ‘dollared’ if succesful."
   (interactive "*") 
   (ar-th-trim 'dollared))

(defun ar-trim-doublequoted-atpt ()
  "Returns beginning position of ‘doublequoted’ if succesful."
   (interactive "*") 
   (ar-th-trim 'doublequoted))

(defun ar-trim-equalized-atpt ()
  "Returns beginning position of ‘equalized’ if succesful."
   (interactive "*") 
   (ar-th-trim 'equalized))

(defun ar-trim-hashed-atpt ()
  "Returns beginning position of ‘hashed’ if succesful."
   (interactive "*") 
   (ar-th-trim 'hashed))

(defun ar-trim-hyphened-atpt ()
  "Returns beginning position of ‘hyphened’ if succesful."
   (interactive "*") 
   (ar-th-trim 'hyphened))

(defun ar-trim-piped-atpt ()
  "Returns beginning position of ‘piped’ if succesful."
   (interactive "*") 
   (ar-th-trim 'piped))

(defun ar-trim-singlequoted-atpt ()
  "Returns beginning position of ‘singlequoted’ if succesful."
   (interactive "*") 
   (ar-th-trim 'singlequoted))

(defun ar-trim-slashed-atpt ()
  "Returns beginning position of ‘slashed’ if succesful."
   (interactive "*") 
   (ar-th-trim 'slashed))

(defun ar-trim-stared-atpt ()
  "Returns beginning position of ‘stared’ if succesful."
   (interactive "*") 
   (ar-th-trim 'stared))

(defun ar-trim-tilded-atpt ()
  "Returns beginning position of ‘tilded’ if succesful."
   (interactive "*") 
   (ar-th-trim 'tilded))

(defun ar-trim-underscored-atpt ()
  "Returns beginning position of ‘underscored’ if succesful."
   (interactive "*") 
   (ar-th-trim 'underscored))

(defun ar-trim-whitespaced-atpt ()
  "Returns beginning position of ‘whitespaced’ if succesful."
   (interactive "*") 
   (ar-th-trim 'whitespaced))

;; ar-thingatpt-trim-formen-anlegen-intern ar-unpaired-delimited-passiv: end
;; ar-thingatpt-trim-formen-anlegen-intern ar-atpt-classes: start
(defun ar-trim-alnum-atpt ()
  "Returns beginning position of ‘alnum’ if succesful."
   (interactive "*") 
   (ar-th-trim 'alnum))

(defun ar-trim-alpha-atpt ()
  "Returns beginning position of ‘alpha’ if succesful."
   (interactive "*") 
   (ar-th-trim 'alpha))

(defun ar-trim-ascii-atpt ()
  "Returns beginning position of ‘ascii’ if succesful."
   (interactive "*") 
   (ar-th-trim 'ascii))

(defun ar-trim-blank-atpt ()
  "Returns beginning position of ‘blank’ if succesful."
   (interactive "*") 
   (ar-th-trim 'blank))

(defun ar-trim-cntrl-atpt ()
  "Returns beginning position of ‘cntrl’ if succesful."
   (interactive "*") 
   (ar-th-trim 'cntrl))

(defun ar-trim-digit-atpt ()
  "Returns beginning position of ‘digit’ if succesful."
   (interactive "*") 
   (ar-th-trim 'digit))

(defun ar-trim-graph-atpt ()
  "Returns beginning position of ‘graph’ if succesful."
   (interactive "*") 
   (ar-th-trim 'graph))

(defun ar-trim-lower-atpt ()
  "Returns beginning position of ‘lower’ if succesful."
   (interactive "*") 
   (ar-th-trim 'lower))

(defun ar-trim-nonascii-atpt ()
  "Returns beginning position of ‘nonascii’ if succesful."
   (interactive "*") 
   (ar-th-trim 'nonascii))

(defun ar-trim-print-atpt ()
  "Returns beginning position of ‘print’ if succesful."
   (interactive "*") 
   (ar-th-trim 'print))

(defun ar-trim-punct-atpt ()
  "Returns beginning position of ‘punct’ if succesful."
   (interactive "*") 
   (ar-th-trim 'punct))

(defun ar-trim-space-atpt ()
  "Returns beginning position of ‘space’ if succesful."
   (interactive "*") 
   (ar-th-trim 'space))

(defun ar-trim-upper-atpt ()
  "Returns beginning position of ‘upper’ if succesful."
   (interactive "*") 
   (ar-th-trim 'upper))

(defun ar-trim-xdigit-atpt ()
  "Returns beginning position of ‘xdigit’ if succesful."
   (interactive "*") 
   (ar-th-trim 'xdigit))

;; ar-thingatpt-trim-formen-anlegen-intern ar-atpt-classes: end

(provide 'ar-thingatpt-trim-formen)
;;; ar-thingatpt-trim-formen.el ends here



(provide 'ar-thingatpt-trim-formen)
;;; ar-thingatpt-trim-formen.el ends here
