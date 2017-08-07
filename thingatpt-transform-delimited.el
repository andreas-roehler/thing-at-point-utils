;;; thingatpt-transform-delimited.el --- transform delimited forms

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

(require 'thingatpt-utils-core)
(defun ar-braced2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "bracketed"))

(defun ar-braced2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "lesserangled"))

(defun ar-braced2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "greaterangled"))

(defun ar-braced2leftrightsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "leftrightsinglequoted"))

(defun ar-braced2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "parentized"))

(defun ar-bracketed2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "braced"))

(defun ar-bracketed2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "lesserangled"))

(defun ar-bracketed2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "greaterangled"))

(defun ar-bracketed2leftrightsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "leftrightsinglequoted"))

(defun ar-bracketed2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "parentized"))

(defun ar-lesserangled2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "braced"))

(defun ar-lesserangled2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "bracketed"))

(defun ar-lesserangled2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "greaterangled"))

(defun ar-lesserangled2leftrightsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "leftrightsinglequoted"))

(defun ar-lesserangled2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "parentized"))

(defun ar-greaterangled2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "braced"))

(defun ar-greaterangled2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "bracketed"))

(defun ar-greaterangled2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "lesserangled"))

(defun ar-greaterangled2leftrightsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "leftrightsinglequoted"))

(defun ar-greaterangled2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "parentized"))

(defun ar-leftrightsinglequoted2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "leftrightsinglequoted" "braced"))

(defun ar-leftrightsinglequoted2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "leftrightsinglequoted" "bracketed"))

(defun ar-leftrightsinglequoted2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "leftrightsinglequoted" "lesserangled"))

(defun ar-leftrightsinglequoted2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "leftrightsinglequoted" "greaterangled"))

(defun ar-leftrightsinglequoted2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "leftrightsinglequoted" "parentized"))

(defun ar-parentized2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "braced"))

(defun ar-parentized2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "bracketed"))

(defun ar-parentized2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "lesserangled"))

(defun ar-parentized2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "greaterangled"))

(defun ar-parentized2leftrightsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "leftrightsinglequoted"))

(defun ar-braced2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "backslashed"))

(defun ar-braced2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "backticked"))

(defun ar-braced2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "coloned"))

(defun ar-braced2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "dollared"))

(defun ar-braced2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "doublequoted"))

(defun ar-braced2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "equalized"))

(defun ar-braced2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "hyphened"))

(defun ar-braced2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "singlequoted"))

(defun ar-braced2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "slashed"))

(defun ar-braced2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "stared"))

(defun ar-braced2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "underscored"))

(defun ar-braced2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "whitespaced"))

(defun ar-bracketed2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "backslashed"))

(defun ar-bracketed2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "backticked"))

(defun ar-bracketed2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "coloned"))

(defun ar-bracketed2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "dollared"))

(defun ar-bracketed2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "doublequoted"))

(defun ar-bracketed2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "equalized"))

(defun ar-bracketed2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "hyphened"))

(defun ar-bracketed2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "singlequoted"))

(defun ar-bracketed2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "slashed"))

(defun ar-bracketed2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "stared"))

(defun ar-bracketed2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "underscored"))

(defun ar-bracketed2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "whitespaced"))

(defun ar-lesserangled2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "backslashed"))

(defun ar-lesserangled2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "backticked"))

(defun ar-lesserangled2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "coloned"))

(defun ar-lesserangled2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "dollared"))

(defun ar-lesserangled2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "doublequoted"))

(defun ar-lesserangled2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "equalized"))

(defun ar-lesserangled2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "hyphened"))

(defun ar-lesserangled2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "singlequoted"))

(defun ar-lesserangled2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "slashed"))

(defun ar-lesserangled2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "stared"))

(defun ar-lesserangled2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "underscored"))

(defun ar-lesserangled2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "whitespaced"))

(defun ar-greaterangled2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "backslashed"))

(defun ar-greaterangled2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "backticked"))

(defun ar-greaterangled2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "coloned"))

(defun ar-greaterangled2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "dollared"))

(defun ar-greaterangled2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "doublequoted"))

(defun ar-greaterangled2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "equalized"))

(defun ar-greaterangled2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "hyphened"))

(defun ar-greaterangled2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "singlequoted"))

(defun ar-greaterangled2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "slashed"))

(defun ar-greaterangled2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "stared"))

(defun ar-greaterangled2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "underscored"))

(defun ar-greaterangled2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "whitespaced"))

(defun ar-leftrightsinglequoted2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "leftrightsinglequoted" "backslashed"))

(defun ar-leftrightsinglequoted2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "leftrightsinglequoted" "backticked"))

(defun ar-leftrightsinglequoted2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "leftrightsinglequoted" "coloned"))

(defun ar-leftrightsinglequoted2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "leftrightsinglequoted" "dollared"))

(defun ar-leftrightsinglequoted2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "leftrightsinglequoted" "doublequoted"))

(defun ar-leftrightsinglequoted2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "leftrightsinglequoted" "equalized"))

(defun ar-leftrightsinglequoted2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "leftrightsinglequoted" "hyphened"))

(defun ar-leftrightsinglequoted2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "leftrightsinglequoted" "singlequoted"))

(defun ar-leftrightsinglequoted2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "leftrightsinglequoted" "slashed"))

(defun ar-leftrightsinglequoted2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "leftrightsinglequoted" "stared"))

(defun ar-leftrightsinglequoted2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "leftrightsinglequoted" "underscored"))

(defun ar-leftrightsinglequoted2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "leftrightsinglequoted" "whitespaced"))

(defun ar-parentized2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "backslashed"))

(defun ar-parentized2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "backticked"))

(defun ar-parentized2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "coloned"))

(defun ar-parentized2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "dollared"))

(defun ar-parentized2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "doublequoted"))

(defun ar-parentized2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "equalized"))

(defun ar-parentized2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "hyphened"))

(defun ar-parentized2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "singlequoted"))

(defun ar-parentized2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "slashed"))

(defun ar-parentized2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "stared"))

(defun ar-parentized2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "underscored"))

(defun ar-parentized2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "whitespaced"))

(defun ar-backslashed2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "braced"))

(defun ar-backslashed2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "bracketed"))

(defun ar-backslashed2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "lesserangled"))

(defun ar-backslashed2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "greaterangled"))

(defun ar-backslashed2leftrightsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "leftrightsinglequoted"))

(defun ar-backslashed2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "parentized"))

(defun ar-backticked2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "braced"))

(defun ar-backticked2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "bracketed"))

(defun ar-backticked2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "lesserangled"))

(defun ar-backticked2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "greaterangled"))

(defun ar-backticked2leftrightsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "leftrightsinglequoted"))

(defun ar-backticked2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "parentized"))

(defun ar-coloned2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "braced"))

(defun ar-coloned2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "bracketed"))

(defun ar-coloned2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "lesserangled"))

(defun ar-coloned2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "greaterangled"))

(defun ar-coloned2leftrightsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "leftrightsinglequoted"))

(defun ar-coloned2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "parentized"))

(defun ar-dollared2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "braced"))

(defun ar-dollared2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "bracketed"))

(defun ar-dollared2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "lesserangled"))

(defun ar-dollared2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "greaterangled"))

(defun ar-dollared2leftrightsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "leftrightsinglequoted"))

(defun ar-dollared2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "parentized"))

(defun ar-doublequoted2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "braced"))

(defun ar-doublequoted2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "bracketed"))

(defun ar-doublequoted2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "lesserangled"))

(defun ar-doublequoted2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "greaterangled"))

(defun ar-doublequoted2leftrightsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "leftrightsinglequoted"))

(defun ar-doublequoted2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "parentized"))

(defun ar-equalized2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "braced"))

(defun ar-equalized2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "bracketed"))

(defun ar-equalized2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "lesserangled"))

(defun ar-equalized2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "greaterangled"))

(defun ar-equalized2leftrightsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "leftrightsinglequoted"))

(defun ar-equalized2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "parentized"))

(defun ar-hyphened2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "braced"))

(defun ar-hyphened2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "bracketed"))

(defun ar-hyphened2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "lesserangled"))

(defun ar-hyphened2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "greaterangled"))

(defun ar-hyphened2leftrightsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "leftrightsinglequoted"))

(defun ar-hyphened2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "parentized"))

(defun ar-singlequoted2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "braced"))

(defun ar-singlequoted2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "bracketed"))

(defun ar-singlequoted2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "lesserangled"))

(defun ar-singlequoted2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "greaterangled"))

(defun ar-singlequoted2leftrightsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "leftrightsinglequoted"))

(defun ar-singlequoted2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "parentized"))

(defun ar-slashed2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "braced"))

(defun ar-slashed2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "bracketed"))

(defun ar-slashed2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "lesserangled"))

(defun ar-slashed2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "greaterangled"))

(defun ar-slashed2leftrightsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "leftrightsinglequoted"))

(defun ar-slashed2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "parentized"))

(defun ar-stared2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "braced"))

(defun ar-stared2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "bracketed"))

(defun ar-stared2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "lesserangled"))

(defun ar-stared2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "greaterangled"))

(defun ar-stared2leftrightsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "leftrightsinglequoted"))

(defun ar-stared2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "parentized"))

(defun ar-underscored2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "braced"))

(defun ar-underscored2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "bracketed"))

(defun ar-underscored2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "lesserangled"))

(defun ar-underscored2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "greaterangled"))

(defun ar-underscored2leftrightsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "leftrightsinglequoted"))

(defun ar-underscored2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "parentized"))

(defun ar-whitespaced2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "braced"))

(defun ar-whitespaced2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "bracketed"))

(defun ar-whitespaced2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "lesserangled"))

(defun ar-whitespaced2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "greaterangled"))

(defun ar-whitespaced2leftrightsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "leftrightsinglequoted"))

(defun ar-whitespaced2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "parentized"))

(defun ar-backslashed2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "backticked"))

(defun ar-backslashed2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "coloned"))

(defun ar-backslashed2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "dollared"))

(defun ar-backslashed2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "doublequoted"))

(defun ar-backslashed2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "equalized"))

(defun ar-backslashed2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "hyphened"))

(defun ar-backslashed2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "singlequoted"))

(defun ar-backslashed2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "slashed"))

(defun ar-backslashed2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "stared"))

(defun ar-backslashed2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "underscored"))

(defun ar-backslashed2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "whitespaced"))

(defun ar-backticked2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "backslashed"))

(defun ar-backticked2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "coloned"))

(defun ar-backticked2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "dollared"))

(defun ar-backticked2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "doublequoted"))

(defun ar-backticked2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "equalized"))

(defun ar-backticked2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "hyphened"))

(defun ar-backticked2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "singlequoted"))

(defun ar-backticked2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "slashed"))

(defun ar-backticked2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "stared"))

(defun ar-backticked2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "underscored"))

(defun ar-backticked2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "whitespaced"))

(defun ar-coloned2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "backslashed"))

(defun ar-coloned2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "backticked"))

(defun ar-coloned2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "dollared"))

(defun ar-coloned2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "doublequoted"))

(defun ar-coloned2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "equalized"))

(defun ar-coloned2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "hyphened"))

(defun ar-coloned2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "singlequoted"))

(defun ar-coloned2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "slashed"))

(defun ar-coloned2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "stared"))

(defun ar-coloned2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "underscored"))

(defun ar-coloned2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "whitespaced"))

(defun ar-dollared2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "backslashed"))

(defun ar-dollared2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "backticked"))

(defun ar-dollared2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "coloned"))

(defun ar-dollared2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "doublequoted"))

(defun ar-dollared2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "equalized"))

(defun ar-dollared2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "hyphened"))

(defun ar-dollared2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "singlequoted"))

(defun ar-dollared2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "slashed"))

(defun ar-dollared2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "stared"))

(defun ar-dollared2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "underscored"))

(defun ar-dollared2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "whitespaced"))

(defun ar-doublequoted2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "backslashed"))

(defun ar-doublequoted2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "backticked"))

(defun ar-doublequoted2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "coloned"))

(defun ar-doublequoted2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "dollared"))

(defun ar-doublequoted2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "equalized"))

(defun ar-doublequoted2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "hyphened"))

(defun ar-doublequoted2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "singlequoted"))

(defun ar-doublequoted2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "slashed"))

(defun ar-doublequoted2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "stared"))

(defun ar-doublequoted2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "underscored"))

(defun ar-doublequoted2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "whitespaced"))

(defun ar-equalized2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "backslashed"))

(defun ar-equalized2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "backticked"))

(defun ar-equalized2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "coloned"))

(defun ar-equalized2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "dollared"))

(defun ar-equalized2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "doublequoted"))

(defun ar-equalized2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "hyphened"))

(defun ar-equalized2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "singlequoted"))

(defun ar-equalized2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "slashed"))

(defun ar-equalized2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "stared"))

(defun ar-equalized2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "underscored"))

(defun ar-equalized2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "whitespaced"))

(defun ar-hyphened2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "backslashed"))

(defun ar-hyphened2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "backticked"))

(defun ar-hyphened2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "coloned"))

(defun ar-hyphened2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "dollared"))

(defun ar-hyphened2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "doublequoted"))

(defun ar-hyphened2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "equalized"))

(defun ar-hyphened2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "singlequoted"))

(defun ar-hyphened2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "slashed"))

(defun ar-hyphened2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "stared"))

(defun ar-hyphened2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "underscored"))

(defun ar-hyphened2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "whitespaced"))

(defun ar-singlequoted2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "backslashed"))

(defun ar-singlequoted2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "backticked"))

(defun ar-singlequoted2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "coloned"))

(defun ar-singlequoted2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "dollared"))

(defun ar-singlequoted2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "doublequoted"))

(defun ar-singlequoted2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "equalized"))

(defun ar-singlequoted2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "hyphened"))

(defun ar-singlequoted2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "slashed"))

(defun ar-singlequoted2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "stared"))

(defun ar-singlequoted2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "underscored"))

(defun ar-singlequoted2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "whitespaced"))

(defun ar-slashed2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "backslashed"))

(defun ar-slashed2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "backticked"))

(defun ar-slashed2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "coloned"))

(defun ar-slashed2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "dollared"))

(defun ar-slashed2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "doublequoted"))

(defun ar-slashed2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "equalized"))

(defun ar-slashed2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "hyphened"))

(defun ar-slashed2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "singlequoted"))

(defun ar-slashed2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "stared"))

(defun ar-slashed2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "underscored"))

(defun ar-slashed2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "whitespaced"))

(defun ar-stared2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "backslashed"))

(defun ar-stared2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "backticked"))

(defun ar-stared2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "coloned"))

(defun ar-stared2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "dollared"))

(defun ar-stared2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "doublequoted"))

(defun ar-stared2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "equalized"))

(defun ar-stared2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "hyphened"))

(defun ar-stared2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "singlequoted"))

(defun ar-stared2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "slashed"))

(defun ar-stared2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "underscored"))

(defun ar-stared2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "whitespaced"))

(defun ar-underscored2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "backslashed"))

(defun ar-underscored2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "backticked"))

(defun ar-underscored2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "coloned"))

(defun ar-underscored2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "dollared"))

(defun ar-underscored2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "doublequoted"))

(defun ar-underscored2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "equalized"))

(defun ar-underscored2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "hyphened"))

(defun ar-underscored2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "singlequoted"))

(defun ar-underscored2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "slashed"))

(defun ar-underscored2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "stared"))

(defun ar-underscored2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "whitespaced"))

(defun ar-whitespaced2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "backslashed"))

(defun ar-whitespaced2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "backticked"))

(defun ar-whitespaced2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "coloned"))

(defun ar-whitespaced2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "dollared"))

(defun ar-whitespaced2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "doublequoted"))

(defun ar-whitespaced2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "equalized"))

(defun ar-whitespaced2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "hyphened"))

(defun ar-whitespaced2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "singlequoted"))

(defun ar-whitespaced2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "slashed"))

(defun ar-whitespaced2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "stared"))

(defun ar-whitespaced2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "underscored"))

(provide 'thingatpt-transform-delimited)
;;; thingatpt-transform-delimited.el ends here
