;;; ar-thingatpt-transform-delimited.el --- transform delimited forms -*- lexical-binding: t; -*-

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

(require 'ar-thingatpt-utils-core)
;;;###autoload
(defun ar-braced2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "symboled"))

;;;###autoload
(defun ar-braced2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "bracketed"))

;;;###autoload
(defun ar-braced2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "lesserangled"))

;;;###autoload
(defun ar-braced2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "greaterangled"))

;;;###autoload
(defun ar-braced2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "curvedsinglequoted"))

;;;###autoload
(defun ar-braced2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "curveddoublequoted"))

;;;###autoload
(defun ar-braced2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "parentized"))

;;;###autoload
(defun ar-symboled2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "braced"))

;;;###autoload
(defun ar-symboled2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "bracketed"))

;;;###autoload
(defun ar-symboled2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "lesserangled"))

;;;###autoload
(defun ar-symboled2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "greaterangled"))

;;;###autoload
(defun ar-symboled2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "curvedsinglequoted"))

;;;###autoload
(defun ar-symboled2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "curveddoublequoted"))

;;;###autoload
(defun ar-symboled2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "parentized"))

;;;###autoload
(defun ar-bracketed2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "braced"))

;;;###autoload
(defun ar-bracketed2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "symboled"))

;;;###autoload
(defun ar-bracketed2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "lesserangled"))

;;;###autoload
(defun ar-bracketed2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "greaterangled"))

;;;###autoload
(defun ar-bracketed2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "curvedsinglequoted"))

;;;###autoload
(defun ar-bracketed2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "curveddoublequoted"))

;;;###autoload
(defun ar-bracketed2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "parentized"))

;;;###autoload
(defun ar-lesserangled2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "braced"))

;;;###autoload
(defun ar-lesserangled2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "symboled"))

;;;###autoload
(defun ar-lesserangled2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "bracketed"))

;;;###autoload
(defun ar-lesserangled2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "greaterangled"))

;;;###autoload
(defun ar-lesserangled2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "curvedsinglequoted"))

;;;###autoload
(defun ar-lesserangled2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "curveddoublequoted"))

;;;###autoload
(defun ar-lesserangled2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "parentized"))

;;;###autoload
(defun ar-greaterangled2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "braced"))

;;;###autoload
(defun ar-greaterangled2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "symboled"))

;;;###autoload
(defun ar-greaterangled2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "bracketed"))

;;;###autoload
(defun ar-greaterangled2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "lesserangled"))

;;;###autoload
(defun ar-greaterangled2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "curvedsinglequoted"))

;;;###autoload
(defun ar-greaterangled2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "curveddoublequoted"))

;;;###autoload
(defun ar-greaterangled2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "parentized"))

;;;###autoload
(defun ar-curvedsinglequoted2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "braced"))

;;;###autoload
(defun ar-curvedsinglequoted2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "symboled"))

;;;###autoload
(defun ar-curvedsinglequoted2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "bracketed"))

;;;###autoload
(defun ar-curvedsinglequoted2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "lesserangled"))

;;;###autoload
(defun ar-curvedsinglequoted2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "greaterangled"))

;;;###autoload
(defun ar-curvedsinglequoted2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "curveddoublequoted"))

;;;###autoload
(defun ar-curvedsinglequoted2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "parentized"))

;;;###autoload
(defun ar-curveddoublequoted2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "braced"))

;;;###autoload
(defun ar-curveddoublequoted2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "symboled"))

;;;###autoload
(defun ar-curveddoublequoted2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "bracketed"))

;;;###autoload
(defun ar-curveddoublequoted2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "lesserangled"))

;;;###autoload
(defun ar-curveddoublequoted2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "greaterangled"))

;;;###autoload
(defun ar-curveddoublequoted2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "curvedsinglequoted"))

;;;###autoload
(defun ar-curveddoublequoted2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "parentized"))

;;;###autoload
(defun ar-parentized2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "braced"))

;;;###autoload
(defun ar-parentized2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "symboled"))

;;;###autoload
(defun ar-parentized2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "bracketed"))

;;;###autoload
(defun ar-parentized2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "lesserangled"))

;;;###autoload
(defun ar-parentized2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "greaterangled"))

;;;###autoload
(defun ar-parentized2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "curvedsinglequoted"))

;;;###autoload
(defun ar-parentized2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "curveddoublequoted"))

;;;###autoload
(defun ar-braced2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "backslashed"))

;;;###autoload
(defun ar-braced2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "backticked"))

;;;###autoload
(defun ar-braced2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "coloned"))

;;;###autoload
(defun ar-braced2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "dollared"))

;;;###autoload
(defun ar-braced2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "doublequoted"))

;;;###autoload
(defun ar-braced2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "equalized"))

;;;###autoload
(defun ar-braced2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "hyphened"))

;;;###autoload
(defun ar-braced2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "singlequoted"))

;;;###autoload
(defun ar-braced2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "slashed"))

;;;###autoload
(defun ar-braced2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "stared"))

;;;###autoload
(defun ar-braced2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "underscored"))

;;;###autoload
(defun ar-braced2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "braced" "whitespaced"))

;;;###autoload
(defun ar-symboled2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "backslashed"))

;;;###autoload
(defun ar-symboled2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "backticked"))

;;;###autoload
(defun ar-symboled2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "coloned"))

;;;###autoload
(defun ar-symboled2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "dollared"))

;;;###autoload
(defun ar-symboled2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "doublequoted"))

;;;###autoload
(defun ar-symboled2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "equalized"))

;;;###autoload
(defun ar-symboled2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "hyphened"))

;;;###autoload
(defun ar-symboled2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "singlequoted"))

;;;###autoload
(defun ar-symboled2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "slashed"))

;;;###autoload
(defun ar-symboled2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "stared"))

;;;###autoload
(defun ar-symboled2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "underscored"))

;;;###autoload
(defun ar-symboled2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "symboled" "whitespaced"))

;;;###autoload
(defun ar-bracketed2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "backslashed"))

;;;###autoload
(defun ar-bracketed2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "backticked"))

;;;###autoload
(defun ar-bracketed2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "coloned"))

;;;###autoload
(defun ar-bracketed2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "dollared"))

;;;###autoload
(defun ar-bracketed2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "doublequoted"))

;;;###autoload
(defun ar-bracketed2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "equalized"))

;;;###autoload
(defun ar-bracketed2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "hyphened"))

;;;###autoload
(defun ar-bracketed2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "singlequoted"))

;;;###autoload
(defun ar-bracketed2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "slashed"))

;;;###autoload
(defun ar-bracketed2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "stared"))

;;;###autoload
(defun ar-bracketed2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "underscored"))

;;;###autoload
(defun ar-bracketed2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "bracketed" "whitespaced"))

;;;###autoload
(defun ar-lesserangled2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "backslashed"))

;;;###autoload
(defun ar-lesserangled2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "backticked"))

;;;###autoload
(defun ar-lesserangled2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "coloned"))

;;;###autoload
(defun ar-lesserangled2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "dollared"))

;;;###autoload
(defun ar-lesserangled2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "doublequoted"))

;;;###autoload
(defun ar-lesserangled2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "equalized"))

;;;###autoload
(defun ar-lesserangled2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "hyphened"))

;;;###autoload
(defun ar-lesserangled2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "singlequoted"))

;;;###autoload
(defun ar-lesserangled2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "slashed"))

;;;###autoload
(defun ar-lesserangled2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "stared"))

;;;###autoload
(defun ar-lesserangled2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "underscored"))

;;;###autoload
(defun ar-lesserangled2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "lesserangled" "whitespaced"))

;;;###autoload
(defun ar-greaterangled2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "backslashed"))

;;;###autoload
(defun ar-greaterangled2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "backticked"))

;;;###autoload
(defun ar-greaterangled2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "coloned"))

;;;###autoload
(defun ar-greaterangled2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "dollared"))

;;;###autoload
(defun ar-greaterangled2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "doublequoted"))

;;;###autoload
(defun ar-greaterangled2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "equalized"))

;;;###autoload
(defun ar-greaterangled2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "hyphened"))

;;;###autoload
(defun ar-greaterangled2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "singlequoted"))

;;;###autoload
(defun ar-greaterangled2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "slashed"))

;;;###autoload
(defun ar-greaterangled2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "stared"))

;;;###autoload
(defun ar-greaterangled2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "underscored"))

;;;###autoload
(defun ar-greaterangled2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "greaterangled" "whitespaced"))

;;;###autoload
(defun ar-curvedsinglequoted2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "backslashed"))

;;;###autoload
(defun ar-curvedsinglequoted2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "backticked"))

;;;###autoload
(defun ar-curvedsinglequoted2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "coloned"))

;;;###autoload
(defun ar-curvedsinglequoted2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "dollared"))

;;;###autoload
(defun ar-curvedsinglequoted2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "doublequoted"))

;;;###autoload
(defun ar-curvedsinglequoted2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "equalized"))

;;;###autoload
(defun ar-curvedsinglequoted2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "hyphened"))

;;;###autoload
(defun ar-curvedsinglequoted2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "singlequoted"))

;;;###autoload
(defun ar-curvedsinglequoted2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "slashed"))

;;;###autoload
(defun ar-curvedsinglequoted2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "stared"))

;;;###autoload
(defun ar-curvedsinglequoted2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "underscored"))

;;;###autoload
(defun ar-curvedsinglequoted2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curvedsinglequoted" "whitespaced"))

;;;###autoload
(defun ar-curveddoublequoted2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "backslashed"))

;;;###autoload
(defun ar-curveddoublequoted2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "backticked"))

;;;###autoload
(defun ar-curveddoublequoted2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "coloned"))

;;;###autoload
(defun ar-curveddoublequoted2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "dollared"))

;;;###autoload
(defun ar-curveddoublequoted2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "doublequoted"))

;;;###autoload
(defun ar-curveddoublequoted2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "equalized"))

;;;###autoload
(defun ar-curveddoublequoted2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "hyphened"))

;;;###autoload
(defun ar-curveddoublequoted2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "singlequoted"))

;;;###autoload
(defun ar-curveddoublequoted2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "slashed"))

;;;###autoload
(defun ar-curveddoublequoted2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "stared"))

;;;###autoload
(defun ar-curveddoublequoted2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "underscored"))

;;;###autoload
(defun ar-curveddoublequoted2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "curveddoublequoted" "whitespaced"))

;;;###autoload
(defun ar-parentized2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "backslashed"))

;;;###autoload
(defun ar-parentized2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "backticked"))

;;;###autoload
(defun ar-parentized2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "coloned"))

;;;###autoload
(defun ar-parentized2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "dollared"))

;;;###autoload
(defun ar-parentized2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "doublequoted"))

;;;###autoload
(defun ar-parentized2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "equalized"))

;;;###autoload
(defun ar-parentized2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "hyphened"))

;;;###autoload
(defun ar-parentized2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "singlequoted"))

;;;###autoload
(defun ar-parentized2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "slashed"))

;;;###autoload
(defun ar-parentized2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "stared"))

;;;###autoload
(defun ar-parentized2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "underscored"))

;;;###autoload
(defun ar-parentized2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "parentized" "whitespaced"))

;;;###autoload
(defun ar-backslashed2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "braced"))

;;;###autoload
(defun ar-backslashed2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "symboled"))

;;;###autoload
(defun ar-backslashed2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "bracketed"))

;;;###autoload
(defun ar-backslashed2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "lesserangled"))

;;;###autoload
(defun ar-backslashed2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "greaterangled"))

;;;###autoload
(defun ar-backslashed2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "curvedsinglequoted"))

;;;###autoload
(defun ar-backslashed2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "curveddoublequoted"))

;;;###autoload
(defun ar-backslashed2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "parentized"))

;;;###autoload
(defun ar-backticked2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "braced"))

;;;###autoload
(defun ar-backticked2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "symboled"))

;;;###autoload
(defun ar-backticked2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "bracketed"))

;;;###autoload
(defun ar-backticked2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "lesserangled"))

;;;###autoload
(defun ar-backticked2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "greaterangled"))

;;;###autoload
(defun ar-backticked2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "curvedsinglequoted"))

;;;###autoload
(defun ar-backticked2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "curveddoublequoted"))

;;;###autoload
(defun ar-backticked2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "parentized"))

;;;###autoload
(defun ar-coloned2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "braced"))

;;;###autoload
(defun ar-coloned2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "symboled"))

;;;###autoload
(defun ar-coloned2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "bracketed"))

;;;###autoload
(defun ar-coloned2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "lesserangled"))

;;;###autoload
(defun ar-coloned2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "greaterangled"))

;;;###autoload
(defun ar-coloned2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "curvedsinglequoted"))

;;;###autoload
(defun ar-coloned2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "curveddoublequoted"))

;;;###autoload
(defun ar-coloned2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "parentized"))

;;;###autoload
(defun ar-dollared2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "braced"))

;;;###autoload
(defun ar-dollared2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "symboled"))

;;;###autoload
(defun ar-dollared2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "bracketed"))

;;;###autoload
(defun ar-dollared2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "lesserangled"))

;;;###autoload
(defun ar-dollared2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "greaterangled"))

;;;###autoload
(defun ar-dollared2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "curvedsinglequoted"))

;;;###autoload
(defun ar-dollared2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "curveddoublequoted"))

;;;###autoload
(defun ar-dollared2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "parentized"))

;;;###autoload
(defun ar-doublequoted2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "braced"))

;;;###autoload
(defun ar-doublequoted2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "symboled"))

;;;###autoload
(defun ar-doublequoted2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "bracketed"))

;;;###autoload
(defun ar-doublequoted2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "lesserangled"))

;;;###autoload
(defun ar-doublequoted2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "greaterangled"))

;;;###autoload
(defun ar-doublequoted2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "curvedsinglequoted"))

;;;###autoload
(defun ar-doublequoted2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "curveddoublequoted"))

;;;###autoload
(defun ar-doublequoted2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "parentized"))

;;;###autoload
(defun ar-equalized2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "braced"))

;;;###autoload
(defun ar-equalized2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "symboled"))

;;;###autoload
(defun ar-equalized2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "bracketed"))

;;;###autoload
(defun ar-equalized2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "lesserangled"))

;;;###autoload
(defun ar-equalized2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "greaterangled"))

;;;###autoload
(defun ar-equalized2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "curvedsinglequoted"))

;;;###autoload
(defun ar-equalized2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "curveddoublequoted"))

;;;###autoload
(defun ar-equalized2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "parentized"))

;;;###autoload
(defun ar-hyphened2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "braced"))

;;;###autoload
(defun ar-hyphened2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "symboled"))

;;;###autoload
(defun ar-hyphened2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "bracketed"))

;;;###autoload
(defun ar-hyphened2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "lesserangled"))

;;;###autoload
(defun ar-hyphened2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "greaterangled"))

;;;###autoload
(defun ar-hyphened2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "curvedsinglequoted"))

;;;###autoload
(defun ar-hyphened2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "curveddoublequoted"))

;;;###autoload
(defun ar-hyphened2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "parentized"))

;;;###autoload
(defun ar-singlequoted2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "braced"))

;;;###autoload
(defun ar-singlequoted2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "symboled"))

;;;###autoload
(defun ar-singlequoted2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "bracketed"))

;;;###autoload
(defun ar-singlequoted2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "lesserangled"))

;;;###autoload
(defun ar-singlequoted2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "greaterangled"))

;;;###autoload
(defun ar-singlequoted2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "curvedsinglequoted"))

;;;###autoload
(defun ar-singlequoted2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "curveddoublequoted"))

;;;###autoload
(defun ar-singlequoted2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "parentized"))

;;;###autoload
(defun ar-slashed2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "braced"))

;;;###autoload
(defun ar-slashed2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "symboled"))

;;;###autoload
(defun ar-slashed2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "bracketed"))

;;;###autoload
(defun ar-slashed2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "lesserangled"))

;;;###autoload
(defun ar-slashed2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "greaterangled"))

;;;###autoload
(defun ar-slashed2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "curvedsinglequoted"))

;;;###autoload
(defun ar-slashed2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "curveddoublequoted"))

;;;###autoload
(defun ar-slashed2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "parentized"))

;;;###autoload
(defun ar-stared2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "braced"))

;;;###autoload
(defun ar-stared2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "symboled"))

;;;###autoload
(defun ar-stared2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "bracketed"))

;;;###autoload
(defun ar-stared2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "lesserangled"))

;;;###autoload
(defun ar-stared2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "greaterangled"))

;;;###autoload
(defun ar-stared2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "curvedsinglequoted"))

;;;###autoload
(defun ar-stared2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "curveddoublequoted"))

;;;###autoload
(defun ar-stared2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "parentized"))

;;;###autoload
(defun ar-underscored2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "braced"))

;;;###autoload
(defun ar-underscored2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "symboled"))

;;;###autoload
(defun ar-underscored2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "bracketed"))

;;;###autoload
(defun ar-underscored2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "lesserangled"))

;;;###autoload
(defun ar-underscored2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "greaterangled"))

;;;###autoload
(defun ar-underscored2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "curvedsinglequoted"))

;;;###autoload
(defun ar-underscored2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "curveddoublequoted"))

;;;###autoload
(defun ar-underscored2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "parentized"))

;;;###autoload
(defun ar-whitespaced2braced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "braced"))

;;;###autoload
(defun ar-whitespaced2symboled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "symboled"))

;;;###autoload
(defun ar-whitespaced2bracketed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "bracketed"))

;;;###autoload
(defun ar-whitespaced2lesserangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "lesserangled"))

;;;###autoload
(defun ar-whitespaced2greaterangled-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "greaterangled"))

;;;###autoload
(defun ar-whitespaced2curvedsinglequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "curvedsinglequoted"))

;;;###autoload
(defun ar-whitespaced2curveddoublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "curveddoublequoted"))

;;;###autoload
(defun ar-whitespaced2parentized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "parentized"))

;;;###autoload
(defun ar-backslashed2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "backticked"))

;;;###autoload
(defun ar-backslashed2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "coloned"))

;;;###autoload
(defun ar-backslashed2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "dollared"))

;;;###autoload
(defun ar-backslashed2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "doublequoted"))

;;;###autoload
(defun ar-backslashed2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "equalized"))

;;;###autoload
(defun ar-backslashed2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "hyphened"))

;;;###autoload
(defun ar-backslashed2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "singlequoted"))

;;;###autoload
(defun ar-backslashed2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "slashed"))

;;;###autoload
(defun ar-backslashed2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "stared"))

;;;###autoload
(defun ar-backslashed2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "underscored"))

;;;###autoload
(defun ar-backslashed2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backslashed" "whitespaced"))

;;;###autoload
(defun ar-backticked2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "backslashed"))

;;;###autoload
(defun ar-backticked2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "coloned"))

;;;###autoload
(defun ar-backticked2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "dollared"))

;;;###autoload
(defun ar-backticked2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "doublequoted"))

;;;###autoload
(defun ar-backticked2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "equalized"))

;;;###autoload
(defun ar-backticked2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "hyphened"))

;;;###autoload
(defun ar-backticked2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "singlequoted"))

;;;###autoload
(defun ar-backticked2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "slashed"))

;;;###autoload
(defun ar-backticked2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "stared"))

;;;###autoload
(defun ar-backticked2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "underscored"))

;;;###autoload
(defun ar-backticked2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "backticked" "whitespaced"))

;;;###autoload
(defun ar-coloned2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "backslashed"))

;;;###autoload
(defun ar-coloned2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "backticked"))

;;;###autoload
(defun ar-coloned2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "dollared"))

;;;###autoload
(defun ar-coloned2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "doublequoted"))

;;;###autoload
(defun ar-coloned2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "equalized"))

;;;###autoload
(defun ar-coloned2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "hyphened"))

;;;###autoload
(defun ar-coloned2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "singlequoted"))

;;;###autoload
(defun ar-coloned2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "slashed"))

;;;###autoload
(defun ar-coloned2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "stared"))

;;;###autoload
(defun ar-coloned2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "underscored"))

;;;###autoload
(defun ar-coloned2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "coloned" "whitespaced"))

;;;###autoload
(defun ar-dollared2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "backslashed"))

;;;###autoload
(defun ar-dollared2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "backticked"))

;;;###autoload
(defun ar-dollared2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "coloned"))

;;;###autoload
(defun ar-dollared2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "doublequoted"))

;;;###autoload
(defun ar-dollared2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "equalized"))

;;;###autoload
(defun ar-dollared2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "hyphened"))

;;;###autoload
(defun ar-dollared2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "singlequoted"))

;;;###autoload
(defun ar-dollared2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "slashed"))

;;;###autoload
(defun ar-dollared2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "stared"))

;;;###autoload
(defun ar-dollared2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "underscored"))

;;;###autoload
(defun ar-dollared2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "dollared" "whitespaced"))

;;;###autoload
(defun ar-doublequoted2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "backslashed"))

;;;###autoload
(defun ar-doublequoted2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "backticked"))

;;;###autoload
(defun ar-doublequoted2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "coloned"))

;;;###autoload
(defun ar-doublequoted2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "dollared"))

;;;###autoload
(defun ar-doublequoted2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "equalized"))

;;;###autoload
(defun ar-doublequoted2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "hyphened"))

;;;###autoload
(defun ar-doublequoted2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "singlequoted"))

;;;###autoload
(defun ar-doublequoted2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "slashed"))

;;;###autoload
(defun ar-doublequoted2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "stared"))

;;;###autoload
(defun ar-doublequoted2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "underscored"))

;;;###autoload
(defun ar-doublequoted2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "doublequoted" "whitespaced"))

;;;###autoload
(defun ar-equalized2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "backslashed"))

;;;###autoload
(defun ar-equalized2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "backticked"))

;;;###autoload
(defun ar-equalized2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "coloned"))

;;;###autoload
(defun ar-equalized2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "dollared"))

;;;###autoload
(defun ar-equalized2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "doublequoted"))

;;;###autoload
(defun ar-equalized2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "hyphened"))

;;;###autoload
(defun ar-equalized2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "singlequoted"))

;;;###autoload
(defun ar-equalized2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "slashed"))

;;;###autoload
(defun ar-equalized2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "stared"))

;;;###autoload
(defun ar-equalized2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "underscored"))

;;;###autoload
(defun ar-equalized2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "equalized" "whitespaced"))

;;;###autoload
(defun ar-hyphened2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "backslashed"))

;;;###autoload
(defun ar-hyphened2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "backticked"))

;;;###autoload
(defun ar-hyphened2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "coloned"))

;;;###autoload
(defun ar-hyphened2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "dollared"))

;;;###autoload
(defun ar-hyphened2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "doublequoted"))

;;;###autoload
(defun ar-hyphened2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "equalized"))

;;;###autoload
(defun ar-hyphened2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "singlequoted"))

;;;###autoload
(defun ar-hyphened2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "slashed"))

;;;###autoload
(defun ar-hyphened2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "stared"))

;;;###autoload
(defun ar-hyphened2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "underscored"))

;;;###autoload
(defun ar-hyphened2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "hyphened" "whitespaced"))

;;;###autoload
(defun ar-singlequoted2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "backslashed"))

;;;###autoload
(defun ar-singlequoted2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "backticked"))

;;;###autoload
(defun ar-singlequoted2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "coloned"))

;;;###autoload
(defun ar-singlequoted2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "dollared"))

;;;###autoload
(defun ar-singlequoted2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "doublequoted"))

;;;###autoload
(defun ar-singlequoted2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "equalized"))

;;;###autoload
(defun ar-singlequoted2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "hyphened"))

;;;###autoload
(defun ar-singlequoted2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "slashed"))

;;;###autoload
(defun ar-singlequoted2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "stared"))

;;;###autoload
(defun ar-singlequoted2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "underscored"))

;;;###autoload
(defun ar-singlequoted2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "singlequoted" "whitespaced"))

;;;###autoload
(defun ar-slashed2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "backslashed"))

;;;###autoload
(defun ar-slashed2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "backticked"))

;;;###autoload
(defun ar-slashed2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "coloned"))

;;;###autoload
(defun ar-slashed2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "dollared"))

;;;###autoload
(defun ar-slashed2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "doublequoted"))

;;;###autoload
(defun ar-slashed2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "equalized"))

;;;###autoload
(defun ar-slashed2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "hyphened"))

;;;###autoload
(defun ar-slashed2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "singlequoted"))

;;;###autoload
(defun ar-slashed2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "stared"))

;;;###autoload
(defun ar-slashed2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "underscored"))

;;;###autoload
(defun ar-slashed2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "slashed" "whitespaced"))

;;;###autoload
(defun ar-stared2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "backslashed"))

;;;###autoload
(defun ar-stared2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "backticked"))

;;;###autoload
(defun ar-stared2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "coloned"))

;;;###autoload
(defun ar-stared2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "dollared"))

;;;###autoload
(defun ar-stared2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "doublequoted"))

;;;###autoload
(defun ar-stared2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "equalized"))

;;;###autoload
(defun ar-stared2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "hyphened"))

;;;###autoload
(defun ar-stared2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "singlequoted"))

;;;###autoload
(defun ar-stared2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "slashed"))

;;;###autoload
(defun ar-stared2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "underscored"))

;;;###autoload
(defun ar-stared2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "stared" "whitespaced"))

;;;###autoload
(defun ar-underscored2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "backslashed"))

;;;###autoload
(defun ar-underscored2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "backticked"))

;;;###autoload
(defun ar-underscored2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "coloned"))

;;;###autoload
(defun ar-underscored2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "dollared"))

;;;###autoload
(defun ar-underscored2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "doublequoted"))

;;;###autoload
(defun ar-underscored2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "equalized"))

;;;###autoload
(defun ar-underscored2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "hyphened"))

;;;###autoload
(defun ar-underscored2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "singlequoted"))

;;;###autoload
(defun ar-underscored2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "slashed"))

;;;###autoload
(defun ar-underscored2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "stared"))

;;;###autoload
(defun ar-underscored2whitespaced-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "underscored" "whitespaced"))

;;;###autoload
(defun ar-whitespaced2backslashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "backslashed"))

;;;###autoload
(defun ar-whitespaced2backticked-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "backticked"))

;;;###autoload
(defun ar-whitespaced2coloned-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "coloned"))

;;;###autoload
(defun ar-whitespaced2dollared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "dollared"))

;;;###autoload
(defun ar-whitespaced2doublequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "doublequoted"))

;;;###autoload
(defun ar-whitespaced2equalized-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "equalized"))

;;;###autoload
(defun ar-whitespaced2hyphened-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "hyphened"))

;;;###autoload
(defun ar-whitespaced2singlequoted-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "singlequoted"))

;;;###autoload
(defun ar-whitespaced2slashed-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "slashed"))

;;;###autoload
(defun ar-whitespaced2stared-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "stared"))

;;;###autoload
(defun ar-whitespaced2underscored-atpt ()
  (interactive "*")
  (ar--transform-delimited-intern "whitespaced" "underscored"))

(provide 'ar-thingatpt-transform-delimited)
;;; ar-thingatpt-transform-delimited.el ends here
