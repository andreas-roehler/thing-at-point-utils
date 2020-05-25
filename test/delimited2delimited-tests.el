;;; delimited2delimited-tests.el --- delimited to delimited tests -*- lexical-binding: t; -*-


;; Copyright (C) 2010-2020 Andreas Röhler, unless
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

;;; Commentary: Don't edit, file is generated

;;; Code:
(ert-deftest ar-braced2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-braced2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-braced2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-braced2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-braced2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-braced2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-braced2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-braced2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-braced2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-braced2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-braced2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-braced2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2whitespaced-atpt)
    (should (eq (char-before) ? ))))

(ert-deftest ar-bracketed2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-bracketed2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-bracketed2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-bracketed2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-bracketed2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-bracketed2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-bracketed2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-bracketed2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-bracketed2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-bracketed2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-bracketed2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-bracketed2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2whitespaced-atpt)
    (should (eq (char-before) ? ))))

(ert-deftest ar-lesserangled2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-lesserangled2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-lesserangled2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-lesserangled2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-lesserangled2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-lesserangled2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-lesserangled2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-lesserangled2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-lesserangled2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-lesserangled2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-lesserangled2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-lesserangled2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2whitespaced-atpt)
    (should (eq (char-before) ? ))))

(ert-deftest ar-greaterangled2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-greaterangled2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-greaterangled2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-greaterangled2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-greaterangled2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-greaterangled2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-greaterangled2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-greaterangled2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-greaterangled2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-greaterangled2stared-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-greaterangled2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-greaterangled2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2whitespaced-atpt)
    (should (eq (char-before) ? ))))

(ert-deftest ar-curvedsinglequoted2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-curvedsinglequoted2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-curvedsinglequoted2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-curvedsinglequoted2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-curvedsinglequoted2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-curvedsinglequoted2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-curvedsinglequoted2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-curvedsinglequoted2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-curvedsinglequoted2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-curvedsinglequoted2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-curvedsinglequoted2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-curvedsinglequoted2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-curvedsinglequoted2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-curvedsinglequoted2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-curvedsinglequoted2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-curvedsinglequoted2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-curvedsinglequoted2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-curvedsinglequoted2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-curvedsinglequoted2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-curvedsinglequoted2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-curvedsinglequoted2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-curvedsinglequoted2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-curvedsinglequoted2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-curvedsinglequoted2whitespaced-atpt)
    (should (eq (char-before) ? ))))

(ert-deftest ar-curveddoublequoted2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "“asdf”"
    (forward-char -2)
    (ar-curveddoublequoted2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-curveddoublequoted2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "“asdf”"
    (forward-char -2)
    (ar-curveddoublequoted2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-curveddoublequoted2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "“asdf”"
    (forward-char -2)
    (ar-curveddoublequoted2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-curveddoublequoted2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "“asdf”"
    (forward-char -2)
    (ar-curveddoublequoted2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-curveddoublequoted2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "“asdf”"
    (forward-char -2)
    (ar-curveddoublequoted2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-curveddoublequoted2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "“asdf”"
    (forward-char -2)
    (ar-curveddoublequoted2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-curveddoublequoted2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "“asdf”"
    (forward-char -2)
    (ar-curveddoublequoted2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-curveddoublequoted2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "“asdf”"
    (forward-char -2)
    (ar-curveddoublequoted2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-curveddoublequoted2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "“asdf”"
    (forward-char -2)
    (ar-curveddoublequoted2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-curveddoublequoted2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "“asdf”"
    (forward-char -2)
    (ar-curveddoublequoted2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-curveddoublequoted2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "“asdf”"
    (forward-char -2)
    (ar-curveddoublequoted2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-curveddoublequoted2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "“asdf”"
    (forward-char -2)
    (ar-curveddoublequoted2whitespaced-atpt)
    (should (eq (char-before) ? ))))

(ert-deftest ar-parentized2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-parentized2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-parentized2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-parentized2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-parentized2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-parentized2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-parentized2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-parentized2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-parentized2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-parentized2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-parentized2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-parentized2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2whitespaced-atpt)
    (should (eq (char-before) ? ))))

(ert-deftest ar-backslashed2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-backslashed2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-backslashed2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-backslashed2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-backslashed2curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2curvedsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-backslashed2curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2curveddoublequoted-atpt)
    (should (eq (char-before) ?”))))

(ert-deftest ar-backslashed2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-backticked2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-backticked2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-backticked2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-backticked2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-backticked2curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2curvedsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-backticked2curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2curveddoublequoted-atpt)
    (should (eq (char-before) ?”))))

(ert-deftest ar-backticked2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-coloned2braced-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-coloned2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-coloned2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-coloned2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-coloned2curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2curvedsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-coloned2curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2curveddoublequoted-atpt)
    (should (eq (char-before) ?”))))

(ert-deftest ar-coloned2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-dollared2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-dollared2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-dollared2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-dollared2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-dollared2curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2curvedsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-dollared2curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2curveddoublequoted-atpt)
    (should (eq (char-before) ?”))))

(ert-deftest ar-dollared2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-doublequoted2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-doublequoted2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-doublequoted2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-doublequoted2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-doublequoted2curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2curvedsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-doublequoted2curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2curveddoublequoted-atpt)
    (should (eq (char-before) ?”))))

(ert-deftest ar-doublequoted2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-equalized2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-equalized2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-equalized2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-equalized2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-equalized2curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2curvedsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-equalized2curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2curveddoublequoted-atpt)
    (should (eq (char-before) ?”))))

(ert-deftest ar-equalized2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-hyphened2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-hyphened2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-hyphened2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-hyphened2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-hyphened2curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2curvedsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-hyphened2curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2curveddoublequoted-atpt)
    (should (eq (char-before) ?”))))

(ert-deftest ar-hyphened2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-singlequoted2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-singlequoted2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-singlequoted2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-singlequoted2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-singlequoted2curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2curvedsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-singlequoted2curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2curveddoublequoted-atpt)
    (should (eq (char-before) ?”))))

(ert-deftest ar-singlequoted2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-slashed2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-slashed2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-slashed2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-slashed2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-slashed2curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2curvedsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-slashed2curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2curveddoublequoted-atpt)
    (should (eq (char-before) ?”))))

(ert-deftest ar-slashed2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-stared2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-stared2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-stared2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-stared2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-stared2curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2curvedsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-stared2curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2curveddoublequoted-atpt)
    (should (eq (char-before) ?”))))

(ert-deftest ar-stared2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-underscored2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-underscored2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-underscored2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-underscored2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-underscored2curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2curvedsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-underscored2curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2curveddoublequoted-atpt)
    (should (eq (char-before) ?”))))

(ert-deftest ar-underscored2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-whitespaced2braced-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-whitespaced2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-whitespaced2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-whitespaced2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-whitespaced2curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2curvedsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-whitespaced2curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2curveddoublequoted-atpt)
    (should (eq (char-before) ?”))))

(ert-deftest ar-whitespaced2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-braced2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-braced2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-braced2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-braced2curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2curvedsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-braced2curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2curveddoublequoted-atpt)
    (should (eq (char-before) ?”))))

(ert-deftest ar-braced2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-bracketed2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-bracketed2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-bracketed2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-bracketed2curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2curvedsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-bracketed2curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2curveddoublequoted-atpt)
    (should (eq (char-before) ?”))))

(ert-deftest ar-bracketed2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-lesserangled2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-lesserangled2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-lesserangled2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-lesserangled2curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2curvedsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-lesserangled2curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2curveddoublequoted-atpt)
    (should (eq (char-before) ?”))))

(ert-deftest ar-lesserangled2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-greaterangled2braced-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-greaterangled2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-greaterangled2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-greaterangled2curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2curvedsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-greaterangled2curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2curveddoublequoted-atpt)
    (should (eq (char-before) ?”))))

(ert-deftest ar-greaterangled2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-curvedsinglequoted2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-curvedsinglequoted2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-curvedsinglequoted2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-curvedsinglequoted2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-curvedsinglequoted2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-curvedsinglequoted2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-curvedsinglequoted2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-curvedsinglequoted2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-curvedsinglequoted2curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-curvedsinglequoted2curveddoublequoted-atpt)
    (should (eq (char-before) ?”))))

(ert-deftest ar-curvedsinglequoted2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-curvedsinglequoted2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-curveddoublequoted2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "“asdf”"
    (forward-char -2)
    (ar-curveddoublequoted2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-curveddoublequoted2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "“asdf”"
    (forward-char -2)
    (ar-curveddoublequoted2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-curveddoublequoted2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "“asdf”"
    (forward-char -2)
    (ar-curveddoublequoted2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-curveddoublequoted2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "“asdf”"
    (forward-char -2)
    (ar-curveddoublequoted2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-curveddoublequoted2curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "“asdf”"
    (forward-char -2)
    (ar-curveddoublequoted2curvedsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-curveddoublequoted2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "“asdf”"
    (forward-char -2)
    (ar-curveddoublequoted2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-parentized2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-parentized2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-parentized2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-parentized2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-parentized2curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2curvedsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-parentized2curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2curveddoublequoted-atpt)
    (should (eq (char-before) ?”))))

(ert-deftest ar-backslashed2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-backslashed2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-backslashed2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-backslashed2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-backslashed2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-backslashed2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-backslashed2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-backslashed2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-backslashed2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-backslashed2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-backslashed2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2whitespaced-atpt)
    (should (eq (char-before) ? ))))

(ert-deftest ar-backticked2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-backticked2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-backticked2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-backticked2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-backticked2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-backticked2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-backticked2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-backticked2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-backticked2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-backticked2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-backticked2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2whitespaced-atpt)
    (should (eq (char-before) ? ))))

(ert-deftest ar-coloned2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-coloned2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-coloned2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-coloned2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-coloned2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-coloned2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-coloned2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-coloned2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-coloned2stared-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-coloned2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-coloned2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2whitespaced-atpt)
    (should (eq (char-before) ? ))))

(ert-deftest ar-dollared2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-dollared2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-dollared2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-dollared2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-dollared2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-dollared2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-dollared2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-dollared2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-dollared2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-dollared2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-dollared2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2whitespaced-atpt)
    (should (eq (char-before) ? ))))

(ert-deftest ar-doublequoted2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-doublequoted2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-doublequoted2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-doublequoted2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-doublequoted2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-doublequoted2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-doublequoted2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-doublequoted2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-doublequoted2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-doublequoted2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-doublequoted2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2whitespaced-atpt)
    (should (eq (char-before) ? ))))

(ert-deftest ar-equalized2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-equalized2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-equalized2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-equalized2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-equalized2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-equalized2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-equalized2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-equalized2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-equalized2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-equalized2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-equalized2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2whitespaced-atpt)
    (should (eq (char-before) ? ))))

(ert-deftest ar-hyphened2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-hyphened2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-hyphened2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-hyphened2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-hyphened2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-hyphened2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-hyphened2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-hyphened2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-hyphened2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-hyphened2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-hyphened2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2whitespaced-atpt)
    (should (eq (char-before) ? ))))

(ert-deftest ar-singlequoted2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-singlequoted2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-singlequoted2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-singlequoted2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-singlequoted2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-singlequoted2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-singlequoted2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-singlequoted2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-singlequoted2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-singlequoted2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-singlequoted2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2whitespaced-atpt)
    (should (eq (char-before) ? ))))

(ert-deftest ar-slashed2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-slashed2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-slashed2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-slashed2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-slashed2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-slashed2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-slashed2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-slashed2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-slashed2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-slashed2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-slashed2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2whitespaced-atpt)
    (should (eq (char-before) ? ))))

(ert-deftest ar-stared2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-stared2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-stared2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-stared2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-stared2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-stared2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-stared2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-stared2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-stared2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-stared2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-stared2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2whitespaced-atpt)
    (should (eq (char-before) ? ))))

(ert-deftest ar-underscored2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-underscored2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-underscored2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-underscored2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-underscored2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-underscored2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-underscored2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-underscored2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-underscored2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-underscored2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-underscored2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2whitespaced-atpt)
    (should (eq (char-before) ? ))))

(ert-deftest ar-whitespaced2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-whitespaced2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-whitespaced2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-whitespaced2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-whitespaced2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-whitespaced2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-whitespaced2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-whitespaced2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-whitespaced2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-whitespaced2stared-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-whitespaced2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2underscored-atpt)
    (should (eq (char-before) ?_))))

(provide 'delimited2delimited-tests)
;;; delimited2delimited-tests.el ends here
