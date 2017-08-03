;;; delimited2delimited-tests.el --- delimited to delimited tests -*- lexical-binding: t; -*-


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

(ert-deftest ar-braced2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2crossed-atpt)
    (should (eq (char-before) ?+))))

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

(ert-deftest ar-braced2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2hashed-atpt)
    (should (eq (char-before) ?#))))

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

(ert-deftest ar-braced2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2tilded-atpt)
    (should (eq (char-before) ?~))))

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

(ert-deftest ar-bracketed2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2crossed-atpt)
    (should (eq (char-before) ?+))))

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

(ert-deftest ar-bracketed2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2hashed-atpt)
    (should (eq (char-before) ?#))))

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

(ert-deftest ar-bracketed2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2tilded-atpt)
    (should (eq (char-before) ?~))))

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

(ert-deftest ar-lesserangled2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2crossed-atpt)
    (should (eq (char-before) ?+))))

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

(ert-deftest ar-lesserangled2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2hashed-atpt)
    (should (eq (char-before) ?#))))

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

(ert-deftest ar-lesserangled2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2tilded-atpt)
    (should (eq (char-before) ?~))))

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

(ert-deftest ar-greaterangled2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2crossed-atpt)
    (should (eq (char-before) ?+))))

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

(ert-deftest ar-greaterangled2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2hashed-atpt)
    (should (eq (char-before) ?#))))

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

(ert-deftest ar-greaterangled2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2tilded-atpt)
    (should (eq (char-before) ?~))))

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

(ert-deftest ar-leftrightsinglequoted2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-leftrightsinglequoted2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-leftrightsinglequoted2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-leftrightsinglequoted2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2crossed-atpt)
    (should (eq (char-before) ?+))))

(ert-deftest ar-leftrightsinglequoted2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-leftrightsinglequoted2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-leftrightsinglequoted2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-leftrightsinglequoted2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2hashed-atpt)
    (should (eq (char-before) ?#))))

(ert-deftest ar-leftrightsinglequoted2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-leftrightsinglequoted2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-leftrightsinglequoted2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-leftrightsinglequoted2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-leftrightsinglequoted2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2tilded-atpt)
    (should (eq (char-before) ?~))))

(ert-deftest ar-leftrightsinglequoted2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-leftrightsinglequoted2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2whitespaced-atpt)
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

(ert-deftest ar-parentized2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2crossed-atpt)
    (should (eq (char-before) ?+))))

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

(ert-deftest ar-parentized2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2hashed-atpt)
    (should (eq (char-before) ?#))))

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

(ert-deftest ar-parentized2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2tilded-atpt)
    (should (eq (char-before) ?~))))

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

(ert-deftest ar-backslashed2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

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

(ert-deftest ar-backticked2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

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

(ert-deftest ar-coloned2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-coloned2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-crossed2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-crossed2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-crossed2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-crossed2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-crossed2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-crossed2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2parentized-atpt)
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

(ert-deftest ar-dollared2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

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

(ert-deftest ar-doublequoted2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

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

(ert-deftest ar-equalized2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-equalized2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-hashed2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-hashed2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-hashed2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-hashed2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-hashed2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-hashed2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2parentized-atpt)
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

(ert-deftest ar-hyphened2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

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

(ert-deftest ar-singlequoted2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

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

(ert-deftest ar-slashed2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

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

(ert-deftest ar-stared2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-stared2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-tilded2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-tilded2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-tilded2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-tilded2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-tilded2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-tilded2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2parentized-atpt)
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

(ert-deftest ar-underscored2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

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

(ert-deftest ar-whitespaced2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

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

(ert-deftest ar-braced2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "{asdf}"
    (forward-char -2)
    (ar-braced2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

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

(ert-deftest ar-bracketed2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "[asdf]"
    (forward-char -2)
    (ar-bracketed2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

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

(ert-deftest ar-lesserangled2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "<asdf>"
    (forward-char -2)
    (ar-lesserangled2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

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

(ert-deftest ar-greaterangled2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

(ert-deftest ar-greaterangled2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    ">asdf<"
    (forward-char -2)
    (ar-greaterangled2parentized-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-leftrightsinglequoted2braced-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2braced-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-leftrightsinglequoted2bracketed-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2bracketed-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-leftrightsinglequoted2lesserangled-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2lesserangled-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-leftrightsinglequoted2greaterangled-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2greaterangled-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-leftrightsinglequoted2parentized-atpt-test ()
  (ar-test-with-temp-buffer
    "‘asdf’"
    (forward-char -2)
    (ar-leftrightsinglequoted2parentized-atpt)
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

(ert-deftest ar-parentized2leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "(asdf)"
    (forward-char -2)
    (ar-parentized2leftrightsinglequoted-atpt)
    (should (eq (char-before) ?’))))

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

(ert-deftest ar-backslashed2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2crossed-atpt)
    (should (eq (char-before) ?+))))

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

(ert-deftest ar-backslashed2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2hashed-atpt)
    (should (eq (char-before) ?#))))

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

(ert-deftest ar-backslashed2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    "\\asdf\\"
    (forward-char -2)
    (ar-backslashed2tilded-atpt)
    (should (eq (char-before) ?~))))

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

(ert-deftest ar-backticked2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2crossed-atpt)
    (should (eq (char-before) ?+))))

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

(ert-deftest ar-backticked2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2hashed-atpt)
    (should (eq (char-before) ?#))))

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

(ert-deftest ar-backticked2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    "`asdf`"
    (forward-char -2)
    (ar-backticked2tilded-atpt)
    (should (eq (char-before) ?~))))

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

(ert-deftest ar-coloned2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2crossed-atpt)
    (should (eq (char-before) ?+))))

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

(ert-deftest ar-coloned2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2hashed-atpt)
    (should (eq (char-before) ?#))))

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

(ert-deftest ar-coloned2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    ":asdf:"
    (forward-char -2)
    (ar-coloned2tilded-atpt)
    (should (eq (char-before) ?~))))

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

(ert-deftest ar-crossed2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-crossed2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-crossed2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-crossed2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-crossed2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-crossed2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-crossed2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2hashed-atpt)
    (should (eq (char-before) ?#))))

(ert-deftest ar-crossed2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-crossed2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-crossed2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-crossed2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-crossed2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2tilded-atpt)
    (should (eq (char-before) ?~))))

(ert-deftest ar-crossed2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-crossed2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "+asdf+"
    (forward-char -2)
    (ar-crossed2whitespaced-atpt)
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

(ert-deftest ar-dollared2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2crossed-atpt)
    (should (eq (char-before) ?+))))

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

(ert-deftest ar-dollared2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2hashed-atpt)
    (should (eq (char-before) ?#))))

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

(ert-deftest ar-dollared2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    "$asdf$"
    (forward-char -2)
    (ar-dollared2tilded-atpt)
    (should (eq (char-before) ?~))))

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

(ert-deftest ar-doublequoted2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2crossed-atpt)
    (should (eq (char-before) ?+))))

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

(ert-deftest ar-doublequoted2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2hashed-atpt)
    (should (eq (char-before) ?#))))

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

(ert-deftest ar-doublequoted2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    "\"asdf\""
    (forward-char -2)
    (ar-doublequoted2tilded-atpt)
    (should (eq (char-before) ?~))))

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

(ert-deftest ar-equalized2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2crossed-atpt)
    (should (eq (char-before) ?+))))

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

(ert-deftest ar-equalized2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2hashed-atpt)
    (should (eq (char-before) ?#))))

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

(ert-deftest ar-equalized2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    "=asdf="
    (forward-char -2)
    (ar-equalized2tilded-atpt)
    (should (eq (char-before) ?~))))

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

(ert-deftest ar-hashed2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-hashed2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-hashed2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-hashed2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2crossed-atpt)
    (should (eq (char-before) ?+))))

(ert-deftest ar-hashed2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-hashed2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-hashed2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-hashed2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-hashed2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-hashed2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-hashed2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-hashed2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2tilded-atpt)
    (should (eq (char-before) ?~))))

(ert-deftest ar-hashed2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-hashed2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "#asdf#"
    (forward-char -2)
    (ar-hashed2whitespaced-atpt)
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

(ert-deftest ar-hyphened2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2crossed-atpt)
    (should (eq (char-before) ?+))))

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

(ert-deftest ar-hyphened2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2hashed-atpt)
    (should (eq (char-before) ?#))))

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

(ert-deftest ar-hyphened2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    "-asdf-"
    (forward-char -2)
    (ar-hyphened2tilded-atpt)
    (should (eq (char-before) ?~))))

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

(ert-deftest ar-singlequoted2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2crossed-atpt)
    (should (eq (char-before) ?+))))

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

(ert-deftest ar-singlequoted2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2hashed-atpt)
    (should (eq (char-before) ?#))))

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

(ert-deftest ar-singlequoted2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    "'asdf'"
    (forward-char -2)
    (ar-singlequoted2tilded-atpt)
    (should (eq (char-before) ?~))))

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

(ert-deftest ar-slashed2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2crossed-atpt)
    (should (eq (char-before) ?+))))

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

(ert-deftest ar-slashed2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2hashed-atpt)
    (should (eq (char-before) ?#))))

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

(ert-deftest ar-slashed2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    "/asdf/"
    (forward-char -2)
    (ar-slashed2tilded-atpt)
    (should (eq (char-before) ?~))))

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

(ert-deftest ar-stared2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2crossed-atpt)
    (should (eq (char-before) ?+))))

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

(ert-deftest ar-stared2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2hashed-atpt)
    (should (eq (char-before) ?#))))

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

(ert-deftest ar-stared2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    "*asdf*"
    (forward-char -2)
    (ar-stared2tilded-atpt)
    (should (eq (char-before) ?~))))

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

(ert-deftest ar-tilded2backslashed-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2backslashed-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-tilded2backticked-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2backticked-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-tilded2coloned-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2coloned-atpt)
    (should (eq (char-before) ?:))))

(ert-deftest ar-tilded2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2crossed-atpt)
    (should (eq (char-before) ?+))))

(ert-deftest ar-tilded2dollared-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2dollared-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-tilded2doublequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2doublequoted-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-tilded2equalized-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2equalized-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-tilded2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2hashed-atpt)
    (should (eq (char-before) ?#))))

(ert-deftest ar-tilded2hyphened-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2hyphened-atpt)
    (should (eq (char-before) ?-))))

(ert-deftest ar-tilded2singlequoted-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2singlequoted-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-tilded2slashed-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2slashed-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-tilded2stared-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2stared-atpt)
    (should (eq (char-before) ?*))))

(ert-deftest ar-tilded2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2underscored-atpt)
    (should (eq (char-before) ?_))))

(ert-deftest ar-tilded2whitespaced-atpt-test ()
  (ar-test-with-temp-buffer
    "~asdf~"
    (forward-char -2)
    (ar-tilded2whitespaced-atpt)
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

(ert-deftest ar-underscored2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2crossed-atpt)
    (should (eq (char-before) ?+))))

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

(ert-deftest ar-underscored2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2hashed-atpt)
    (should (eq (char-before) ?#))))

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

(ert-deftest ar-underscored2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    "_asdf_"
    (forward-char -2)
    (ar-underscored2tilded-atpt)
    (should (eq (char-before) ?~))))

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

(ert-deftest ar-whitespaced2crossed-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2crossed-atpt)
    (should (eq (char-before) ?+))))

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

(ert-deftest ar-whitespaced2hashed-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2hashed-atpt)
    (should (eq (char-before) ?#))))

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

(ert-deftest ar-whitespaced2tilded-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2tilded-atpt)
    (should (eq (char-before) ?~))))

(ert-deftest ar-whitespaced2underscored-atpt-test ()
  (ar-test-with-temp-buffer
    " asdf "
    (forward-char -2)
    (ar-whitespaced2underscored-atpt)
    (should (eq (char-before) ?_))))

(provide 'delimited2delimited-tests)
;;; delimited2delimited-tests.el ends here
