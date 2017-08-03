;;; thingatpt-transform-delimited-test.el --- delimited to delimited tests -*- lexical-binding: t; -*- 

;; Author: Andreas Röhler <andreas.roehler@online.de>
;; Keywords: languages, convenience

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

;;; Code:

(ert-deftest ar-ert-braced2bracketed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "[foo]"

(ert-deftest ar-ert-braced2lesserangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "<foo>"
(global-set-key [(control c) (control \{)(control \<)] 'ar-braced2lesserangled-atpt)

(ert-deftest ar-ert-braced2greaterangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "<foo>"

(ert-deftest ar-ert-braced2leftrightsinglequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-braced2parentized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-bracketed2braced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-bracketed2lesserangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-bracketed2greaterangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-bracketed2leftrightsinglequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-bracketed2parentized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-lesserangled2braced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-lesserangled2bracketed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-lesserangled2greaterangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-lesserangled2leftrightsinglequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-lesserangled2parentized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-greaterangled2braced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-greaterangled2bracketed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-greaterangled2lesserangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-greaterangled2leftrightsinglequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-greaterangled2parentized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-leftrightsinglequoted2braced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-leftrightsinglequoted2bracketed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-leftrightsinglequoted2lesserangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-leftrightsinglequoted2greaterangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-leftrightsinglequoted2parentized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-parentized2braced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-parentized2bracketed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-parentized2lesserangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-parentized2greaterangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-parentized2leftrightsinglequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-braced2backslashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-braced2backticked-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-braced2dollared-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-braced2doublequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-braced2equalized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-braced2hyphened-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-braced2singlequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-braced2slashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-braced2underscored-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-braced2whitespaced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-bracketed2backslashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-bracketed2backticked-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-bracketed2dollared-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-bracketed2doublequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-bracketed2equalized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-bracketed2hyphened-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-bracketed2singlequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-bracketed2slashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-bracketed2underscored-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-bracketed2whitespaced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-lesserangled2backslashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-lesserangled2backticked-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-lesserangled2dollared-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-lesserangled2doublequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-lesserangled2equalized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-lesserangled2hyphened-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-lesserangled2singlequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-lesserangled2slashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-lesserangled2underscored-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-lesserangled2whitespaced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-greaterangled2backslashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-greaterangled2backticked-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-greaterangled2dollared-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-greaterangled2doublequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-greaterangled2equalized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-greaterangled2hyphened-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-greaterangled2singlequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-greaterangled2slashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-greaterangled2underscored-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-greaterangled2whitespaced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-leftrightsinglequoted2backslashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-leftrightsinglequoted2backticked-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-leftrightsinglequoted2dollared-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-leftrightsinglequoted2doublequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-leftrightsinglequoted2equalized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-leftrightsinglequoted2hyphened-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-leftrightsinglequoted2singlequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-leftrightsinglequoted2slashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-leftrightsinglequoted2underscored-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-leftrightsinglequoted2whitespaced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-parentized2backslashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-parentized2backticked-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-parentized2dollared-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-parentized2doublequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-parentized2equalized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-parentized2hyphened-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-parentized2singlequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-parentized2slashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-parentized2underscored-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-parentized2whitespaced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backslashed2braced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backslashed2bracketed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backslashed2lesserangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backslashed2greaterangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backslashed2leftrightsinglequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backslashed2parentized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backticked2braced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backticked2bracketed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backticked2lesserangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backticked2greaterangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backticked2leftrightsinglequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backticked2parentized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-dollared2braced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-dollared2bracketed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-dollared2lesserangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-dollared2greaterangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-dollared2leftrightsinglequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-dollared2parentized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-doublequoted2braced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-doublequoted2bracketed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-doublequoted2lesserangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-doublequoted2greaterangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-doublequoted2leftrightsinglequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-doublequoted2parentized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-equalized2braced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-equalized2bracketed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-equalized2lesserangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-equalized2greaterangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-equalized2leftrightsinglequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-equalized2parentized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-hyphened2braced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-hyphened2bracketed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-hyphened2lesserangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-hyphened2greaterangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-hyphened2leftrightsinglequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-hyphened2parentized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-singlequoted2braced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-singlequoted2bracketed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-singlequoted2lesserangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-singlequoted2greaterangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-singlequoted2leftrightsinglequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-singlequoted2parentized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-slashed2braced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-slashed2bracketed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-slashed2lesserangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-slashed2greaterangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-slashed2leftrightsinglequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-slashed2parentized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-underscored2braced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-underscored2bracketed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-underscored2lesserangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-underscored2greaterangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-underscored2leftrightsinglequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-underscored2parentized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-whitespaced2braced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-whitespaced2bracketed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-whitespaced2lesserangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-whitespaced2greaterangled-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-whitespaced2leftrightsinglequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-whitespaced2parentized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backslashed2backticked-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backslashed2dollared-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backslashed2doublequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backslashed2equalized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backslashed2hyphened-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backslashed2singlequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backslashed2slashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backslashed2underscored-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backslashed2whitespaced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backticked2backslashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backticked2dollared-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backticked2doublequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backticked2equalized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backticked2hyphened-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backticked2singlequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backticked2slashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backticked2underscored-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-backticked2whitespaced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-dollared2backslashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-dollared2backticked-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-dollared2doublequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-dollared2equalized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-dollared2hyphened-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-dollared2singlequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-dollared2slashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-dollared2underscored-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-dollared2whitespaced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-doublequoted2backslashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-doublequoted2backticked-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-doublequoted2dollared-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-doublequoted2equalized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-doublequoted2hyphened-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-doublequoted2singlequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-doublequoted2slashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-doublequoted2underscored-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-doublequoted2whitespaced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-equalized2backslashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-equalized2backticked-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-equalized2dollared-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-equalized2doublequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-equalized2hyphened-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-equalized2singlequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-equalized2slashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-equalized2underscored-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-equalized2whitespaced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-hyphened2backslashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-hyphened2backticked-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-hyphened2dollared-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-hyphened2doublequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-hyphened2equalized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-hyphened2singlequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-hyphened2slashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-hyphened2underscored-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-hyphened2whitespaced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-singlequoted2backslashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-singlequoted2backticked-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-singlequoted2dollared-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-singlequoted2doublequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-singlequoted2equalized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-singlequoted2hyphened-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-singlequoted2slashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-singlequoted2underscored-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-singlequoted2whitespaced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-slashed2backslashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-slashed2backticked-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-slashed2dollared-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-slashed2doublequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-slashed2equalized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-slashed2hyphened-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-slashed2singlequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-slashed2underscored-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-slashed2whitespaced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-underscored2backslashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-underscored2backticked-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-underscored2dollared-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-underscored2doublequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-underscored2equalized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-underscored2hyphened-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-underscored2singlequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-underscored2slashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-underscored2whitespaced-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-whitespaced2backslashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-whitespaced2backticked-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-whitespaced2dollared-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-whitespaced2doublequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-whitespaced2equalized-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-whitespaced2hyphened-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-whitespaced2singlequoted-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-whitespaced2slashed-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(ert-deftest ar-ert-whitespaced2underscored-atpt ()
  (ar-test-with-elisp-buffer-point-min
  "{foo}"

(provide 'thingatpt-transform-delimited)
;;; thingatpt-transform-delimited.el ends here
