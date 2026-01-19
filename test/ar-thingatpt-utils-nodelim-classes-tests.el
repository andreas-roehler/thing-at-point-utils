;;; ar-thing-at-point-utils-nodelim-classes-tests.el --- ar-thing-at-point-utils-nodelim-classes

;; Copyright (C) 2015-2026  Andreas Roehler

;; Author: Andreas Roehler <andreas.roehler@online.de>
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

;;; Commentary: test strings edited after generation

;; 

;;; Code:



;; ar-thing-at-point-utils-nodelim-tests: ar-atpt-classes-tests start
(ert-deftest ar-alnum-atpt-test ()
  (with-temp-buffer
    (let (thing-copy-region) 
      (insert "asdf")
      (forward-char -1) 
      (should (stringp (ar-th 'alnum))))))

(ert-deftest ar-alpha-atpt-test ()
  (with-temp-buffer
    (let (thing-copy-region) 
      (insert "asdf")
      (forward-char -1) 
      (should (stringp (ar-th 'alpha))))))

(ert-deftest ar-ascii-atpt-test ()
  (with-temp-buffer
    (let (thing-copy-region) 
      (insert "asdf")
      (forward-char -1) 
      (should (stringp (ar-th 'ascii))))))

(ert-deftest ar-blank-atpt-test ()
  (with-temp-buffer
    (let (thing-copy-region) 
      (insert " ")
      (forward-char -1) 
      (should (stringp (ar-th 'blank))))))

(ert-deftest ar-digit-atpt-test ()
  (with-temp-buffer
    (let (thing-copy-region) 
      (insert "1234")
      (forward-char -1) 
      (should (stringp (ar-th 'digit))))))

(ert-deftest ar-graph-atpt-test ()
  (with-temp-buffer
    (let (thing-copy-region) 
      (insert "asdf")
      (forward-char -1) 
      (should (stringp (ar-th 'graph))))))

(ert-deftest ar-lower-atpt-test ()
  (with-temp-buffer
    (let (thing-copy-region) 
      (insert "asdf")
      (forward-char -1) 
      (should (stringp (ar-th 'lower))))))

(ert-deftest ar-nonascii-atpt-test ()
  (with-temp-buffer
    (let (thing-copy-region) 
      (insert "öäü")
      (forward-char -1) 
      (should (stringp (ar-th 'nonascii))))))

(ert-deftest ar-print-atpt-test ()
  (with-temp-buffer
    (let (thing-copy-region) 
      (insert "asdf")
      (forward-char -1) 
      (should (stringp (ar-th 'print))))))

(ert-deftest ar-punct-atpt-test ()
  (with-temp-buffer
    (let (thing-copy-region) 
      (insert "...")
      (forward-char -1) 
      (should (stringp (ar-th 'punct))))))

(ert-deftest ar-space-atpt-test ()
  (with-temp-buffer
    (let (thing-copy-region) 
      (insert " ")
      (forward-char -1) 
      (should (stringp (ar-th 'space))))))

(ert-deftest ar-upper-atpt-test ()
  (with-temp-buffer
    (let (thing-copy-region) 
      (insert "ASDF")
      (forward-char -1) 
      (should (stringp (ar-th 'upper))))))

(ert-deftest ar-xdigit-atpt-test ()
  (with-temp-buffer
    (let (thing-copy-region) 
      (insert "#x22")
      (forward-char -1) 
      (should (stringp (ar-th 'xdigit))))))

;; ar-thing-at-point-utils-nodelim-core: ar-atpt-classes-tests end


(provide 'ar-thing-at-point-utils-nodelim-classes-tests)
;;; ar-thing-at-point-utils-nodelim-classes-tests.el ends here
