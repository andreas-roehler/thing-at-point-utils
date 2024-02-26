;;; ar-paired-delimit-tests.el --- more th-at-point tests

;; Copyright (C) 2010-2022 Andreas Röhler, unless
;; indicated otherwise

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>, unless
;; indicated otherwise

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

(require 'ar-thingatpt-utils)

(ert-deftest ar-brace-alnum-atpt-test ()
  (ar-test-with-temp-buffer "asdf48"
    (forward-char -1)
    (ar-brace-alnum-atpt)
    (forward-char -1)
    (should (eq 8 (length (ar-braced-atpt))))))

(ert-deftest ar-bracket-alnum-atpt-test ()
  (ar-test-with-temp-buffer "asdf48"
    (forward-char -1)
    (ar-bracket-alnum-atpt)
    (forward-char -1)
    (should (eq 8 (length (ar-bracketed-atpt))))))

(ert-deftest ar-lesserangle-alnum-atpt-test ()
  (ar-test-with-temp-buffer "asdf48"
    (forward-char -1)
    (ar-lesserangle-alnum-atpt)
    (forward-char -1)
    (should (eq 8 (length (ar-lesserangled-atpt))))))

(ert-deftest ar-greaterangle-alnum-atpt-test ()
  (ar-test-with-temp-buffer "asdf48"
    (forward-char -1)
    (ar-greaterangle-alnum-atpt)
    (forward-char -1)
    (should (eq 8 (length (ar-greaterangled-atpt))))))

(ert-deftest ar-curvedsinglequote-alnum-atpt-test ()
  (ar-test-with-temp-buffer "asdf48"
    (forward-char -1)
    (ar-curvedsinglequote-alnum-atpt)
    (forward-char -1)
    (should (eq 8 (length (ar-curvedsinglequoted-atpt))))))

(ert-deftest ar-parentize-alnum-atpt-test ()
  (ar-test-with-temp-buffer "asdf48"
    (forward-char -1)
    (ar-parentize-alnum-atpt)
    (forward-char -1)
    (should (eq 8 (length (ar-parentized-atpt))))))

(ert-deftest ar-angled-atpt-YzYGzY-test ()
  (ar-test-with-temp-buffer
      "<2021-01-25 Mo>"
      (should-not (eq 8 (length (ar-delimited-atpt))))))

(ert-deftest ar-left-right-single-quotation-atpt-YzYGzY-test ()
  (ar-test-with-temp-buffer
      "‘#+OPTIONS: tex:verbatim’"
      (goto-char (point-max))
    (backward-char 2)
    (should (eq 25 (length (ar-delimited-atpt))))))

(ert-deftest ar-left-right-angle-atpt-test-YzYGzY ()
  (ar-test-with-temp-buffer
      "<foobar@example.com>"
      (goto-char (point-max))
    (search-backward "p")
    (should (eq 20 (length (ar-delimited-atpt))))))

(ert-deftest ar-left-right-angle-atpt-test-1iuDg9 ()
  (ar-test-with-temp-buffer
      "<foobar@example.com>"
      (goto-char (point-max))
    (search-backward "@") 
    (should (eq 20 (length (ar-delimited-atpt))))))

(provide 'ar-paired-delimit-tests)
;; ar-paired-delimit-tests.el ends here
