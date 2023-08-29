;;; ar-paired-delimited-tests.el --- Created by ar-write-tests.el, don't edit -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2023 Andreas Röhler, unless
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

(ert-deftest ar-braced-paired-delimited-test ()
  (ar-test-with-temp-buffer "`<{[“(‘
     ;;  'c. \$’)”]}>"
      (let ((braced-laenge 25))
        (goto-char 7)
        (should (eq braced-laenge (length (ar-braced-atpt)))))))
      
(ert-deftest ar-symboled-paired-delimited-test ()
  (ar-test-with-temp-buffer "`<{[“(‘
     ;;  'c. \$’)”]}>"
      (let ((symboled-laenge 18))
        (goto-char 7)
        (should (eq symboled-laenge (length (ar-symboled-atpt)))))))
      
(ert-deftest ar-bracketed-paired-delimited-test ()
  (ar-test-with-temp-buffer "`<{[“(‘
     ;;  'c. \$’)”]}>"
      (let ((bracketed-laenge 23))
        (goto-char 7)
        (should (eq bracketed-laenge (length (ar-bracketed-atpt)))))))
      
(ert-deftest ar-lesserangled-paired-delimited-test ()
  (ar-test-with-temp-buffer "`<{[“(‘
     ;;  'c. \$’)”]}>"
      (let ((lesserangled-laenge 27))
        (goto-char 7)
        (should (eq lesserangled-laenge (length (ar-lesserangled-atpt)))))))
      
(ert-deftest ar-curvedsinglequoted-paired-delimited-test ()
  (ar-test-with-temp-buffer "`<{[“(‘
     ;;  'c. \$’)”]}>"
      (let ((curvedsinglequoted-laenge 17))
        (goto-char 7)
        (should (eq curvedsinglequoted-laenge (length (ar-curvedsinglequoted-atpt)))))))
      
(ert-deftest ar-curveddoublequoted-paired-delimited-test ()
  (ar-test-with-temp-buffer "`<{[“(‘
     ;;  'c. \$’)”]}>"
      (let ((curveddoublequoted-laenge 21))
        (goto-char 7)
        (should (eq curveddoublequoted-laenge (length (ar-curveddoublequoted-atpt)))))))
      
(ert-deftest ar-parentized-paired-delimited-test ()
  (ar-test-with-temp-buffer "`<{[“(‘
     ;;  'c. \$’)”]}>"
      (let ((parentized-laenge 19))
        (goto-char 7)
        (should (eq parentized-laenge (length (ar-parentized-atpt)))))))
      
(provide 'ar-paired-delimited-tests)
;; ar-paired-delimited-tests.el ends here
