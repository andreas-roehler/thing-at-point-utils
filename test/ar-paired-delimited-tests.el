;;; ar-paired-delimited-tests.el --- more th-at-point tests

;; Copyright (C) 2010-2018 Andreas Roehler, unless
;; indicated otherwise

;; Author: Andreas Roehler <andreas.roehler@easy-emacs.de>, unless
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

(ert-deftest ar-braced-atpt-test ()
  (ar-test-with-temp-buffer "<{[(‘
     ;;  'etc. \"’)]}>"
      (let ((braced-laenge 25)
	    (ar-generic-match-p t))
        (goto-char 7)
        (should (eq braced-laenge (length (ar-braced-atpt)))))))
      
(ert-deftest ar-bracketed-atpt-test ()
  (ar-test-with-temp-buffer "<{[(‘
     ;;  'etc. \"’)]}>"
      (let ((bracketed-laenge 23)
	    (ar-generic-match-p t))
        (goto-char 7)
        (should (eq bracketed-laenge (length (ar-bracketed-atpt)))))))
      
(ert-deftest ar-lesserangled-atpt-test ()
  (ar-test-with-temp-buffer "<{[(‘
     ;;  'etc. \"’)]}>"
      (let ((ar-generic-match-p t)
	    (lesserangled-laenge 27))
        (goto-char 7)
        (should (eq lesserangled-laenge (length (ar-lesserangled-atpt)))))))
      
(ert-deftest ar-leftrightsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer "<{[(‘
     ;;  'etc. \"’)]}>"
      (let ((leftrightsinglequoted-laenge 19)
	    (ar-generic-match-p t))
        (goto-char 7)
        (should (eq leftrightsinglequoted-laenge (length (ar-leftrightsinglequoted-atpt)))))))
      
(ert-deftest ar-parentized-atpt-test ()
  (ar-test-with-temp-buffer "<{[(‘
     ;;  'etc. \"’)]}>"
      (let ((ar-generic-match-p t)
	    (parentized-laenge 21))
        (goto-char 7)
        (should (eq parentized-laenge (length (ar-parentized-atpt)))))))
      
(provide 'ar-paired-delimited-tests)
;; ar-paired-delimited-tests.el ends here
