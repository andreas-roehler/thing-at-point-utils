;;; ar-unpaired-delimited-tests.el --- Created by ar-write-tests.el, don't edit -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2018 Andreas Röhler, unless
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

(ert-deftest backslashed-unpaired-delimited-test ()
  (ar-test-with-temp-buffer "\\
     ;;; \\ \\ Write 'etc. \" \\"
      (text-mode)
    (beginning-of-line)

    (let ((erg (ar-backslashed-atpt)))
      (should (< 7 (length erg))))
    (goto-char 22)
    (let ((erg (ar-backslashed-atpt)))
      (should (< 8 (length erg))))))

(ert-deftest backticked-unpaired-delimited-test ()
  (ar-test-with-temp-buffer "`
     ;;; ` ` Write 'etc. \" `"
      (text-mode)
    (beginning-of-line)

    (let ((erg (ar-backticked-atpt)))
      (should (< 7 (length erg))))
    (goto-char 22)
    (let ((erg (ar-backticked-atpt)))
      (should (< 8 (length erg))))))

(ert-deftest coloned-unpaired-delimited-test ()
  (ar-test-with-temp-buffer ":
     ;;; : : Write 'etc. \" :"
      (text-mode)
    (beginning-of-line)

    (let ((erg (ar-coloned-atpt)))
      (should (< 7 (length erg))))
    (goto-char 22)
    (let ((erg (ar-coloned-atpt)))
      (should (< 8 (length erg))))))

(ert-deftest dollared-unpaired-delimited-test ()
  (ar-test-with-temp-buffer "\$
     ;;; \$ \$ Write 'etc. \" \$"
      (text-mode)
    (beginning-of-line)

    (let ((erg (ar-dollared-atpt)))
      (should (< 7 (length erg))))
    (goto-char 22)
    (let ((erg (ar-dollared-atpt)))
      (should (< 8 (length erg))))))

(ert-deftest doublequoted-unpaired-delimited-test ()
  (ar-test-with-temp-buffer "\"
     ;;; \" \" Write 'etc. \" \""
      (text-mode)
    (beginning-of-line)

    (let ((erg (ar-doublequoted-atpt)))
      (should (< 7 (length erg))))
    (goto-char 22)
    (let ((erg (ar-doublequoted-atpt)))
      (should (< 8 (length erg))))))

(ert-deftest equalized-unpaired-delimited-test ()
  (ar-test-with-temp-buffer "=
     ;;; = = Write 'etc. \" ="
      (text-mode)
    (beginning-of-line)

    (let ((erg (ar-equalized-atpt)))
      (should (< 7 (length erg))))
    (goto-char 22)
    (let ((erg (ar-equalized-atpt)))
      (should (< 8 (length erg))))))

(ert-deftest hyphened-unpaired-delimited-test ()
  (ar-test-with-temp-buffer "-
     ;;; - - Write 'etc. \" -"
      (text-mode)
    (beginning-of-line)

    (let ((erg (ar-hyphened-atpt)))
      (should (< 7 (length erg))))
    (goto-char 22)
    (let ((erg (ar-hyphened-atpt)))
      (should (< 8 (length erg))))))

(ert-deftest singlequoted-unpaired-delimited-test ()
  (ar-test-with-temp-buffer "'
     ;;; ' ' Write 'etc. \" '"
      (text-mode)
    (beginning-of-line)

    (let ((erg (ar-singlequoted-atpt)))
      (should (< 7 (length erg))))
    (goto-char 22)
    (let ((erg (ar-singlequoted-atpt)))
      (should (< 8 (length erg))))))

(ert-deftest slashed-unpaired-delimited-test ()
  (ar-test-with-temp-buffer "/
     ;;; / / Write 'etc. \" /"
      (text-mode)
    (beginning-of-line)

    (let ((erg (ar-slashed-atpt)))
      (should (< 7 (length erg))))
    (goto-char 22)
    (let ((erg (ar-slashed-atpt)))
      (should (< 8 (length erg))))))

(ert-deftest stared-unpaired-delimited-test ()
  (ar-test-with-temp-buffer "\*
     ;;; \* \* Write 'etc. \" \*"
      (text-mode)
    (beginning-of-line)

    (let ((erg (ar-stared-atpt)))
      (should (< 7 (length erg))))
    (goto-char 22)
    (let ((erg (ar-stared-atpt)))
      (should (< 8 (length erg))))))

(ert-deftest underscored-unpaired-delimited-test ()
  (ar-test-with-temp-buffer "_
     ;;; _ _ Write 'etc. \" _"
      (text-mode)
    (beginning-of-line)

    (let ((erg (ar-underscored-atpt)))
      (should (< 7 (length erg))))
    (goto-char 22)
    (let ((erg (ar-underscored-atpt)))
      (should (< 8 (length erg))))))

(ert-deftest whitespaced-unpaired-delimited-test ()
  (ar-test-with-temp-buffer " 
     ;;;     Write 'etc. \"  "
      (text-mode)
    (beginning-of-line)
    (search-forward "rit")
    (let ((erg (ar-whitespaced-atpt)))
      (should (< 2 (length erg))))
    (goto-char 22)
    (let ((erg (ar-whitespaced-atpt)))
      (should (< 4 (length erg))))))

(provide 'ar-unpaired-delimited-tests)
;; ar-unpaired-delimited-tests.el ends here
