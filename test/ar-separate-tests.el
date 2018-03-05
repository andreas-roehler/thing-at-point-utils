;;; ar-separate-tests.el --- Created by ar-write-tests.el, don't edit -*- lexical-binding: t; -*-

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

(ert-deftest ar-separate-braced-paired-delimited-test ()
  (ar-test-with-temp-buffer "<{([>“{([‘asdf’])}”<])}>"
    (goto-char 10)
    (ar-separate-braced-atpt)
    (should (bolp))
    (forward-line -1)
    (beginning-of-line)  
    (should (char-equal ?\{ (char-after)))))

(ert-deftest ar-separate-bracketed-paired-delimited-test ()
  (ar-test-with-temp-buffer "<{([>“{([‘asdf’])}”<])}>"
    (goto-char 10)
    (ar-separate-bracketed-atpt)
    (should (bolp))
    (forward-line -1)
    (beginning-of-line)  
    (should (char-equal ?\[ (char-after)))))

(ert-deftest ar-separate-lesserangled-paired-delimited-test ()
  (ar-test-with-temp-buffer "<{([>“{([‘asdf’])}”<])}>"
    (goto-char 2)
    (ar-separate-lesserangled-atpt)
    (should (bolp))
    (forward-line -1)
    (beginning-of-line)  
    (should (char-equal ?\< (char-after)))))

(ert-deftest ar-separate-greaterangled-paired-delimited-test ()
  (ar-test-with-temp-buffer "<{([>“{([‘asdf’])}”<])}>"
    (goto-char 10)
    (ar-separate-greaterangled-atpt)
    (should (bolp))
    (forward-line -1)
    (beginning-of-line)  
    (should (char-equal ?\> (char-after)))))

(ert-deftest ar-separate-leftrightsinglequoted-paired-delimited-test ()
  (ar-test-with-temp-buffer "<{([>“{([‘asdf’])}”<])}>"
    (goto-char 10)
    (ar-separate-leftrightsinglequoted-atpt)
    (should (bolp))
    (forward-line -1)
    (beginning-of-line)  
    (should (char-equal ?\‘ (char-after)))))

(ert-deftest ar-separate-leftrightdoublequoted-paired-delimited-test ()
  (ar-test-with-temp-buffer "<{([>“{([‘asdf’])}”<])}>"
    (goto-char 10)
    (ar-separate-leftrightdoublequoted-atpt)
    (should (bolp))
    (forward-line -1)
    (beginning-of-line)  
    (should (char-equal ?\“ (char-after)))))

(ert-deftest ar-separate-parentized-paired-delimited-test ()
  (ar-test-with-temp-buffer "<{([>“{([‘asdf’])}”<])}>"
    (goto-char 10)
    (ar-separate-parentized-atpt)
    (should (bolp))
    (forward-line -1)
    (beginning-of-line)  
    (should (char-equal ?\( (char-after)))))

(provide 'ar-separate-tests)
;; ar-separate-tests.el ends here
