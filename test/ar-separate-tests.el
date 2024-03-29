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
    (goto-char (point-min))
    (end-of-line)
    (skip-chars-forward " \t\r\n\f")
    (should (char-equal ?\{ (char-after)))
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (should (char-equal ?\} (char-before)))
    ))

(ert-deftest ar-separate-bracketed-paired-delimited-test ()
  (ar-test-with-temp-buffer "<{([>“{([‘asdf’])}”<])}>"
    (goto-char 10)
    (ar-separate-bracketed-atpt)
    (goto-char (point-min))
    (end-of-line)
    (skip-chars-forward " \t\r\n\f")
    (should (char-equal ?\[ (char-after)))
    (end-of-line)
    (should (char-equal ?\] (char-before)))
    ))

(ert-deftest ar-separate-lesserangled-paired-delimited-test ()
  (ar-test-with-temp-buffer "<{([>“{([‘asdf’])}”<])}>"
    (goto-char 2)
    (ar-separate-lesserangled-atpt)
    (goto-char (point-min))
    (should (char-equal ?\< (char-after)))
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (should (char-equal ?\> (char-before)))
    ))

(ert-deftest ar-separate-greaterangled-paired-delimited-test ()
  (ar-test-with-temp-buffer "<{([>“{([‘asdf’])}”<])}>"
      (goto-char 10)
    (ar-separate-greaterangled-atpt)
    (goto-char (point-min))
    (end-of-line)
    (skip-chars-forward " \t\r\n\f")
    (should (char-equal ?\> (char-after)))
    (end-of-line)
    (should (char-equal ?\< (char-before)))))

(ert-deftest ar-separate-curvedsinglequoted-paired-delimited-test ()
  (ar-test-with-temp-buffer "<{([>“{([‘asdf’])}”<])}>"
      (goto-char 11)
    (ar-separate-curvedsinglequoted-atpt)
    (goto-char (point-min))
    (end-of-line)
    (skip-chars-forward " \t\r\n\f")
    (should (char-equal ?\‘ (char-after)))
    (end-of-line)
    (should (char-equal ?\’ (char-before)))))

(ert-deftest ar-separate-curveddoublequoted-paired-delimited-test ()
  (ar-test-with-temp-buffer "<{([>“{([‘asdf’])}”<])}>"
      (goto-char 10)
    (ar-separate-curveddoublequoted-atpt)
    (goto-char (point-min))
    (end-of-line)
    (skip-chars-forward " \t\r\n\f")
    (should (char-equal ?\“ (char-after)))
    (end-of-line)
    (should (char-equal ?\” (char-before)))))

(ert-deftest ar-separate-parentized-paired-delimited-test ()
  (ar-test-with-temp-buffer "<{([>“{([‘asdf’])}”<])}>"
    (goto-char 10)
    (ar-separate-parentized-atpt)
    (goto-char (point-min))
    (end-of-line)
    (skip-chars-forward " \t\r\n\f")
    (should (char-equal ?\( (char-after)))
    (end-of-line)
    (should (char-equal ?\) (char-before)))
    ))

(provide 'ar-separate-tests)
;; ar-separate-tests.el ends here
