;;; ar-thingatpt-more-delimited-tests.el --- More thing-atpt tests -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2024  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>
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

;;; Commentary:

;;

;;; Code:

(ert-deftest ar-delimited-test-yqgUcN ()
  (ar-test-with-elisp-buffer
      "((use-region-p)
 (region-beginning))"
    (goto-char (point-max))
    (search-backward "us")
    (should (< (length (ar-delimited-atpt)) 30))))

(ert-deftest ar-delimited-bracketed-atpt-test-CHahdS ()
  (ar-test-with-elisp-buffer
      "[[1,3,4,8]]"
    (goto-char (point-max))
    (backward-char 2)
    (should (string= "[1,3,4,8]" (ar-delimited-atpt)))))

(ert-deftest ar-trim-delimited-atpt-test-CHahdS ()
  (ar-test-with-elisp-buffer
      "(* 2 2)"
    (goto-char (point-max))
    (backward-char 2)
    (ar-trim-delimited-atpt)
    (goto-char (point-min))
    (should (eq (char-after) ?*))))

(ert-deftest ar-forward-symbol-atpt-test-CHahdS ()
  (ar-test-with-elisp-buffer-point-min
      "+++"
    (ar-forward-symbol-atpt)
    (should (eq (char-before) ?+))
    (should (eobp))))

(ert-deftest ar-backward-symbol-atpt-test-CHahdS ()
  (ar-test-with-elisp-buffer
      "+++"
    (goto-char (point-max))
    (ar-backward-symbol-atpt)
    (should (eq (char-after) ?+))
    (should (bobp))))

(ert-deftest ar-delimited-test-nslZtA ()
  (ar-test
      "(defun foo1 (&optional beg end))"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "opt")
    (should (string= "(&optional beg end)" (ar-delimited-atpt)))))

(ert-deftest ar-delimited-test-ORxX4x ()
  (ar-test
      "(add-to-list 'load-path \"~/foo\")"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "oo")
    (should (string= (ar-delimited-atpt) "\"~/foo\"" ))))

(ert-deftest ar-delimited-test-CkaEZw ()
  (ar-test
      "(* 2 2*)"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "2")
   (should  (string= "* 2 2*" (ar-delimited-atpt)))))

(ert-deftest ar-delimited-xml-test-r9C7hI ()
  (ar-test
      "<rdg wit=\"a2\">Foo bar baz<milestone unit=\"stanza\"/></rdg>"
    'sgml-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "wit")
    (should
     (string=  (ar-delimited-atpt) "<rdg wit=\"a2\">"))))

(ert-deftest ar-delimited-xml-test-Ixa2Qy ()
  (ar-test
   "<rdg wit=\"a2\">Foo bar baz<milestone unit=\"stanza\"/></rdg>"
   'sgml-mode
   ar-debug-p
   (goto-char (point-max))
   (search-backward "stanz")
   (should
    (string=  (ar-delimited-atpt) "\"stanza\""))))

(ert-deftest ar-delimited-test-1DthHC ()
  (ar-test
      "foo -="
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "o")
    (should-not (string=  (ar-delimited-atpt) "foo -="))))

(ert-deftest ar-delimited-test-wZseP5 ()
  (ar-test
      "\"foo -=\""
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "o")
    (should (string=  (ar-delimited-atpt) "\"foo -=\""))))

(ert-deftest ar-delimited-test-8XxN4R ()
  (ar-test
      "`fstring`"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "g")
    (should (string=  (ar-delimited-atpt) "`fstring`"))))

(ert-deftest ar-delimited-singlequoted-test-8XxN4R ()
  (ar-test
      "result = 'Congratulations! You won a ' ++ 'penguin'"
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "C")
    (should (string=  (ar-delimited-atpt) "'Congratulations! You won a '"))))

(ert-deftest ar-delimited-angled-test-8XxN4R ()
  (ar-test
      "<greeting>"
    'text-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "e")
    (should (string=  (ar-delimited-atpt) "<greeting>"))))

;; (ert-deftest ar-delimited-braced-occurrence-test-8XxN4R ()
;;   (ar-test
;;       "\"Hr {},\\r\\rTrrr rr r rrrr {} rrrr rr \\
;;  rrrrrrrr\. Yrr'rr rrrrrrr rrrrr rr {}  rrrrrrrr \\
;; rr {} rr rrr rrrr\.\\r\\r\""
;;     'python-mode
;;     ar-debug-p
;;     (goto-char (point-max))
;;     (search-backward "Y")
;;     (let ((erg (ar-delimited-atpt)))
;;       (should (string=  erg "\"Hr {},\r\rTrrr rr r rrrr {} rrrr rr \
;;  rrrrrrrr\. Yrr'rr rrrrrrr rrrrr rr {}  rrrrrrrr \
;; rr {} rr rrr rrrr\.\r\r\"")))))

(ert-deftest ar-iterable-test-8XxN4R ()
  (ar-test
      "for i in foo:
    for j in bar:
        if j in i:
            print(foo_bar[j])
"
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "foo")
    (should (string=  (ar-delimited-atpt) "(foo_bar[j])"))))

(ert-deftest ar-backtick-test-1977A4N ()
  (ar-test-with-temp-buffer
    "(defun foo1 (&`optional` beg end)"
    (goto-char (point-max))
    (search-backward "o")
    (should (string= "`optional`" (ar-delimited-atpt)))))

(ert-deftest ar-backtick-test-uoClPB ()
  (ar-test-with-temp-buffer
    "(defun foo1 (`&optional` beg end)"
    (goto-char (point-max))
    (search-backward "o")
    (should (string= "`&optional`" (ar-delimited-atpt)))))

;; (ert-deftest ar-minused-test-uoClPB ()
;;   (ar-test-with-temp-buffer-point-min
;;       " ‘M-x append-to-register <RET> R’ can use ‘C-x r +’
;;  "
;;       (goto-char (point-min))
;;     (search-forward "a")
;;     (should (string= "‘M-x append-to-register <RET> R’" (ar-delimited-atpt)))))

(ert-deftest ar-less-than-greater-than-test-uoClPB ()
  (ar-test-with-temp-buffer
      "‘C-x r <SPC> R’"
      (goto-char (point-max))
    (search-backward "<")
    (should (string= "<SPC>" (ar-delimited-atpt)))))

(ert-deftest ar-less-than-org-mode-test-uoClPB ()
  (ar-test-with-temp-buffer
      "‘C-x r <SPC> R’"
      (org-mode)
      (goto-char (point-max))
    (search-backward "<")
    (should (string= "<SPC>" (ar-delimited-atpt)))))

(ert-deftest ar-delimited-test-01Mzp2 ()
  (ar-test
      "‘@2’, ..., ‘@N’"
    'Info-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "2")
    (should (string=  (ar-delimited-atpt) "@2’, ..., ‘@"))))

(ert-deftest ar-delimited-test-959FwB ()
  (ar-test
      "‘22’, ..., ‘NN’"
    'Info-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "2")
    (should (string=  (ar-delimited-atpt) "‘22’"))))

(ert-deftest ar-delimited-test-2H1v4o ()
  (ar-test
      "(defun foo (arg)
  \" ( Some command   (\"
  ;; \"Some command (\"  )
  ;; (  or ] not )
  ;; ( asdf
  (interactive \"p*\")
  (message \"%s\" arg))"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "command" nil t 2)
    (end-of-line)
    (should (string=  (ar-delimited-atpt) "(defun foo (arg)
  \" ( Some command   (\"
  ;; \"Some command (\"  )
  ;; (  or ] not )
  ;; ( asdf
  (interactive \"p*\")
  (message \"%s\" arg))"))))

(ert-deftest ar-delimited-underlines-test-8XxN4R ()
  (ar-test
      "foo = 'foo_bar_baz.txt'"
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "a")
    (should (string=  (ar-delimited-atpt) "'foo_bar_baz.txt'"))))

(ert-deftest ar-delimited-underlines-test-AVgCeb ()
  (ar-test
      "foo = 'foo_bar_baz.txt'"
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "xt")
    (should (string=  (ar-delimited-atpt) "'foo_bar_baz.txt'"))))

(ert-deftest ar-delimited-allzeichenabk-test-qLwOV9 ()
  (ar-test
   "(add-to-list 'load-path \"~/foo/Emacs-allzeichenabk/emacs-26\.2\")"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-max))
   (search-backward "allz")
   (should (string=  "-allzeichenabk/emacs-" (ar-delimited-atpt)))))

(ert-deftest ar-delimited-allzeichenabk-test-ns28ON ()
  (ar-test-with-elisp-buffer-point-min
   "(add-to-list 'load-path \"~/foo/Emacs-allzeichenabk/emacs-26\.2\")"
   (goto-char (point-min))
   (search-forward "Emac")
   (sit-for 0.1)
   (should (string= (ar-delimited-atpt) "/Emacs-allzeichenabk/"))))

(ert-deftest ar-end-of-delimited-test-wTNokm ()
  (ar-test-with-python-buffer-point-min
      "print(f\"Foo {asdf[0]}\")"
    (goto-char (point-min))
    (search-forward "F")
    ;; (sit-for 0.1)
    (ar-end-of-delimited-atpt)
    (should (eq (char-after) 41))))

(ert-deftest ar-end-of-delimited-test-nIxpIP ()
  (ar-test-with-python-buffer-point-min
      "print(f\"Foo {asdf[0]}\")"
    (goto-char (point-min))
    (search-forward "0")
    (ar-end-of-delimited-atpt)
    (sit-for 0.1)
    (should (eq (char-after) ?}))))

(ert-deftest ar-end-of-delimited-test-zRwEF8 ()
  (ar-test-with-python-buffer-point-min
      "print(f\"Foo {asdf[0]}\")"
    (goto-char (point-min))
    (search-forward "[")
    ;; (sit-for 0.1)
    (ar-end-of-delimited-atpt)
    (should (eq (char-after) ?}))))

(ert-deftest ar-end-of-delimited-test-BdUoxG ()
  (ar-test-with-python-buffer-point-min
      "print(f\"Foo {asd[0]}\")"
    (goto-char (point-min))
    (search-forward "d")
    ;; (sit-for 0.1)
    (ar-end-of-delimited-atpt)
    (should (eq (char-after) ?}))))

(provide 'ar-thingatpt-more-delimited-tests)
;;; ar-thingatpt-more-delimited-tests.el ends here
