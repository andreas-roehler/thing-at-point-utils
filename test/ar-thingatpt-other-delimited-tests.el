;;; ar-thingatpt-other-delimited-tests.el --- More thing-atpt tests -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2025  Andreas Röhler

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

(require 'py-setup-ert-tests)

(ert-deftest ar-kill-comment-atpt-test ()
  (py-test-with-temp-buffer-point-min
   "import time

# import wx

# app = wx.PySimpleApp()
# frame = wx.Frame(None,-1,\"Roulade\")
# frame.Show(1)
# app.MainLoop()

# treffer, gruen, rot, schwarz, pair, impair, passe, manque
args = sys.argv"
   (search-forward "treffer")
   (ar-kill-comment-atpt)
   (skip-chars-backward " \t\r\n\f")
   (should (eq (char-before) ?e))
   (skip-chars-forward " \t\r\n\f")
   (should (eq (char-after) ?a))))

(ert-deftest ar-in-string-atpt-test ()
  (py-test-with-temp-buffer-point-min
      "\"asdf
\(defun foo1 (&optional beg end)
  sdsd\n\n"
    (should (ar-in-string-p))
    (forward-line 1)
    (should (ar-in-string-p-fast))
    (goto-char (point-max))
    (should (ar-in-string-p))
    ;; (should (nth 3 (syntax-ppss)))
    ))

(ert-deftest ar-beginning-of-defun-test ()
  (ar-test-with-elisp-buffer "(defun asdf ()
  \"
\(defun foo1 (&optional beg end)
  sdsd\"\n\)\n"
    (ar-beginning-of-defun)
    (should (bobp))))

(ert-deftest ar-doubleslash-char-test ()
  (ar-test-with-elisp-buffer-point-min "asdf"
    (ar-doubleslash-char-atpt)
    (should (eq (char-before) ?\/))))

(ert-deftest ar-doublebackslash-char-test ()
  (ar-test-with-elisp-buffer-point-min "asdf"
    (ar-doublebackslash-char-atpt)
    (sit-for 0.1)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-doublebackslashparen-char-test ()
  (ar-test-with-elisp-buffer-point-min "as"
    (ar-doublebackslashparen-char-atpt 2)
    (sit-for 0.1)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-bracked-braced-numarg-test ()
  (ar-test-with-elisp-buffer-point-min "[a][s][d][f]"
      (forward-char 2)
    (ar-brace-bracketed-atpt)
    (should (eq (char-before) ?}))
    (should (eq 6 (point)))))

(ert-deftest ar-separate-alnum-in-parentized-atpt-test ()
  (ar-test-with-elisp-buffer
      "(defun asdf (&optional arg for bar))"
    (goto-char (point-max))
    (forward-char -3)
    (ar-separate-alnum-in-parentized-atpt)
    (beginning-of-line)
    (back-to-indentation)
    (should (char-equal ?b (char-after)))
    (forward-line -2)
    (back-to-indentation)
    (should (char-equal ?f (char-after)))))

(ert-deftest ar-separate-doublequoted-in-bracketed-atpt-test-GkKIGn ()
  (ar-test-with-elisp-buffer
	"[\"defun\" \"asdf\" \"&optional\" \"arg\" \"for\" \"bar\"]"
      (let ((ar-thing-no-nest t))
	(goto-char (point-max))
	(forward-char -3)
	(ar-separate-doublequoted-in-bracketed-atpt)
	(beginning-of-line)
	(back-to-indentation)
	(should (char-equal ?\" (char-after)))
	(forward-line -1)
	(back-to-indentation)
	(should (char-equal ?\" (char-after))))))

(ert-deftest ar-doublequote-alnum-atpt-test ()
    (ar-test-with-elisp-buffer-point-min
  "asdfg"
  (ar-doublequote-alnum-atpt)
  (should (eq (char-before) ?\"))))

(ert-deftest ar-name-atpt-test-6sTtES ()
  (ar-test-with-elisp-buffer
      "asdfg"
    (goto-char (point-max))
    (forward-char -1)
    (let ((erg (ar-name-atpt)))
      (should (string= erg "asdfg")))))

(ert-deftest ar-in-doublequoted-atpt-test-R6VIxg ()
  (ar-test-with-elisp-buffer
      "(setq foo \"asdf\""
    (goto-char (point-max))
    (forward-char -2)
    (should (eq 6 (length (ar-doublequoted-atpt))))))

(ert-deftest ar-in-doublequoted-no-delimiters-test-fj1fKE ()
  (ar-test-with-elisp-buffer
      ";; (setq foo
\"asdf\""
    (goto-char (point-max))
    (forward-char -2)
    (sit-for 0.1)
    (should (eq 4 (length (ar-doublequoted-atpt '(4)))))))

(ert-deftest ar-in-string-atpt-test-pXDGrZ ()
  (ar-test-with-elisp-buffer
      ";; (setq foo
\"asdf\""
    (goto-char (point-max))
    (forward-char -2)
    (should (eq 6 (length (ar-string-atpt))))))

(ert-deftest ar-in-string-no-delimiters-test-3Lc1Ie ()
  (ar-test-with-elisp-buffer
      ";; (setq foo
\"asdf\""
    (goto-char (point-max))
    (forward-char -2)
    ;; (sit-for 1)
    (should (eq 4 (length (ar-string-atpt '(4)))))))

(ert-deftest ar-peel-list-atpt-test-yAeeNz ()
    (ar-test-with-elisp-buffer-point-min
	"(defun foo ()
  (let ((bar (save-excursion (baz nil nil))))
    (setq asdf nil)))"
	(search-forward "save-")
      (sit-for 0.1)
      (ar-peel-list-atpt)
      (sit-for 0.1)
      (should (looking-at "(baz"))))

(ert-deftest ar-kill-doublequoted-atpt-test-R1feEn () ()
  (ar-test-with-elisp-buffer
      "\"foo\""
    (goto-char (point-max))
    (forward-char -2)
    (ar-kill-doublequoted-atpt)
    (should (eobp))))

(ert-deftest ar-kill-doublequoted-atpt-test-2 ()
    (ar-test-with-elisp-buffer
	"(defun general-close--typedef-maybe (beg regexp &optional closer)
  (let (done)
    (when (save-excursion
	    (goto-char beg)
	    (skip-chars-forward \" \\t\\r\\n\\f\")
	    (looking-at regexp))
      (general-close-insert-with-padding-maybe \"Int\")
      (setq done t))
    done))"
      (goto-char (point-max))
      (search-backward "Int")
      (ar-kill-doublequoted-atpt)
      (should (eq (char-after) ?\)))))

(ert-deftest doublequoted-unpaired-delimited-test-vMnHFn ()
  (ar-test-with-temp-buffer "\"
     ;;; \" \" Write 'etc. \" \""
      (text-mode)
    (search-backward "rite")
    (let ((erg (ar-doublequoted-atpt)))
      (should erg))))

(ert-deftest ar-end-of-list-test-VRZ1MG ()
  (ar-test-with-elisp-buffer
      "(defun foo1 (&optional beg end))"
    (goto-char (point-max))
    (search-backward "&")
    (ar-end-of-list-atpt)
    (should (eq (char-after) ?\)))))

(ert-deftest ar-sort-indent-test-DZReV9 ()
  (ar-test-with-elisp-buffer
      "===
Z\n\nB\nC\nA\n\nA\n---"
    (goto-char (point-max))
    (search-backward "C")
    (ar-sort-indent)
    (forward-line 1)
    (should (eq (char-after) ?B))))

(ert-deftest ar-sort-indent-test-u7kbsF ()
  (ar-test-with-elisp-buffer
      "set(SRC
  zaz\.c  # <- cursor on this line\.
  bar\.c
  baz\.c
  foo\.c
)"
    (goto-char (point-max))
    (search-backward "foo")
    (ar-sort-indent)
    (end-of-line)
    (should (looking-back "^ +bar.c" (line-beginning-position)))))

(ert-deftest ar-sort-indent-test-VRZ1MG ()
  (ar-test-with-elisp-buffer
      "/* Structs, keep in order\. */

struct FooBaz\;
struct FooBar\;
struct FooFoo\;
struct AbcBaz\;  /* <- cursor on this line\. */"
    (goto-char (point-max))
    (search-backward "FooBaz")
    (ar-sort-indent)
    (should (looking-at "struct AbcBaz"))))

(ert-deftest ar-curved-singlequotes-delimited-test-VRZ1MG ()
  (ar-test-with-elisp-buffer
      "('‘asdf’'"
    (goto-char (point-max))
    (search-backward "f")
    (should (string= "‘asdf’" (ar-delimited-atpt)))))

(ert-deftest ar-curved-singlequotes-delimited-test-Mt4YnS ()
  (ar-test-with-elisp-buffer
      "('\"‘asdf’\"'"
    (goto-char (point-max))
    (search-backward "f")
    (should (string= "‘asdf’" (ar-delimited-atpt)))))

(ert-deftest ar-open-only-paren-stars-delimited-test-Mt4YnS ()
  (ar-test-with-elisp-buffer
      "(* 2 2*"
    (goto-char (point-max))
    (search-backward "2")
    (should (string=  (ar-delimited-atpt) "* 2 2*" ))))

(ert-deftest ar-ert-trim-test-zJuViH ()
  (ar-test-with-elisp-buffer
      "'(org-mode)"
    (goto-char (point-max))
    (skip-chars-backward "^o" (line-beginning-position))
    (ar-trim-delimited-atpt)
    (end-of-line)
    (should (eq (char-before) ?e))
    (skip-chars-backward "^'" (line-beginning-position))
    (should (eq (char-after) ?o))))

(ert-deftest ar-doublequote-graph-in-bracketed-atpt-test ()
  (py-test-with-temp-buffer
      "[defun asdf &optional arg for bar]"
    (goto-char (point-max))
    (unless (bobp) (forward-char -1))
    (ar-doublequote-graph-in-bracketed-atpt)
    (should (ar-in-string-p))))

(ert-deftest ar-align-inline-comment-HLk4Vq ()
  (py-test-with-temp-buffer
"dates = [
    r'\\d{2}.\\d{2}.\\d{2}',      #  \"DD.MM.YY\" foo
    r'\\bw+ \\d{1,2}, \\d{4}\\d' #  \"Month DD, YYYY\" foo
]
"
    ar-debug-p
    (goto-char (point-max))
    (search-backward "#")
    (ar-align-symbol "#")
    (skip-chars-forward " \t\r\n\f")
    (should (eq (current-column) 31))))

(ert-deftest ar-align-inline-comment-qjDxBH ()
  (py-test-with-temp-buffer
"dates = [
    r'\\d{2}.\\d{2}.\\d{2}', #  \"DD.MM.YY\" foo
    r'\\bw+ \\d{1,2}, \\d{4}\\d' #  \"Month DD, YYYY\" foo
]
"
    ar-debug-p
    (goto-char (point-max))
    (search-backward "#")
    (ar-align-symbol "#")
    (forward-line -1)
    (beginning-of-line)
    (search-forward "#")
    (should (eq (current-column) 30))))

(ert-deftest ar-backward-delimited-qjDxBH ()
  (ar-test
      ": :"
    'fundamental-mode
    ar-debug-p
    (forward-char -1)
    (ar-backward-delimited-atpt)
    (should (eq (char-after) ?:))))

;; dates = [
;;     r'',                     #  \"DD.MM.YY\" foo
;;     r'\b\w+ \d{1,2}, \d{4}\b',                  #  \"Month DD, YYYY\" foo
;;     r'\b\w+ \d{1,2}(th|st|nd|rd)?, \d{4}\b',    #  \"Month DDth, YYYY\" foo
;;     r'\b\d{1,2} - \d{1,2} \w+, \d{4}\b',        #  \"DD - DD Month, YYYY\" foo
;;     r'\b\w+ \d{1,2}(-\d{1,2})? \d{4}\b',        #  \"Month DD(-DD) YYYY\" foo
;;     r'\b\d{1,2}(-\d{1,2})? \w+ \d{4}\b'         #  \"DD(-DD) Month YYYY\" foo
;; ]

(ert-deftest ar-triplebackticked-test-dOGoBa ()
  (ar-test
      "```asd```"
    'fundamental-mode
    'ar-debug-p
    (goto-char (point-max))
    (search-backward "d")
    (should (eq (length (ar-triplebackticked-atpt)) 9))))

(provide 'ar-thingatpt-other-delimited-tests)
;;; ar-thingatpt-other-delimited-tests.el ends here
