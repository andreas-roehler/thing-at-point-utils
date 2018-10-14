;;; ar-thing-atpt-other-test.el --- More thing-atpt tests -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Andreas Röhler

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

(ert-deftest ar-kill-comment-atpt-test ()
  (ar-test-with-python-buffer-point-min
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

(ert-deftest ar-kill-comment-atpt-test ()
  (ar-test-with-python-buffer-point-min "\"asdf
\(defun foo1 (&optional beg end)
  sdsd\n\n"
    (should (ar-in-string-p))
    (forward-line 1)
    (should (ar-in-string-p-fast))
    (goto-char (point-max))
    (should (ar-in-string-p))
    (should (nth 3 (syntax-ppss)))
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
    (should (eq (char-before) ?\\))))

(ert-deftest ar-doublebackslashparen-char-test ()
  (ar-test-with-elisp-buffer-point-min "as"
    (ar-doublebackslashparen-char-atpt 2)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-bracked-braced-numarg-test ()
  (ar-test-with-elisp-buffer-point-min "[a][s][d][f]"
      (forward-char 2)
    (ar-brace-bracketed-atpt 4)
    (should (eobp))
    (should (eq (char-before) ?}))))

(ert-deftest ar-separate-alnum-in-parentized-atpt-test ()
  (ar-test-with-elisp-buffer
      "(defun asdf (&optional arg for bar))"
    (forward-char -3)
    (ar-separate-alnum-in-parentized-atpt)
    (beginning-of-line)
    (back-to-indentation)
    (should (char-equal ?b (char-after)))
    (forward-line -1)
    (back-to-indentation)
    (should (char-equal ?f (char-after)))))

(ert-deftest ar-doublequote-graph-in-bracketed-atpt-test ()
  (ar-test-with-python-buffer
      "[defun asdf &optional arg for bar]"
    (forward-char -1)
    (ar-doublequote-graph-in-bracketed-atpt)
    (forward-char -1)
    (should (ar-in-string-p))))

(ert-deftest ar-separate-doublequoted-in-bracketed-atpt-test ()
  (ar-test-with-elisp-buffer
	"[\"defun\" \"asdf\" \"&optional\" \"arg\" \"for\" \"bar\"]"
      (let ((ar-thing-no-nest t))
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

(ert-deftest ar-name-atpt-test ()
  (ar-test-with-elisp-buffer
      "asdfg"
    (forward-char -1)
    (let ((erg (ar-name-atpt)))
      (should (string= erg "asdfg")))))

(ert-deftest ar-in-doublequoted-atpt-test ()
  (ar-test-with-elisp-buffer
      ";; (setq foo
\"asdf\""
    (forward-char -1)
    (should (eq 6 (length (ar-doublequoted-atpt))))))

(ert-deftest ar-in-doublequoted-no-delimiters-test ()
  (ar-test-with-elisp-buffer
      ";; (setq foo
\"asdf\""
    (forward-char -1)
    (should (eq 6 (length (ar-doublequoted-atpt nil t))))))

(ert-deftest ar-in-string-atpt-test ()
  (ar-test-with-elisp-buffer
      ";; (setq foo
\"asdf\""
    (forward-char -1)
    (should (eq 6 (length (ar-string-atpt))))))

(ert-deftest ar-in-string-no-delimiters-test ()
  (ar-test-with-elisp-buffer
      ";; (setq foo
\"asdf\""
    (forward-char -1)
    (should (eq 4 (length (ar-string-atpt '(4)))))))

(ert-deftest ar-peel-list-atpt-test ()
    (ar-test-with-elisp-buffer-point-min
	"(defun foo ()
  (let ((bar (save-excursion (baz nil nil))))
    (setq asdf nil)))"
	(search-forward "save-")
      (ar-peel-list-atpt)
      (should (looking-at "(baz"))))

(ert-deftest ar-kill-doublequoted-atpt-test-1 ()
    (ar-test-with-elisp-buffer
	"\"foo\""
      (forward-char -1)
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
      (search-backward "Int")
      (ar-kill-doublequoted-atpt)
      (should (eq (char-after) ?\)))))

(ert-deftest ar-kill-doublequoted-atpt-test-3 ()
    (ar-test-with-elisp-buffer
	";;; general-close.el --- Insert closing delimiter -*- lexical-binding: t; -*-

;; Authored and maintained by
;; Emacs User Group Berlin <emacs-berlin@emacs-berlin.org>

;; Version: 0.1
;; Keywords: languages, lisp

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

;;; Commentary: M-x general-close RET: close any syntactic element.

;; With optional `general-close-electric-listify-p' set to `t':

;; ['a','b   ==> ['a','b'
;; ['a','b'  ==> ['a','b',

;; With `C-u'
;; ['a','b', ==> ['a','b']

;; An explicit M-x general-close RET will then revert the
;; timer-triggered auto-closed, allowing to continue with contents

;; Some valid Emacs Lisp suitable for testing
;; (setq foo (list \"([{123}])\"))

;; A first draft was published at emacs-devel list:
;; http://lists.gnu.org/archive/html/emacs-devel/2013-09/msg00512.html

;;; Code:

\(defvar general-close-comint-pre-assignment-re   \"let [[:alpha:]][A-Za-z0-9_]\")
\(defcustom general-close-comint-pre-assignment-re
  \"let [[:alpha:]][A-Za-z0-9_]\"
  \"Insert \\\"=\\\" when looking back. \"
  :type 'string
  :tag \"general-close-comint-pre-assignment-re\"
  :group 'general-close)

\(defun general-close--typedef-maybe (beg regexp &optional closer)
  (let (done)
    (when (save-excursion
	    (goto-char beg)
	    (skip-chars-forward \" \\t\\r\\n\\f\")
	    (looking-at regexp))
      (general-close-insert-with-padding-maybe \"Int\")
      (setq done t))
    done))"
      (search-backward "Int")
      (ar-kill-doublequoted-atpt)
      (should (eq (char-after) ?\)))))

(ert-deftest doublequoted-unpaired-delimited-test-1 ()
  (ar-test-with-temp-buffer "\"
     ;;; \" \" Write 'etc. \" \""
      (text-mode)
    (search-backward "rite")
    (let ((erg (ar-doublequoted-atpt)))
      (should erg))))

(ert-deftest doublequoted-unpaired-delimited-test-2 ()
  (ar-test-with-temp-buffer "\"
     ;;; \" \" Write 'etc. \" \""
      (text-mode)
    (goto-char 27)
    (let* (ar-thing-no-nest
	   (erg (ar-doublequoted-atpt)))
      (should erg))))

(ert-deftest doublequoted-unpaired-delimited-test-3 ()
  (ar-test-with-temp-buffer "\"
     ;;; \" \" Write 'etc. \" \""
      (text-mode)
    (search-backward "rite")
    (let* ((ar-thing-no-nest t)
	   (erg (ar-doublequoted-atpt)))
      (should (< 0 (length erg))))))

(ert-deftest doublequoted-scan-whole-buffer-delimited-test-1 ()
  (ar-test-with-temp-buffer "\"
     ;;; \" \" Write 'etc. \" \""
      (emacs-lisp-mode)
    (goto-char 28)
    (let* (ar-thing-no-nest
	   (erg (ar-doublequoted-atpt)))
      (should erg))))

(ert-deftest doublequoted-unpaired-delimited-test-5 ()
  (ar-test-with-temp-buffer
      "(setq foo
      \"class OrderedDict1(dict):
    \\\"\\\"\\\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \\\"\\\"\\\"\")"
      (emacs-lisp-mode)
    (search-backward "class")
        (let ((erg (length (ar-doublequoted-atpt))))
      (should (< 33 erg)))))

(ert-deftest ar-string-test-1 ()
  (ar-test-with-temp-buffer-point-min
      "def foo(arg1, arg2, arg3):
    '''print decorated function call data to stdout.
    '''
    def bar(f):
        print 'Inside wwrap()'
        def wrapped_f(*args):
            print 'Inside wrapped_f()'
            print 'Decorator arguments:', arg1, arg2, arg3
            f(*args)
            print 'After f(*args)'
        return wrapped_f
return wwrap"
    (python-mode)
    (search-forward "print")
    (should (< 2 (length (ar-string-atpt))))))

(ert-deftest ar-brace-greaterangled-test ()
  (ar-test-with-temp-buffer
      ">defun: ())<"
      (forward-char -1)
      (ar-brace-greaterangled-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-count-parentized-in-doublequoted-until-point-test ()
  (ar-test-with-temp-buffer
      "\"(foo) asdf ( foo1 ) (bar) (baz))\""
      (forward-char -2)
    (eq 4  (ar-count-parentized-in-doublequoted-until-point))))

(ert-deftest ar-kill-doublequoted-test-1 ()
  (ar-test-with-temp-buffer
      "\"ssss\"a"
      (forward-char -2)
      (ar-kill-doublequoted-atpt)
    (should (eq (char-after) ?a))))

(ert-deftest ar-kill-doublequoted-test-2 ()
  (ar-test-with-temp-buffer
      "\"ssss\"a"
      (forward-char -2)
      (ar-doublequote-or-copy-atpt -1)
    (should (eq (char-after) ?a))))

;; fails in batch-mode only, moved into interactive tests
;; (ert-deftest ar-delimited2bracket-test-1 ()
;;   (ar-test-with-temp-buffer
;;       "(asdf)"
;;       (forward-char -1)
;;     (ar-delimited2bracketed-atpt)
;;     (sit-for 0.1)
;;     (should (eq 93 (char-after)))))

(ert-deftest ar-ert-peel-list-test-1 ()
  (ar-test-with-elisp-buffer-point-min
      "(/ (* (* n 1) (1+ (* n 1))) 2)"
      (forward-char 2)
    (ar-peel-list-atpt)
    (forward-sexp)
    (should (eobp))))

(ert-deftest ar-ert-forward-parentized-test-1 ()
  (ar-test-with-elisp-buffer-point-min
      "(/ (* (* n 1) (1+ (* n 1))) 2)"
      (search-forward ")" nil t 1)
    (forward-char -1)
    (ar-forward-parentized-atpt)
    (should (eq (char-after) ?\)))))

(ert-deftest doublequoted-escaped-commented-delimited-test ()
  (ar-test-with-elisp-buffer "\"
     ;;; \" \\\" Writer 'etc. \" \""
    (search-backward "riter")
    (let*  ((ar-thing-escaped t)
	    (ar-thing-inside-comments t)
	    (ar-scan-whole-buffer t)
	    (erg (ar-doublequoted-atpt)))
      (should (< 7 (length erg))))))

(ert-deftest elisp-delete-comment-test ()
  (ar-test-with-elisp-buffer
      "(defun ;; foo1"
    (ar-delete-comment-atpt)
    (should (not (eq (char-before) ?\;)))))

(ert-deftest elisp-comment-backward-test ()
  (ar-test-with-elisp-buffer
      "(defun ;; foo1"
    (ar-backward-comment-atpt)
    (should (not (eq (char-before) ?\;)))))

(ert-deftest elisp-comment-beginning-pos-test ()
  (ar-test-with-elisp-buffer
      "(defun ;; foo1
;; asdf"
    (should (eq (ar-comment-beginning-position-atpt) 8))))

(ert-deftest elisp-comment-beginning-pos-test-2 ()
  (ar-test-with-elisp-buffer
      "(defun \;\; foo1 (\&optional beg end)
  \;\; \" \"
  \;\; (interactive \"\*\")"
        (should (eq (ar-comment-beginning-position-atpt) 8))))

(ert-deftest elisp-forward-number-test ()
  (ar-test-with-elisp-buffer-point-min
      "f2oo1"
      (ar-forward-number-atpt)
    (should (eq (char-after) ?2))))

(ert-deftest delimited-paren-test ()
  (ar-test-with-elisp-buffer-point-min
      "(f2oo1)"
      (should (eq 7 (length (ar-delimited-atpt))))))

(ert-deftest ar-doublebackslashparen-char-in-region-test ()
  (ar-test-with-elisp-buffer
      "asdf"
    (push-mark)
    (goto-char (point-min))
    (ar-doublebackslashparen-char-in-region-atpt)
    (skip-chars-forward "^s")
    (should (eq (char-before) 40))
    ))

(ert-deftest ar-hide-delimited-test-cZqMPO ()
  (ar-test-with-elisp-buffer
      "(bar . [((qux . \"hello\")) 3])"
    (search-backward "l")
    (ar-hide-delimited-atpt)
    (skip-chars-backward "^(")))

(provide 'ar-thing-atpt-other-test)
;;; ar-thing-atpt-other-test.el ends here
