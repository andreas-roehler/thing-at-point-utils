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

(ert-deftest delimited-paren-test-AV7ZAT ()
  (ar-test-with-elisp-buffer-point-min
      "(f2oo1) "
      (should (eq 7 (length (ar-delimited-atpt))))))

(ert-deftest ar-doublebackslashparen-char-in-region-test ()
  (ar-test-with-elisp-buffer
      "asdf"
    (goto-char (point-max))
    (push-mark)
    (goto-char (point-min))
    (ar-doublebackslashparen-char-in-region-atpt)
    (skip-chars-forward "^s")
    (should (eq (char-before) 40))))

(ert-deftest ar-hide-delimited-test-cZqMPO ()
  (ar-test-with-elisp-buffer
      "(bar . [((qux . \"hello\")) 3])"
    (search-backward "l")
    (ar-hide-delimited-atpt)
    (skip-chars-backward "^(")))

(ert-deftest ar-trim-delimited-test-yqgUcN ()
  (ar-test-with-elisp-buffer
      "sort2 :: [Int]"
    (goto-char (point-max))
    (forward-char -2)
    (ar-trim-delimited-atpt)
    (should (eq 3 (length (ar-graph-atpt))))))

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
    (backward-char 2)
    (should (string= "[1,3,4,8]" (ar-delimited-atpt)))))

(ert-deftest ar-trim-delimited-atpt-test-CHahdS ()
  (ar-test-with-elisp-buffer
      "(* 2 2)"
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
    (ar-backward-symbol-atpt)
    (should (eq (char-after) ?+))
    (should (bobp))))

(ert-deftest ar-delimited-test-V5mQXw ()
  (ar-test-point-min
      "srcdir=@srcdir@
# MinGW CPPFLAGS may use this.
abs_top_srcdir=@abs_top_srcdir@
"
    'sh-mode
    ar-switch-p
      (search-forward "@s")
    (should (string= "@srcdir@" (ar-delimited-atpt)))))

(ert-deftest ar-delimited-test-nslZtA ()
  (ar-test
      "(defun foo1 (&optional beg end))"
    'emacs-lisp-mode
    ar-switch-p
    (goto-char (point-max))
    (search-backward "opt")
    (should (string= "(&optional beg end)" (ar-delimited-atpt)))))

(ert-deftest ar-delimited-test-ORxX4x ()
  (ar-test
      "(add-to-list 'load-path \"~/foo\")"
    'emacs-lisp-mode
    ar-switch-p
    (search-backward "oo")
    (string= "foo" (ar-delimited-atpt))))

(ert-deftest ar-delimited-test-CkaEZw ()
  (ar-test
      "(* 2 2*)"
    'fundamental-mode
    ar-switch-p
    (search-backward "2")
   (should  (string= "(* 2 2*)" (ar-delimited-atpt)))))

(ert-deftest ar-delimited-test-qLwOV9 ()
  (ar-test
   "(add-to-list 'load-path \"~/arbeit/Emacs-allzeichenabk/emacs-26\.2\")"
   'emacs-lisp-mode
   ar-switch-p
   (search-backward "ema")
   (should (string=  "\"~/arbeit/Emacs-allzeichenabk/emacs-26.2\"" (ar-delimited-atpt)))))

(provide 'ar-thing-atpt-other-test)
;;; ar-thing-atpt-other-test.el ends here
