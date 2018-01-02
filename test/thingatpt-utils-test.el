;;; thingatpt-utils-test.el --- th-at-point edit functions -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2017 Andreas Röhler, unless
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

;;; Commentary:

;;; Code:

(require 'thingatpt-utils-core)
(require 'time-stamp)

(defvar ar-atpt-functions-testlist '())
  (defvar ar-atpt-mv-functionlist '())
  (defvar ar-atpt-delim-test-list '())

(require 'time-stamp)
(require 'thing-at-point-utils)
(require 'thing-classes-in-unpaired-delimited-list)
(require 'ar-comment-lor)
(defcustom th-test-delay 1
    "Seconds to wait until next delimtest functioncall"
    :type 'number
    :group 'werkstatt)

(defun ar-th-emacs-lisp-comment-test (&optional arg)
  " "
  (interactive "p")
  (ar-th-provide-test-buffer "emacs-lisp")
  (when arg (switch-to-buffer (current-buffer)))
  (emacs-lisp-mode)
  (goto-char 9)
  (ar-comment-or-uncomment-lor)
  (forward-line -1)
  (ar-forward-comment-atpt)
  (and
   (assert (eolp)
	   nil "ar-th-emacs-lisp-comment-test failed")
   (message "%s" "ar-th-emacs-lisp-comment-test passed")))

(defun ar-th-html-comment-test (&optional arg)
  (interactive "p")
  (ar-th-provide-test-buffer "html")
  (when arg (switch-to-buffer (current-buffer)))
  (goto-char (point-min)) 
  (search-forward "<!-- ") 
  (sgml-mode)
  (assert (< 0 (ar-length-of-comment-atpt)))
  (ar-backward-comment-atpt)
  (assert (eq (char-after) ?<))
  (assert (and (ar-forward-comment-atpt)(eolp)))
  (message "ar-th-html-comment-tests passed! %s " (time-stamp-string)))


(defun ar-th-test (&optional arg)
  "Checks available THING-atpt forms at point
  Prints forms with values in bufffer `th-test',
  with ARG also forms, which returned `nil' "
  (interactive "p")
  (let ((pos (point))
        (arg (or arg 1))
        (oldbuf (current-buffer)))
    (set-buffer (get-buffer-create "th-test"))
    (erase-buffer)
    (insert (format "Point: %s" pos))
    (newline)
    (set-buffer oldbuf)
    (dolist (elt ar-atpt-functions-testlist)
      (let* ((first (split-string (format "%s" elt) "-"))
             (thing (nth 1 first)))
        (unless (and (featurep 'xemacs) (member thing ar-atpt-classes))
          (save-excursion
            (let* ((item (funcall elt))
                   (output-p (if arg
                                 t
                               (cond ((eq nil item)
                                      nil)
                                     ((and (listp item)
                                           (eq nil (car item)))
                                      nil)
                                     ((and (stringp item)
                                           (string= "" item))
                                      nil)
                                     (t t)))))
              (when output-p
                (set-buffer "th-test")
                (if (listp item)
                    (setq item (concat (format "%s" (car item))" "(format "%s" (cdr item)))))
                (insert (concat (format "%s: " elt) (format "%s \n" item)))))))))
    (set-buffer "th-test")
    (when arg (switch-to-buffer (current-buffer)))
    (goto-char (point-min))
    (message "%s %s" "th-test finished." (time-stamp-string))))


(defun ar-th-mv-test (arg &optional delay)
  "Checks available move-THING-atpt forms at point.
  Shows moves while indicating executed form in message-buffer"
  (interactive "P\np")
    (let ((old-buf (current-buffer))
                  (outbuf "mv-test-newlist")
                  (neuliste ())
                  (arg (or arg 1)))
      ;; (if (string= "emacs-lisp-test" (buffer-name))
      ;; (progn
      (dolist (elt ar-atpt-mv-functionlist)
        (unless (and (featurep 'xemacs) (member thing ar-atpt-classes))
          (when
              (functionp (intern-soft elt))
            (when (eq 4 (prefix-numeric-value arg)) (message "   %s" elt))
            (save-excursion
              (funcall elt)
              (when (eq 4 (prefix-numeric-value delay)) (message "Point: %s %s " (point) elt) (sit-for 1))
              (push (number-to-string (point)) neuliste)
              (when (eq 4 (prefix-numeric-value delay)) (sit-for th-test-delay))))))
      ;;  don't work, bug?
      ;; (with-output-to-temp-buffer "mv-test-newlist"
      (goto-outbuf outbuf arg)
      (erase-buffer)
      (emacs-lisp-mode)
      ;; (when (eq 4 (prefix-numeric-value arg))
      ;; (list-insert-assert neuliste mv-test-assert-values "th-mv-test"))
      ;; (message "%s" "Need a \"emacs-lisp-test\"-buffer, run th-delimtest before")))
      (set-buffer old-buf)
      (when arg (switch-to-buffer (current-buffer)))
      (message "%s %s" "th-mv-test done" (time-stamp-string))))


(defun list-insert-assert (newlist oldlist &optional info reverse)
  "Inserts newlist, evals it and asserts with oldlist. "
  (unless (empty-line-p) (newline 2))
  (insert "(setq newlist '(")
                  (dolist (elt newlist)
                    (insert elt) (insert " "))
                  (insert "))
")
  (backward-list)
  (narrow-to-region (point) (progn (forward-list) (point)))
  (eval-buffer)
  (widen)
  (assert (equal newlist oldlist) t info))

(defun inaugurate-test-buf (test-buffer-name &optional vorlage iact)
  (let ((test-buffer-content (or vorlage ar-th-emacs-lisp-test-string)))
    (set-buffer (get-buffer-create test-buffer-name))
    (erase-buffer)
    (save-excursion
      (insert test-buffer-content))))

(defun goto-outbuf (outbuf &optional iact)
  "Go to the end of a named puffer, provide a newline.
  Show it, if interactively called. "
  (if (buffer-live-p outbuf)
      (set-buffer outbuf)
    (set-buffer (get-buffer-create outbuf)))
  (goto-char (point-max))
  (unless (empty-line-p) (newline)))

(defun mv-test-assertions (&optional mv-liste)
  (interactive "p")
    (let ((liste (or mv-liste mv-liste (nreverse mv-test-assert-values))))
    (dolist (elt ar-atpt-mv-functionlist)
      (save-excursion (funcall elt)
                      (assert (eq (pop liste) (point)))))
  (message "%s %s" "Test mv-test-assertions done!" (time-stamp-string))))

(defun ar-th-delimtest (&optional arg liste)
  " "
  (interactive "*P")
  (let ((testlist (or liste ar-atpt-delim-test-list)))
    (ar-th-provide-test-buffer "emacs-lisp")
    (when arg (switch-to-buffer (current-buffer)))
    (goto-char 9)
    (save-excursion
      (dolist (elt testlist)
        (when arg (message "   %s" elt))
        (save-excursion
          (let* ((command (format "%s" elt))
                 (point-max-orig (point-max))
                 (zeichen (substring command (1+ (string-match "-[^-]+-[^-]+$" command)) (string-match "-[^-]+$" command))))
            (when (memq (intern-soft zeichen) ar-atpt-classes)
              (skip-chars-forward (concat "^[:" zeichen ":]")))
            (funcall elt)
;;            (unless (string-match "abbrev\\|acronym\\|angled-greater\\|comment\\|date\\|upper" command)
;;              (assert (< point-max-orig (point-max)) t (concat command " schlug fehl!")))
            (when (eq 4 (prefix-numeric-value arg)) (sit-for th-test-delay)))))
      (message "%s %s" "th-delimtest done" (time-stamp-string)))))


(defun ar-th-sort-test (&optional arg) 
  (interactive "p")
  (let ((new-sort-test-assert-values ())
        (old-buf (current-buffer)))
    (inaugurate-test-buf "sort-test-assertions" nil arg)
    (when arg (switch-to-buffer (current-buffer)))
    (narrow-to-defun)
    (mark-whole-buffer)
    (ar-sort-word-atpt nil (point-min) (point-max))
    (goto-char (point-min)))
    (message "%s %s"  "th-sort-test done" (time-stamp-string)))

(defun sort-test-assertions ()
  (interactive "p")
  (let ((sort-test-assert-values sort-test-assert-values))
    (while (ar-forward-word-atpt)
      (dolist (elt sort-test-assert-values)
        (assert (string= (pop sort-test-assert-values) (word-at-point)))))
    (message "   %s" "Test sort-test-assertions done!")))

(defun ar-th-string-strip-test ()
  (interactive "p")
  (let ((text " asdf "))
    (assert (eq 4 (length (strip text))))
    (assert (eq 5 (length (lstrip text))))
    (assert (eq 5 (length (rstrip text)))))
  (message "%s %s" "ar-th-string-strip-test done" (time-stamp-string)))

(defun ar-th-provide-test-buffer (mode)
  (set-buffer (get-buffer-create (concat "ar-th-" mode "-test")))
  (erase-buffer)
  (insert (eval (intern-soft (concat "ar-th-" mode "-test-string"))))
  (unless (featurep 'xemacs)(transient-mark-mode 1)))


(defun ar-th-forward-backward-test ()
  (interactive "p")
  (ar-th-provide-test-buffer "html")
  (when arg (switch-to-buffer (current-buffer)))
  (goto-char 111)
  (assert (eq 119 (ar-forward-graph-atpt)))
  (assert (eq 169 (ar-forward-mlattribut-atpt)))
  (assert (eq 202 (ar-forward-mlattribut-atpt)))
  (assert (eq 175 (ar-backward-mlattribut-atpt)))
  (assert (eq 134 (ar-backward-mlattribut-atpt)))
  (assert (eq 246 (ar-forward-markup-atpt)))
  (message "%s %s" "ar-th-forward-backward-test done" (time-stamp-string)))

(defun ar-th-markup-test ()
  (interactive "p")
  (ar-th-provide-test-buffer "html")
  (when arg (switch-to-buffer (current-buffer)))
  (goto-char (point-min)) 
  (search-forward ".org") 
  (ar-th-markup-attribut-test)
  (message "%s %s" "ar-th-markup-test done" (time-stamp-string)))

(defun ar-th-markup-attribut-test ()
  (let ((att (ar-mlattribut-atpt))
        (bounds (ar-bounds-of-mlattribut-atpt)))
    (assert (stringp att))
    (assert (not (string= "" att)))
    (assert (< (car bounds) (cdr bounds)))))

(defun ar-th-delimit-tests (&optional arg) 
  (interactive "p")
  (ar-th-provide-test-buffer "html")
  (when arg (switch-to-buffer (current-buffer)))
  (goto-char 106)
  (let ((orig (copy-marker (point))))
    (ar-doublebackslashparen-alnum-atpt)
    (goto-char orig)
    (ar-blok-graph-atpt)
    (goto-char orig)
    (goto-char orig)
    (let ((bounds (ar-bounds-of-blok-atpt)))
      (assert (and (< (car bounds) orig) (< orig (cdr bounds)))))
    (let ((bounds (ar-bounds-of-backslashedparen-atpt)))
      (assert (and (< (car bounds) orig) (< orig (cdr bounds)))))
    (let ((bounds (ar-bounds-of-doublebackslashedparen-atpt)))
      (assert (and (< (car bounds) orig) (< orig (cdr bounds)))))
    (message "%s %s" "ar-th-delimit-tests done" (time-stamp-string))))

(setq ar-th-html-test-string "
<!doctype html public \"-//W3C//DTD HTML 3.2//EN\">
<html>
<head><title>About emacs-w3m</title></head>
<body>
<center>
Welcome to <a href=\"http://emacs-w3m.namazu.org/\"><img src=\"about://emacs-w3m.gif\" alt=\"emacs-w3m\" width=\"83\"
height=\"14\"></a>!<br><br>
emacs-w3m is an interface program of
<!-- emacs-w3m is an interface program of -->
<!-- emacs-w3m is an interface program of -->
<!-- emacs-w3m is an interface program of -->
<!-- emacs-w3m is an interface program of -->
<a href=\"http://w3m.sourceforge.net/\">w3m</a>,
works on Emacs.
</center>
</body>
</html>
")

(defun ar-th-do-tests (&optional arg)
  (interactive "p")
  (message "%s %s" "ar-th-emacs-lisp-comment-test start" (time-stamp-string))
  (ar-th-emacs-lisp-comment-test)
  (message "%s %s" "ar-th-html-comment-test start" (time-stamp-string))
  (ar-th-html-comment-test)
  (ar-th-provide-test-buffer "emacs-lisp")
  (let ((thing-mark-region nil)
        (thing-copy-region nil)
        (oldbuf (current-buffer)))
    (message "%s %s" "th-delimtest start" (time-stamp-string))
    (ar-th-delimtest th-test-delay)
    (message "%s %s" "th-mv-test start" (time-stamp-string))
    (ar-th-mv-test arg)
    (set-buffer oldbuf) 
    (when arg (switch-to-buffer oldbuf))
    (message "%s %s" "th-test start" (time-stamp-string))
    (ar-th-test)
    (message "%s %s" "ar-th-markup-test start" (time-stamp-string))
    (ar-th-markup-test)
    (message "%s %s" "ar-th-delimit-tests start" (time-stamp-string))
    (ar-th-delimit-tests)
    (message "%s %s" "ar-th-string-strip-test start" (time-stamp-string))
    (ar-th-string-strip-test)
    (message "%s %s" "ar-test-trim-braces-in-doublequotes-atpt start" (time-stamp-string))
    (ar-test-trim-braces-in-doublequotes-atpt)
    (message "%s %s" "ar-th-sort-test start" (time-stamp-string))
    (ar-th-sort-test)))

(setq ar-atpt-functions-testlist '(ar-alnum-atpt ar-bounds-of-alnum-atpt ar-alnum-end-position-atpt ar-copy-alnum-atpt ar-alpha-atpt ar-bounds-of-alpha-atpt ar-alpha-end-position-atpt ar-copy-alpha-atpt ar-ascii-atpt ar-bounds-of-ascii-atpt ar-ascii-end-position-atpt ar-copy-ascii-atpt ar-blank-atpt ar-bounds-of-blank-atpt ar-blank-end-position-atpt ar-copy-blank-atpt ar-cntrl-atpt ar-bounds-of-cntrl-atpt ar-cntrl-end-position-atpt ar-copy-cntrl-atpt ar-digit-atpt ar-bounds-of-digit-atpt ar-digit-end-position-atpt ar-copy-digit-atpt ar-graph-atpt ar-bounds-of-graph-atpt ar-graph-end-position-atpt ar-copy-graph-atpt ar-lower-atpt ar-bounds-of-lower-atpt ar-lower-end-position-atpt ar-copy-lower-atpt ar-nonascii-atpt ar-bounds-of-nonascii-atpt ar-nonascii-end-position-atpt ar-copy-nonascii-atpt ar-print-atpt ar-bounds-of-print-atpt ar-print-end-position-atpt ar-copy-print-atpt ar-punct-atpt ar-bounds-of-punct-atpt ar-punct-end-position-atpt ar-copy-punct-atpt ar-space-atpt ar-bounds-of-space-atpt ar-space-end-position-atpt ar-copy-space-atpt ar-upper-atpt ar-bounds-of-upper-atpt ar-upper-end-position-atpt ar-copy-upper-atpt ar-xdigit-atpt ar-bounds-of-xdigit-atpt ar-xdigit-end-position-atpt ar-copy-xdigit-atpt ar-braced-atpt ar-bounds-of-braced-atpt ar-braced-end-position-atpt ar-copy-braced-atpt ar-bracketed-atpt ar-bounds-of-bracketed-atpt ar-bracketed-end-position-atpt ar-copy-bracketed-atpt ar-lesserangled-atpt ar-bounds-of-lesserangled-atpt ar-lesserangled-end-position-atpt ar-copy-lesserangled-atpt ar-greaterangled-atpt ar-bounds-of-greaterangled-atpt ar-greaterangled-end-position-atpt ar-copy-greaterangled-atpt ar-leftrightsinglequoted-atpt ar-bounds-of-leftrightsinglequoted-atpt ar-leftrightsinglequoted-end-position-atpt ar-copy-leftrightsinglequoted-atpt ar-leftrightdoublequoted-atpt ar-bounds-of-leftrightdoublequoted-atpt ar-leftrightdoublequoted-end-position-atpt ar-copy-leftrightdoublequoted-atpt ar-parentized-atpt ar-bounds-of-parentized-atpt ar-parentized-end-position-atpt ar-copy-parentized-atpt ))

(setq ar-atpt-mv-functionlist '(ar-forward-alnum-atpt  ar-backward-alnum-atpt  ar-forward-alpha-atpt  ar-backward-alpha-atpt  ar-forward-ascii-atpt  ar-backward-ascii-atpt  ar-forward-blank-atpt  ar-backward-blank-atpt  ar-forward-cntrl-atpt  ar-backward-cntrl-atpt  ar-forward-digit-atpt  ar-backward-digit-atpt  ar-forward-graph-atpt  ar-backward-graph-atpt  ar-forward-lower-atpt  ar-backward-lower-atpt  ar-forward-nonascii-atpt  ar-backward-nonascii-atpt  ar-forward-print-atpt  ar-backward-print-atpt  ar-forward-punct-atpt  ar-backward-punct-atpt  ar-forward-space-atpt  ar-backward-space-atpt  ar-forward-upper-atpt  ar-backward-upper-atpt  ar-forward-xdigit-atpt  ar-backward-xdigit-atpt  ar-forward-braced-atpt  ar-backward-braced-atpt  ar-forward-bracketed-atpt  ar-backward-bracketed-atpt  ar-forward-lesserangled-atpt  ar-backward-lesserangled-atpt  ar-forward-greaterangled-atpt  ar-backward-greaterangled-atpt  ar-forward-leftrightsinglequoted-atpt  ar-backward-leftrightsinglequoted-atpt  ar-forward-leftrightdoublequoted-atpt  ar-backward-leftrightdoublequoted-atpt  ar-forward-parentized-atpt  ar-backward-parentized-atpt  ))

(setq ar-atpt-delim-test-list '(ar-brace-alnum-atpt  ar-bracket-alnum-atpt  ar-lesserangle-alnum-atpt  ar-greaterangle-alnum-atpt  ar-leftrightsinglequote-alnum-atpt  ar-leftrightdoublequote-alnum-atpt  ar-parentize-alnum-atpt  ar-backslash-alnum-atpt  ar-backtick-alnum-atpt  ar-colon-alnum-atpt  ar-cross-alnum-atpt  ar-dollar-alnum-atpt  ar-doublequote-alnum-atpt  ar-equalize-alnum-atpt  ar-escape-alnum-atpt  ar-hash-alnum-atpt  ar-hyphen-alnum-atpt  ar-singlequote-alnum-atpt  ar-slash-alnum-atpt  ar-star-alnum-atpt  ar-tild-alnum-atpt  ar-underscore-alnum-atpt  ar-whitespace-alnum-atpt  ar-doubleslash-alnum-atpt  ar-brace-alpha-atpt  ar-bracket-alpha-atpt  ar-lesserangle-alpha-atpt  ar-greaterangle-alpha-atpt  ar-leftrightsinglequote-alpha-atpt  ar-leftrightdoublequote-alpha-atpt  ar-parentize-alpha-atpt  ar-backslash-alpha-atpt  ar-backtick-alpha-atpt  ar-colon-alpha-atpt  ar-cross-alpha-atpt  ar-dollar-alpha-atpt  ar-doublequote-alpha-atpt  ar-equalize-alpha-atpt  ar-escape-alpha-atpt  ar-hash-alpha-atpt  ar-hyphen-alpha-atpt  ar-singlequote-alpha-atpt  ar-slash-alpha-atpt  ar-star-alpha-atpt  ar-tild-alpha-atpt  ar-underscore-alpha-atpt  ar-whitespace-alpha-atpt  ar-doubleslash-alpha-atpt  ar-brace-ascii-atpt  ar-bracket-ascii-atpt  ar-lesserangle-ascii-atpt  ar-greaterangle-ascii-atpt  ar-leftrightsinglequote-ascii-atpt  ar-leftrightdoublequote-ascii-atpt  ar-parentize-ascii-atpt  ar-backslash-ascii-atpt  ar-backtick-ascii-atpt  ar-colon-ascii-atpt  ar-cross-ascii-atpt  ar-dollar-ascii-atpt  ar-doublequote-ascii-atpt  ar-equalize-ascii-atpt  ar-escape-ascii-atpt  ar-hash-ascii-atpt  ar-hyphen-ascii-atpt  ar-singlequote-ascii-atpt  ar-slash-ascii-atpt  ar-star-ascii-atpt  ar-tild-ascii-atpt  ar-underscore-ascii-atpt  ar-whitespace-ascii-atpt  ar-doubleslash-ascii-atpt  ar-brace-blank-atpt  ar-bracket-blank-atpt  ar-lesserangle-blank-atpt  ar-greaterangle-blank-atpt  ar-leftrightsinglequote-blank-atpt  ar-leftrightdoublequote-blank-atpt  ar-parentize-blank-atpt  ar-backslash-blank-atpt  ar-backtick-blank-atpt  ar-colon-blank-atpt  ar-cross-blank-atpt  ar-dollar-blank-atpt  ar-doublequote-blank-atpt  ar-equalize-blank-atpt  ar-escape-blank-atpt  ar-hash-blank-atpt  ar-hyphen-blank-atpt  ar-singlequote-blank-atpt  ar-slash-blank-atpt  ar-star-blank-atpt  ar-tild-blank-atpt  ar-underscore-blank-atpt  ar-whitespace-blank-atpt  ar-doubleslash-blank-atpt  ar-brace-cntrl-atpt  ar-bracket-cntrl-atpt  ar-lesserangle-cntrl-atpt  ar-greaterangle-cntrl-atpt  ar-leftrightsinglequote-cntrl-atpt  ar-leftrightdoublequote-cntrl-atpt  ar-parentize-cntrl-atpt  ar-backslash-cntrl-atpt  ar-backtick-cntrl-atpt  ar-colon-cntrl-atpt  ar-cross-cntrl-atpt  ar-dollar-cntrl-atpt  ar-doublequote-cntrl-atpt  ar-equalize-cntrl-atpt  ar-escape-cntrl-atpt  ar-hash-cntrl-atpt  ar-hyphen-cntrl-atpt  ar-singlequote-cntrl-atpt  ar-slash-cntrl-atpt  ar-star-cntrl-atpt  ar-tild-cntrl-atpt  ar-underscore-cntrl-atpt  ar-whitespace-cntrl-atpt  ar-doubleslash-cntrl-atpt  ar-brace-digit-atpt  ar-bracket-digit-atpt  ar-lesserangle-digit-atpt  ar-greaterangle-digit-atpt  ar-leftrightsinglequote-digit-atpt  ar-leftrightdoublequote-digit-atpt  ar-parentize-digit-atpt  ar-backslash-digit-atpt  ar-backtick-digit-atpt  ar-colon-digit-atpt  ar-cross-digit-atpt  ar-dollar-digit-atpt  ar-doublequote-digit-atpt  ar-equalize-digit-atpt  ar-escape-digit-atpt  ar-hash-digit-atpt  ar-hyphen-digit-atpt  ar-singlequote-digit-atpt  ar-slash-digit-atpt  ar-star-digit-atpt  ar-tild-digit-atpt  ar-underscore-digit-atpt  ar-whitespace-digit-atpt  ar-doubleslash-digit-atpt  ar-brace-graph-atpt  ar-bracket-graph-atpt  ar-lesserangle-graph-atpt  ar-greaterangle-graph-atpt  ar-leftrightsinglequote-graph-atpt  ar-leftrightdoublequote-graph-atpt  ar-parentize-graph-atpt  ar-backslash-graph-atpt  ar-backtick-graph-atpt  ar-colon-graph-atpt  ar-cross-graph-atpt  ar-dollar-graph-atpt  ar-doublequote-graph-atpt  ar-equalize-graph-atpt  ar-escape-graph-atpt  ar-hash-graph-atpt  ar-hyphen-graph-atpt  ar-singlequote-graph-atpt  ar-slash-graph-atpt  ar-star-graph-atpt  ar-tild-graph-atpt  ar-underscore-graph-atpt  ar-whitespace-graph-atpt  ar-doubleslash-graph-atpt  ar-brace-lower-atpt  ar-bracket-lower-atpt  ar-lesserangle-lower-atpt  ar-greaterangle-lower-atpt  ar-leftrightsinglequote-lower-atpt  ar-leftrightdoublequote-lower-atpt  ar-parentize-lower-atpt  ar-backslash-lower-atpt  ar-backtick-lower-atpt  ar-colon-lower-atpt  ar-cross-lower-atpt  ar-dollar-lower-atpt  ar-doublequote-lower-atpt  ar-equalize-lower-atpt  ar-escape-lower-atpt  ar-hash-lower-atpt  ar-hyphen-lower-atpt  ar-singlequote-lower-atpt  ar-slash-lower-atpt  ar-star-lower-atpt  ar-tild-lower-atpt  ar-underscore-lower-atpt  ar-whitespace-lower-atpt  ar-doubleslash-lower-atpt  ar-brace-nonascii-atpt  ar-bracket-nonascii-atpt  ar-lesserangle-nonascii-atpt  ar-greaterangle-nonascii-atpt  ar-leftrightsinglequote-nonascii-atpt  ar-leftrightdoublequote-nonascii-atpt  ar-parentize-nonascii-atpt  ar-backslash-nonascii-atpt  ar-backtick-nonascii-atpt  ar-colon-nonascii-atpt  ar-cross-nonascii-atpt  ar-dollar-nonascii-atpt  ar-doublequote-nonascii-atpt  ar-equalize-nonascii-atpt  ar-escape-nonascii-atpt  ar-hash-nonascii-atpt  ar-hyphen-nonascii-atpt  ar-singlequote-nonascii-atpt  ar-slash-nonascii-atpt  ar-star-nonascii-atpt  ar-tild-nonascii-atpt  ar-underscore-nonascii-atpt  ar-whitespace-nonascii-atpt  ar-doubleslash-nonascii-atpt  ar-brace-print-atpt  ar-bracket-print-atpt  ar-lesserangle-print-atpt  ar-greaterangle-print-atpt  ar-leftrightsinglequote-print-atpt  ar-leftrightdoublequote-print-atpt  ar-parentize-print-atpt  ar-backslash-print-atpt  ar-backtick-print-atpt  ar-colon-print-atpt  ar-cross-print-atpt  ar-dollar-print-atpt  ar-doublequote-print-atpt  ar-equalize-print-atpt  ar-escape-print-atpt  ar-hash-print-atpt  ar-hyphen-print-atpt  ar-singlequote-print-atpt  ar-slash-print-atpt  ar-star-print-atpt  ar-tild-print-atpt  ar-underscore-print-atpt  ar-whitespace-print-atpt  ar-doubleslash-print-atpt  ar-brace-punct-atpt  ar-bracket-punct-atpt  ar-lesserangle-punct-atpt  ar-greaterangle-punct-atpt  ar-leftrightsinglequote-punct-atpt  ar-leftrightdoublequote-punct-atpt  ar-parentize-punct-atpt  ar-backslash-punct-atpt  ar-backtick-punct-atpt  ar-colon-punct-atpt  ar-cross-punct-atpt  ar-dollar-punct-atpt  ar-doublequote-punct-atpt  ar-equalize-punct-atpt  ar-escape-punct-atpt  ar-hash-punct-atpt  ar-hyphen-punct-atpt  ar-singlequote-punct-atpt  ar-slash-punct-atpt  ar-star-punct-atpt  ar-tild-punct-atpt  ar-underscore-punct-atpt  ar-whitespace-punct-atpt  ar-doubleslash-punct-atpt  ar-brace-space-atpt  ar-bracket-space-atpt  ar-lesserangle-space-atpt  ar-greaterangle-space-atpt  ar-leftrightsinglequote-space-atpt  ar-leftrightdoublequote-space-atpt  ar-parentize-space-atpt  ar-backslash-space-atpt  ar-backtick-space-atpt  ar-colon-space-atpt  ar-cross-space-atpt  ar-dollar-space-atpt  ar-doublequote-space-atpt  ar-equalize-space-atpt  ar-escape-space-atpt  ar-hash-space-atpt  ar-hyphen-space-atpt  ar-singlequote-space-atpt  ar-slash-space-atpt  ar-star-space-atpt  ar-tild-space-atpt  ar-underscore-space-atpt  ar-whitespace-space-atpt  ar-doubleslash-space-atpt  ar-brace-upper-atpt  ar-bracket-upper-atpt  ar-lesserangle-upper-atpt  ar-greaterangle-upper-atpt  ar-leftrightsinglequote-upper-atpt  ar-leftrightdoublequote-upper-atpt  ar-parentize-upper-atpt  ar-backslash-upper-atpt  ar-backtick-upper-atpt  ar-colon-upper-atpt  ar-cross-upper-atpt  ar-dollar-upper-atpt  ar-doublequote-upper-atpt  ar-equalize-upper-atpt  ar-escape-upper-atpt  ar-hash-upper-atpt  ar-hyphen-upper-atpt  ar-singlequote-upper-atpt  ar-slash-upper-atpt  ar-star-upper-atpt  ar-tild-upper-atpt  ar-underscore-upper-atpt  ar-whitespace-upper-atpt  ar-doubleslash-upper-atpt  ar-brace-xdigit-atpt  ar-bracket-xdigit-atpt  ar-lesserangle-xdigit-atpt  ar-greaterangle-xdigit-atpt  ar-leftrightsinglequote-xdigit-atpt  ar-leftrightdoublequote-xdigit-atpt  ar-parentize-xdigit-atpt  ar-backslash-xdigit-atpt  ar-backtick-xdigit-atpt  ar-colon-xdigit-atpt  ar-cross-xdigit-atpt  ar-dollar-xdigit-atpt  ar-doublequote-xdigit-atpt  ar-equalize-xdigit-atpt  ar-escape-xdigit-atpt  ar-hash-xdigit-atpt  ar-hyphen-xdigit-atpt  ar-singlequote-xdigit-atpt  ar-slash-xdigit-atpt  ar-star-xdigit-atpt  ar-tild-xdigit-atpt  ar-underscore-xdigit-atpt  ar-whitespace-xdigit-atpt  ar-doubleslash-xdigit-atpt  ar-brace-braced-atpt  ar-bracket-braced-atpt  ar-lesserangle-braced-atpt  ar-greaterangle-braced-atpt  ar-leftrightsinglequote-braced-atpt  ar-leftrightdoublequote-braced-atpt  ar-parentize-braced-atpt  ar-backslash-braced-atpt  ar-backtick-braced-atpt  ar-colon-braced-atpt  ar-cross-braced-atpt  ar-dollar-braced-atpt  ar-doublequote-braced-atpt  ar-equalize-braced-atpt  ar-escape-braced-atpt  ar-hash-braced-atpt  ar-hyphen-braced-atpt  ar-singlequote-braced-atpt  ar-slash-braced-atpt  ar-star-braced-atpt  ar-tild-braced-atpt  ar-underscore-braced-atpt  ar-whitespace-braced-atpt  ar-doubleslash-braced-atpt  ar-brace-bracketed-atpt  ar-bracket-bracketed-atpt  ar-lesserangle-bracketed-atpt  ar-greaterangle-bracketed-atpt  ar-leftrightsinglequote-bracketed-atpt  ar-leftrightdoublequote-bracketed-atpt  ar-parentize-bracketed-atpt  ar-backslash-bracketed-atpt  ar-backtick-bracketed-atpt  ar-colon-bracketed-atpt  ar-cross-bracketed-atpt  ar-dollar-bracketed-atpt  ar-doublequote-bracketed-atpt  ar-equalize-bracketed-atpt  ar-escape-bracketed-atpt  ar-hash-bracketed-atpt  ar-hyphen-bracketed-atpt  ar-singlequote-bracketed-atpt  ar-slash-bracketed-atpt  ar-star-bracketed-atpt  ar-tild-bracketed-atpt  ar-underscore-bracketed-atpt  ar-whitespace-bracketed-atpt  ar-doubleslash-bracketed-atpt  ar-brace-lesserangled-atpt  ar-bracket-lesserangled-atpt  ar-lesserangle-lesserangled-atpt  ar-greaterangle-lesserangled-atpt  ar-leftrightsinglequote-lesserangled-atpt  ar-leftrightdoublequote-lesserangled-atpt  ar-parentize-lesserangled-atpt  ar-backslash-lesserangled-atpt  ar-backtick-lesserangled-atpt  ar-colon-lesserangled-atpt  ar-cross-lesserangled-atpt  ar-dollar-lesserangled-atpt  ar-doublequote-lesserangled-atpt  ar-equalize-lesserangled-atpt  ar-escape-lesserangled-atpt  ar-hash-lesserangled-atpt  ar-hyphen-lesserangled-atpt  ar-singlequote-lesserangled-atpt  ar-slash-lesserangled-atpt  ar-star-lesserangled-atpt  ar-tild-lesserangled-atpt  ar-underscore-lesserangled-atpt  ar-whitespace-lesserangled-atpt  ar-doubleslash-lesserangled-atpt  ar-brace-greaterangled-atpt  ar-bracket-greaterangled-atpt  ar-lesserangle-greaterangled-atpt  ar-greaterangle-greaterangled-atpt  ar-leftrightsinglequote-greaterangled-atpt  ar-leftrightdoublequote-greaterangled-atpt  ar-parentize-greaterangled-atpt  ar-backslash-greaterangled-atpt  ar-backtick-greaterangled-atpt  ar-colon-greaterangled-atpt  ar-cross-greaterangled-atpt  ar-dollar-greaterangled-atpt  ar-doublequote-greaterangled-atpt  ar-equalize-greaterangled-atpt  ar-escape-greaterangled-atpt  ar-hash-greaterangled-atpt  ar-hyphen-greaterangled-atpt  ar-singlequote-greaterangled-atpt  ar-slash-greaterangled-atpt  ar-star-greaterangled-atpt  ar-tild-greaterangled-atpt  ar-underscore-greaterangled-atpt  ar-whitespace-greaterangled-atpt  ar-doubleslash-greaterangled-atpt  ar-brace-leftrightsinglequoted-atpt  ar-bracket-leftrightsinglequoted-atpt  ar-lesserangle-leftrightsinglequoted-atpt  ar-greaterangle-leftrightsinglequoted-atpt  ar-leftrightsinglequote-leftrightsinglequoted-atpt  ar-leftrightdoublequote-leftrightsinglequoted-atpt  ar-parentize-leftrightsinglequoted-atpt  ar-backslash-leftrightsinglequoted-atpt  ar-backtick-leftrightsinglequoted-atpt  ar-colon-leftrightsinglequoted-atpt  ar-cross-leftrightsinglequoted-atpt  ar-dollar-leftrightsinglequoted-atpt  ar-doublequote-leftrightsinglequoted-atpt  ar-equalize-leftrightsinglequoted-atpt  ar-escape-leftrightsinglequoted-atpt  ar-hash-leftrightsinglequoted-atpt  ar-hyphen-leftrightsinglequoted-atpt  ar-singlequote-leftrightsinglequoted-atpt  ar-slash-leftrightsinglequoted-atpt  ar-star-leftrightsinglequoted-atpt  ar-tild-leftrightsinglequoted-atpt  ar-underscore-leftrightsinglequoted-atpt  ar-whitespace-leftrightsinglequoted-atpt  ar-doubleslash-leftrightsinglequoted-atpt  ar-brace-leftrightdoublequoted-atpt  ar-bracket-leftrightdoublequoted-atpt  ar-lesserangle-leftrightdoublequoted-atpt  ar-greaterangle-leftrightdoublequoted-atpt  ar-leftrightsinglequote-leftrightdoublequoted-atpt  ar-leftrightdoublequote-leftrightdoublequoted-atpt  ar-parentize-leftrightdoublequoted-atpt  ar-backslash-leftrightdoublequoted-atpt  ar-backtick-leftrightdoublequoted-atpt  ar-colon-leftrightdoublequoted-atpt  ar-cross-leftrightdoublequoted-atpt  ar-dollar-leftrightdoublequoted-atpt  ar-doublequote-leftrightdoublequoted-atpt  ar-equalize-leftrightdoublequoted-atpt  ar-escape-leftrightdoublequoted-atpt  ar-hash-leftrightdoublequoted-atpt  ar-hyphen-leftrightdoublequoted-atpt  ar-singlequote-leftrightdoublequoted-atpt  ar-slash-leftrightdoublequoted-atpt  ar-star-leftrightdoublequoted-atpt  ar-tild-leftrightdoublequoted-atpt  ar-underscore-leftrightdoublequoted-atpt  ar-whitespace-leftrightdoublequoted-atpt  ar-doubleslash-leftrightdoublequoted-atpt  ar-brace-parentized-atpt  ar-bracket-parentized-atpt  ar-lesserangle-parentized-atpt  ar-greaterangle-parentized-atpt  ar-leftrightsinglequote-parentized-atpt  ar-leftrightdoublequote-parentized-atpt  ar-parentize-parentized-atpt  ar-backslash-parentized-atpt  ar-backtick-parentized-atpt  ar-colon-parentized-atpt  ar-cross-parentized-atpt  ar-dollar-parentized-atpt  ar-doublequote-parentized-atpt  ar-equalize-parentized-atpt  ar-escape-parentized-atpt  ar-hash-parentized-atpt  ar-hyphen-parentized-atpt  ar-singlequote-parentized-atpt  ar-slash-parentized-atpt  ar-star-parentized-atpt  ar-tild-parentized-atpt  ar-underscore-parentized-atpt  ar-whitespace-parentized-atpt  ar-doubleslash-parentized-atpt  ))
;; ar-th-emacs-lisp-test-string start
(setq ar-th-emacs-lisp-test-string "
\(defun foo1 (&optional arg beg end)
  \" \"
  (interactive \"\*p\")
  (let ((beg (cond (beg beg)
		   ((use-region-p)
		    (region-beginning))
		   (t (point-min))))
	(end (cond (end (copy-marker end))
		   ((use-region-p)
		    (copy-marker (region-end)))
		   (t (copy-marker (point-max))))))
    (save-excursion
      (goto-char beg))
    (when arg (message \"%s %s\" beg end))))

\(defun foo2 (&optional arg beg end)
  \" \"
  (interactive \"\*p\")
  (let ((beg (cond (beg beg)
		   ((use-region-p)
		    (region-beginning))
		   (t (point-min))))
	(end (cond (end (copy-marker end))
		   ((use-region-p)
		    (copy-marker (region-end)))
		   (t (copy-marker (point-max))))))
    (save-excursion
      (goto-char beg))
    (when arg (message \"%s %s\" beg end))))

\(defun foo3 (&optional arg beg end)
  \" \"
  (interactive \"\*p\")
  (let ((beg (cond (beg beg)
		   ((use-region-p)
		    (region-beginning))
		   (t (point-min))))
	(end (cond (end (copy-marker end))
		   ((use-region-p)
		    (copy-marker (region-end)))
		   (t (copy-marker (point-max))))))
    (save-excursion
      (goto-char beg))
    (when arg (message \"%s %s\" beg end))))")
;; ar-th-emacs-lisp-test-string end
(setq mv-test-assert-values '(343 459 343 494 343 350 337 351 1 826 1 1723 1 1723 1 1723 342 346 343 365 322 360 79 484 342 352 341 347 342 346 1 1722 1 1723 80 458 1 528 1 485 218 390 292 348 1 1722 1 1722 337 351 1 1723 332 432 343 494 282 348 1 528 1 1723 1 1723 80 483 1 1281 1 459 342 346 1 1723 322 360 338 350 1 459 342 353 1 1281 337 351 1 1723 1 1723 1 529 215 351 214 356 204 356 79 484 333 364 1 1722 330 365 332 421 332 366 343 494 343 459))

(setq sort-test-assert-values (nreverse '(%s %s active active beg beg beg beg beg beg beginning char cond cond copy copy copy defun end end end end end end excursion foo1 goto interactive interactive let marker marker marker max message min optional p p p point point region region region region save t t when)))
