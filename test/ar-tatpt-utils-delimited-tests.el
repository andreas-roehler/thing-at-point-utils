;;; ar-tatpt-utils-delimited-tests.el --- tests

;; Copyright (C) 2015-2020  Andreas Röhler

;; Author: Andreas Roehler <andreas.roehler@online.de>
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

;;; Commentary: test strings edited after generation

;;

;;; Code:

;; ar-thing-at-point-utils-delimited-tests: ar-unpaired-delimited-raw start

;; (defvar ar-debug-p t)

(ert-deftest ar-backslashed-atpt-old-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (should (stringp (ar-th 'backslashed)))))

(ert-deftest  ar-backslashed-bounds-atpt-old-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (should (listp (ar-th-bounds 'backslashed)))))

(ert-deftest ar-backslashed-beginning-position-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (should (ar-th-beg 'backslashed))))

(ert-deftest ar-backslashed-end-position-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (should (ar-th-end 'backslashed))))

(ert-deftest ar-backslashed-beginning-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (should (ar-th-gotobeg 'backslashed))))

(ert-deftest ar-backslashed-end-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-gotoend 'backslashed)
    (should (eq (char-after) 92))))

(ert-deftest ar-backslashed-length-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (should (ar-th-length 'backslashed))))

(ert-deftest ar-backslashed-copy-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (should (stringp (ar-th-copy 'backslashed)))))

;; (ert-deftest ar-backslashed-delete-in-region-test ()
;;   (ar-test-with-elisp-buffer-point-min
;;       "\\asdf\\"
;;       (forward-char 1)
;;     (ar-th-delete-in-region 'backslashed (point-min) (point-max))
;;     (should (eobp))))

(ert-deftest  ar-blok-backslashed-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-blok 'backslashed)
    (should (eq ?} (char-before)))))

(ert-deftest ar-backslashed-escape-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-escape 'backslashed)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-backslashed-doublequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-doublequote 'backslashed)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-backslashed-slash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-slash 'backslashed)
    (should (eq (char-before) ?/))))

(ert-deftest ar-backslashed-doublebackslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-doublebackslash 'backslashed)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-backslashed-doubleslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-doubleslash 'backslashed)
    (should (eq (char-before) ?/))))

(ert-deftest ar-backslashed-slash-paren-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-backslashparen 'backslashed)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-backslashed-dollar-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-dollar 'backslashed)
    (should (eq (char-before) ?$))))

(ert-deftest ar-backslashed-equalize-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-equalize 'backslashed)
    (should (eq (char-before) ?=))))

(ert-deftest ar-backslashed-greaterangle-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-greaterangle 'backslashed)
    (should (eq ?\< (char-before)))))

(ert-deftest ar-backslashed-lesserangle-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-lesserangle 'backslashed)
    (should (eq ?> (char-before)))))

(ert-deftest ar-backslashed-backslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-backslash 'backslashed)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-backslashed-brace-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-brace 'backslashed)
    (should (eq (char-before) ?\}))))

(ert-deftest ar-backslashed-bracket-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-bracket 'backslashed)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-comment-backslashed-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-comment 'backslashed)
    (end-of-line)
    (should (ar-in-comment-p))))

(ert-deftest ar-commatize-backslashed-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-commatize 'backslashed)
    (should (eq ?, (char-before)))))

(ert-deftest ar-quote-backslashed-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-quote 'backslashed)
    (should (eq ?' (char-before)))))

(ert-deftest ar-backslashed-hyphen-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-hyphen 'backslashed)
    (should (eq (char-before) ?-))))

(ert-deftest ar-backslashed-mark-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-mark 'backslashed)
    (should (eq 6 (- (mark) (point))))))

(ert-deftest ar-backslashed-hide-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-hide 'backslashed)
    (should (overlays-at (point)))))

(ert-deftest ar-backslashed-highlight-atpt-mode ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (should (ar-th-highlight 'backslashed))))

(ert-deftest ar-backslashed-kill-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-kill 'backslashed)
    (should (eq 1 (point)))))

(ert-deftest ar-backslashed-kill-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
    (ar-th-kill 'backslashed)
    (should (eq 1 (point)))))

(ert-deftest ar-backslashed-curvedsinglequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-curvedsinglequote 'backslashed)
    (should (eq (char-before) 8217))))

(ert-deftest ar-backslashed-parentize-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-parentize 'backslashed)
    (should (eq ?\) (char-before)))))

(ert-deftest ar-backslashed-separate-atpt-test ()
  (ar-test-with-elisp-buffer
      "asdf\\asdf\\"
      (forward-char -1)
    (ar-th-separate 'backslashed)
    (should (eq (length (buffer-substring-no-properties (line-beginning-position) (line-end-position))) 6))))

(ert-deftest ar-backslashed-singlequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-singlequote 'backslashed)
    (should (eq (char-before) ?'))))

(ert-deftest ar-backslashed-trim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-trim 'backslashed)
    (goto-char (point-max))
    (should (eq (char-before) ?f))))

(ert-deftest ar-backslashed-lefttrim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-trim-left 'backslashed)
    (goto-char (point-min))
    (should (eq (char-after) ?a))))

(ert-deftest ar-backslashed-right-trim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-trim-right 'backslashed)
    (goto-char (point-max))
    (should (eq (char-before) ?f))))

(ert-deftest ar-underscore-backslashed-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-underscore 'backslashed)
    (goto-char (point-max))
    (should (eq (char-before) ?_))))

(ert-deftest ar-whitespace-backslashed-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (ar-th-whitespace 'backslashed)
    (goto-char (point-max))
    (should (eq (char-before) ?\ ))))

(ert-deftest ar-backslashed-backward-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (should (ar-th-backward 'backslashed))))

(ert-deftest ar-backslashed-forward-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\\asdf\\"
      (forward-char 1)
    (should (ar-th-forward 'backslashed))))

(ert-deftest ar-dollared-atpt-old-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (should (ar-th 'dollared))))

(ert-deftest  ar-dollared-bounds-atpt-old-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (should (listp (ar-th-bounds 'dollared)))))

(ert-deftest ar-dollared-beginning-position-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (should (ar-th-beg 'dollared))))

(ert-deftest ar-dollared-end-position-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (should (ar-th-end 'dollared))))

(ert-deftest ar-dollared-beginning-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (should (ar-th-gotobeg 'dollared))))

(ert-deftest ar-dollared-end-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-gotoend 'dollared)
    (should (eq (char-after) ?$))))

(ert-deftest ar-dollared-length-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (should (eq 6 (ar-th-length 'dollared)))))

(ert-deftest ar-dollared-copy-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (should (stringp (ar-th-copy 'dollared)))))

;; (ert-deftest ar-dollared-delete-in-region-test ()
;;   (ar-test-with-elisp-buffer-point-min
;;       "$asdf$"
;;       (forward-char 1)
;;     (ar-th-delete-in-region 'dollared (point-min) (point-max))
;;     (should (eobp))))

(ert-deftest  ar-blok-dollared-atpt-test-1 ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-blok 'dollared)
    (should (eq ?} (char-before)))))

(ert-deftest  ar-blok-dollared-atpt-test-2 ()
  (ar-test-with-elisp-buffer
      "$asdf$"
    (forward-char -1)
    (ar-th-blok 'dollared)
    (should (eq ?} (char-before)))))

(ert-deftest ar-dollared-escape-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-escape 'dollared)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-dollared-doublequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-doublequote 'dollared)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-dollared-slash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-slash 'dollared)
    (should (eq (char-before) ?/))))

(ert-deftest ar-dollared-doublebackslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-doublebackslash 'dollared)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-dollared-doubleslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-doubleslash 'dollared)
    (should (eq (char-before) ?/))))

(ert-deftest ar-dollared-slash-paren-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-backslashparen 'dollared)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-dollared-dollar-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-dollar 'dollared)
    (should (eq (char-before) ?$))))

(ert-deftest ar-dollared-equalize-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-equalize 'dollared)
    (should (eq (char-before) ?=))))

(ert-deftest ar-dollared-greaterangle-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-greaterangle 'dollared)
    (should (eq ?\< (char-before)))))

(ert-deftest ar-dollared-lesserangle-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-lesserangle 'dollared)
    (should (eq ?\> (char-before)))))

(ert-deftest ar-dollared-backslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-backslash 'dollared)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-dollared-brace-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-brace 'dollared)
    (should (eq (char-before) ?\}))))

(ert-deftest ar-dollared-bracket-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-bracket 'dollared)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-comment-dollared-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-comment-dollared-atpt)
    (end-of-line)
    (should (ar-in-comment-p))))

(ert-deftest ar-commatize-dollared-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-commatize 'dollared)
    (should (eq ?, (char-before)))))

(ert-deftest ar-quote-dollared-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-quote 'dollared)
    (should (eq ?' (char-before)))))

(ert-deftest ar-dollared-hyphen-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-hyphen 'dollared)
    (should (eq (char-before) ?-))))

(ert-deftest ar-dollared-mark-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-mark 'dollared)
    (should (eq 6 (- (mark) (point))))))

(ert-deftest ar-dollared-hide-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-hide 'dollared)
    (should (overlays-at (point)))))

(ert-deftest ar-dollared-highlight-atpt-mode ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (should (ar-th-highlight 'dollared))))

(ert-deftest ar-dollared-kill-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-kill 'dollared)
    (should (eq 1 (point)))))

(ert-deftest ar-dollared-curvedsinglequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-curvedsinglequote 'dollared)
    (should (eq 8217 (char-before)))))

(ert-deftest ar-dollared-parentize-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-parentize 'dollared)
    (should (eq ?\) (char-before)))))

(ert-deftest ar-dollared-separate-atpt-test ()
  (ar-test-with-elisp-buffer
      "asdf$asdf$"
      (forward-char -1)
    (ar-th-separate 'dollared)
    (should (eq (length (buffer-substring-no-properties (line-beginning-position) (line-end-position))) 6))))

(ert-deftest ar-dollared-singlequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-singlequote 'dollared)
    (should (eq (char-before) ?'))))

(ert-deftest ar-dollared-trim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-trim 'dollared)
    (goto-char (point-max))
    (should (eq (char-before) ?f))))

(ert-deftest ar-dollared-lefttrim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$ "
      (forward-char 1)
    (ar-th-trim 'dollared)
    (goto-char (point-min))
    (should (eq (char-after) ?a))))

(ert-deftest ar-dollared-right-trim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-trim-right 'dollared)
    (goto-char (point-max))
    (should (eq (char-before) ?f))))

(ert-deftest ar-underscore-dollared-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-underscore 'dollared)
    (should (eq (char-before) ?_))))

(ert-deftest ar-whitespace-dollared-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (ar-th-whitespace 'dollared)
    (should (eq (char-before) ?\ ))))

(ert-deftest ar-dollared-backward-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (should (ar-th-backward 'dollared))))

(ert-deftest ar-dollared-forward-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "$asdf$"
      (forward-char 1)
    (should (ar-th-forward 'dollared))))

(ert-deftest ar-doublequoted-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (should (ar-th 'doublequoted))))

(ert-deftest  ar-doublequoted-bounds-atpt-old-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (should (listp (ar-th-bounds 'doublequoted)))))

(ert-deftest ar-doublequoted-beginning-position-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (should (ar-th-beg 'doublequoted))))

(ert-deftest ar-doublequoted-end-position-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (should (ar-th-end 'doublequoted))))

(ert-deftest ar-doublequoted-beginning-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (should (ar-th-gotobeg 'doublequoted))))

(ert-deftest ar-doublequoted-end-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-gotoend 'doublequoted)
    (should (eq (char-after) ?\"))))

(ert-deftest ar-doublequoted-length-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (should (ar-th-length 'doublequoted))))

(ert-deftest ar-doublequoted-copy-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (should (stringp (ar-th-copy 'doublequoted)))))

;; (ert-deftest ar-doublequoted-delete-in-region-test ()
;;   (ar-test-with-elisp-buffer-point-min
;;       "\"asdf\""
;;       (forward-char 1)
;;     (should (ar-th-delete-in-region 'doublequoted (point-min) (point-max)))))

(ert-deftest  ar-blok-doublequoted-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-blok 'doublequoted)
    (should (eq ?} (char-before)))))

(ert-deftest ar-doublequoted-escape-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-escape 'doublequoted)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-doublequoted-doublequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-doublequote 'doublequoted)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-doublequoted-slash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-slash 'doublequoted)
    (should (eq (char-before) ?/))))

(ert-deftest ar-doublequoted-doublebackslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-doublebackslash 'doublequoted)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-doublequoted-doubleslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-doubleslash 'doublequoted)
    (should (eq (char-before) ?/))))

(ert-deftest ar-doublequoted-slash-paren-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-backslashparen 'doublequoted)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-doublequoted-dollar-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-dollar 'doublequoted)
    (should (eq (char-before) ?$))))

(ert-deftest ar-doublequoted-equalize-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-equalize 'doublequoted)
    (should (eq (char-before) ?=))))

(ert-deftest ar-doublequoted-greaterangle-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-greaterangle 'doublequoted)
    (should (eq ?\< (char-before)))))

(ert-deftest ar-doublequoted-lesserangle-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-lesserangle 'doublequoted)
    (should (eq ?\> (char-before)))))

(ert-deftest ar-doublequoted-backslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-backslash 'doublequoted)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-doublequoted-brace-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-brace 'doublequoted)
    (should (eq (char-before) ?\}))))

(ert-deftest ar-doublequoted-bracket-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-bracket 'doublequoted)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-comment-doublequoted-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-comment 'doublequoted)
    (end-of-line)
    (should (ar-in-comment-p))))

(ert-deftest ar-commatize-doublequoted-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-commatize 'doublequoted)
    (should (eq ?, (char-before)))))

(ert-deftest ar-quote-doublequoted-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-quote 'doublequoted)
    (should (eq ?' (char-before)))))

(ert-deftest ar-doublequoted-hyphen-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-hyphen 'doublequoted)
    (should (eq (char-before) ?-))))

(ert-deftest ar-doublequoted-mark-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
      (ar-th-mark 'doublequoted)
    (should (eq 6 (- (mark) (point))))))

(ert-deftest ar-doublequoted-hide-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-hide 'doublequoted)
    (should (overlays-at (point)))))

(ert-deftest ar-doublequoted-highlight-atpt-mode ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (should (ar-th-highlight 'doublequoted))))

(ert-deftest ar-doublequoted-kill-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-kill 'doublequoted)
    (should (eq 1 (point)))))

(ert-deftest ar-doublequoted-curvedsinglequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-curvedsinglequote 'doublequoted)
    (should (eq (char-before) 8217))))

(ert-deftest ar-doublequoted-parentize-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-parentize 'doublequoted)
    (should (eq ?\) (char-before)))))

(ert-deftest ar-doublequoted-separate-atpt-test ()
  (ar-test-with-elisp-buffer
      "asdf\"asdf\""
      (forward-char -1)
    (ar-th-separate 'doublequoted)
    (forward-line -1)
    (should (bolp))))

(ert-deftest ar-doublequoted-singlequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-singlequote 'doublequoted)
    (should (eq (char-before) ?'))))

(ert-deftest ar-doublequoted-trim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-trim 'doublequoted)
    (goto-char (point-max))
    (should (eq (char-before) ?f))))

(ert-deftest ar-doublequoted-lefttrim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-trim 'doublequoted)
    (goto-char (point-min))
    (should (eq (char-after) ?a))))

(ert-deftest ar-doublequoted-right-trim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-trim-right 'doublequoted)
    (goto-char (point-max))
    (should (eq (char-before) ?f))))

(ert-deftest ar-underscore-doublequoted-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-underscore 'doublequoted)
    (should (eq (char-before) ?_))))

(ert-deftest ar-whitespace-doublequoted-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (ar-th-whitespace 'doublequoted)
    (should (eq (char-before) ?\ ))))

(ert-deftest ar-doublequoted-backward-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (should (ar-th-backward 'doublequoted))))

(ert-deftest ar-doublequoted-forward-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "\"asdf\""
      (forward-char 1)
    (should (ar-th-forward 'doublequoted))))

(ert-deftest ar-equalized-atpt-old-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (should (ar-th 'equalized))))

(ert-deftest  ar-equalized-bounds-atpt-old-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (should (listp (ar-th-bounds 'equalized)))))

(ert-deftest ar-equalized-beginning-position-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (should (ar-th-beg 'equalized))))

(ert-deftest ar-equalized-end-position-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (should (ar-th-end 'equalized))))

(ert-deftest ar-equalized-beginning-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (should (ar-th-gotobeg 'equalized))))

(ert-deftest ar-equalized-end-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-gotoend 'equalized)
    (should (eq (char-after) ?=))))

(ert-deftest ar-equalized-length-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (should (ar-th-length 'equalized))))

(ert-deftest ar-equalized-copy-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (should (stringp (ar-th-copy 'equalized)))))

;; (ert-deftest ar-equalized-delete-in-region-test ()
;;   (ar-test-with-elisp-buffer-point-min
;;       "=asdf="
;;       (forward-char 1)
;;     (ar-th-delete-in-region 'equalized (point-min) (point-max))
;;     (should (eobp))))

(ert-deftest  ar-blok-equalized-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-blok 'equalized)
    (should (eq ?} (char-before)))))

(ert-deftest ar-equalized-escape-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-escape 'equalized)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-equalized-doublequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-doublequote 'equalized)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-equalized-slash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-slash 'equalized)
    (should (eq (char-before) ?/))))

(ert-deftest ar-equalized-doublebackslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-doublebackslash 'equalized)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-equalized-doubleslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-doubleslash 'equalized)
    (should (eq (char-before) ?/))))

(ert-deftest ar-equalized-slash-paren-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-backslashparen 'equalized)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-equalized-dollar-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-dollar 'equalized)
    (should (eq (char-before) ?$))))

(ert-deftest ar-equalized-equalize-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-equalize 'equalized)
    (should (eq (char-before) ?=))))

(ert-deftest ar-equalized-greaterangle-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-greaterangle 'equalized)
    (should (eq ?\< (char-before)))))

(ert-deftest ar-equalized-lesserangle-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-lesserangle 'equalized)
    (should (eq ?\> (char-before)))))

(ert-deftest ar-equalized-backslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-backslash 'equalized)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-equalized-brace-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-brace 'equalized)
    (should (eq (char-before) ?\}))))

(ert-deftest ar-equalized-bracket-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-bracket 'equalized)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-comment-equalized-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-comment 'equalized)
    (end-of-line)
    (should (ar-in-comment-p))))

(ert-deftest ar-commatize-equalized-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-commatize 'equalized)
    (should (eq ?, (char-before)))))

(ert-deftest ar-quote-equalized-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-quote 'equalized)
    (should (eq ?' (char-before)))))

(ert-deftest ar-equalized-hyphen-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-hyphen 'equalized)
    (should (eq (char-before) ?-))))

(ert-deftest ar-equalized-mark-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-mark 'equalized)
    (should (eq 6 (- (mark) (point))))))

(ert-deftest ar-equalized-hide-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-hide 'equalized)
    (should (overlays-at (point)))))

(ert-deftest ar-equalized-highlight-atpt-mode ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (should (ar-th-highlight 'equalized))))

(ert-deftest ar-equalized-kill-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-kill 'equalized)
    (should (eq 1 (point)))))

(ert-deftest ar-equalized-curvedsinglequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-curvedsinglequote 'equalized)
    (should (eq (char-before) 8217))))

(ert-deftest ar-equalized-parentize-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-parentize 'equalized)
    (should (eq ?\) (char-before)))))

(ert-deftest ar-equalized-separate-atpt-test ()
  (ar-test-with-elisp-buffer
      "asdf=asdf="
      (forward-char -1)
    (ar-th-separate 'equalized)
        (should (eq (length (buffer-substring-no-properties (line-beginning-position) (line-end-position))) 6))))

(ert-deftest ar-equalized-singlequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-singlequote 'equalized)
    (should (eq (char-before) ?'))))

(ert-deftest ar-equalized-trim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-trim 'equalized)
    (goto-char (point-min))
    (should (eq (char-after) ?a))))

(ert-deftest ar-equalized-lefttrim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-trim 'equalized)
    (goto-char (point-min))
    (should (eq (char-after) ?a))))

(ert-deftest ar-equalized-right-trim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-trim-right 'equalized)
    (goto-char (point-max))
    (should (eq (char-before) ?f))))

(ert-deftest ar-underscore-equalized-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-underscore 'equalized)
    (should (eq (char-before) ?_))))

(ert-deftest ar-whitespace-equalized-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (ar-th-whitespace 'equalized)
    (should (eq (char-before) ?\ ))))

(ert-deftest ar-equalized-backward-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (should (ar-th-backward 'equalized))))

(ert-deftest ar-equalized-forward-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "=asdf="
      (forward-char 1)
    (should (ar-th-forward 'equalized))))

(ert-deftest ar-hyphened-atpt-old-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (should (ar-th 'hyphened))))

(ert-deftest  ar-hyphened-bounds-atpt-old-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (should (listp (ar-th-bounds 'hyphened)))))

(ert-deftest ar-hyphened-beginning-position-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (should (ar-th-beg 'hyphened))))

(ert-deftest ar-hyphened-end-position-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (should (ar-th-end 'hyphened))))

(ert-deftest ar-hyphened-beginning-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (should (ar-th-gotobeg 'hyphened))))

(ert-deftest ar-hyphened-end-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-gotoend 'hyphened)
    (should (eq (char-after) ?-))))

(ert-deftest ar-hyphened-length-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (should (ar-th-length 'hyphened))))

(ert-deftest ar-hyphened-copy-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (should (stringp (ar-th-copy 'hyphened)))))

;; (ert-deftest ar-hyphened-delete-in-region-test ()
;;   (ar-test-with-elisp-buffer-point-min
;;       "-asdf-"
;;       (forward-char 1)
;;     (ar-th-delete-in-region 'hyphened (point-min) (point-max))
;;     (should (eobp))))

(ert-deftest  ar-blok-hyphened-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-blok 'hyphened)
    (should (eq ?} (char-before)))))

(ert-deftest ar-hyphened-escape-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-escape 'hyphened)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-hyphened-doublequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-doublequote 'hyphened)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-hyphened-slash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-slash 'hyphened)
    (should (eq (char-before) ?/))))

(ert-deftest ar-hyphened-doublebackslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-doublebackslash 'hyphened)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-hyphened-doubleslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-doubleslash 'hyphened)
    (should (eq (char-before) ?/))))

(ert-deftest ar-hyphened-slash-paren-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-backslashparen 'hyphened)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-hyphened-dollar-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-dollar 'hyphened)
    (should (eq (char-before) ?$))))

(ert-deftest ar-hyphened-equalize-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-equalize 'hyphened)
    (should (eq (char-before) ?=))))

(ert-deftest ar-hyphened-greaterangle-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-greaterangle 'hyphened)
    (should (eq ?\< (char-before)))))

(ert-deftest ar-hyphened-lesserangle-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-lesserangle 'hyphened)
    (should (eq ?\> (char-before)))))

(ert-deftest ar-hyphened-backslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-backslash 'hyphened)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-hyphened-brace-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-brace 'hyphened)
    (should (eq (char-before) ?\}))))

(ert-deftest ar-hyphened-bracket-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-bracket 'hyphened)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-comment-hyphened-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-comment 'hyphened)
    (end-of-line)
    (should (ar-in-comment-p))))

(ert-deftest ar-commatize-hyphened-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-commatize 'hyphened)
    (should (eq ?, (char-before)))))

(ert-deftest ar-quote-hyphened-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-quote 'hyphened)
    (should (eq ?' (char-before)))))

(ert-deftest ar-hyphened-hyphen-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-hyphen 'hyphened)
    (should (eq (char-before) ?-))))

(ert-deftest ar-hyphened-mark-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-mark 'hyphened)
    (should (eq 6 (- (mark) (point))))))

(ert-deftest ar-hyphened-hide-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-hide 'hyphened)
    (should (overlays-at (point)))))

(ert-deftest ar-hyphened-highlight-atpt-mode ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (should (ar-th-highlight 'hyphened))))

(ert-deftest ar-hyphened-kill-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-kill 'hyphened)
    (should (eq 1 (point)))))

(ert-deftest ar-hyphened-curvedsinglequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-curvedsinglequote 'hyphened)
    (should (eq 8217 (char-before)))))

(ert-deftest ar-hyphened-parentize-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-parentize 'hyphened)
    (should (eq ?\) (char-before)))))

(ert-deftest ar-hyphened-separate-atpt-test ()
  (ar-test-with-elisp-buffer
      "asdf-asdf-"
      (forward-char -1)
    (ar-th-separate 'hyphened)
    (should (eq (length (buffer-substring-no-properties (line-beginning-position) (line-end-position))) 6))))

(ert-deftest ar-hyphened-singlequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-singlequote 'hyphened)
    (should (eq (char-before) ?'))))

(ert-deftest ar-hyphened-trim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-trim 'hyphened)
    (goto-char (point-min))
    (should (eq (char-after) ?a))))

(ert-deftest ar-hyphened-lefttrim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-trim 'hyphened)
    (goto-char (point-min))
    (should (eq (char-after) ?a))))

(ert-deftest ar-hyphened-right-trim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-trim-right 'hyphened)
    (goto-char (point-max))
    (should (eq (char-before) ?f))))

(ert-deftest ar-underscore-hyphened-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-underscore 'hyphened)
    (should (eq (char-before) ?_))))

(ert-deftest ar-whitespace-hyphened-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (ar-th-whitespace 'hyphened)
    (should (eq (char-before) ?\ ))))

(ert-deftest ar-hyphened-backward-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (should (ar-th-backward 'hyphened))))

(ert-deftest ar-hyphened-forward-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "-asdf-"
      (forward-char 1)
    (should (ar-th-forward 'hyphened))))

(ert-deftest ar-singlequoted-atpt-old-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (should (ar-th 'singlequoted))))

(ert-deftest  ar-singlequoted-bounds-atpt-old-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (should (listp (ar-th-bounds 'singlequoted)))))

(ert-deftest ar-singlequoted-beginning-position-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (should (ar-th-beg 'singlequoted))))

(ert-deftest ar-singlequoted-end-position-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (should (ar-th-end 'singlequoted))))

(ert-deftest ar-singlequoted-beginning-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (should (ar-th-gotobeg 'singlequoted))))

(ert-deftest ar-singlequoted-end-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-gotoend 'singlequoted)
    (should (eq (char-after) ?'))))

(ert-deftest ar-singlequoted-length-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (should (ar-th-length 'singlequoted))))

(ert-deftest ar-singlequoted-copy-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (should (stringp (ar-th-copy 'singlequoted)))))

;; (ert-deftest ar-singlequoted-delete-in-region-test ()
;;   (ar-test-with-elisp-buffer-point-min
;;       "'asdf'"
;;       (forward-char 1)
;;     (ar-th-delete-in-region 'singlequoted (point-min) (point-max))
;;     (should (eopb))))

(ert-deftest  ar-blok-singlequoted-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-blok 'singlequoted)
    (should (eq ?} (char-before)))))

(ert-deftest ar-singlequoted-escape-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-escape 'singlequoted)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-singlequoted-doublequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-doublequote 'singlequoted)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-singlequoted-slash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-slash 'singlequoted)
    (should (eq (char-before) ?/))))

(ert-deftest ar-singlequoted-doublebackslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-doublebackslash 'singlequoted)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-singlequoted-doubleslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-doubleslash 'singlequoted)
    (should (eq (char-before) ?/))))

(ert-deftest ar-singlequoted-slash-paren-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-backslashparen 'singlequoted)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-singlequoted-dollar-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-dollar 'singlequoted)
    (should (eq (char-before) ?$))))

(ert-deftest ar-singlequoted-equalize-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-equalize 'singlequoted)
    (should (eq (char-before) ?=))))

(ert-deftest ar-singlequoted-greaterangle-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-greaterangle 'singlequoted)
    (should (eq ?\< (char-before)))))

(ert-deftest ar-singlequoted-lesserangle-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-lesserangle 'singlequoted)
    (should (eq ?\> (char-before)))))

(ert-deftest ar-singlequoted-backslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-backslash 'singlequoted)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-singlequoted-brace-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-brace 'singlequoted)
    (should (eq (char-before) ?\}))))

(ert-deftest ar-singlequoted-bracket-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-bracket 'singlequoted)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-comment-singlequoted-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-comment 'singlequoted)
    (end-of-line)
    (should (ar-in-comment-p))))

(ert-deftest ar-commatize-singlequoted-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-commatize 'singlequoted)
    (should (eq ?, (char-before)))))

(ert-deftest ar-quote-singlequoted-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-quote 'singlequoted)
    (should (eq ?' (char-before)))))

(ert-deftest ar-singlequoted-hyphen-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-hyphen 'singlequoted)
    (should (eq (char-before) ?-))))

(ert-deftest ar-singlequoted-mark-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-mark 'singlequoted)
    (should (eq 6 (- (mark) (point))))))

(ert-deftest ar-singlequoted-hide-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-hide 'singlequoted)
    (should (overlays-at (point)))))

(ert-deftest ar-singlequoted-highlight-atpt-mode ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (should (ar-th-highlight 'singlequoted))))

(ert-deftest ar-singlequoted-kill-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-kill 'singlequoted)
    (should (eq 1 (point)))))

(ert-deftest ar-singlequoted-curvedsinglequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-curvedsinglequote 'singlequoted)
    (should (eq (char-before) 8217))))

(ert-deftest ar-singlequoted-parentize-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-parentize 'singlequoted)
    (should (eq ?\) (char-before)))))

(ert-deftest ar-singlequoted-separate-atpt-test ()
  (ar-test-with-elisp-buffer
      "asdf'asdf'"
    (forward-char -1)
    (ar-th-separate 'singlequoted)
    (should (eq (length (buffer-substring-no-properties (line-beginning-position) (line-end-position))) 6))))

(ert-deftest ar-singlequoted-singlequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-singlequote 'singlequoted)
    (should (eq (char-before) ?'))))

(ert-deftest ar-singlequoted-trim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-trim 'singlequoted)
    (goto-char (point-min))
    (should (eq (char-after) ?a))))

(ert-deftest ar-singlequoted-lefttrim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-trim 'singlequoted)
    (goto-char (point-min))
    (should (eq (char-after) ?a))))

(ert-deftest ar-singlequoted-right-trim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-trim-right 'singlequoted)
    (goto-char (point-max))
    (should (eq (char-before) ?f))))

(ert-deftest ar-underscore-singlequoted-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-underscore 'singlequoted)
    (should (eq (char-before) ?_))))

(ert-deftest ar-whitespace-singlequoted-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (ar-th-whitespace 'singlequoted)
    (should (eq (char-before) ?\ ))))

(ert-deftest ar-singlequoted-backward-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (should (ar-th-backward 'singlequoted))))

(ert-deftest ar-singlequoted-forward-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "'asdf'"
      (forward-char 1)
    (should (ar-th-forward 'singlequoted))))

(ert-deftest ar-slashed-atpt-old-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (should (ar-th 'slashed))))

(ert-deftest  ar-slashed-bounds-atpt-old-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (should (listp (ar-th-bounds 'slashed)))))

(ert-deftest ar-slashed-beginning-position-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (should (ar-th-beg 'slashed))))

(ert-deftest ar-slashed-end-position-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (should (ar-th-end 'slashed))))

(ert-deftest ar-slashed-beginning-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (should (ar-th-gotobeg 'slashed))))

(ert-deftest ar-slashed-end-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-gotoend 'slashed)
    (should (eq (char-after) 47))))

(ert-deftest ar-slashed-length-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (should (ar-th-length 'slashed))))

(ert-deftest ar-slashed-copy-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (should (stringp (ar-th-copy 'slashed)))))

;; (ert-deftest ar-slashed-delete-in-region-test ()
;;   (ar-test-with-elisp-buffer-point-min
;;       "/asdf/"
;;       (forward-char 1)
;;     (ar-th-delete-in-region 'slashed (point-min) (point-max))
;;     (should (eobp))))

(ert-deftest  ar-blok-slashed-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-blok 'slashed)
    (should (eq ?} (char-before)))))

(ert-deftest ar-slashed-escape-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-escape 'slashed)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-slashed-doublequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-doublequote 'slashed)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-slashed-slash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-slash 'slashed)
    (should (eq (char-before) ?/))))

(ert-deftest ar-slashed-doublebackslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-doublebackslash 'slashed)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-slashed-doubleslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-doubleslash 'slashed)
    (should (eq (char-before) ?/))))

(ert-deftest ar-slashed-slash-paren-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-backslashparen 'slashed)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-slashed-dollar-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-dollar 'slashed)
    (should (eq (char-before) ?$))))

(ert-deftest ar-slashed-equalize-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-equalize 'slashed)
    (should (eq (char-before) ?=))))

(ert-deftest ar-slashed-greaterangle-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-greaterangle 'slashed)
    (should (eq ?\< (char-before)))))

(ert-deftest ar-slashed-lesserangle-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-lesserangle 'slashed)
    (should (eq ?\> (char-before)))))

(ert-deftest ar-slashed-backslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-backslash 'slashed)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-slashed-brace-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-brace 'slashed)
    (should (eq (char-before) ?\}))))

(ert-deftest ar-slashed-bracket-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-bracket 'slashed)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-comment-slashed-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-comment 'slashed)
    (end-of-line)
    (should (ar-in-comment-p))))

(ert-deftest ar-commatize-slashed-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-commatize 'slashed)
    (should (eq ?, (char-before)))))

(ert-deftest ar-quote-slashed-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-quote 'slashed)
    (should (eq ?' (char-before)))))

(ert-deftest ar-slashed-hyphen-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-hyphen 'slashed)
    (should (eq (char-before) ?-))))

(ert-deftest ar-slashed-mark-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-mark 'slashed)
    (should (eq 6 (- (mark) (point))))))

(ert-deftest ar-slashed-hide-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-hide 'slashed)
    (should (overlays-at (point)))))

(ert-deftest ar-slashed-highlight-atpt-mode ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (should (ar-th-highlight 'slashed))))

(ert-deftest ar-slashed-kill-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-kill 'slashed)
    (should (eq 1 (point)))))

(ert-deftest ar-slashed-curvedsinglequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-curvedsinglequote 'slashed)
    (should (eq 8217 (char-before)))))

(ert-deftest ar-slashed-parentize-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-parentize 'slashed)
    (should (eq ?\) (char-before)))))

(ert-deftest ar-slashed-separate-atpt-test ()
  (ar-test-with-elisp-buffer
      "asdf/asdf/"
    (forward-char -1)
    (ar-th-separate 'slashed)
    (should (eq (length (buffer-substring-no-properties (line-beginning-position) (line-end-position))) 6))))

(ert-deftest ar-slashed-singlequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-singlequote 'slashed)
    (should (eq (char-before) ?'))))

(ert-deftest ar-slashed-trim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-trim 'slashed)
    (goto-char (point-min))
    (should (eq (char-after) ?a))))

(ert-deftest ar-slashed-lefttrim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-trim'slashed)
    (goto-char (point-min))
    (should (eq (char-after) ?a))))

(ert-deftest ar-slashed-right-trim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-trim-right 'slashed)
    (goto-char (point-max))
    (should (eq (char-before) ?f))))

(ert-deftest ar-underscore-slashed-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-underscore 'slashed)
    (should (eq (char-before) ?_))))

(ert-deftest ar-whitespace-slashed-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (ar-th-whitespace 'slashed)
    (should (eq (char-before) ?\ ))))

(ert-deftest ar-slashed-backward-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (should (ar-th-backward 'slashed))))

(ert-deftest ar-slashed-forward-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "/asdf/"
      (forward-char 1)
    (should (ar-th-forward 'slashed))))

(ert-deftest ar-underscored-atpt-old-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (should (ar-th 'underscored))))

(ert-deftest  ar-underscored-bounds-atpt-old-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (should (listp (ar-th-bounds 'underscored)))))

(ert-deftest ar-underscored-beginning-position-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (should (ar-th-beg 'underscored))))

(ert-deftest ar-underscored-end-position-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (should (ar-th-end 'underscored))))

(ert-deftest ar-underscored-beginning-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (should (ar-th-gotobeg 'underscored))))

(ert-deftest ar-underscored-end-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (goto-char (point-min))
      (forward-char 1)
    (ar-th-gotoend 'underscored)
    (should (eq (char-after) ?_))))

(ert-deftest ar-underscored-length-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (should (ar-th-length 'underscored))))

(ert-deftest ar-underscored-copy-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (should (stringp (ar-th-copy 'underscored)))))

;; (ert-deftest ar-underscored-delete-in-region-test ()
;;   (ar-test-with-elisp-buffer-point-min
;;       "_asdf_"
;;       (forward-char 1)
;;     (ar-th-delete-in-region 'underscored (point-min) (point-max))
;;     (should (eobp))))

(ert-deftest  ar-blok-underscored-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-blok 'underscored)
    (should (eq ?} (char-before)))))

(ert-deftest ar-underscored-escape-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-escape 'underscored)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-underscored-doublequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-doublequote 'underscored)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-underscored-slash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-slash 'underscored)
    (should (eq (char-before) ?/))))

(ert-deftest ar-underscored-doublebackslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-doublebackslash 'underscored)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-underscored-doubleslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-doubleslash 'underscored)
    (should (eq (char-before) ?/))))

(ert-deftest ar-underscored-slash-paren-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-backslashparen 'underscored)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-underscored-dollar-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-dollar 'underscored)
    (should (eq (char-before) ?$))))

(ert-deftest ar-underscored-equalize-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-equalize 'underscored)
    (should (eq (char-before) ?=))))

(ert-deftest ar-underscored-greaterangle-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-greaterangle 'underscored)
    (should (eq ?\< (char-before)))))

(ert-deftest ar-underscored-lesserangle-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-lesserangle 'underscored)
    (should (eq ?\> (char-before)))))

(ert-deftest ar-underscored-backslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-backslash 'underscored)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-underscored-brace-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-brace 'underscored)
    (should (eq (char-before) ?\}))))

(ert-deftest ar-underscored-bracket-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-bracket 'underscored)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-comment-underscored-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-comment 'underscored)
    (end-of-line)
    (should (ar-in-comment-p))))

(ert-deftest ar-commatize-underscored-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-commatize 'underscored)
    (should (eq ?, (char-before)))))

(ert-deftest ar-quote-underscored-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-quote 'underscored)
    (should (eq ?' (char-before)))))

(ert-deftest ar-underscored-hyphen-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-hyphen 'underscored)
    (should (eq (char-before) ?-))))

(ert-deftest ar-underscored-mark-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-mark 'underscored)
    (should (eq 6 (- (mark) (point))))))

(ert-deftest ar-underscored-hide-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-hide 'underscored)
    (should (overlays-at (point)))))

(ert-deftest ar-underscored-highlight-atpt-mode ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (should (ar-th-highlight 'underscored))))

(ert-deftest ar-underscored-kill-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-kill 'underscored)
    (should (eq 1 (point)))))

(ert-deftest ar-underscored-curvedsinglequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-curvedsinglequote 'underscored)
    (should (eq 8217 (char-before)))))

(ert-deftest ar-underscored-parentize-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-parentize 'underscored)
    (should (eq ?\) (char-before)))))

(ert-deftest ar-underscored-separate-atpt-test ()
  (ar-test-with-elisp-buffer
      "asdf_asdf_"
    (forward-char -1)
    (ar-th-separate 'underscored)
    (should (eq (length (buffer-substring-no-properties (line-beginning-position) (line-end-position))) 6))))

(ert-deftest ar-underscored-singlequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-singlequote 'underscored)
    (should (eq (char-before) ?'))))

(ert-deftest ar-underscored-trim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-trim 'underscored)
    (goto-char (point-min))
    (should (eq (char-after) ?a))))

(ert-deftest ar-underscored-lefttrim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-trim 'underscored)
    (goto-char (point-min))
    (should (eq (char-after) ?a))))

(ert-deftest ar-underscored-right-trim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-trim-right 'underscored)
    (goto-char (point-max))
    (should (eq (char-before) ?f))))

(ert-deftest ar-underscore-underscored-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-underscore 'underscored)
    (should (eq (char-before) ?_))))

(ert-deftest ar-whitespace-underscored-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (ar-th-whitespace 'underscored)
    (should (eq (char-before) ?\ ))))

(ert-deftest ar-underscored-backward-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (should (ar-th-backward 'underscored))))

(ert-deftest ar-underscored-forward-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "_asdf_"
      (forward-char 1)
    (should (ar-th-forward 'underscored))))

(ert-deftest ar-whitespaced-atpt-old-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (should (ar-th 'whitespaced))))

(ert-deftest  ar-whitespaced-bounds-atpt-old-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (should (listp (ar-th-bounds 'whitespaced)))))

(ert-deftest ar-whitespaced-beginning-position-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (should (ar-th-beg 'whitespaced))))

(ert-deftest ar-whitespaced-end-position-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (should (ar-th-end 'whitespaced))))

(ert-deftest ar-whitespaced-beginning-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (should (ar-th-gotobeg 'whitespaced))))

(ert-deftest ar-whitespaced-end-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-gotoend 'whitespaced)
    (should (eq (char-after) 32))))

(ert-deftest ar-whitespaced-length-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (should (ar-th-length 'whitespaced))))

(ert-deftest ar-whitespaced-copy-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (should (stringp (ar-th-copy 'whitespaced)))))

;; (ert-deftest ar-whitespaced-delete-in-region-test ()
;;   (ar-test-with-elisp-buffer-point-min
;;       " asdf "
;;       (forward-char 1)
;;     (ar-th-delete-in-region 'whitespaced (point-min) (point-max))
;;     (should (eobp))))

(ert-deftest  ar-blok-whitespaced-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-blok 'whitespaced)
    (should (eq ?} (char-before)))))

(ert-deftest ar-whitespaced-escape-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-escape 'whitespaced)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-whitespaced-doublequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-doublequote 'whitespaced)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-whitespaced-slash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-slash 'whitespaced)
    (should (eq (char-before) ?/))))

(ert-deftest ar-whitespaced-doublebackslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-doublebackslash 'whitespaced)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-whitespaced-doubleslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-doubleslash 'whitespaced)
    (should (eq (char-before) ?/))))

(ert-deftest ar-whitespaced-slash-paren-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-backslashparen 'whitespaced)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-whitespaced-dollar-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-dollar 'whitespaced)
    (should (eq (char-before) ?$))))

(ert-deftest ar-whitespaced-equalize-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-equalize 'whitespaced)
    (should (eq (char-before) ?=))))

(ert-deftest ar-whitespaced-greaterangle-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-greaterangle 'whitespaced)
    (should (eq ?\< (char-before)))))

(ert-deftest ar-whitespaced-lesserangle-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-lesserangle 'whitespaced)
    (should (eq ?\> (char-before)))))

(ert-deftest ar-whitespaced-backslash-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-backslash 'whitespaced)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-whitespaced-brace-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-brace 'whitespaced)
    (should (eq (char-before) ?\}))))

(ert-deftest ar-whitespaced-bracket-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-bracket 'whitespaced)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-comment-whitespaced-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-comment 'whitespaced)
    (end-of-line)
    (should (ar-in-comment-p))))

(ert-deftest ar-commatize-whitespaced-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-commatize 'whitespaced)
    (should (eq ?, (char-before)))))

(ert-deftest ar-quote-whitespaced-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-quote 'whitespaced)
    (should (eq ?' (char-before)))))

(ert-deftest ar-whitespaced-hyphen-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-hyphen 'whitespaced)
    (should (eq (char-before) ?-))))

(ert-deftest ar-whitespaced-mark-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-mark 'whitespaced)
    (should (eq 6 (- (mark) (point))))))

(ert-deftest ar-whitespaced-hide-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-hide 'whitespaced)
    (should (overlays-at (point)))))

(ert-deftest ar-whitespaced-highlight-atpt-mode ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (should (ar-th-highlight 'whitespaced))))

(ert-deftest ar-whitespaced-kill-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-kill 'whitespaced)
    (should (eq 1 (point)))))

(ert-deftest ar-whitespaced-curvedsinglequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-curvedsinglequote 'whitespaced)
    (should (eq (char-before) ?’))))

(ert-deftest ar-whitespaced-parentize-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-parentize 'whitespaced)
    (should (eq ?\) (char-before)))))

(ert-deftest ar-whitespaced-separate-atpt-test ()
  (ar-test-with-elisp-buffer
      "asdf asdf "
    (forward-char -2)
    (ar-th-separate 'whitespaced)
    (should (eq (length (buffer-substring-no-properties (line-beginning-position) (line-end-position))) 5))))

(ert-deftest ar-whitespaced-singlequote-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-singlequote 'whitespaced)
    (should (eq (char-before) ?'))))

(ert-deftest ar-whitespaced-trim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-trim 'whitespaced)
    (should (eq (char-after) ?a))))

(ert-deftest ar-whitespaced-lefttrim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-trim 'whitespaced)
    (should (eq (char-after) ?a))))

(ert-deftest ar-whitespaced-right-trim-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-trim-right 'whitespaced)
    (goto-char (point-max))
    (should (eq (char-before) ?f))))

(ert-deftest ar-underscore-whitespaced-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-underscore 'whitespaced)
    (should (eq (char-before) ?_))))

(ert-deftest ar-whitespace-whitespaced-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (forward-char 1)
    (ar-th-whitespace 'whitespaced)
    (should (eq (char-before) ?\ ))))

(ert-deftest ar-whitespaced-forward-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      " asdf "
      (goto-char (point-min))
      (forward-char 1)
    (should (ar-th-forward 'whitespaced))))

(ert-deftest ar-backtick-word-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "asdf"
      (goto-char (point-min))
    (ar-backtick-word-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-triplebacktick-word-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "asdf"
    (ar-triplebacktick-word-atpt)
    (should (looking-back "```" (line-beginning-position)))))

(ert-deftest ar-bracketed-atpt-test-V5h2sg ()
  (py-test-with-temp-buffer
      "\[\"<3\", \" Haskell\"]"
    (goto-char (point-max))
    (backward-char)
    (should (string= "\[\"<3\", \" Haskell\"]" (ar-delimited-atpt)))))

(ert-deftest ar-angled-atpt-test-Zq97HB ()
  (py-test-with-temp-buffer
      "(<$>)"
    (goto-char (point-max))
    (backward-char 2)
    (should (string= "<$>" (ar-delimited-atpt)))))

(ert-deftest ar-braced-atpt-test-Zq97HB ()
  (py-test-with-temp-buffer
      "{<$>}"
    (goto-char (point-max))
    (backward-char 1)
    (should (string= "{<$>}" (ar-delimited-atpt)))))

(ert-deftest ar-backticked-atpt-test-Zq97HB ()
  (ar-test
      "1. **Ensure that you have [LaTeX](https://www.latex-project.org/get) installed on your machine.**

    The commands `which latex` and `which dvisvgm` must both return paths to the executables. `dvisvgm` should be present as part of your LaTeX installation, but it's also available [here](https://dvisvgm.de/Downloads).
"
    'markdown-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "dvisvgm" nil t 3)
    (should (string= "dvisvgm" (ar-delimited-atpt)))))

;; ar-thing-at-point-utils-delimited-tests: ar-unpaired-delimited-raw end

(provide 'ar-tatpt-utils-delimited-tests)
;;; ar-tatpt-utils-delimited-tests.el ends here
