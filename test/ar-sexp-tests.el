;;; ar-sexp-tests.el --- Provide needed forms -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Andreas Röhler

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

;;; Code:

(ert-deftest ar-sexp-test-NyIYwZ ()
  (ar-test
      "[(asdf]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (ar-forward-sexp)
   (should (eq (char-before) ?\]))))

(ert-deftest ar-sexp-test-RgMjiG ()
  (ar-test
      "((asdf))"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (ar-forward-sexp)
   (should-not (eq (char-after) ?\)))
   (should (eq (char-before) ?\)))
   ))

(ert-deftest ar-sexp-test-jWGSy4 ()
  (ar-test
      "{(asdf}"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (ar-forward-sexp)
   (should (eq (char-before) ?\}))
   ))

(ert-deftest ar-emacs-lisp-sexp-test-6esVUB ()
  (ar-test-point-min
      "((asdf))"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eobp))
    (should (eq (char-before) ?\)))))

(ert-deftest ar-emacs-fundamental-test-m0YUfQ ()
  (ar-test-point-min
      "((asdf))"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eobp))
    (should (eq (char-before) ?\)))))

(ert-deftest ar-emacs-fundamental-test-tqmEcC ()
  (ar-test-point-min
      "[[asdf]]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eobp))
    (should (eq (char-before) ?\]))))

(ert-deftest ar-emacs-fundamental-test-cMVJMz ()
  (ar-test-point-min
      "[(asdf]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eobp))
    (should (eq (char-before) 93))))

(ert-deftest ar-emacs-fundamental-test-wuhBwe ()
  (ar-test-point-min
      "{(asdf}"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eobp))
    (should (eq (char-before) ?}))))
;;
(ert-deftest ar-emacs-lisp-sexp-test-WY3Y8m ()
  (ar-test-point-min
      "((asdf))"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) ?\)))))

(ert-deftest ar-emacs-fundamental-test-uBDjZR ()
  (ar-test-point-min
      "((asdf))"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) ?\)))))

(ert-deftest ar-emacs-fundamental-test-nxie12 ()
  (ar-test-point-min
      "[[asdf]]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) ?\]))))

(ert-deftest ar-emacs-fundamental-test-aGtMJU ()
  (ar-test-point-min
      "[(asdf]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) 93))))

(ert-deftest ar-emacs-fundamental-test-sUG5Xy ()
  (ar-test-point-min
      "{(asdf}"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) ?}))))

(ert-deftest ar-sexp-test-0Qwg73 ()
  (ar-test
      "[(asdf]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should (eq (char-after) ?\[))))

(ert-deftest ar-sexp-test-K5CfKV ()
  (ar-test
      "((asdf))"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should-not (eq (char-after) ?\)))
   ))

(ert-deftest ar-sexp-test-g64KOA ()
  (ar-test
      "{(asdf}"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should (eq (char-after) ?\{))
   ))

(ert-deftest ar-emacs-lisp-sexp-test-6WqoA8 ()
  (ar-test
      "((asdf))"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?\())))

(ert-deftest ar-emacs-fundamental-test-FC9JZG ()
  (ar-test
      "((asdf))"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?\())))

(ert-deftest ar-emacs-test-tqmEcC ()
  (ar-test
      "[[asdf]]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?\[))))

(ert-deftest ar-emacs--test-cMVJMz ()
  (ar-test
      "[(asdf]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?\[))))

(ert-deftest ar-emacs--test-wuhBwe ()
  (ar-test
      "{(asdf}"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?{))))
;;
(ert-deftest ar-emacs-lisp-sexp-backward-test-WY3Y8m ()
  (ar-test
      "((asdf))"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp-atpt)
    (should (bobp))
    (should (eq (char-after) ?\())))

(ert-deftest ar-emacs--test-uBDjZR ()
  (ar-test
      "((asdf))"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp-atpt)
    (should (bobp))
    (should (eq (char-after) ?\())))

(ert-deftest ar-emacs--test-nxie12 ()
  (ar-test
      "[[asdf]]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp-atpt)
    (should (bobp))
    (should (eq (char-after) ?\[))))

(ert-deftest ar-emacs--test-aGtMJU ()
  (ar-test
      "[(asdf]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp-atpt)
    (should (bobp))
    (should (eq (char-after) ?\[))))

(ert-deftest ar-emacs--test-sUG5Xy ()
  (ar-test
      "{(asdf}"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp-atpt)
    (should (bobp))
    (should (eq (char-after) ?{))))

(ert-deftest ar-sexp-test-7wdeGZ ()
  (ar-test
      "\\s-*[[<({].[]>)}]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (search-forward "[" nil t 1)
   (ar-forward-sexp)
   (should (eq (char-after) ?.))
   (should (eq (char-before) ?\]))
   ))

(ert-deftest ar-sexp-test-uwGqMo ()
  (ar-test
      "\\s-*[[<({].[]>)}]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (search-forward "[" nil t 2)
   (ar-forward-sexp)
   (should (eq (char-after) ?\)))
   (should (eq (char-before) ?>))
   ))

(ert-deftest ar-ert-forward-sexp-test-LGnrk9 ()
  (ar-test-with-elisp-buffer-point-min
      ";;;\\\;; (beg)
(defun foo1 (&optional beg end))"
      (goto-char (point-min))
    (forward-char 9)
    (ar-forward-sexp)
    (should (eq (char-after) ?\())))

(provide 'ar-sexp-tests)
;; ar-sexp-tests.el ends here
