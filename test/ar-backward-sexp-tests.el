;;; ar-backward-sexp-tests.el --- Provide needed forms -*- lexical-binding: t; -*-

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

(ert-deftest ar-sexp-test-SKyWXX ()
  (ar-test
      "[(asdf]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should (eq (char-after) ?\[))))

(ert-deftest ar-sexp-test-RWfoG3 ()
  (ar-test
      "((asdf))"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should-not (eq (char-after) ?\)))
   ))

(ert-deftest ar-sexp-test-Zk58le ()
  (ar-test
      "{(asdf}"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should (eq (char-after) ?\{))
   ))

(ert-deftest ar-emacs-lisp-sexp-test-awtsCR ()
  (ar-test
      "((asdf))"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?\())))

(ert-deftest ar-emacs-fundamental-test-8gy0wy ()
  (ar-test
      "((asdf))"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?\())))

(ert-deftest ar-emacs-test-OlO7H1 ()
  (ar-test
      "[[asdf]]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?\[))))

(ert-deftest ar-emacs--test-3QZk6j ()
  (ar-test
      "[(asdf]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?\[))))

(ert-deftest ar-emacs--test-oy2hnH ()
  (ar-test
      "{(asdf}"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?{))))

(ert-deftest ar-sexp-test-0Qwg73 ()
  (ar-test
      "[(asdf\"]\"]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should (eq (char-after) ?\[))))

(ert-deftest ar-sexp-test-K5CfKV ()
  (ar-test
      "((asdf)\")\")"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should-not (eq (char-after) ?\)))
   ))

(ert-deftest ar-sexp-test-g64KOA ()
  (ar-test
      "{(asdf\"}\"}"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-max))
   (skip-chars-backward " \t\r\n\f")
   (ar-backward-sexp)
   (should (eq (char-after) ?\{))
   ))





(ert-deftest ar-emacs-test-tqmEcC ()
  (ar-test
      "[[asdf\"]\"]]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?\[))))

(ert-deftest ar-emacs--test-cMVJMz ()
  (ar-test
      "[(asdf\"]\"]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?\[))))

(ert-deftest ar-emacs--test-wuhBwe ()
  (ar-test
      "{(asdf\"}\"}"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?{))))

(ert-deftest ar-ert-backward-sexp-test-k1ALAI ()
  (ar-test-with-elisp-buffer
      "\\s\"-*({<[\\]. []]>)*}\"]\"]"
      (goto-char (point-max))
    (skip-chars-backward "^*")
    (ar-backward-sexp)
    (should (eq (char-after) ?*))
    (should (eq (char-before) ?-))
    ))

(ert-deftest ar-sexp-test-cos9FQ ()
  (ar-test
      "[(asdf\"]\"]"
   'fundamental-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should (eq (char-after) ?\[))))

(ert-deftest ar-sexp-test-5mnMay ()
  (ar-test
      "((asdf)\")\")"
   'fundamental-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should-not (eq (char-after) ?\)))
   ))

(ert-deftest ar-sexp-test-p669g4 ()
  (ar-test
      "{(asdf\"}\"}"
   'fundamental-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should (eq (char-after) ?\{))
   ))

(ert-deftest ar-emacs-lisp-sexp-test-OAHdHO ()
  (ar-test
      "((asdf)\")\")"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (eq (char-after) ?\())))

(ert-deftest ar-emacs-lisp-sexp-test-6WqoA8 ()
  (ar-test
      "((asdf)\")\")"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (eq (char-after) ?\())))

(provide 'ar-backward-sexp-tests)
;; ar-backward-sexp-tests.el ends here
