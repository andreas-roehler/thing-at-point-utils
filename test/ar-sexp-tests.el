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

(ert-deftest ar-sexp-test-iHa1er ()
  (ar-test
      "((asdf))"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (ar-forward-sexp)
   (should-not (eq (char-after) ?\)))
   (should (eq (char-before) ?\)))
   ))

(ert-deftest ar-sexp-test-AH1o3P ()
  (ar-test
      "{(asdf}"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (ar-forward-sexp)
   (should (eq (char-before) ?\}))
   ))

(ert-deftest ar-emacs-lisp-sexp-test-BIRGJ2 ()
  (ar-test-point-min
      "((asdf))"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eobp))
    (should (eq (char-before) ?\)))))

(ert-deftest ar-emacs-fundamental-test-thwxry ()
  (ar-test-point-min
      "((asdf))"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eq (char-before) ?\)))
    (should (eq (char-before (1- (point))) ?\)))
    ))

(ert-deftest ar-emacs-fundamental-test-u0s6R8 ()
  (ar-test-point-min
      "[[asdf]]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eobp))
    (should (eq (char-before) ?\]))))

(ert-deftest ar-emacs-fundamental-test-cSt0BF ()
  (ar-test-point-min
      "[(asdf]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eobp))
    (should (eq (char-before) 93))))

(ert-deftest ar-emacs-fundamental-test-uHkpk1 ()
  (ar-test-point-min
      "{(asdf}"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eobp))
    (should (eq (char-before) ?}))))
;;
(ert-deftest ar-emacs-lisp-sexp-test-H7eXAm ()
  (ar-test-point-min
      "((asdf))"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) ?\)))))

(ert-deftest ar-emacs-fundamental-test-g8Q0mD ()
  (ar-test-point-min
      "((asdf))"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) ?\)))))

(ert-deftest ar-emacs-fundamental-test-hT9LEl ()
  (ar-test-point-min
      "[[asdf]]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) ?\]))))

(ert-deftest ar-emacs-fundamental-test-cyaubB ()
  (ar-test-point-min
      "[(asdf]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) 93))))

(ert-deftest ar-emacs-fundamental-test-EzlSpP ()
  (ar-test-point-min
      "{(asdf}"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) ?}))))

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
;;
(ert-deftest ar-emacs-lisp-sexp-backward-test-jzruRW ()
  (ar-test
      "((asdf))"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp-atpt)
    (should (bobp))
    (should (eq (char-after) ?\())))

(ert-deftest ar-emacs--test-uKEAqa ()
  (ar-test
      "((asdf))"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp-atpt)
    (should (bobp))
    (should (eq (char-after) ?\())))

(ert-deftest ar-emacs--test-cBd4jC ()
  (ar-test
      "[[asdf]]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp-atpt)
    (should (bobp))
    (should (eq (char-after) ?\[))))

(ert-deftest ar-emacs--test-gw6kEV ()
  (ar-test
      "[(asdf]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp-atpt)
    (should (bobp))
    (should (eq (char-after) ?\[))))

(ert-deftest ar-emacs--test-vAyvGz ()
  (ar-test
      "{(asdf}"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp-atpt)
    (should (bobp))
    (should (eq (char-after) ?{))))

(ert-deftest ar-sexp-test-pZ6izl ()
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

(ert-deftest ar-sexp-test-2x3N0X ()
  (ar-test
      "\\s-*[[<({].[]>)}]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (search-forward "[" nil t 2)
   (ar-forward-sexp-intern)
   (should (eq (char-after) ?\)))
   (should (eq (char-before) ?>))
   ))

(ert-deftest ar-ert-forward-sexp-test-YURL6b ()
  (ar-test-with-elisp-buffer-point-min
      ";;;\\\;; (beg)
(defun foo1 (&optional beg end))"
      (goto-char (point-min))
    (forward-char 9)
    (ar-forward-sexp)
    (should (eq (char-after) ?\())))

(ert-deftest ar-ert-forward-sexp-test-ElPh0c ()
  (ar-test-with-elisp-buffer-point-min
"(defun foo ()
  \"Closing paren matched here: )\"
  )"
      (goto-char (point-min))
    (ar-forward-sexp)
    (should-not (nth 3 (parse-partial-sexp (point-min) (point))))
    (should (eq (char-before) ?\)))))

(ert-deftest ar-sexp-test-CG33ej ()
  (ar-test
      "[(asdf\"]\"]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (ar-forward-sexp)
   (should (eq (char-before) ?\]))))

(ert-deftest ar-sexp-test-5ZJTdA ()
  (ar-test
      "((asdf)\")\")"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (ar-forward-sexp)
   (should-not (eq (char-after) ?\)))
   (should (eq (char-before) ?\)))
   ))

(ert-deftest ar-sexp-test-Ov97OT ()
  (ar-test
      "{(asdf\"}\"}"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (ar-forward-sexp)
   (should (eq (char-before) ?\}))
   ))

(ert-deftest ar-emacs-lisp-sexp-test-N5u5zn ()
  (ar-test-point-min
      "((asdf)\")\")"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eobp))
    (should (eq (char-before) ?\)))))

(ert-deftest ar-emacs-fundamental-test-LBTvtu ()
  (ar-test-point-min
      "((asdf)\")\")"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eq (char-before) ?\)))
    (should (eq (char-before (1- (point))) ?\"))
    ))

(ert-deftest ar-emacs-fundamental-test-MERLU5 ()
  (ar-test-point-min
      "[[asdf\"]\"]]"
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
      "{(asdf\"}\"}"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eobp))
    (should (eq (char-before) ?}))))
;;
(ert-deftest ar-emacs-lisp-sexp-test-WY3Y8m ()
  (ar-test-point-min
      "((asdf)\")\")"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) ?\)))))

(ert-deftest ar-emacs-fundamental-test-uBDjZR ()
  (ar-test-point-min
      "((asdf)\")\")"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) ?\)))))

(ert-deftest ar-emacs-fundamental-test-nxie12 ()
  (ar-test-point-min
      "[[asdf\"]\"]]"
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
      "{(asdf\"}\"}"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) ?}))))

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
   (ar-backward-sexp)
   (should (eq (char-after) ?\{))
   ))

(ert-deftest ar-emacs-lisp-sexp-test-6WqoA8 ()
  (ar-test
      "((asdf)\")\")"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?\())))

(ert-deftest ar-emacs-fundamental-test-FC9JZG ()
  (ar-test
      "((asdf)\")\")"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?\())))

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
;;
(ert-deftest ar-emacs-lisp-sexp-backward-test-tZLyKD ()
  (ar-test
      "((asdf)\")\")"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp-atpt)
    (should (bobp))
    (should (eq (char-after) ?\())))

(ert-deftest ar-emacs--test-o22xai ()
  (ar-test
      "((asdf)\")\")"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp-atpt)
    (should (bobp))
    (should (eq (char-after) ?\())))

(ert-deftest ar-emacs--test-7DBXk7 ()
  (ar-test
      "[[asdf\"]\"]\"]\"]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp-atpt)
    (should (bobp))
    (should (eq (char-after) ?\[))))

(ert-deftest ar-emacs--test-3qLdI3 ()
  (ar-test
      "[(asdf\"]\"]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp-atpt)
    (should (bobp))
    (should (eq (char-after) ?\[))))

(ert-deftest ar-emacs--test-AAnUsp ()
  (ar-test
      "{(asdf\"}\"}"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp-atpt)
    (should (bobp))
    (should (eq (char-after) ?{))))

(ert-deftest ar-sexp-test-OxZJD6 ()
  (ar-test
      "\\s-*[[<({\"]\"].[\"]\"]>\")\"}\"]\"]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (search-forward "[" nil t 1)
   (ar-forward-sexp)
   (should (eq (char-after) ?.))
   (should (eq (char-before) ?\]))
   ))

(ert-deftest ar-sexp-test-Zq562F ()
  (ar-test
      "\\s-*[[<({\"]\"].[\"]\"]>\")\"}\"]\"]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (skip-chars-forward "^[")
   (ar-forward-sexp)
   (should (eobp))
   (should (eq (char-before) ?\]))
   ))

(ert-deftest ar-sexp-test-tEV2zp ()
  (ar-test
      "\\s-*[[<({\"]\"].[\"]\"]>\")\"}\"]\"]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (skip-chars-forward "^<")
   (ar-forward-sexp)
   (should (eq (char-before) ?>))
   ))

(ert-deftest ar-sexp-test-5FSYjd ()
  (ar-test
      "\\s-*<({\"[\"].[\"]\"]>\")\"}\"]\"]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (skip-chars-forward "^[")
   (ar-forward-sexp-intern)
   (should-not (eq (char-before) ?\]))
   ))

(ert-deftest ar-sexp-test-qWvlkE ()
  (ar-test
      "\\s-*<({\"[.[]]\">\")\"}\"]\"]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (skip-chars-forward "^[")
   (ar-forward-sexp-intern)
   (should (eq (char-before) ?\]))
   (should (eq (char-after) ?\"))
   ))

(ert-deftest ar-ert-forward-sexp-test-OM0sSF ()
  (ar-test-with-elisp-buffer-point-min
      ";;;\\\;; (beg)
\(defun foo1 (&optional beg end))"
      (goto-char (point-min))
    (forward-char 9)
    (ar-forward-sexp)
    (should (eq (char-after) ?\())))

(ert-deftest ar-ert-forward-sexp-test-QH86kg ()
  (ar-test-with-elisp-buffer-point-min
"(defun foo ()
  \"Closing paren matched here: )\"
  )"
      (goto-char (point-min))
    (ar-forward-sexp)
    (should-not (nth 3 (parse-partial-sexp (point-min) (point))))
    (should (eq (char-before) ?\)))))

(ert-deftest ar-ert-forward-sexp-test-s8mXBt ()
  (ar-test-with-elisp-buffer-point-min
      "(when (and (< 1 (length endstr))(looking-at (beg-end-regexp-quote-maybe searchform)))
      (goto-char (match-end 0)))"
      (goto-char (point-min))
    (ar-forward-sexp)
    (should (eobp))
    (should-not (nth 3 (parse-partial-sexp (point-min) (point))))
    (should (eq (char-before) ?\)))))

(ert-deftest ar-ert-forward-sexp-test-iT50EO ()
  (ar-test-with-elisp-buffer-point-min
      "\\s-*({\"<[\\].[]]>\")\"}\"]\"]"
      (goto-char (point-min))
    (skip-chars-forward "^<")
    (ar-forward-sexp)
    (should (nth 3 (parse-partial-sexp (point-min) (point))))
    (should (eq (char-before) ?>))))

(ert-deftest ar-ert-forward-sexp-test-clLpbC ()
  (ar-test-with-elisp-buffer-point-min
      "\\s-*({\"<[\\].[]]\")>\"}\"]\"]"
      (goto-char (point-min))
    (skip-chars-forward "^<")
    (ar-forward-sexp)
    (should-not (eq (char-before) ?>))))

(ert-deftest ar-ert-forward-sexp-test-8OVzzq ()
  (ar-test-with-elisp-buffer-point-min
      ";;\\s-*({<[\\].[]]>\")\"}\"]\"]"
      (goto-char (point-min))
    (skip-chars-forward "^<")
    (ar-forward-sexp)
    (should (nth 4 (parse-partial-sexp (point-min) (point))))
    (should (eq (char-before) ?>))))

(ert-deftest ar-ert-forward-sexp-test-8tWuvH ()
  (ar-test-with-elisp-buffer-point-min
      ";;\\s-*({<[\\].
[]]>\")\"}\"]\"]"
      (goto-char (point-min))
    (skip-chars-forward "^<")
    (ar-forward-sexp)
    (should-not (eq (char-before) ?>))
    (should (eq (point-max) 27))
    ))

(ert-deftest ar-ert-forward-sexp-test-ybW06J ()
  (ar-test-with-elisp-buffer-point-min
      ";;\\s-*({<[\\].
[]]>\")\"}\"]\"]"
      (goto-char (point-min))
    (skip-chars-forward "^(")
    (ar-forward-sexp)
    (should-not (eq (char-before) ?\)))
    (should (eq (point-max) 27))
    ))

(ert-deftest ar-ert-forward-sexp-test-Xbr66P ()
  (ar-test-with-elisp-buffer-point-min
      ";;\\s-*({<[\\].
;; []]>)\"}\"]\"]"
      (goto-char (point-min))
    (skip-chars-forward "^(")
    (ar-forward-sexp)
    (should (eq (char-before) ?\)))
    (should (eq (char-after) ?\"))
    ))

(ert-deftest ar-ert-forward-sexp-test-Mfiybl ()
  (ar-test-with-elisp-buffer-point-min
      ";;\\s-*({<[\\].
;; []]>)\"}\"]\"]"
      (goto-char (point-min))
    (skip-chars-forward "^{")
    (ar-forward-sexp)
    (should (eq (char-before) ?}))
    (should (eq (char-after) ?\"))
    ))

(ert-deftest ar-ert-forward-sexp-test-4OUCqP ()
  (ar-test-with-elisp-buffer-point-min
      ";;\\s\"-*({<[\\].
;; []]>)}\"]\"]"
      (goto-char (point-min))
    (skip-chars-forward "^{")
    (ar-forward-sexp)
    (should (eq (char-before) ?}))
    (should (eq (char-after) ?\"))
    ))

(ert-deftest ar-ert-forward-sexp-test-Slu0k4 ()
  (ar-test-with-elisp-buffer-point-min
      ";;\\s\"-*({<[\\].
[]]>)}\"]\"]"
      (goto-char (point-min))
    (skip-chars-forward "^{")
    (ar-forward-sexp)
    (should-not (eq (char-before) ?}))
    (should-not (eq (char-after) ?\"))
    ))

(ert-deftest ar-ert-forward-sexp-test-c1VmPr ()
  (ar-test-with-elisp-buffer-point-min
      "\\s\"-*({<[\\]. []]>)}\"]\"]"
      (goto-char (point-min))
    (skip-chars-forward "^{")
    (ar-forward-sexp)
    (should (eq (char-before) ?}))
    (should (eq (char-after) ?\"))
    ))

(ert-deftest ar-ert-forward-sexp-test-g1sCom ()
  (ar-test-with-elisp-buffer-point-min
      "\\s\"-*({<[\\]. []]>)*}\"]\"]"
      (goto-char (point-min))
    (skip-chars-forward "^*")
    (ar-forward-sexp)
    (should (eq (char-before) ?*))
    (should (eq (char-after) ?}))
    ))

(provide 'ar-sexp-tests)
;; ar-sexp-tests.el ends here
