;;; ar-forward-sexp-tests.el --- Provide needed forms -*- lexical-binding: t; -*-

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

(ert-deftest ar-forward-sexp-test-Yn2O6E ()
  (ar-test
      "[(asdf]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (ar-forward-sexp)
   (should (eq (char-before) ?\]))))

(ert-deftest ar-forward-sexp-test-iHa1er ()
  (ar-test
      "((asdf))"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (ar-forward-sexp)
   (should-not (eq (char-after) ?\)))
   (should (eq (char-before) ?\)))
   ))

(ert-deftest ar-forward-sexp-test-AH1o3P ()
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

(ert-deftest ar-emacs-forward-sexp-fundamental-test-thwxry ()
  (ar-test-point-min
      "((asdf))"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eq (char-before) ?\)))
    (should (eq (char-before (1- (point))) ?\)))
    ))

(ert-deftest ar-emacs-forward-sexp-fundamental-test-u0s6R8 ()
  (ar-test-point-min
      "[[asdf]]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eobp))
    (should (eq (char-before) ?\]))))

(ert-deftest ar-emacs-forward-sexp-fundamental-test-cSt0BF ()
  (ar-test-point-min
      "[(asdf]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eobp))
    (should (eq (char-before) 93))))

(ert-deftest ar-emacs-forward-sexp-fundamental-test-uHkpk1 ()
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

(ert-deftest ar-emacs-forward-sexp-fundamental-test-g8Q0mD ()
  (ar-test-point-min
      "((asdf))"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) ?\)))))

(ert-deftest ar-emacs-forward-sexp-fundamental-test-hT9LEl ()
  (ar-test-point-min
      "[[asdf]]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) ?\]))))

(ert-deftest ar-emacs-forward-sexp-fundamental-test-cyaubB ()
  (ar-test-point-min
      "[(asdf]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) 93))))

(ert-deftest ar-emacs-forward-sexp-fundamental-test-EzlSpP ()
  (ar-test-point-min
      "{(asdf}"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eq (char-before) ?}))))

;;

(ert-deftest ar-forward-sexp-test-pZ6izl ()
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

(ert-deftest ar-forward-sexp-test-2x3N0X ()
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

(ert-deftest ar-ert-forward-sexp-test-YURL6b ()
  (ar-test-with-elisp-buffer-point-min
      ";;;\\\\;; (beg)
(defun foo1 (&optional beg end))"
      (goto-char (point-min))
    (forward-char 9)
    (ar-forward-sexp)
    (should (eolp))
    (should (eq (char-before) ?\)))))

(ert-deftest ar-ert-forward-sexp-test-ElPh0c ()
  (ar-test-with-elisp-buffer-point-min
"(defun foo ()
  \"Closing paren matched here: )\"
  )"
      (goto-char (point-min))
    (ar-forward-sexp)
    (should-not (nth 3 (parse-partial-sexp (point-min) (point))))
    (should (eq (char-before) ?\)))))

(ert-deftest ar-forward-sexp-test-CG33ej ()
  (ar-test
      "[(asdf\"]\"]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (ar-forward-sexp)
   (should (eq (char-before) ?\]))))

(ert-deftest ar-forward-sexp-test-5ZJTdA ()
  (ar-test
      "((asdf)\")\")"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (ar-forward-sexp)
   (should-not (eq (char-after) ?\)))
   (should (eq (char-before) ?\)))
   ))

(ert-deftest ar-forward-sexp-test-Ov97OT ()
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

(ert-deftest ar-emacs-forward-sexp-fundamental-test-LBTvtu ()
  (ar-test-point-min
      "((asdf)\")\")"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eq (char-before) ?\)))
    (should (eq (char-before (1- (point))) ?\"))
    ))

(ert-deftest ar-emacs-forward-sexp-fundamental-test-MERLU5 ()
  (ar-test-point-min
      "[[asdf\"]\"]]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eobp))
    (should (eq (char-before) ?\]))))

(ert-deftest ar-emacs-forward-sexp-fundamental-test-cMVJMz ()
  (ar-test-point-min
      "[(asdf]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eobp))
    (should (eq (char-before) 93))))

(ert-deftest ar-emacs-forward-sexp-fundamental-test-wuhBwe ()
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

(ert-deftest ar-emacs-forward-sexp-fundamental-test-uBDjZR ()
  (ar-test-point-min
      "((asdf)\")\")"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) ?\)))))

(ert-deftest ar-emacs-forward-sexp-fundamental-test-nxie12 ()
  (ar-test-point-min
      "[[asdf\"]\"]]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) ?\]))))

(ert-deftest ar-emacs-forward-sexp-fundamental-test-aGtMJU ()
  (ar-test-point-min
      "[(asdf]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) 93))))

(ert-deftest ar-emacs-forward-sexp-fundamental-test-sUG5Xy ()
  (ar-test-point-min
      "{(asdf\"}\"}"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp-atpt)
    (should (eobp))
    (should (eq (char-before) ?}))))

;;

(ert-deftest ar-forward-sexp-test-OxZJD6 ()
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

(ert-deftest ar-forward-sexp-test-Zq562F ()
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

(ert-deftest ar-forward-sexp-test-tEV2zp ()
  (ar-test
      "\\s-*[[<({\"]\"].[\"]\"]>\")\"}\"]\"]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (skip-chars-forward "^<")
   (ar-forward-sexp)
   (should (eq (char-before) ?>))
   ))

(ert-deftest ar-forward-sexp-test-5FSYjd ()
  (ar-test
      "\\s-*<({\"[\"].[\"]\"]>\")\"}\"]\"]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (skip-chars-forward "^[")
   (ar-forward-sexp)
   (should-not (eq (char-before) ?\]))
   ))

(ert-deftest ar-sexp-forward-test-qWvlkE ()
  (ar-test-point-min
      "\\s-*<({\"[.[]]\">\")\"}\"]\"]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (skip-chars-forward "^[")
   (ar-forward-sexp)
   (should (eq (char-before) ?\]))
   (should (eq (char-after) ?\"))
   ))

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
      "\\s-*({\"<[\\\\].[]]>\")\"}\"]\"]"
      (goto-char (point-min))
    (skip-chars-forward "^<")
    (ar-forward-sexp)
    ;; (sit-for 1)
    (should (nth 3 (parse-partial-sexp (point-min) (point))))
    (should (eq (char-before) ?>))))

(ert-deftest ar-ert-forward-sexp-test-clLpbC ()
  (ar-test-with-elisp-buffer-point-min
      "\\s-*({\"<[\\].[]]\")>\"}\"]\"]"
      (goto-char (point-min))
    (skip-chars-forward "^<")
    (ar-forward-sexp)
    (should-not (eq (char-before) ?>))))

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
    (should (eq (char-before) 41))
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

(ert-deftest ar-forward-sexp-test-NyIYwZ ()
  (ar-test
      "[(asdf\"]\"]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (ar-forward-sexp)
   (should (eq (char-before) ?\]))))

(ert-deftest ar-forward-sexp-test-RgMjiG ()
  (ar-test
      "((asdf)\")\")"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (ar-forward-sexp)
   (should-not (eq (char-after) ?\)))
   (should (eq (char-before) ?\)))
   ))

(ert-deftest ar-forward-sexp-test-jWGSy4 ()
  (ar-test
      "{(asdf\"}\"}"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-min))
   (ar-forward-sexp)
   (should (eq (char-before) ?\}))
   ))

(ert-deftest ar-emacs-lisp-sexp-test-6esVUB ()
  (ar-test-point-min
      "((asdf)\")\")"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eobp))
    (should (eq (char-before) ?\)))))

(ert-deftest ar-emacs-forward-sexp-fundamental-test-m0YUfQ ()
  (ar-test-point-min
      "((asdf)\")\")"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eq (char-before) ?\)))
    (should (eq (char-before (1- (point))) ?\"))
    ))

(ert-deftest ar-emacs-forward-sexp-fundamental-test-tqmEcC ()
  (ar-test-point-min
      "[[asdf\"]\"]]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-min))
    (ar-forward-sexp)
    (should (eobp))
    (should (eq (char-before) 93))))

(ert-deftest ar-forward-sexp-test-m2rh7d ()
  (ar-test-with-elisp-buffer-point-min
      "(defun foo (arg)
  \" ( Some command ]  (\"
  ;; \"Some command (\"  )
  ;; (  or ] not )
  ;; ( asdf
  (interactive \"p*\")
  (message \"%s\" arg))"
      (goto-char (point-min))
    (search-forward "(")
    (ar-forward-sexp)
    (should (eq (char-before) ?\)))
    ;; (should (eq (char-after) ?\())
    ))

(ert-deftest ar-forward-sexp-test-Yixwry ()
  (ar-test-with-elisp-buffer-point-min
"(defun foo (arg)
  \"(Some command \\\"]\\\" )  ( \"
  ;; \"Some command (\"  )
  ;; (  or ] not )
  ;; ( asdf
  (interactive \"p*\")
  (message \"%s\" arg))
"
      (goto-char (point-min))
    (search-forward "Some")
    (forward-char -5)
    (ar-forward-sexp)
    (should-not (eq (char-before) ?\"))
    (should (eq (char-before) ?\)))
    ))

(ert-deftest ar-forward-sexp-test-BM94u0 ()
  (ar-test-with-elisp-buffer-point-min
      "print(f\"Foo {asdf[0]}\")"
      (goto-char (point-min))
    (search-forward "a")
    (ar-forward-sexp)
    (should (eq (char-before) ?}))
    ;; (should-not (eq (char-before) ?\]))
    ))

(ert-deftest ar-forward-sexp-test-ptFoeA ()
  (ar-test-with-elisp-buffer-point-min
      "(defun foo (arg)
  \" ( Some command   (\"
  ;; \"Some command (\")
  ;; (  or ] not)
  ;; ( asdf
  (interactive \"p*\")
  (message \"%s\" arg))"
      (goto-char (point-min))
    (search-forward "Some")
    (forward-char -6)
    (ar-forward-sexp)
    (should (eq (char-before) ?\"))
    (should (eq (char-before (1- (point))) ?\())))

(ert-deftest ar-forward-sexp-test-WXZ8XH ()
  (ar-test-with-elisp-buffer-point-min
      "(defun foo (arg)
  \" ( Some command   (\"
  ;; \"Some command (\"  )
  ;; (  or ] not )
  ;; ( asdf
  (interactive \"p*\")
  (message \"%s\" arg))"
      (goto-char (point-min))
    (end-of-line)
    (ar-forward-sexp)
    (should-not (eq (char-before (1- (point))) ?\"))
    (should (eq (char-before) ?\)))
    ))

(ert-deftest ar-forward-sexp-test-8jsFZt ()
  (ar-test
      "print(f\"Foo {asdf[0]}\")"
    'python-mode
    ar-debug-p
      (goto-char (point-min))
    (search-forward "0")
    (ar-forward-sexp)
    (should (eq (char-before) ?\]))
    ))

(ert-deftest ar-forward-sexp-test-NezJud ()
  (ar-test
      "print(f\"Foo {asdf[0]}\")"
    'python-mode
    ar-debug-p
      (goto-char (point-min))
    (ar-forward-sexp)
    (should (eq (char-after) ?\())
    ))

(ert-deftest ar-forward-sexp-test-jiIyLX ()
  (ar-test
      "print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})
"
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "{") 
    (ar-forward-sexp)
    (should (eq (char-before) ?}))
    ))



(provide 'ar-forward-sexp-tests)
;; ar-forward-sexp-tests.el ends here
