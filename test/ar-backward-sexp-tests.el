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

(ert-deftest ar-backward-sexp-test-SKyWXX ()
  (ar-test
      "[(asdf]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should (eq (char-after) ?\[))))

(ert-deftest ar-backward-sexp-test-RWfoG3 ()
  (ar-test
      "((asdf))"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should-not (eq (char-after) ?\)))
   ))

(ert-deftest ar-backward-sexp-test-Zk58le ()
  (ar-test
      "{(asdf}"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should (eq (char-after) ?\{))
   ))

(ert-deftest ar-emacs-lisp-backward-sexp-test-awtsCR ()
  (ar-test
      "((asdf))"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?\())))

(ert-deftest ar-emacs-backward-sexp-test-OlO7H1 ()
  (ar-test
      "[[asdf]]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?\[))))

(ert-deftest ar-emacs-backward-sexp-test-3QZk6j ()
  (ar-test
      "[(asdf]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?\[))))

(ert-deftest ar-emacs-backward-sexp-test-oy2hnH ()
  (ar-test
      "{(asdf}"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?{))))

(ert-deftest ar-backward-sexp-test-0Qwg73 ()
  (ar-test
      "[(asdf\"]\"]"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should (eq (char-after) ?\[))))

(ert-deftest ar-backward-sexp-test-K5CfKV ()
  (ar-test
      "((asdf)\")\")"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should-not (eq (char-after) ?\)))
   ))

(ert-deftest ar-backward-sexp-test-g64KOA ()
  (ar-test
      "{(asdf\"}\"}"
   'emacs-lisp-mode
   ar-debug-p
   (goto-char (point-max))
   (skip-chars-backward " \t\r\n\f")
   (ar-backward-sexp)
   (should (eq (char-after) ?\{))
   ))

(ert-deftest ar-emacs-backward-sexp-test-tqmEcC ()
  (ar-test
      "[[asdf\"]\"]]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?\[))))

(ert-deftest ar-emacs-backward-sexp-test-cMVJMz ()
  (ar-test
      "[(asdf\"]\"]"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?\[))))

(ert-deftest ar-emacs-backward-sexp-test-wuhBwe ()
  (ar-test
      "{(asdf\"}\"}"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?{))))


(ert-deftest ar-backward-sexp-test-cos9FQ ()
  (ar-test
      "[(asdf\"]\"]"
   'fundamental-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should (eq (char-after) ?\[))))

(ert-deftest ar-backward-sexp-test-5mnMay ()
  (ar-test
      "((asdf)\")\")"
   'fundamental-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should-not (eq (char-after) ?\)))
   ))

(ert-deftest ar-backward-sexp-test-p669g4 ()
  (ar-test
      "{(asdf\"}\"}"
   'fundamental-mode
   ar-debug-p
   (goto-char (point-max))
   (ar-backward-sexp)
   (should (eq (char-after) ?\{))
   ))

(ert-deftest ar-emacs-lisp-backward-sexp-test-OAHdHO ()
  (ar-test
      "((asdf)\")\")"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "\"" nil t 2)
    (ar-backward-sexp)
    (should (eq (char-after) ?\())
    (should (eq (char-before) ?\())
    ))

(ert-deftest ar-emacs-backward-sexp-fundamental-test-8gy0wy ()
  (ar-test
      "((asdf))"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (bobp))
    (should (eq (char-after) ?\())))

(ert-deftest ar-emacs-backward-sexp-fundamental-test-jmXbbv ()
  (ar-test
      "((asdf))"
    'fundamental-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward ")" nil t 2)
    (ar-backward-sexp)
    (should-not (bobp))
    (should (eq (char-after) ?\())
    (should (eq (char-before) ?\())
    ))

(ert-deftest ar-py-backward-sexp-test-lyqzaM ()
  (ar-test
      "print(f\"Foo {asdf[0]}\")"
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (eq (char-after) ?\())
    (should (eq (char-before) ?t))
    ))

(ert-deftest ar-py-backward-sexp-test-shZeCk ()
  (ar-test
      "print(f\"Foo {asdf[0]}\")"
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward ")" nil t 1)
    (ar-backward-sexp)
    (should (eq (char-after) ?\"))
    (should (eq (char-before) ?f))
    ))

(ert-deftest ar-py-backward-sexp-test-0Nw96j ()
  (ar-test
      "print(f\"Foo {asdf[0]}\")"
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "\"" nil t 1)
    (ar-backward-sexp)
    (should (eq (char-after) ?{))
    (should (eq (char-before) 32))
    ))



(ert-deftest ar-py-backward-sexp-test-h1tsLI ()
  (ar-test
      "print(f\"Foo {asdf[0]}\")"
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "}" nil t 1)
    (ar-backward-sexp)
    (should (eq (char-after) ?\[))
    (should (eq (char-before) ?f))
    ))

(ert-deftest ar-py-backward-sexp-test-YvMT0f ()
  (ar-test
      "print(f\"Foo {asdf[0]}\")"
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "]" nil t 1)
    (ar-backward-sexp)
    (should (eq (char-after) ?\[))
    (should (eq (char-before) ?f))
    ))

(ert-deftest ar-py-backward-sexp-test-ZFPK6U ()
  (ar-test
      "print(f\"Foo {asdf[0]}\")"
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "0" nil t 1)
    (ar-backward-sexp)
    (should (eq (char-after) ?\[))
    (should (eq (char-before) ?f))
    ))

(ert-deftest ar-py-backward-sexp-test-JsaamG ()
  (ar-test
      "print(f\"Foo {asdf[0]}\")"
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "{" nil t 1)
    (ar-backward-sexp)
    (should (eq (char-after) ?F))
    ))

(ert-deftest ar-py-backward-sexp-test-3K8z9v ()
  (ar-test
      "print(f\"Foo {asdf[0]}\")"
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "\"" nil t 2)
    (ar-backward-sexp)
    (should (eq (char-after) ?\())
    ;; (should (eq (char-before) 32))
    ))

(ert-deftest ar-py-backward-sexp-test-IpGVKy ()
  (ar-test
      "print(f\"Foo {asdf[0]}\")"
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "f" nil t 2)
    (ar-backward-sexp)
    (should (eq (char-after) ?\())
    ))

(ert-deftest ar-py-backward-sexp-test-tjfudh ()
  (ar-test
      "print(f\"Foo {asdf[0]}\")"
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "(" nil t 1)
    (ar-backward-sexp)
    (should (eq (char-after) ?p))
    ))


(ert-deftest ar-ert-backward-sexp-test-k1ALAI ()
  (ar-test-with-elisp-buffer
      "\\s\"-*({<[\\\\]. []]>)*}\"]\\\"]"
      (goto-char (point-max))
    (skip-chars-backward "^*")
    (ar-backward-sexp)
    (should (eq (char-after) ?{))
    (should (eq (char-before) ?\())
    ))

(ert-deftest ar-ert-backward-sexp-test-LqsUub ()
  (ar-test
      "print(f\"Foo {asdf[0]}\")"
    'python-mode
    ar-debug-p
    (goto-char (point-min))
    (search-forward "f")
    (ar-backward-sexp)
    (should (eq (char-after) ?\())
    (should (eq (char-before) ?t))
    ))

(ert-deftest ar-ert-backward-sexp-test-dLvHy6 ()
  (ar-test
"def usage():
    print(\"\"\"Fehler: %s:
'python roulette.py 1, 'python roulette.py 2', ... 'python roulette.py n'.
\"\"\" % (
          os.path.basename(sys.argv[0])))
"
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "%")
    (skip-chars-backward " \t\r\n\f")
    (ar-backward-sexp)
    (should (looking-at "\"\"\""))
    ))

(ert-deftest ar-ert-backward-sexp-test-UgZLKt ()
  (ar-test
      "'''asdf'''"
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (ar-backward-sexp)
    (should (looking-at "'''"))
    ))





(provide 'ar-backward-sexp-tests)
;; ar-backward-sexp-tests.el ends here
