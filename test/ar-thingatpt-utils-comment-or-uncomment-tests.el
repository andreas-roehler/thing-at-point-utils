;; ar-thingatpt-utils-comment-or-uncomment-tests.el --- Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Free Software Foundation, Inc.
;; Copyright (C) 2014-2025 Andreas Röhler, <andreas.roehler@online.de>

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; (require 'ert)

(require 'ar-subr)
(require 'ar-thingatpt-setup-tests)
(require 'ar-comment-lor)

;; (setq ar-debug-p t)

(ert-deftest python-comment-text-nYJIEQ ()
  (ar-test
      "# print(\"tagged_tokens[0]: %s \" % tagged_tokens[0])
erg = nltk.pos_tag(res)
"
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "erg")
    (sit-for 0.1)
    (should-not (ar-in-comment-p-atpt))))

(ert-deftest python-comment-text-ZRWCVJ ()
  (ar-test
"    # print(\"tagged_tokens[0]: %s \" % tagged_tokens[0])
    erg = nltk.pos_tag(res)
        # for i in erg:
        # print(i)
    "
    'python-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "erg" nil t 2)
    (save-excursion (ar-comment-or-uncomment-lor))
    (back-to-indentation)
    (should (eq 4 (current-indentation)))
    (should (eq (char-after) ?#))))

(ert-deftest python-comment-text-t8l5IA ()
  (ar-test-point-min
"# for i in text:
    #     print(\"(i): {}\".format(str(i)))
    #     aus.write(str(i))
    "
    'python-mode
    ar-debug-p
    (goto-char (point-min))
    ;; (sit-for 0.1)
    (save-excursion (ar-comment-or-uncomment-lor))
    (back-to-indentation)
    (should (eq 0 (current-indentation)))
    (should (eq (char-after) ?f))))

(ert-deftest ar-kill-comment-test-5gVmaJ ()
  (ar-test-with-elisp-buffer
      "defun foo1 (&optional beg end)
  \" \"
  (interactive \"*\")
  ;; (let ((beg (cond (beg)
  ;;                  ((use-region-p)
  ;;                   (region-beginning))
  ;;                  (t (point-min))))
  ;;       (end (cond (end (copy-marker end))
  ;;                  ((use-region-p)
  ;;                   (copy-marker (region-end)))
  ;;                  (t (copy-marker (point-max))))))"
    (goto-char (point-max))
    (search-backward ";; (let")
    (ar-kill-comment-atpt)
    (should (eobp))))

(ert-deftest ar-ert-delete-comment-test-BoqoQs ()
  (ar-test-with-elisp-buffer-point-min
      "(defun foo1 (&optional beg end)
  \" \"
  (interactive \"*\")
  (let ((beg (cond (beg)
                   ((use-region-p)
                    (region-beginning))
                   (t (point-min))))
        (end (cond (end (copy-marker end))
                   ((use-region-p)
                    (copy-marker (region-end)))
                   (t (copy-marker (point-max))))))
    (save-excursion
      (goto-char beg))
    (when (interactive-p) (message \"%s %s\" beg end))))
;;\\s-*({<[\\]. []]>\")\"}\"]\"]"
      (goto-char (point-max))
    (search-backward "s")
    (ar-delete-comment-atpt)
    (skip-chars-backward " \t\r\n\f") 
    (should (eq (char-before) ?\)))
    ))

(provide 'ar-thingatpt-utils-comment-or-uncomment-tests)
;;; ar-thingatpt-utils-comment-or-uncomment-tests.el ends here
