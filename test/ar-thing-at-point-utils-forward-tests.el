;;; ar-thing-at-point-utils-forward-tests.el --- test forward forms -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>
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

(defvar ar-atpt-forward-test-string "Vorwort.
Der Plan einer Herausgabe meiner Abhandlungen ist durch die Großzügigkeit von FERDINAND SPRINGER verwirklicht worden. Ihm und meinem
Freunde RICHARD COURANT bin ich wegen ihrer stets bereiten, durch Rat
und Tat wirksamen Hilfe zu größtem Dank verpflichtet.
Die wissenschaftliche Arbeit bei der Herausgabe hat nicht nur äußerste
Sorgfalt, sondern auch feinstes Verständnis erfordert und konnte daher nur
geleistet werden von Gelehrten, die durch gründliche Studien in den dabei
behandelten Fächern dazu vorbereitet sind. Diese Aufgabe ist für den vorliegenden ersten Band, der insbesondere meinen großen zahlentheoretischen
Bericht enthält, in vollkommener Weise von den Mathematikern WILHELM
MAGNUS, ÜLGA TAUS\"lKY, HELMUT ULM gelöst worden. Die Entwicklung der
Theorie der algebraischen Zahlen bis in die neueste Zeit wird in dem Nachwort von H. HASSE dargestellt.")

(setq ar-atpt-forward-test-string "Vorwort.
Der Plan einer Herausgabe meiner Abhandlungen ist durch die Großzügigkeit von FERDINAND SPRINGER verwirklicht worden! Ihm und meinem
Freunde RICHARD COURANT bin ich wegen ihrer stets bereiten, durch Rat
und Tat wirksamen Hilfe zu größtem Dank verpflichtet.
Die wissenschaftliche Arbeit bei der Herausgabe hat nicht nur äußerste
Sorgfalt, sondern auch feinstes Verständnis erfordert und konnte daher nur
geleistet werden von Gelehrten, die durch gründliche Studien in den dabei
behandelten Fächern dazu vorbereitet sind. Diese Aufgabe ist für den vorliegenden ersten Band, der insbesondere meinen großen zahlentheoretischen
Bericht enthält, in vollkommener Weise von den Mathematikern WILHELM
MAGNUS, ÜLGA TAUS\"lKY, HELMUT ULM gelöst worden. Die Entwicklung der
Theorie der algebraischen Zahlen bis in die neueste Zeit wird in dem Nachwort von H. HASSE dargestellt.")

(ert-deftest ar-forward-sentence-test-1 ()
  (ar-test-with-elisp-buffer-point-min
      ;; "{asdf} {asdf} {asdf}"
      ar-atpt-forward-test-string
      (should (eq 9 (ar-forward-sentence-atpt)))))

(ert-deftest ar-forward-sentence-test-2 ()
  (ar-test-with-elisp-buffer-point-min
      ;; "{asdf} {asdf} {asdf}"
      ar-atpt-forward-test-string
      (end-of-line)
    (ar-forward-sentence-atpt)
    (should (eq (char-before) ?!))))

;; (search-forward "worden." nil t 1)

(ert-deftest ar-doublequote-alnum-in-region-test ()
  (ar-test-with-elisp-buffer
      "foo\nbar\nbaz"
    (push-mark)
    (goto-char (point-min))
    (ar-doublequote-alnum-in-region-atpt)
    (should (eq (char-after) ?\"))))

(ert-deftest ar-bracket-alnum-in-region-test ()
  (ar-test-with-elisp-buffer
      "foo\nbar\nbaz"
    (push-mark)
    (goto-char (point-min))
    (ar-bracket-alnum-in-region-atpt)
    (should (eq (char-after) ?\[))))

(ert-deftest ar-bracket-bracketed-in-region-test ()
  (ar-test-with-elisp-buffer
      "[foo]\n[bar]\n[baz]"
    (push-mark)
    (goto-char (point-min))
    (ar-bracket-bracketed-in-region-atpt)
    (should (looking-at "\\[\\["))))

(ert-deftest ar-doublequote-bracketed-in-region-test ()
  (ar-test-with-elisp-buffer
      "foo\n[bar]\n[baz]"
    (push-mark)
    (goto-char (point-min))
    (ar-doublequote-bracketed-in-region-atpt)
    (goto-char (point-max))
    (should (eq (char-before) ?\"))))

(ert-deftest ar-brace-doublequoted-in-region-test ()
  (ar-test-with-elisp-buffer
      "\"foo1\"\n\"[bar]\""
    (push-mark)
    (goto-char (point-min))
    (ar-brace-doublequoted-in-region-atpt)
    (end-of-line)
    (should (eq (char-before) ?\}))
    (forward-char -3)
    (should (eq (char-after) ?1))))

(ert-deftest ar-brace-hyphened-in-region-test ()
  (ar-test-with-elisp-buffer
      "-foo1-\n-bar-"
    (push-mark)
    (goto-char (point-min))
    (ar-brace-hyphened-in-region-atpt)
    (end-of-line)
    (forward-char -1)
    (should (eq (char-after) ?}))
    (forward-char -2)
    (should (eq (char-after) ?1))))

(provide 'ar-thing-at-point-utils-forward-tests)
;;; ar-thing-at-point-utils-forward-tests.el ends here
