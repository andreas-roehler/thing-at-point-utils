;;; ar-thingatpt-scala-mode-tests.el --- More thing-atpt tests -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2025  Andreas Röhler

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

(when (file-readable-p (expand-file-name "~/.emacs.d/straight/build/scala-mode"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/straight/build/scala-mode")))

(require 'scala-mode)

(ert-deftest ar-scala-mode-delimited-test-l1Szyp ()
  (ar-test-with-scala-buffer
      "// scala> digitsToDouble (Seq('2', '0', '4', '.', '5'))"
    ar-debug-p
    (goto-char (point-max))
    (search-backward "5")
    (let ((erg (ar-delimited-atpt)))
      (sit-for 0.1)
      (should (string= erg "'5'")))))

(ert-deftest ar-scala-mode-delimited-test-SpDdtE ()
  (ar-test-with-scala-buffer
      "var b = Seq[(Int, Int)]()"
    ar-debug-p
    (goto-char (point-max))
    (search-backward "[")
    (ar-trim-delimited-atpt)
    (should (eq (char-after) 40))))

(provide 'ar-thingatpt-scala-mode-tests)
;;; ar-thingatpt-scala-mode-tests.el ends here
