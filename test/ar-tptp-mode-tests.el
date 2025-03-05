;;; ar-tptp-mode-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Andreas Röhler, <andreas.roehler@online.de>

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

(if (file-readable-p (expand-file-name "~/werkstatt/thing-at-point-utils/ar-tptp-mode.el"))
    (progn
      (add-to-list 'load-path (expand-file-name "~/werkstatt/thing-at-point-utils/ar-tptp-mode.el"))
      (require 'ar-tptp-mode))
  (error "ar-tptp-mode (not available"))

(require 'ar-thingatpt-setup-tests)

(ert-deftest ar-compute-tptp-indentation-test-RbPpW2 ()
  (ar-test-point-min
      ;; "% File     : AGT035^1 : TPTP v8.2.0. Bugfixed v5.4.0."
      "asdf"                                                                
    'ar-tptp-mode
    ar-debug-p
    (goto-char (point-min))
    (font-lock-fontify-buffer)
    (should (eq (face-at-point) 'font-lock-comment-delimiter-face))
    (forward-char 4)
    (should (eq (face-at-point) 'font-lock-comment-face))
    ))



(provide 'ar-tptp-mode-tests)
;;; ar-tptp-mode-tests.el ends here
