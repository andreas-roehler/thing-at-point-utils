;;; translate-paired-delimiters.el --- Translate delimiter chars

;; Copyright (C) 2014-2017  Andreas Röhler

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

;; Translate delimited forms in region, like parentized into bracketed

;;; Code:

(unless (functionp 'ar-escaped)
  (defun ar-escaped ()
    "Return t if char is preceded by an odd number of backslashes. "
    (interactive "p")
    (let ((orig (point))
	  erg)
      (goto-char (match-beginning 0))
      (setq erg (< 0 (abs (% (skip-chars-backward "\\\\")2))))
      (goto-char orig)
      erg)))

(defun ar--translate-paired-delimiters (to)
  (delete-char 1)
  (insert to))

(defun ar--translate-paired-delimiters-base (beg end opening-old opening-new ending-new)
  (save-excursion
    (save-restriction
      ;; (when ar-debug-p (switch-to-buffer (current-buffer)))
      (narrow-to-region beg end)
      (goto-char beg)
      (unless (eq (char-after) opening-old)
	(while (and
		(search-forward (char-to-string opening-old))
		(ar-escaped)))
	(forward-char -1))
      (save-excursion
	(if (eq 123 opening-old)
	    (while (and
		    (search-forward (char-to-string 125))
		    (ar-escaped)))
	  (forward-sexp))
	(forward-char -1)
	(ar--translate-paired-delimiters ending-new))
      (ar--translate-paired-delimiters opening-new))))

(defun ar-paren2bracket (beg end)
  (interactive "r*")
  "Translate delimiter chars FROM into char TO. "
  (ar--translate-paired-delimiters-base beg end ?\( ?\[ ?\]))

(defun ar-paren2brace (beg end)
  (interactive "r*")
  "Translate delimiter chars FROM into char TO. "
  (ar--translate-paired-delimiters-base beg end ?\( ?{ ?}))

(defun ar-bracket2paren (beg end)
  (interactive "r*")
  "Translate delimiter chars FROM into char TO. "
  (ar--translate-paired-delimiters-base beg end ?\[ ?( ?)))

(defun ar-bracket2brace (beg end)
  (interactive "r*")
  "Translate delimiter chars FROM into char TO. "
  (ar--translate-paired-delimiters-base beg end ?\[  ?{ ?}))

(defun ar-brace2paren (beg end)
  (interactive "r*")
  "Translate delimiter chars FROM into char TO. "
  (ar--translate-paired-delimiters-base beg end ?{ ?\( ?\)))

(defun ar-brace2bracket (beg end)
  (interactive "r*")
  "Translate delimiter chars FROM into char TO. "
  (ar--translate-paired-delimiters-base beg end ?{ ?\[ ?\]))

(provide 'translate-paired-delimiters)
;;; translate-paired-delimiters.el ends here
