;; Peel functions start
(defun ar--raise-inner-list (beg end)
  (let* ((bounds (ar-bounds-of-list-atpt))
	 (inner-beg (or (ignore-errors (caar bounds))(car-save bounds)))
	 (inner-end
	  (cond ((ignore-errors (listp bounds))
		 (if (ignore-errors (numberp (cdr bounds)))
		     (copy-marker (cdr bounds))
		   (copy-marker (or (ignore-errors (cdr (cadr bounds)))(ignore-errors (cadr bounds))))))))
	 ;; (inner-end (cadr bounds))
	 (erg (buffer-substring inner-beg inner-end)))
    ;; (delete-region inner-beg inner-end)
    (goto-char beg)
    (delete-region beg end)
    (insert erg)))

(defun ar--raise-inner-sexp (beg end)
  (skip-chars-forward (concat "^" th-beg-delimiter ar-delimiters-atpt))
  (let* ((bounds (ar-bounds-of-delimited-atpt))
	 (inner-beg (or (ignore-errors (caar bounds))(car-safe bounds)))
	 (inner-end
	  (cond ((ignore-errors (listp bounds))
		 (or (ignore-errors (cdr (cadr bounds)))(ignore-errors (cadr bounds))
		     (cdr bounds)))))
	 (erg (buffer-substring inner-beg inner-end)))
    (goto-char beg)
    (delete-region beg end)
    (insert erg)))

(defun ar-peel-list-atpt (&optional arg)  
 "Remove list at point, preserve inner lists. "
  (interactive "*p")
  (ar-th-peel 'list arg))



(provide 'ar-thingatpt-peel)
;;; ar-thingatpt-peel.el ends here
