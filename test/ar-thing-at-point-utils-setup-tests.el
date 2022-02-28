;;; ar-thing-at-point-utils-setup-tests.el --- Provide needed forms -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2022  Andreas Röhler

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

(defvar ar-switch-p nil
  "Switch into test-buffer.")

;; (setq ar-switch-p t)

(defvar ar-debug-p nil
  "Switch into test-buffer.")

;; (setq ar-debug-p t)

(add-to-list 'load-path "/home/speck/arbeit/emacs-lisp/python-modes/components-python-mode/")
(if (file-readable-p "/home/speck/arbeit/emacs-lisp/python-modes/components-python-mode/python-components-mode.el")
    (load "/home/speck/arbeit/emacs-lisp/python-modes/components-python-mode/python-components-mode.el" nil t)
  (when (file-readable-p "../python-mode.el")
    (load "../python-mode.el")))

(defmacro ar-test-with-temp-buffer (contents &rest body)
  "Create temp buffer inserting CONTENTS.
 BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (when ar-switch-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min) (point-max)))
       ,@body)))

(defmacro ar-test-with-temp-buffer-point-min (contents &rest body)
  "Create temp buffer inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (goto-char (point-min))
       (when ar-switch-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min) (point-max)))
       ,@body)))

(defmacro ar-test (contents mode verbose &rest body)
  "Create temp buffer inserting CONTENTS.

BODY is code to be executed within the temp buffer "
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (funcall ,mode)
       (when ,verbose
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min) (point-max)))
       ,@body))
  (sit-for 0.1))

(defmacro ar-test-point-min (contents mode verbose &rest body)
  "Create temp buffer in `python-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (funcall ,mode)
       (insert ,contents)
       (goto-char (point-min))
       (when ,verbose
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min) (point-max)))
       ,@body)))

(defmacro ar-test-with-elisp-buffer (contents &rest body)
  "Create temp buffer in `emacs-lisp-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (emacs-lisp-mode)
       (insert ,contents)
       (when ar-switch-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min) (point-max)))
       ,@body)))

(defmacro ar-test-with-elisp-buffer-point-min (contents &rest body)
  "Create temp buffer inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (emacs-lisp-mode)
       (goto-char (point-min))
       (when ar-switch-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min) (point-max)))
       ,@body)))

(defvar py-kugel-text
"class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]
        if treffer in gruen:
            # print \"0, Gruen\"
        elif treffer in schwarz:
            # print \"%i, Schwarz\" % (treffer)
            ausgabe[1] = treffer
        else:
            # print \"%i, manque\" % (treffer)
            ausgabe[7] = treffer
")

(setq py-kugel-text "class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]
        if treffer in gruen:
            # print \"0, Gruen\"
        elif treffer in schwarz:
            # print \"%i, Schwarz\" % (treffer)
            ausgabe[1] = treffer
        else:
            # print \"%i, manque\" % (treffer)
            ausgabe[7] = treffer
")

(defvar py-forward-text "
# {{
class bar:
    def foo ():
        try:
            if foo:
                for a in b:
                    print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

            elif bar:
                for a in b:
                    pass
            else:
                for a in b:
                    pass
# }}
        except:
            block2
")

(defvar py-up-text "
def foo():
    if True:
        def bar():
            pass
    elif False:
        def baz():
            pass
    else:
        try:
            1 == 1
        except True:
            def foo1():
                if True:
                    def bar1():
                        pass
                elif False:
                    def baz1():
                        pass
                else:
                    try:
                        1 == 1
                    except True:
                        pass
                    else True:
                        pass
                    finally:
                        pass
        else True:
            pass
        finally:
            pass
")

(defmacro py-test-with-temp-buffer (contents &rest body)
  "Create temp buffer in `python-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode py--imenu-create-index-p)
       (insert ,contents)
       (python-mode)
       (when py-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min) (point-max)))
       ,@body)))

(defmacro py-test-with-temp-buffer-point-min (contents &rest body)
  "Create temp buffer in `python-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode py--imenu-create-index-p)
       (insert ,contents)
       (goto-char (point-min))
       (python-mode)
       (when py-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min) (point-max)))
       ,@body)))

;; (defmacro ar-test-with-python-buffer-point-min (contents &rest body)
;;   "Create temp buffer in `python-mode' inserting CONTENTS.
;; BODY is code to be executed within the temp buffer.  Point is
;;  at the beginning of buffer."
;;   (declare (indent 1) (debug t))
;;   `(with-temp-buffer
;;      ;; requires python.el
;;      ;; (and (featurep 'semantic) (unload-feature 'semantic))
;;      ;; (and (featurep 'python) (unload-feature 'python))
;;      (let (hs-minor-mode py--imenu-create-index-p)
;;        (insert ,contents)
;;        (if (featurep 'python-mode)(python-mode)(python))
;;        (goto-char (point-min))
;;        ;; (message "(current-buffer): %s" (current-buffer))
;;        (when ar-debug-p (switch-to-buffer (current-buffer))
;; 	     (font-lock-fontify-region (point-min) (point-max)))
;;        ,@body)
;;      (sit-for 0.1)))

;; (defmacro ar-test-with-python-buffer (contents &rest body)
;;   "Create temp buffer in `python-mode' inserting CONTENTS.
;; BODY is code to be executed within the temp buffer.  Point is
;;  at the end of buffer."
;;   (declare (indent 1) (debug t))
;;   `(with-temp-buffer
;;      ;; (and (featurep 'python) (unload-feature 'python))
;;      (let (hs-minor-mode py--imenu-create-index-p)
;;        (insert ,contents)
;;        (python-mode)
;;        (when ar-debug-p (switch-to-buffer (current-buffer))
;; 	     (font-lock-fontify-region (point-min) (point-max)))
;;        ;; (message "ERT %s" (point))
;;        ,@body)
;;      (sit-for 0.1)))

(defmacro ar-test-with-scala-buffer-point-min (contents &rest body)
  "Create temp buffer in `scala-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     ;; requires scala.el
     ;; (and (featurep 'semantic) (unload-feature 'semantic))
     ;; (and (featurep 'scala) (unload-feature 'scala))
     (let (hs-minor-mode py--imenu-create-index-p)
       (insert ,contents)
       (scala-mode)
       (goto-char (point-min))
       ;; (message "(current-buffer): %s" (current-buffer))
       (when ar-debug-p (switch-to-buffer (current-buffer))
	     (font-lock-fontify-region (point-min) (point-max)))
       ,@body)
     (sit-for 0.1)))

(defmacro ar-test-with-scala-buffer (contents &rest body)
  "Create temp buffer in `scala-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     ;; (and (featurep 'scala) (unload-feature 'scala))
     (let (hs-minor-mode py--imenu-create-index-p)
       (insert ,contents)
       (scala-mode)
       (when ar-debug-p (switch-to-buffer (current-buffer))
	     (font-lock-fontify-region (point-min) (point-max)))
       ;; (message "ERT %s" (point))
       ,@body)
     (sit-for 0.1)))

(defmacro ar-test-with-shell-script-buffer (contents &rest body)
  "Create temp buffer in `emacs-lisp-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (shell-script-mode)
       (insert ,contents)
       (when ar-switch-p
	 (switch-to-buffer (current-buffer))
       (font-lock-fontify-region (point-min) (point-max)))
       ,@body)))

(defmacro ar-test-with-shell-script-buffer-point-min (contents &rest body)
  "Create temp buffer inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (shell-script-mode)
       (goto-char (point-min))
       (when ar-switch-p
	 (switch-to-buffer (current-buffer))
       (font-lock-fontify-region (point-min) (point-max)))
       ,@body)))

(defmacro ar-test-with-insert-function-elisp (function &rest body)
  "Create temp buffer in `emacs-lisp-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  `(with-temp-buffer
     ,function
     (let (hs-minor-mode thing-copy-region)
       (emacs-lisp-mode)
       (when ar-switch-p
	 (switch-to-buffer (current-buffer)))
       ,@body)))

(defmacro ar-test-with-insert-function-constents-elisp (contents function &rest body)
  "Create temp buffer in `emacs-lisp-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  `(with-temp-buffer
     (let (hs-minor-mode thing-copy-region)
       (emacs-lisp-mode)
       (insert ,contents)
       ,function
       (when ar-switch-p
	 (switch-to-buffer (current-buffer)))
       ,@body)))

(provide 'ar-thing-at-point-utils-setup-tests)
;; ar-thing-at-point-utils-setup-tests.el ends here
