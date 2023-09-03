;;; thingatpt-utils-core.el --- th-at-point edit functions -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2023 Andreas Röhler, unless
;; indicated otherwise

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>, unless
;; indicated otherwise

;; Version: 0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Keywords: convenience

;;; Commentary:

;; Delivers a set of functions to return, mover over or
;; manipulate a given THING. THING may be a well known
;; form as word, paragraph, but also a char class as
;; ‘alnum’ or a new defined thing.

;; For example ‘ar-alnum-atpt’ will return all
;; alpha-numerical chars below and around cursor as a
;; string. ‘ar-bounds-of-alnum-atpt’ returns the
;; borders of that string as a list and so on.

;; Presently for a given THING the following is
;; implemented:

;; ar-THING-atpt
;; ar-THING-bounds-atpt
;; ar-THING-beginning-position-atpt
;; ar-THING-end-position-atpt
;; ar-THING-beginning-atpt
;; ar-THING-end-atpt
;; ar-THING-length-atpt
;; ar-THING-copy-atpt
;; ar-THING-kill-atpt
;; ar-THING-forward-atpt
;; ar-THING-backward-atpt
;; ar-THING-transpose-atpt
;; ar-THING-sort-atpt
;; ar-THING-check-atpt

;; Beside of the mentioned above, esists still a couple of
;; functions, whose use is much less probable:

;; ar-THING-slash-atpt
;; ar-THING-doublebackslash-atpt
;; ar-THING-doubleslash-atpt
;; ar-THING-delete-in-region
;; ar-blok-THING-atpt
;; ar-THING-escape-atpt
;; ar-THING-doublequote-atpt
;; ar-THING-doublebackslashparen-atpt
;; ar-THING-dollar-atpt
;; ar-THING-equalize-atpt
;; ar-THING-greaterangle-atpt
;; ar-THING-lesserangle-atpt
;; ar-THING-backslash-atpt
;; ar-THING-brace-atpt
;; ar-THING-bracket-atpt
;; ar-comment-THING-atpt
;; ar-commatize-THING-atpt
;; ar-quote-THING-atpt
;; ar-THING-hyphen-atpt
;; ar-THING-mark-atpt
;; ar-THING-hide-atpt
;; ar-THING-show-atpt
;; ar-THING-hide-show-atpt
;; ar-THING-curvedsinglequote-atpt
;; ar-THING-parentize-atpt
;; ar-THING-separate-atpt
;; ar-THING-singlequote-atpt
;; ar-THING-trim-atpt
;; ar-THING-left-trim-atpt
;; ar-THING-right-trim-atpt
;; ar-underscore-THING-atpt
;; ar-whitespace-THING-atpt

;; To see what's implemented, consult contents of
;; variables at the end of this file as
;; ‘ar-paired-delimit-aktiv’, ‘ar-paired-delimited-passiv’, etc.

;; Call one of the test-functions `C-u ar-th-delimtest'
;; with come chars in scratch-buffer
;; or any else changable buffer to get an impression.

;; The idea comes from Mike Williams
;; <mikew@gopher.dosli.govt.nz>, author of
;; thingatpt.el

;; The goal is to have a set of similar forms. For
;; example, to provide a word with double-quotes around
;; it, call ar-doublequote-word-atpt. In a similar way you
;; may double-quote not just a word, but any object
;; instrumented here as THING. To make parentheses
;; around it call ar-parentize-word-atpt, etc.

;; Move-functions of this package differ from common
;; behaviour in such, as ‘ar-forward-word-atpt’ stops
;; not after THING, but on the last char of
;; THING. That's in order to enable a call of
;; thing-at-point functions at the end
;; position. Otherwise, when cursor stops after word
;; (THING) as does ‘forward-word’, ‘ar-word-atpt’ would return
;; nil.

;; To see other features, maybe try ‘ar-separate-list-atpt’
;; or ‘ar-comment-list-atpt’ while point is inside a
;; list. Try it again with an abstract char-class as
;; [:alnum:], i.e. try ‘ar-comment-alnum-atpt’,
;; ‘ar-brace-alnum-atpt’ etc.

;; This utility comes with test-functions which return
;; the possible results of most functions (exception
;; are the kill-fns). Call th-test, th-mv-test
;; or th-delimtest over text. That-delimtest
;; changes but restores the buffer. Customize the speed
;; of execution via ‘ar-th-test-delay’

;; Diffs to basics of required thingatpt.el:
;; ‘bounds-of-thing-at-point’ is replaced by a new
;; ‘ar-th-bounds’, which now first searches
;; backward. As a consequence several
;; ‘beginning-op-at’ and ‘end-op-at’ constructs had
;; to be rewritten.

;; Behavior in general is not validating; i.e. if you
;; call ar-url-atpt and there is no url, all chars at
;; point may be picked, which could be part of a
;; url. Sometimes, however, a kind of validation may be
;; introduced.

;; If calling from a program `bounds-of-THING-atpt' is
;; recommended as an entry-point. It delivers a list
;; with beg and end positions.

;; In case of trouble, please send me a bug report. Any
;; ideas and comments welcome.

;; You might be interested also to visit Drew Adam's
;; http://www.emacswiki.org/emacs/thingatpt+.el
;; which predates this approach and was helpful writing it.

;; Thing-at-point delivers a portion of the
;; buffer. Thats useful, if THING is not as easy to grasp as a word.
;; For example the first string of an objekt like:

;; ("4[[:punct:] \t\r\n]? [[:punct:] \t\r\n]?C[[:punct:] \t\r\n]?.[[:punct:] \t\r\n]?2[[:punct:] \t\r\n]?4[[:punct:] \t\r\n]?6[[:punct:] \t\r\n]?4[[:punct:] \t\r\n]?/[[:punct:] \t\r\n]?0[[:punct:] \t\r\n]?3[[:punct:] \t\r\n]? [[:punct:] \t\r\n]?B" . "blah blub B")

;; Remove comments and put the cursor somewhere into the first
;; string:
;; ‘ar-doublequoted-atpt’ will return it, copied into the kill-ring,
;; enabling yanking it and a lot of further actions.

;; ‘ar-doublequoted-atpt’ here is to
;; (global-set-key [(super \")] 'ar-doublequoted-atpt)

;; alike a range of similar commands exist:
;; (global-set-key [(super \')] 'ar-singlequoted-atpt)
;; (global-set-key [(super \))] 'ar-parentized-atpt)
;; (global-set-key [(super \/)] 'ar-slashed-atpt)
;; (global-set-key [(super \\)] 'ar-backslashed-atpt)
;; (global-set-key [(super \])] 'ar-bracketed-atpt)
;; (global-set-key [(super \})] 'ar-braced-atpt)

;; So far THING is simply picked up.

;; Different approach combines copying, deleting with delimiting

;; if region is active:

;; (global-set-key [(control c) (\")] 'ar-doublequote-or-copy-atpt)

;; will provide doublequotes at beginning and end of region.

;; With negative argument it deletes the doublequoted portion under
;; point.

;; Without any argument these functions return as their simplier
;; counterparts

;; With universal argument [(control u)] delimiters --i.e. doublequotes, slashes, whatever-- are stripped.

;;

;; THING as a buffer substring is determined by
;; move-functions specified for thingatpt, called
;; beginning-op-at and end-op-at. Point is stored
;; after move, beginning and end delivered as pair: as
;; consed bounds-of-thing. It's easy to write your own
;; thing-at-point functions that way. You need the
;; caller and both move forms:

;; (defun MY-FORM-atpt (&optional arg)
;;   " "
;;   (interactive "p")
;;   (ar-th 'MY-FORM arg))

;; (put 'MY-FORM 'beginning-op-at
;;            (lambda () MY-FORWARD-MOVE-CODE))

;; (put 'MY-FORM 'end-op-at
;;      (lambda () MY-BACKWARD-MOVE-CODE))

;; For example if you want to pick all chars at point
;; which are written between a string "AAA" and a
;; "BBB", which may exist as
;; AAA Luckily detected a lot of things! BBB
;; After evaluation of
;; (put 'MY-FORM 'beginning-op-at
;;      (lambda ()
;;        (search-backward "AAA" nil 'move 1)
;;        ;; step chars of search expression back
;;        (forward-char 3)))
;;
;; (put 'MY-FORM 'end-op-at
;;      (lambda ()
;;        (search-forward "BBB" nil 'move 1)
;;        (forward-char -3)))
;; together with the functions definition above, it's ready.
;; M-x MY-FORM-atpt
;; (while point inside) you should see:
;; " Luckily detected a lot of things! "
;; in the minibuffer.

;; Some keys

;; (define-key emacs-lisp-mode-map [(control c)(q)] 'ar-parentized-forward-atpt)
;; (define-key emacs-lisp-mode-map [(super c)())] 'ar-symbol-parentize-atpt)
;; (define-key emacs-lisp-mode-map [(super c)(n)] 'ar-region-parentize-atpt)
;; (global-set-key [(control c)(<)] 'ar-lesserangle-or-copy-atpt)
;; (global-set-key [(control c)(>)] 'ar-greaterangle-or-copy-atpt)
;; (global-set-key [(control c)(")] 'ar-doublequote-or-copy-atpt)
;; (global-set-key [(control c)(')] 'ar-singlequote-or-copy-atpt)
;; (global-set-key [(control c)(()] 'ar-paren-atpt)
;; (global-set-key [(control c)())] 'ar-parentize-or-copy-atpt)
;; (global-set-key [(control c)(/)] 'ar-slash-or-copy-atpt)
;; (global-set-key [(control c)(*)] 'ar-star-or-copy-atpt)

;;; Code:
(require 'ar-subr)
(require 'beg-end)
(require 'hideshow)
(defconst Emacs-Werkstatt-version "1.5")

(when (featurep 'xemacs) (require 'overlay))

(defgroup werkstatt nil
  "Return, mover over or manipulate a given THING."
  :prefix "ar-"
  :group 'matching)

(defcustom sort-fold-case nil
  "Whether alphabetic case affects the sort order

Used by ‘ar-sort-numbers-subr’"

  :type 'boolean
  :group 'werkstatt)

(defvar ar-match-in-string-p nil
  "If an expression starts inside a string.

Internal use only.")

(defvar ar-match-in-comment-p nil
  "If an expression starts inside a comment.

Internal use only.")

(defcustom ar-werkstatt-hs-minor-mode-p nil
  ""

  :type 'boolean
  :group 'werkstatt)

(defcustom thing-copy-region nil
  "If a found THING should be copied into the kill-ring.

Default is nil"
  :type 'boolean
  :group 'werkstatt)

(defcustom ar-newlines-separate-after 1
  "How many newlines at-th-separate should insert at the end"

  :type 'number
  :group 'werkstatt)

(defcustom ar-newlines-separate-before 1
  "How many newlines at-th-separate should insert at the end"

  :type 'number
  :group 'werkstatt)

;; (defvar th-orig 0
;; "Correct orig according to delimiter-length")

(when (featurep 'xemacs)
  (defcustom alnum "\\sw"
    "Rexexp to specify the character class
Follows word-syntax. Use something like
   \"[a-zA-ZäöüßÄÖÜ0-9]\" maybe instead.
‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom alpha "[a-zA-ZäöüßÄÖÜ]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom ascii "[\000-\177]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom blank "[ \t]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom cntrl "[\000-\006]\016-\037]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom digit "[0-9]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom graph "[\041-\177\241-\377]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom lower "[a-zäöüß]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom multibyte "[.]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom nonascii "[^\040-\177]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom print "[\041-\177\241-\377]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom punct "[.,-_:;?!]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom space "[ \t]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom unibyte "[.]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom upper "[A-ZÄÖÜ]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom xdigit "[0-9.,]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))


;; ar-insert-put-classes start

;; Alnum
(put 'alnum 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:alnum:]")))
	      (looking-at "[[:alnum:]]"))
	 (cons (point) (1+ (point))))))

(put 'alnum 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:alnum:]"))
	    (cons (1- (point)) (point)))))


(put 'alnum 'forward-op-at
     (lambda ()
       (if (< 0 (skip-chars-forward "[:alnum:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:alnum:]"))
           (point)))))

(put 'alnum 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:alnum:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:alnum:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:alnum:]")))
	    (point))))))

;; Alpha
(put 'alpha 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:alpha:]")))
	      (looking-at "[[:alpha:]]"))
	 (cons (point) (1+ (point))))))

(put 'alpha 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:alpha:]"))
	    (cons (1- (point)) (point)))))


(put 'alpha 'forward-op-at
     (lambda ()
       (if (< 0 (skip-chars-forward "[:alpha:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:alpha:]"))
           (point)))))

(put 'alpha 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:alpha:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:alpha:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:alpha:]")))
	    (point))))))

;; Ascii
(put 'ascii 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:ascii:]")))
	      (looking-at "[[:ascii:]]"))
	 (cons (point) (1+ (point))))))

(put 'ascii 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:ascii:]"))
	    (cons (1- (point)) (point)))))


(put 'ascii 'forward-op-at
     (lambda ()
       (if (< 0 (skip-chars-forward "[:ascii:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:ascii:]"))
           (point)))))

(put 'ascii 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:ascii:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:ascii:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:ascii:]")))
	    (point))))))

;; Blank
(put 'blank 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:blank:]")))
	      (looking-at "[[:blank:]]"))
	 (cons (point) (1+ (point))))))

(put 'blank 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:blank:]"))
	    (cons (1- (point)) (point)))))


(put 'blank 'forward-op-at
     (lambda ()
       (if (< 0 (skip-chars-forward "[:blank:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:blank:]"))
           (point)))))

(put 'blank 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:blank:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:blank:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:blank:]")))
	    (point))))))

;; Cntrl
(put 'cntrl 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:cntrl:]")))
	      (looking-at "[[:cntrl:]]"))
	 (cons (point) (1+ (point))))))

(put 'cntrl 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:cntrl:]"))
	    (cons (1- (point)) (point)))))


(put 'cntrl 'forward-op-at
     (lambda ()
       (if (< 0 (skip-chars-forward "[:cntrl:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:cntrl:]"))
           (point)))))

(put 'cntrl 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:cntrl:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:cntrl:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:cntrl:]")))
	    (point))))))

;; Digit
(put 'digit 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:digit:]")))
	      (looking-at "[[:digit:]]"))
	 (cons (point) (1+ (point))))))

(put 'digit 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:digit:]"))
	    (cons (1- (point)) (point)))))


(put 'digit 'forward-op-at
     (lambda ()
       (if (< 0 (skip-chars-forward "[:digit:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:digit:]"))
           (point)))))

(put 'digit 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:digit:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:digit:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:digit:]")))
	    (point))))))

;; Graph
(put 'graph 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:graph:]")))
	      (looking-at "[[:graph:]]"))
	 (cons (point) (1+ (point))))))

(put 'graph 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:graph:]"))
	    (cons (1- (point)) (point)))))


(put 'graph 'forward-op-at
     (lambda ()
       (if (< 0 (skip-chars-forward "[:graph:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:graph:]"))
           (point)))))

(put 'graph 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:graph:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:graph:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:graph:]")))
	    (point))))))

;; Lower
(put 'lower 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:lower:]")))
	      (looking-at "[[:lower:]]"))
	 (cons (point) (1+ (point))))))

(put 'lower 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:lower:]"))
	    (cons (1- (point)) (point)))))


(put 'lower 'forward-op-at
     (lambda ()
       (if (< 0 (skip-chars-forward "[:lower:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:lower:]"))
           (point)))))

(put 'lower 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:lower:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:lower:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:lower:]")))
	    (point))))))

;; Nonascii
(put 'nonascii 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:nonascii:]")))
	      (looking-at "[[:nonascii:]]"))
	 (cons (point) (1+ (point))))))

(put 'nonascii 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:nonascii:]"))
	    (cons (1- (point)) (point)))))


(put 'nonascii 'forward-op-at
     (lambda ()
       (if (< 0 (skip-chars-forward "[:nonascii:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:nonascii:]"))
           (point)))))

(put 'nonascii 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:nonascii:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:nonascii:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:nonascii:]")))
	    (point))))))

;; Print
(put 'print 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:print:]")))
	      (looking-at "[[:print:]]"))
	 (cons (point) (1+ (point))))))

(put 'print 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:print:]"))
	    (cons (1- (point)) (point)))))


(put 'print 'forward-op-at
     (lambda ()
       (if (< 0 (skip-chars-forward "[:print:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:print:]"))
           (point)))))

(put 'print 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:print:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:print:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:print:]")))
	    (point))))))

;; Punct
(put 'punct 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:punct:]")))
	      (looking-at "[[:punct:]]"))
	 (cons (point) (1+ (point))))))

(put 'punct 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:punct:]"))
	    (cons (1- (point)) (point)))))


(put 'punct 'forward-op-at
     (lambda ()
       (if (< 0 (skip-chars-forward "[:punct:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:punct:]"))
           (point)))))

(put 'punct 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:punct:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:punct:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:punct:]")))
	    (point))))))

;; Space
(put 'space 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:space:]")))
	      (looking-at "[[:space:]]"))
	 (cons (point) (1+ (point))))))

(put 'space 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:space:]"))
	    (cons (1- (point)) (point)))))


(put 'space 'forward-op-at
     (lambda ()
       (if (< 0 (skip-chars-forward "[:space:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:space:]"))
           (point)))))

(put 'space 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:space:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:space:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:space:]")))
	    (point))))))

;; Upper
(put 'upper 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:upper:]")))
	      (looking-at "[[:upper:]]"))
	 (cons (point) (1+ (point))))))

(put 'upper 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:upper:]"))
	    (cons (1- (point)) (point)))))


(put 'upper 'forward-op-at
     (lambda ()
       (if (< 0 (skip-chars-forward "[:upper:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:upper:]"))
           (point)))))

(put 'upper 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:upper:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:upper:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:upper:]")))
	    (point))))))

;; Xdigit
(put 'xdigit 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:xdigit:]")))
	      (looking-at "[[:xdigit:]]"))
	 (cons (point) (1+ (point))))))

(put 'xdigit 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:xdigit:]"))
	    (cons (1- (point)) (point)))))


(put 'xdigit 'forward-op-at
     (lambda ()
       (if (< 0 (skip-chars-forward "[:xdigit:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:xdigit:]"))
           (point)))))

(put 'xdigit 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:xdigit:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:xdigit:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:xdigit:]")))
	    (point))))))

;; Paired delimited forms start

;; Braced
(put 'braced 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "{" "}" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'braced 'end-op-at
     (lambda ()
       (end-of-form-base "{" "}" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'braced 'forward-op-at
     (lambda ()
       (unless (eobp)
         (end-of-form-base "{" "}" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t))))

(put 'braced 'backward-op-at
     (lambda ()
       (unless (bobp)
         (beginning-of-form-base "{" "}" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t))))

;; Symboled
(put 'symboled 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "`" "'" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'symboled 'end-op-at
     (lambda ()
       (end-of-form-base "`" "'" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'symboled 'forward-op-at
     (lambda ()
       (unless (eobp)
         (end-of-form-base "`" "'" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t))))

(put 'symboled 'backward-op-at
     (lambda ()
       (unless (bobp)
         (beginning-of-form-base "`" "'" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t))))

;; Bracketed
(put 'bracketed 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "\\[" "\]" nil 'move 0 ar-match-in-comment-p t 'ar-syntax ar-match-in-string-p)))

(put 'bracketed 'end-op-at
     (lambda ()
       (end-of-form-base "\\[" "\]" nil 'move 0 ar-match-in-comment-p t 'ar-syntax ar-match-in-string-p)))

(put 'bracketed 'forward-op-at
     (lambda ()
       (unless (eobp)
         (end-of-form-base "\\[" "\]" nil 'move 0 ar-match-in-comment-p t 'ar-syntax ar-match-in-string-p t))))

(put 'bracketed 'backward-op-at
     (lambda ()
       (unless (bobp)
         (beginning-of-form-base "\\[" "\]" nil 'move 0 ar-match-in-comment-p t 'ar-syntax ar-match-in-string-p t))))

;; Lesserangled
(put 'lesserangled 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "<" ">" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'lesserangled 'end-op-at
     (lambda ()
       (end-of-form-base "<" ">" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'lesserangled 'forward-op-at
     (lambda ()
       (unless (eobp)
         (end-of-form-base "<" ">" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t))))

(put 'lesserangled 'backward-op-at
     (lambda ()
       (unless (bobp)
         (beginning-of-form-base "<" ">" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t))))

;; Greaterangled
(put 'greaterangled 'beginning-op-at
     (lambda ()
       (beginning-of-form-base ">" "<" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'greaterangled 'end-op-at
     (lambda ()
       (end-of-form-base ">" "<" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'greaterangled 'forward-op-at
     (lambda ()
       (unless (eobp)
         (end-of-form-base ">" "<" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t))))

(put 'greaterangled 'backward-op-at
     (lambda ()
       (unless (bobp)
         (beginning-of-form-base ">" "<" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t))))

;; Curvedsinglequoted
(put 'curvedsinglequoted 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "‘" "’" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'curvedsinglequoted 'end-op-at
     (lambda ()
       (end-of-form-base "‘" "’" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'curvedsinglequoted 'forward-op-at
     (lambda ()
       (unless (eobp)
         (end-of-form-base "‘" "’" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t))))

(put 'curvedsinglequoted 'backward-op-at
     (lambda ()
       (unless (bobp)
         (beginning-of-form-base "‘" "’" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t))))

;; Curveddoublequoted
(put 'curveddoublequoted 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "“" "”" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'curveddoublequoted 'end-op-at
     (lambda ()
       (end-of-form-base "“" "”" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'curveddoublequoted 'forward-op-at
     (lambda ()
       (unless (eobp)
         (end-of-form-base "“" "”" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t))))

(put 'curveddoublequoted 'backward-op-at
     (lambda ()
       (unless (bobp)
         (beginning-of-form-base "“" "”" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t))))

;; Parentized
(put 'parentized 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "\(" "\)" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'parentized 'end-op-at
     (lambda ()
       (end-of-form-base "\(" "\)" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'parentized 'forward-op-at
     (lambda ()
       (unless (eobp)
         (end-of-form-base "\(" "\)" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t))))

(put 'parentized 'backward-op-at
     (lambda ()
       (unless (bobp)
         (beginning-of-form-base "\(" "\)" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t))))

;; Paired delimited forms end

;; Unpaired delimited forms start

;; Backslashed
(put 'backslashed 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "\\" "\\" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'backslashed 'end-op-at
     (lambda ()
       (end-of-form-base "\\" "\\" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'backslashed 'forward-op-at
     (lambda ()
       (end-of-form-base "\\" "\\" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t)))

(put 'backslashed 'backward-op-at
     (lambda ()
       (beginning-of-form-base "\\" "\\" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))


;; Backticked
(put 'backticked 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "`" "`" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'backticked 'end-op-at
     (lambda ()
       (end-of-form-base "`" "`" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'backticked 'forward-op-at
     (lambda ()
       (end-of-form-base "`" "`" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t)))

(put 'backticked 'backward-op-at
     (lambda ()
       (beginning-of-form-base "`" "`" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))


;; Coloned
(put 'coloned 'beginning-op-at
     (lambda ()
       (beginning-of-form-base ":" ":" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'coloned 'end-op-at
     (lambda ()
       (end-of-form-base ":" ":" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'coloned 'forward-op-at
     (lambda ()
       (end-of-form-base ":" ":" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t)))

(put 'coloned 'backward-op-at
     (lambda ()
       (beginning-of-form-base ":" ":" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))


;; Dollared
(put 'dollared 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "$" "$" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'dollared 'end-op-at
     (lambda ()
       (end-of-form-base "$" "$" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'dollared 'forward-op-at
     (lambda ()
       (end-of-form-base "$" "$" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t)))

(put 'dollared 'backward-op-at
     (lambda ()
       (beginning-of-form-base "$" "$" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))


;; Doublequoted
(put 'doublequoted 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "\"" "\"" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'doublequoted 'end-op-at
     (lambda ()
       (end-of-form-base "\"" "\"" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'doublequoted 'forward-op-at
     (lambda ()
       (end-of-form-base "\"" "\"" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t)))

(put 'doublequoted 'backward-op-at
     (lambda ()
       (beginning-of-form-base "\"" "\"" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))


;; Equalized
(put 'equalized 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "=" "=" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'equalized 'end-op-at
     (lambda ()
       (end-of-form-base "=" "=" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'equalized 'forward-op-at
     (lambda ()
       (end-of-form-base "=" "=" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t)))

(put 'equalized 'backward-op-at
     (lambda ()
       (beginning-of-form-base "=" "=" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))


;; Hyphened
(put 'hyphened 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "-" "-" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'hyphened 'end-op-at
     (lambda ()
       (end-of-form-base "-" "-" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'hyphened 'forward-op-at
     (lambda ()
       (end-of-form-base "-" "-" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t)))

(put 'hyphened 'backward-op-at
     (lambda ()
       (beginning-of-form-base "-" "-" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))


;; Singlequoted
(put 'singlequoted 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "'" "'" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'singlequoted 'end-op-at
     (lambda ()
       (end-of-form-base "'" "'" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'singlequoted 'forward-op-at
     (lambda ()
       (end-of-form-base "'" "'" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t)))

(put 'singlequoted 'backward-op-at
     (lambda ()
       (beginning-of-form-base "'" "'" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))


;; Slashed
(put 'slashed 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "/" "/" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'slashed 'end-op-at
     (lambda ()
       (end-of-form-base "/" "/" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'slashed 'forward-op-at
     (lambda ()
       (end-of-form-base "/" "/" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t)))

(put 'slashed 'backward-op-at
     (lambda ()
       (beginning-of-form-base "/" "/" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))


;; Stared
(put 'stared 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "*" "*" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'stared 'end-op-at
     (lambda ()
       (end-of-form-base "*" "*" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'stared 'forward-op-at
     (lambda ()
       (end-of-form-base "*" "*" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t)))

(put 'stared 'backward-op-at
     (lambda ()
       (beginning-of-form-base "*" "*" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))


;; Underscored
(put 'underscored 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "_" "_" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'underscored 'end-op-at
     (lambda ()
       (end-of-form-base "_" "_" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'underscored 'forward-op-at
     (lambda ()
       (end-of-form-base "_" "_" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t)))

(put 'underscored 'backward-op-at
     (lambda ()
       (beginning-of-form-base "_" "_" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))


;; Whitespaced
(put 'whitespaced 'beginning-op-at
     (lambda ()
       (beginning-of-form-base " " " " nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'whitespaced 'end-op-at
     (lambda ()
       (end-of-form-base " " " " nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'whitespaced 'forward-op-at
     (lambda ()
       (end-of-form-base " " " " nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t)))

(put 'whitespaced 'backward-op-at
     (lambda ()
       (beginning-of-form-base " " " " nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))


;; Unpaired delimited forms end


;; Expression
(put 'expression 'beginning-op-at
     (lambda ()
       (ar-backward-expression)))

(put 'expression 'end-op-at
     (lambda ()
       (ar-forward-expression)))

;; Block
(put 'block 'beginning-op-at
     'ar-backward-block)

(put 'block 'end-op-at
     (lambda ()
       (ar-forward-block)))

;; Char
(put 'char 'beginning-op-at
     (lambda ()(point) ))

(put 'char 'end-op-at
     (lambda ()(unless (eobp) (goto-char (1+ (point))))))

(put 'char 'forward-op-at
     (lambda ()(unless (eobp) (goto-char (1+ (point))))))

;; String
(defcustom th-string-beg-delimiter "»‘“'\""
  "Specify the string start char."
  :type 'string
  :group 'werkstatt)

(defcustom th-string-end-delimiter "«”’'\""
  "Specify the string end char."
  :type 'string
  :group 'werkstatt)

;; String
(put 'string 'beginning-op-at
     (lambda ()
       (save-restriction
         ;; (widen)
         (or (and ar-use-parse-partial-sexp
                  (let* ((pps (parse-partial-sexp (point-min) (point)))
                         (pos8 (nth 8 pps)))
                    (when (nth 3 pps)
                      (goto-char pos8))))
             (member (char-before) (mapcar 'identity th-string-beg-delimiter))
             ;; (re-search-backward (concat "[" th-string-beg-delimiter "]+") nil 'move 1)
             (and (< 0 (abs (skip-chars-backward (concat "^" (char-to-string 8222) th-string-beg-delimiter))))
                  (prog1 (member (char-before) (mapcar 'identity th-string-beg-delimiter))
                    (backward-char))
                  )
             (when (or (eq 15 (car (syntax-after (point))))(eq 7 (car (syntax-after (point)))))
               (list (point) (save-excursion (skip-chars-forward (char-to-string (char-after)))(point)))))
         (cons (point) (1+ (point)))
         )))

(put 'string 'end-op-at
     (lambda ()
       (let (erg)
         (if (member (char-after) (list ?' ?\"))
             (progn
               (forward-sexp)
               (cons (1- (point)) (point)))
           (skip-chars-forward (concat "^" (char-to-string (setq erg (char-after)))))
           (when (eq (char-after) erg)
             (forward-char 1)
             (cons (1- (point)) (point)))))))

(put 'string 'forward-op-at
     (lambda ()
       (and (< 0 (abs (skip-chars-forward "^\"")))
            (eq (char-after) 34)
            (forward-char 2))))

(put 'string 'backward-op-at
     (lambda ()
       (when (and (< 0 (abs (skip-chars-backward "^\"")))
                  (eq (char-before) 34))
         (backward-char 1))))

;; ;; Strings
;; (put 'string 'beginning-op-at
;;      (lambda ()
;;        (save-restriction
;;       (widen)
;;       (if ar-use-parse-partial-sexp
;;           (let* ((pps (parse-partial-sexp (point-min) (point)))
;;                  (pos8 (nth 8 pps)))
;;             (when (nth 3 pps)
;;               (goto-char pos8)))
;;         (when
;;             (re-search-backward "\\([^\\\\]\\)\\(\"\\)" nil 'move 1)
;;           (goto-char (match-beginning 2))))
;;       (when (looking-at "\"*")
;;         (list (match-beginning 0) (match-end 0))))))

;; (put 'string 'end-op-at
;;      (lambda ()
;;        (save-restriction
;;       (widen)
;;       (forward-char 1)
;;       (if ar-use-parse-partial-sexp
;;           (let* ((orig (point))
;;                  (pps (parse-partial-sexp (point-min) (point)))
;;                  (char (char-to-string (nth 3 pps)))
;;                  (done t))
;;             (progn
;;               (while (and (not (eobp)) (prog1 done (forward-char 1))
;;                           (setq done (skip-chars-forward (concat "^" char)))

;;                           (nth 5 (parse-partial-sexp orig (point)))))
;;               (when (and (< orig (point))(looking-at char))
;;                 (list (match-beginning 0) (match-end 0)))))
;;         (when (re-search-forward "[^\\\\]\"" nil 'move 1)
;;           (list (match-beginning 0) (match-end 0)))))))

;; Abbrev
(put 'abbrev 'beginning-op-at
     (lambda ()
       (when
           (looking-at "[[:alnum:]]")
         (skip-chars-backward "[:alnum:].-")(point))))

(put 'abbrev 'end-op-at
     (lambda ()
       (skip-chars-forward "[:alnum:].-")(point)))

;; Acronym
(put 'acronym 'beginning-op-at
     (lambda ()
       (ar-th-gotobeg 'symbol)))

(put 'acronym 'end-op-at
     (lambda ()
       (when (or (looking-at "[\({]\\([A-Z][A-Za-z -]+\\)[\)}]")
                 (looking-at "\\(\\<[A-Z][A-Za-z]*[A-Z][A-Za-z]*\\>\\)"))
         (goto-char (match-end 0)))))

;; Buffer
(put 'buffer 'beginning-op-at
     (lambda ()
       (let ((pos (point-min)))
         (unless (eq (point) pos)
           (goto-char pos)
           pos))))

(put 'buffer 'end-op-at
     (lambda ()
       (let ((pos (point-max)))
         (unless (eq (point) pos)
           (goto-char pos)
           pos))))

;; Comment
(put 'comment 'beginning-op-at
     (lambda ()
       (let* ((nesting (not (or (string= "" comment-end)(eq 10 comment-end))))
              (erg (when nesting
                     (if (looking-at (concat "[ 	]*" (regexp-quote comment-start)))
                         (cons (match-beginning 0) (match-end 0))
                       (beginning-of-form-base comment-start comment-end nil 'move 1 t))))
              last)
         (unless erg
           (when (looking-at (concat "[ 	]*" (regexp-quote comment-start)))
             (setq erg (cons (match-beginning 0) (match-end 0)))
             (setq last (point))
             (skip-chars-backward " \t\r\n\f"))
           (while (and (setq erg (nth 8 (parse-partial-sexp (point-min) (point)))) (goto-char erg) (setq last (point)))
             (skip-chars-backward " 	
"))
           (when last (goto-char last))
           (when (looking-at (concat "[ 	]*" (regexp-quote comment-start)))
             (setq erg (cons (match-beginning 0) (match-end 0)))))
         erg)))

(put 'comment 'end-op-at
     (lambda ()
       (let* ((nesting (not (or (string= "" comment-end)(eq 10 comment-end))))
              (erg
               (when nesting
                 (when (looking-at (concat "[ \t]*" (regexp-quote comment-start)))
                   (progn
                     (goto-char (match-end 0))
                     (ignore-errors (end-of-form-base comment-start comment-end nil 'move 0 t)))))))
         (unless erg
           (when
               (looking-at (concat "[ \t]*" (regexp-quote comment-start)))
             (setq erg (cons (1- (line-end-position)) (line-end-position)))
             (while (and (not (eobp))(forward-line 1)(or (ar-empty-line-p)(looking-at (concat "[ \t]*" (regexp-quote comment-start)))))
               (setq erg (cons (1- (line-end-position)) (line-end-position))))))
         erg)))

(defun comment-forward-op-intern ()
  (let ((orig (point)))
    (end-of-line)
    (setq orig (point))
    (unless (eobp)
      (skip-chars-forward " \t\r\n\f")
      (if (looking-at comment-start)
          (comment-forward-op-intern)
        (goto-char orig)))))

(put 'comment 'forward-op-at
     (lambda ()
       (let ((orig (point)))
         (if (string= comment-end "")
             (comment-forward-op-intern)
           (search-forward comment-end nil t)
           (comment-forward-op-intern))
         (when (< orig (point)) (point)))))

(defun backward-op-at-intern ()
  (let ((this (point))
        pps)
    (skip-chars-backward " \t\r\n\f")
    (unless (bobp)
      (forward-char -1)
      (if (setq pps (and (nth 4 (parse-partial-sexp (point-min) (point)))
                         (nth 8 (parse-partial-sexp (point-min) (point)))))
          (progn
            (goto-char pps)
            (backward-op-at-intern))
        (goto-char this)))))

(put 'comment 'backward-op-at
     (lambda ()
       (let ((orig (point))
             (pps (when (nth 4 (parse-partial-sexp (point-min) (point)))
                    (nth 8 (parse-partial-sexp (point-min) (point))))))
         (if pps
             (progn
               (goto-char pps)
               (backward-op-at-intern))
           (when (search-backward comment-start nil 'move 1)
             (backward-op-at-intern)))
         (when (< (point) orig) (point)))))

;;; CSV
;; Inspired by
;;; csv-mode.el --- major mode for editing comma-separated value files
;; Author: Francis J. Wright <F.J.Wright at qmul.ac.uk>
;; URL: http://centaur.maths.qmul.ac.uk/Emacs/
(defcustom ar-csv-separator-atpt ";"
  "Char to distinguish datasets in a `comma`-separated row"
  :type 'string
  :group 'werkstatt)
;; (when (boundp 'csv-separators)
;; (setq ar-separator-atpt csv-separators))

(put 'csv 'beginning-op-at
     (lambda ()
       (skip-chars-backward (concat "^" ar-csv-separator-atpt))(point)))

(put 'csv 'end-op-at
     (lambda ()
       (skip-chars-forward (concat "^" ar-csv-separator-atpt))(point)))

;; DATE
(put 'date 'beginning-op-at
     (lambda ()
       ;; provide for the case, we are over a
       ;; string-delimiter as `"'
       (when
           (and (not (eq 32 (if (featurep 'xemacs)
                                (encode-char (char-after) 'ucs)
                              (char-after))))
                (or (bobp)
                    (eq 32 (if (featurep 'xemacs)
                               (encode-char (char-before) 'ucs)
                             (char-before)))))
         (forward-char 1)
         ;; as the bounds-function checks position, correct it
         ;; (setq th-orig 1)
         )
       (skip-chars-backward "0-9 .-")
       (skip-chars-forward " ")(point)))

(put 'date 'end-op-at
     (lambda ()
       (skip-chars-forward "0-9 .-")
       (skip-chars-backward " ")(point)))

;; Defun
(put 'defun 'beginning-op-at (lambda (&optional arg) (beginning-of-defun (or arg 1))(point)))

(put 'defun 'end-op-at (lambda (&optional arg)(end-of-defun (or arg 1))(point)))

(defvar delimited-start-pos nil
  "Internal use only.")

(defvar delimited-end-pos nil
  "Internal use only.")

;; Delimited
(defun delimited-atpt-intern--find-end (orig)
  (let ((this (point))
        (erg (end-of-form-base (char-to-string (char-after)) (char-to-string (ar--return-complement-char-maybe (char-after))) nil 'move 0 ar-match-in-comment-p t 'ar-syntax ar-match-in-string-p)))
    (if
        (ignore-errors
          (and erg (< orig (cadr erg))))
        (cons this (point))
      (goto-char this)
      nil)))

(defun delimited-atpt-intern--repeat (begdel orig)
  "Internal use only."
  (unless (bobp)
    (forward-char -1)
    (unless (looking-at (concat "[" begdel "]"))
      (skip-chars-backward (concat "^" begdel)))
    (delimited-atpt-intern begdel orig t)))

(defun delimited-atpt-intern (begdel orig &optional done)
  "Returns borders, a cons, if successful."
  (unless done (goto-char orig))
  (save-excursion
    (cond ((and (member (car (syntax-after (point))) (list 4 7 8 15)) (delimited-atpt-intern--find-end orig)))
	  ((and (looking-at (concat "[" begdel "]"))(delimited-atpt-intern--find-end orig)))
          (t (delimited-atpt-intern--repeat begdel orig)))))

(put 'delimited 'beginning-op-at
     (lambda ()
       (setq delimited-start-pos nil)
       (setq delimited-end-pos nil)
       (let* ((orig (point))
              (pps (parse-partial-sexp (point-min) (point))))
         (let* ((ar-match-in-string-p (when (nth 3 pps) (nth 8 pps)))
                (ar-match-in-comment-p (nth 4 pps))
                opener
	        (begdel (concat th-beg-delimiter ar-delimiters-atpt))
	        (dls (and (looking-at (concat "[" th-end-delimiter "]"))
                          (car-safe (beginning-of-form-base (char-to-string (ar--return-complement-char-maybe (char-after))) (char-to-string (char-after)) nil 'move 0 ar-match-in-comment-p t 'ar-syntax ar-match-in-string-p))))
                (delimited-list-start (or dls (nth 1 pps)))
	        (delimited-list-end
                 ;; starting from the closing delimiter
	         (cond ((and dls delimited-list-start)
                        (goto-char orig)
                        (when (looking-at (concat "[" th-end-delimiter "]"))
                          (setq delimited-start-pos delimited-list-start)
                          (setq delimited-end-pos (match-end 0))))
                       (delimited-list-start
                        (when (and (nth 8 pps) (< (nth 8 pps) delimited-list-start))
                          (progn (goto-char delimited-list-start)
			         (setq opener (char-after))
                                 (car-safe (cdr-safe (end-of-form-base (char-to-string (char-after)) (char-to-string (ar--return-complement-char-maybe (char-after))) nil 'move 0 ar-match-in-comment-p t 'ar-syntax ar-match-in-string-p))))))))
                erg)
           ;; {<$>}
           (and
	    (eq orig (point))
            opener
            (looking-at (char-to-string (ar--return-complement-char-maybe opener)) delimited-list-start)
            ;; started from closing paren
            (setq delimited-start-pos delimited-list-start)
            (setq delimited-end-pos delimited-list-end))
           (if (and delimited-start-pos delimited-end-pos)
               delimited-start-pos
	     (save-restriction
               (if
                   (setq erg (delimited-atpt-intern begdel orig))
                   (progn
                     (setq delimited-start-pos (car erg))
                     (set-mark (car erg))
                     (setq delimited-end-pos (cdr erg))
                     delimited-start-pos)
                 (goto-char orig))))))))


(put 'delimited 'end-op-at
     (lambda ()
       (if (and delimited-start-pos delimited-end-pos)
           delimited-end-pos
         (let ((begdel (concat th-beg-delimiter ar-delimiters-atpt)))
           (unless (looking-at (concat "[" begdel "]"))
             (funcall (get 'delimited 'beginning-op-at)))
           (if (looking-at (concat "[" begdel "]"))
               (end-of-form-base (char-to-string (char-after)) (char-to-string (ar--return-complement-char-maybe (char-after))) nil 'move 0 ar-match-in-comment-p t 'ar-syntax ar-match-in-string-p)
             (error "'delimited 'end-op-at: Can't see start of delimited form"))))))

(put 'delimited 'forward-op-at
     (lambda ()
       (let ((begdel (concat th-beg-delimiter ar-delimiters-atpt)))
         (unless (looking-at (concat "[" begdel "]"))
           (funcall (get 'delimited 'beginning-op-at)))
         (if (looking-at (concat "[" begdel "]"))
             (end-of-form-base (char-to-string (char-after)) (char-to-string (ar--return-complement-char-maybe (char-after))) nil 'move 0 ar-match-in-comment-p t 'ar-syntax ar-match-in-string-p 'forward)
           (error "'delimited 'forward-op-at: Can't see start of delimited form")))))

(defun ar-set-delimiter-zeichen ()
  (setq ar-delimiter-zeichen-atpt
        (if (featurep 'xemacs)
            (encode-char (char-after) 'ucs)
          (char-after))))

(defvar ar-delimiter-zeichen-atpt nil
  "Delimiter char found at place, search it backward then")
(make-variable-buffer-local 'ar-delimiter-zeichen-atpt)

(defvar ar-delimiter-string-atpt nil
  "Delimiter string found at place, search it backward then")
(make-variable-buffer-local 'ar-delimiter-string-atpt)

(defcustom ar-use-parse-partial-sexp t
  "When nil, parse symbolic expressions by regexp. "
  :type 'boolean
  :group 'werkstatt)

(defcustom ar-scan-whole-buffer-p nil
  "When non-nil, scan delimiters from point-min.

Otherwise assume being behind an opening delimiter or at a closing "
  :type 'boolean
  :group 'werkstatt)

(defvar ar-delimiters-atpt "|\"'`#\$/=?!:*+~§%&_\;@-"
  "Specify the delimiter chars. ")


(defvar  th-beg-delimiter "»‘“{<[("
  "Specify the delimiter char.")

(defvar th-end-delimiter "]}>”)’«"
  "Specify the delimiter char.")

;; Email
(put 'email 'beginning-op-at
     (lambda ()
       (when
           (looking-at "[^ \t]")
         (re-search-backward "[,;][[:graph:]]\\|<[[:graph:]]\\|^[[:graph:]]\\|[^[:graph:]][[:graph:]]" (line-beginning-position) t 1)
         (when (looking-at "[[:space:];,<]")
           (forward-char 1)))))

(put 'email 'end-op-at
     (lambda ()
       (when (looking-at "[ <]\\{0,1\\}\\([\041-\132\136-\176]+@[\041-\132\136-\176]+\\)[;,> \t\n]*")
         (goto-char (match-end 1))
         (skip-chars-backward "[:punct:]"))(point)))

;; Filename
(if (featurep 'xemacs)
    (defcustom thingatpt-file-name-chars "@~//A-Za-z0-9ÄÖÜäöüß_.$?={}#%,:-"
      "Characters forseen in filenames. "
      :type 'string
      :group 'werkstatt)

  (defcustom thingatpt-file-name-chars "@~//[:alnum:]_.$?={}#%,:-"
    "Characters forseen in filenames. "
    :type 'string
    :group 'werkstatt))

(put 'filename 'beginning-op-at
     (lambda ()
       (unless
           (member (char-before) (list 32 ?\t 10 ?\r))
         (skip-chars-backward thingatpt-file-name-chars))(point)))

(put 'filename 'end-op-at
     (lambda ()
       (and (< 0 (abs (skip-chars-forward (concat "=" thingatpt-file-name-chars))))
            (skip-chars-backward ": ")(point))))

;; Filename-nondirectory
(if (featurep 'xemacs)
    (defcustom thingatpt-filenamenondirectory-chars "-~A-Za-z0-9ÄÖÜäöüß_.$?={}#%,: "
      "Characters forseen in filenames. "
      :type 'string
      :group 'werkstatt)

  (defcustom thingatpt-filenamenondirectory-chars "-~[:alnum:]_.$?={}#%,"
    "Characters forseen in filenames. "
    :type 'string
    :group 'werkstatt))

(put 'filenamenondirectory 'beginning-op-at
     (lambda ()
       (unless
           (member (char-before) (list 32 ?\t 10 ?\r))
         (skip-chars-backward thingatpt-filenamenondirectory-chars))(point)))

(put 'filenamenondirectory 'end-op-at
     (lambda ()
       (and (< 0 (abs (skip-chars-forward (concat "-=" thingatpt-filenamenondirectory-chars))))
            (skip-chars-backward ": ")(point))))

;; Floats
(put 'float 'beginning-op-at
     (lambda ()
       (when (numberp (read (buffer-substring-no-properties (point) (1+ (point)))))
         (skip-chars-backward "0-9.,"))(point)))

(put 'float 'end-op-at (lambda () (skip-chars-forward "[0-9.,]")(point)))

;; Function
(put 'function 'beginning-op-at
     (lambda ()
       (cond
        ((eq (point) (defun-beginning-position))
         (point))
        (t (beginning-of-defun)
           (point)))))

(put 'function 'end-op-at
     (lambda ()
       (end-of-defun)
       (when (string= major-mode "emacs-lisp-mode")
         (skip-chars-backward " \t\r\n"))(point)))

;; IP
(put 'ip 'beginning-op-at
     (lambda ()
       (unless (looking-at "\\s-")
         (skip-chars-backward "0-9."))(point)))

(put 'ip 'end-op-at
     (lambda ()
       (when (looking-at "[0-9]\\{1,3\\}.[0-9-]\\{1,3\\}.[0-9]\\{1,3\\}.[0-9]\\{1,3\\}")
         (goto-char (match-end 0)))(point)))

;; ISBN
(put 'isbn 'beginning-op-at
     (lambda ()
       (unless (looking-at "\\s-")
         (skip-chars-backward "0-9-")(point))))

(put 'isbn 'end-op-at
     (lambda ()
       (when (looking-at "[0-9]\\{1,3\\}[0-9-]\\{7,12\\}[0-9X]\\{0,1\\}")
         (goto-char (match-end 0)))))

;; Lines
(put 'line 'beginning-op-at (lambda () (beginning-of-line)(point)))

(put 'line 'end-op-at (lambda () (end-of-line)(point)))

;; List
(put 'list 'beginning-op-at
     (lambda ()
       (cond ((eq 4 (car (syntax-after (point))))
              (cons (point) (1+ (point))))
             (t (let ((pps (parse-partial-sexp (point-min) (point))))
                  (when (nth 1 pps)
                    (goto-char (nth 1 pps))
                    (cons (point) (1+ (point)))))))))

(put 'list 'end-op-at
     (lambda ()
       (when (eq 4 (car (syntax-after (point))))
         (forward-sexp)
         (forward-char -1)
         (cons (point)(1+ (point))))))

;; Markup
(defcustom markup-startstring-atpt "<[^<>]+>"
  "Defining the beginning of a markup using ar-markup-atpt functions. "
  :type 'string
  :group 'werkstatt)

(defcustom markup-endstring-atpt "</[^<>]+>"
  "Defining the end of a markup using ar-markup-atpt functions. "
  :type 'string
  :group 'werkstatt)

(put 'markup 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at markup-startstring-atpt))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base markup-startstring-atpt markup-endstring-atpt nil 'move nil ar-match-in-comment-p t nil ar-match-in-string-p))))

(put 'markup 'end-op-at
     (lambda ()
       (let ((this-end (when (looking-at markup-startstring-atpt)
                         (match-string-no-properties 0))))
         (when (stringp this-end)
           (setq this-end (replace-regexp-in-string "<" "</" this-end))
           (end-of-form-base markup-startstring-atpt this-end nil 'move nil nil t)))))

;; Markup-no-nest
;; (put 'markup-no-nest 'beginning-op-at
;;      (lambda ()
;;        (if (ignore-errors (looking-at markup-startstring-atpt))
;;            (point)
;;          (unless (bobp) (forward-char -1))
;;          (while (and (not (bobp) (not (ignore-errors (looking-at markup-startstring-atpt)))))
;;            (forward-char -1))
;;          (when (ignore-errors (looking-at markup-startstring-atpt))
;;            (point)))))
;;
;; (put 'markup-no-nest 'end-op-at
;;      (lambda ()
;;        (when (ignore-errors (looking-at markup-startstring-atpt))
;;          (re-search-forward markup-endstring-atpt nil 'move 1)
;;          (when (ignore-errors (looking-at markup-startstring-atpt))
;;            (point)))))

;; Ml-data
(put 'mldata 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at markup-startstring-atpt))
           (match-end 0)
         (beginning-of-form-base markup-startstring-atpt markup-endstring-atpt nil 'move nil ar-match-in-comment-p t)
         (when (ignore-errors (looking-at markup-startstring-atpt))
           (match-end 0)))))

(put 'mldata 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at markup-startstring-atpt))
         (end-of-form-base markup-startstring-atpt markup-endstring-atpt nil 'move nil nil t)
         (re-search-backward markup-endstring-atpt nil 'move 1))))

;; Ml-tag
(put 'mltag 'beginning-op-at
     (lambda ()
       (if (ignore-errors
             (or
              (looking-at markup-startstring-atpt)
              (looking-at markup-endstring-atpt)))
           (list (point) (1+ (point)))
         (unless (bobp) (forward-char -1))
         (while
             (and (not (bobp))
                  (not
                   (ignore-errors
                     (or
                      (looking-at markup-startstring-atpt)
                      (looking-at markup-endstring-atpt)))))
           (forward-char -1))
         (when
             (ignore-errors
               (or
                (looking-at markup-startstring-atpt)
                (looking-at markup-endstring-atpt)))
           (list (point) (1+ (point)))))))

(put 'mltag 'end-op-at
     (lambda ()
       (when (ignore-errors (or
                             (looking-at markup-startstring-atpt)
                             (looking-at markup-endstring-atpt)))
         (list (1- (match-end 0))(match-end 0)))))

;; Number
(put 'number 'beginning-op-at
     (lambda ()
       (let ((case-fold-search t))
         (when (looking-at "[#xo0-9a-f]")
           (cond ((eq (char-after) ?#)
                  (if (looking-at "#x[0-9a-f]+")
                      (point)
                    (when (looking-at "#o[0-9]+")
                      (point))))
                 ((eq (char-after) ?x)
                  (and (eq (char-before) ?#)
                       (progn
                         (forward-char -1)
                         (looking-at "#x[0-9a-f]+"))
                       (point)))
                 ((eq (char-after) ?o)
                  (and (eq (char-before) ?#)
                       (progn
                         (forward-char -1)
                         (looking-at "#o[0-9]+"))
                       (point)))
                 ((looking-back "#x[0-9a-f]*" (line-beginning-position))
                  (skip-chars-backward "^#" (line-beginning-position))
                  (forward-char -1)
                  (point))
                 ((looking-back "#o[0-9]*" (line-beginning-position))
                  (skip-chars-backward "^#" (line-beginning-position))
                  (forward-char -1)
                  (point))
                 ((looking-back "[0-9]+" (line-beginning-position))
                  (skip-chars-backward "0-9" (line-beginning-position))
                  (point))
                 ((looking-at "[0-9]+")
                  (point)))))))

(put 'number 'end-op-at
     (lambda ()
       (let ((case-fold-search t))
         (when (looking-at "[#x0-9a-f]")
           (cond ((looking-at "#x[0-9a-f]+")
                  (forward-char 2)
                  (skip-chars-forward "0-9a-f" (line-end-position))
                  (point))
                 ((looking-at "#o[0-9]+")
                  (forward-char 2)
                  (skip-chars-forward "0-9" (line-end-position))
                  (point))
                 ((looking-at "[0-9]+")
                  (skip-chars-forward "0-9" (line-end-position))
                  (point)))))))

(put 'number 'forward-op-at
     (lambda ()
       (unless (eobp)
         (let ((case-fold-search t)
               (erg
                (cond ((looking-at "#[0-9a-fA-F]+|x[0-9]+")
                       (forward-char 2)
                       (skip-chars-forward "0-9a-f" (line-end-position))
                       (and (< 0 (skip-chars-forward "^0-9"))(point)))
                      ((looking-at "#o[0-9]+")
                       (forward-char 2)
                       (skip-chars-forward "0-9" (line-end-position))
                       (and (< 0 (skip-chars-forward "^0-9"))(point)))
                      ((looking-at "[0-9]+")
                       (skip-chars-forward "0-9" (line-end-position))
                       (and (< 0 (skip-chars-forward "^0-9"))(point)))
                      (t
                       (re-search-forward "#x[a-fA-F0-9]+\\|#o[0-8]+\\|[0-9e]+" nil t 1)
                       (when (ignore-errors (match-beginning 0))
                         (goto-char (match-beginning 0)))))))
           (cond ((looking-at "#[xX][a-fA-F0-9]+")
                  (setq erg (point)))
                 ((looking-at "#o[0-9]+")
                  (setq erg (point)))
                 ((looking-at "[0-9]+")
                  (setq erg (point)))
                 ((eobp)
                  (setq erg nil)))
           erg))))

(put 'number 'backward-op-at
     (lambda ()
       (unless (bobp)
         (let ((case-fold-search t)
               erg)
           (cond ((and (looking-back "#?x?[0-9a-f]+" (line-beginning-position))
                       (goto-char (match-beginning 0))
                       (ar-number-atpt)))
                 (t
                  (while
                      (or
                       (and
                        (re-search-backward "#?[xo]?[a-f0-9]+" nil t 1)
                        (goto-char (match-beginning 0))
                        (not (setq erg (ar-number-atpt))))))))
           erg))))

;; Name
(defcustom ar-name-chars-atpt "a-zA-Z_;-"
  "Name is just a identifier for general use, described by chars composing it. "
  :type 'regexp
  :group 'werkstatt)

(put 'name 'beginning-op-at
     (lambda ()
       (skip-chars-backward ar-name-chars-atpt)
       (point)))

(put 'name 'end-op-at
     (lambda ()
       (when (looking-at (concat "[" ar-name-chars-atpt "]"))
         (skip-chars-forward ar-name-chars-atpt)
         ;; name may contain char `:' but not at the end, as
         ;; messages tend to insert it there
         (skip-chars-forward ar-name-chars-atpt)
         (skip-chars-backward ":")
         (point))))

;; Page
(put 'page 'beginning-op-at
     (lambda ()
       (backward-page)(point)))

(put 'page 'end-op-at
     (lambda ()
       (forward-page)(point)))

;; Paragraph
(defvar ar-this-paragraph-orig nil)

(defun ar-beginning-of-paragraph-intern ()
  (backward-paragraph)
  (skip-chars-forward " \t\r\n\f")
  (point))

(defun ar-end-of-paragraph-intern ()
  (forward-paragraph)
  (skip-chars-backward " \t\r\n\f")
  (point))

(put 'paragraph 'beginning-op-at
     (lambda ()
       (setq ar-this-paragraph-orig (point))
       (back-to-indentation)
       (when (and (eq (point) ar-this-paragraph-orig))
         (skip-chars-backward " \t\r\n\f"))
       (ar-beginning-of-paragraph-intern)))

(put 'paragraph 'end-op-at
     (lambda ()
       (ar-end-of-paragraph-intern)
       (if (eq (point) ar-this-paragraph-orig)
           (progn
             (skip-chars-forward " \t\r\n\f")
             (ar-end-of-paragraph-intern))
         (point))))

;; Paren
(put 'paren 'beginning-op-at
     (lambda ()
       (cond
        ((looking-at "\\s)")
         (forward-char 1) (backward-list 1))
        (t (while
               (and (< 0 (abs (skip-chars-backward "^(")))
                    (nth 8 (parse-partial-sexp (point-min) (point)))))
           (when (eq (char-before) ?\()
             (forward-char -1)
             (cons (point) (1+ (point))))))))

(put 'paren 'end-op-at
     (lambda ()
       (forward-list 1)
       (when (eq (char-before) ?\))
         (cons (1- (point)) (point)))))

;; Phone
(put 'phone 'beginning-op-at
     (lambda ()
       (when
           (and (looking-at "[0-9 \t.()-]")
                (not (eq (char-before) ?+)))
         (re-search-backward "[^0-9 \t.()-][0-9 ()\t-]+" (line-beginning-position) nil 1) (forward-char 1)(point))))

(put 'phone 'end-op-at
     (lambda ()
       (when
           (looking-at "[0-9;, \t()-]")
         (re-search-forward "[0-9 \t.()-]+[^0-9 \t-]" (1+ (line-end-position)) nil 1) (forward-char -1))(point)))

;; Region
(defvar ar-region-end-atpt nil)
(put 'region 'beginning-op-at
     (lambda ()
       (setq ar-region-end-atpt (region-end))
       (goto-char (region-beginning))))

(put 'region 'end-op-at
     (lambda ()
       (goto-char ar-region-end-atpt)))

;; Sentence
(defvar ar-sentence-end-chars "[.!?]")

(defcustom ar-sentence-end-op-re "[.!?] *$\\|[[:alpha:]][^ \t\r\n\f0-9][.!?] *[^a-z]"
  ""
  :type 'regexp
  :group 'convenience)

(put 'sentence 'beginning-op-at
     (lambda ()
       (if (save-excursion
             (and (looking-at "[A-Z]")
                  (progn
                    (skip-chars-backward " \t\r\n\f")
                    (or (bobp) (member (char-before) (list 63 ?! ?.))))))
           (point)
         (let ((limit (save-excursion (backward-paragraph)(point))))
           (while
               (and (not (bobp))
                    (or
                     (prog1
                         (re-search-backward "[.!?] *$\\|[.!?] *[^a-z]" limit t 1)
                       (forward-char 1)
                       (skip-chars-forward " \t\r\n\f"))
                     (prog1
                         (backward-paragraph)
                       (skip-chars-forward " \t\r\n\f")))
                    (nth 8 (parse-partial-sexp (point-min) (point))))))
         (point))))

(put 'sentence 'end-op-at
     (lambda ()
       (let ((orig (point)))
         (re-search-forward ar-sentence-end-op-re nil t 1)
         (skip-chars-backward "A-Z")
         (skip-chars-backward " \t\r\n\f")
         (when (< orig (point)) (point)))))

(put 'sentence 'forward-op-at
     (lambda ()
       (unless (eobp) (forward-char 1))
       (let ((orig (point)))
         (re-search-forward ar-sentence-end-op-re nil t 1)
         (skip-chars-backward "(A-Z")
         (skip-chars-backward " \t\r\n\f")
         (when (< orig (point)) (point)))))

(put 'sentence 'backward-op-at
     (lambda ()
       (backward-sentence)))

;; Sexp
(put 'sexp 'beginning-op-at
     (lambda ()
       (ar-backward-sexp)))

(put 'sexp 'end-op-at
     (lambda ()
       (ar-forward-sexp)))

(put 'sexp 'forward-op-at
     (lambda ()
       (ar-forward-sexp)))

(put 'sexp 'backward-op-at
     (lambda ()
       (ar-backward-sexp)))

;; Symbol
(put 'symbol 'beginning-op-at
     (unless (looking-at "\\s-")
       (lambda ()
         (let (erg)
           (while
               (or (when
                       (ar-escaped (if (bobp) (point)(1- (point))))
                     (forward-line -1)
                     (setq erg (point)))
                   (and (< 0 (abs (skip-syntax-backward "w_.'\\")))(setq erg (point)))))
           (unless erg (when (looking-at "[^ ]")(setq erg (point))))
           erg))))

(put 'symbol 'end-op-at
     (lambda ()
       (let (erg)
         (while
             (or (when
                     (ar-escaped)
                   (forward-char 1)
                   (setq erg (point)))
                 (and (< 0 (skip-syntax-forward "w_.'\\"))(setq erg (point)))))
         erg)))


(put 'symbol 'forward-op-at
     (lambda ()
       (skip-syntax-forward "^_")
       (and (< 0 (abs (skip-syntax-forward "_")))
            (point))))

(put 'symbol 'backward-op-at
     (lambda ()
       (skip-syntax-backward "^_")
       (and (< 0 (abs (skip-syntax-backward "_")))
            (point))))

;; Triplebackticked
(put 'triplebackticked 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "```" "```" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'triplebackticked 'end-op-at
     (lambda ()
       (end-of-form-base "```" "```" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'triplebackticked 'forward-op-at
     (lambda ()
       (end-of-form-base "```" "```" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t)))

(put 'triplebackticked 'backward-op-at
     (lambda ()
       (beginning-of-form-base "```" "```" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

;; Triplequoted
(put 'triplequoted 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "\"\"\"\\|'''" "\"\"\"\\|'''" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'triplequoted 'end-op-at
     (lambda ()
       (end-of-form-base "\"\"\"\\|'''" "\"\"\"\\|'''" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'triplequoted 'forward-op-at
     (lambda ()
       (end-of-form-base "\"\"\"\\|'''" "\"\"\"\\|'''" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t)))

(put 'triplequoted 'backward-op-at
     (lambda ()
       (beginning-of-form-base "\"\"\"\\|'''" "\"\"\"\\|'''" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

;; Triplequoted-Dq
(put 'triplequoteddq 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "\"\"\"" "\"\"\"" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'triplequoteddq 'end-op-at
     (lambda ()
       (end-of-form-base "\"\"\"" "\"\"\"" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'triplequoteddq 'forward-op-at
     (lambda ()
       (end-of-form-base "\"\"\"" "\"\"\"" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t)))

(put 'triplequoteddq 'backward-op-at
     (lambda ()
       (beginning-of-form-base "\"\"\"" "\"\"\"" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

;; Triplequoted-Sq
(put 'triplequotedsq 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "'''" "'''" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'triplequotedsq 'end-op-at
     (lambda ()
       (end-of-form-base "'''" "'''" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

(put 'triplequotedsq 'forward-op-at
     (lambda ()
       (end-of-form-base "'''" "'''" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p t)))

(put 'triplequotedsq 'backward-op-at
     (lambda ()
       (beginning-of-form-base "'''" "'''" nil 'move 0 ar-match-in-comment-p nil 'ar-syntax ar-match-in-string-p)))

;; Url
;; use thingatpt.el's form here too
(put 'url 'end-op-at (get 'url 'end-op))
(put 'url 'beginning-op-at (get 'url 'beginning-op))

(defcustom url-at-point-chars ":/?#[]@!$&()*+,;=[:alnum:]-._~"
  "Chars which might compose a URL. "
  :type 'string
  :group 'werkstatt)

;; Whitespace
(put 'whitespace 'beginning-op-at
     (lambda () (when (looking-at "[ \t]") (skip-chars-backward "[ \t\r\n[:blank:]]")(point))))

(put 'whitespace 'end-op-at (lambda () (skip-chars-forward " \t\r\n[:blank:]")(point)))

;; Word
(put 'word 'beginning-op-at
     (lambda () (when (looking-at "\\w")
                  (unless (or (looking-back "\\W" (line-beginning-position))(bolp))
                    (forward-word -1))
                  (point))))

(put 'word 'end-op-at
     (lambda () (when (looking-at "\\w")
		  (forward-word 1)(point))))

;; Word-alpha-only
(put 'wordalphaonly 'beginning-op-at
     (lambda () (when (looking-at "[[:alpha:]]")
                  (unless (looking-back "[^[:alpha:]]" (line-beginning-position))
                    (skip-chars-backward "[:alpha:]")
                    (point)))))

(put 'wordalphaonly 'end-op-at
     (lambda () (when (and (looking-back "[^[:alpha:]]" (line-beginning-position))(looking-at "[[:alpha:]]" (line-beginning-position)))
                  (skip-chars-forward "[:alpha:]")
                  (point))))

(defun gen--in-string-p-intern (pps)
  (goto-char (nth 8 pps))
  (list (point) (char-after)(skip-chars-forward (char-to-string (char-after)))))

(defun gen-in-string-p ()
  "if inside a double- triple- or singlequoted string,

If non-nil, return a list composed of
- beginning position
- the character used as string-delimiter (in decimal)
- and length of delimiter, commonly 1 or 3 "
  (interactive "p")
  (save-excursion
    (let* ((pps (parse-partial-sexp (point-min) (point)))
           (erg (when (nth 3 pps)
                  (gen--in-string-p-intern pps))))
      (unless erg
        (when (looking-at "\"\\|'")
          (forward-char 1)
          (setq pps (parse-partial-sexp (line-beginning-position) (point)))
          (when (nth 3 pps)
            (setq erg (gen--in-string-p-intern pps)))))

      ;; (list (nth 8 pps) (char-before) (1+ (skip-chars-forward (char-to-string (char-before)))))
      erg)))

;;
(defcustom copy-or-alternative "word"
  "Copy-or commands may act on thing specified here.

For example when ‘ar-doublequote-or-copy-atpt’ is called with positive
argument but without active region and also thing-at-point
 --i.e. doublequoted here-- doesn't exist,
it would doublequote a word at point "
  :type 'string
  :group 'werkstatt)

(defcustom ar-install-directory "~/werkstatt"
  "Directory where thingatpt-utils are installed"
  :type 'string
  :group 'werkstatt)

;; (update-directory-autoloads (expand-file-name ar-install-directory))

(defun ar--transform-generic-delimited-atpt (replacement)
  (interactive "*")
  (let* ((bounds (ar-bounds-of-delimited-atpt))
         (startcharnew (if (consp replacement)
                           (car replacement)
                         replacement))
         (endcharnew (if (consp replacement)
                         (cdr replacement)
                       replacement))
         (beg (or (and (numberp (car bounds))(car bounds))
                  (and (numberp (caar bounds))(caar bounds))))
         (end (or
               (ignore-errors (and (numberp (cdr bounds)) (cdr bounds)))
               (ignore-errors (and (numberp (cadr (cadr bounds)))(cadr (cadr bounds))))
               (ignore-errors (and (numberp (cadr bounds))(cadr bounds)))
               (ignore-errors (and (numberp (cdr (cadr bounds)))(cdr (cadr bounds))))
               )))
    (if (ignore-errors (numberp beg))
        (save-excursion
          (goto-char beg)
          (delete-char 1)
          (insert startcharnew)
          (if (numberp end)
              (progn
                (goto-char end)
                (delete-char -1)
                (insert endcharnew))
            (error "ar--transform-generic-delimited-atpt: Don't see end")))
      (error "ar--transform-generic-delimited-atpt: Don't see start"))))


;; ML data-forms start

;; Beginendquoted
(put 'beginendquoted 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\\begin{quote}"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\\begin{quote}" "\\end{quote}" nil 'move 1 nil nil nil))))

(put 'beginendquoted 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\\begin{quote}"))
         (goto-char (match-end 0))
         (end-of-form-base "\\begin{quote}" "\\end{quote}" nil 'move 1 nil nil nil))))


;; Blok
(put 'blok 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "{% "))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "{% " " %}" nil 'move 1 nil t nil))))

(put 'blok 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "{% "))
         (goto-char (match-end 0))
         (end-of-form-base "{% " " %}" nil 'move 1 nil t nil))))


;; Doublebackslashed
(put 'doublebackslashed 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\\\\"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\\\\" "\\\\" nil 'move 1 nil nil 'ar-escaped))))

(put 'doublebackslashed 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\\\\"))
         (goto-char (match-end 0))
         (end-of-form-base "\\\\" "\\\\" nil 'move 1 nil nil 'ar-escaped))))


;; Doublebackticked
(put 'doublebackticked 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "``"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "``" "``" nil 'move 1 nil nil 'ar-escaped))))

(put 'doublebackticked 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "``"))
         (goto-char (match-end 0))
         (end-of-form-base "``" "``" nil 'move 1 nil nil 'ar-escaped))))


;; Doubleslashed
(put 'doubleslashed 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "//"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "//" "//" nil 'move 1 nil nil 'ar-escaped))))

(put 'doubleslashed 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "//"))
         (goto-char (match-end 0))
         (end-of-form-base "//" "//" nil 'move 1 nil nil 'ar-escaped))))


;; Doublebackslashedparen
(put 'doublebackslashedparen 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\\\\\\\\("))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\\\\\\\\(" "\\\\\\\\)" nil 'move 1 nil nil 'ar-escaped))))

(put 'doublebackslashedparen 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\\\\\\\\("))
         (goto-char (match-end 0))
         (end-of-form-base "\\\\\\\\(" "\\\\\\\\)" nil 'move 1 nil nil 'ar-escaped))))


;; Tabledatap
(put 'tabledatap 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "<td[^>]*>"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "<td[^>]*>" "</td>" nil 'move 1 nil nil nil))))

(put 'tabledatap 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "<td[^>]*>"))
         (goto-char (match-end 0))
         (end-of-form-base "<td[^>]*>" "</td>" nil 'move 1 nil nil nil))))


;; Backslashedparen
(put 'backslashedparen 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\\\\("))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\\\\(" "\\\\)" nil 'move 1 nil nil 'ar-escaped))))

(put 'backslashedparen 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\\\\("))
         (goto-char (match-end 0))
         (end-of-form-base "\\\\(" "\\\\)" nil 'move 1 nil nil 'ar-escaped))))


;; Slashedparen
(put 'slashedparen 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "////////("))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "////////(" "////////)" nil 'move 1 nil nil 'ar-escaped))))

(put 'slashedparen 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "////////("))
         (goto-char (match-end 0))
         (end-of-form-base "////////(" "////////)" nil 'move 1 nil nil 'ar-escaped))))


;; Triplequoteddq
(put 'triplequoteddq 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\"\"\"\\|'''"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\"\"\"\\|'''" "\"\"\"\\|'''" nil 'move 1 nil nil 'ar-escaped))))

(put 'triplequoteddq 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\"\"\"\\|'''"))
         (goto-char (match-end 0))
         (end-of-form-base "\"\"\"\\|'''" "\"\"\"\\|'''" nil 'move 1 nil nil 'ar-escaped))))


;; Triplequotedsq
(put 'triplequotedsq 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\"\"\"\\|'''"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\"\"\"\\|'''" "\"\"\"\\|'''" nil 'move 1 nil nil 'ar-escaped))))

(put 'triplequotedsq 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\"\"\"\\|'''"))
         (goto-char (match-end 0))
         (end-of-form-base "\"\"\"\\|'''" "\"\"\"\\|'''" nil 'move 1 nil nil 'ar-escaped))))


;; Xslstylesheetp
(put 'xslstylesheetp 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "<xsl:stylesheet[^<]+>.*$"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "<xsl:stylesheet[^<]+>.*$" "</xsl:stylesheet>" nil 'move 1 nil nil nil))))

(put 'xslstylesheetp 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "<xsl:stylesheet[^<]+>.*$"))
         (goto-char (match-end 0))
         (end-of-form-base "<xsl:stylesheet[^<]+>.*$" "</xsl:stylesheet>" nil 'move 1 nil nil nil))))


;; ML data-forms end

;; ar-insert-thingatpt-th-funktionen start

(defun ar-toggle-thing-copy-region ()
  (interactive)
  (setq thing-copy-region (not thing-copy-region)))

(defun ar-th (thing &optional no-delimiters)
  "Returns a buffer substring according to THING.
  THING may be a well known form as ‘symbol’,
  ‘list’, ‘sexp’, ‘defun’ or a newly defined THING.
  When mark-thingatpt is ‘t’ - the default - a found THING
  is set as current region, enabling further action on them

  If NO-DELIMITERS set by user functions, THING returned is
  stripped by delimiters resp. markup
 "
  (condition-case nil
      (let* ((no-delimiters (or no-delimiters (eq 4 (prefix-numeric-value no-delimiters))))
	     (bounds (ar-th-bounds thing no-delimiters))
	     (beg (if no-delimiters
		      (cond ((ignore-errors (numberp (car-safe bounds)))
			     (car-safe bounds))
			    ((ignore-errors (caar bounds))
			     (caar bounds))
			    (t (car-safe bounds)))
		    (cond ((ignore-errors (caar bounds))
			   (caar bounds))
			  (t (car-safe bounds)))))
	     (end (if no-delimiters (car-safe (cdr-safe bounds)) (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))(cdr bounds))))
	     erg)
	(when (and beg end)
	  (setq erg
		(buffer-substring-no-properties beg end))
	  (when thing-copy-region
	    (ar-th-mark thing nil beg end no-delimiters))
	  (when thing-copy-region (kill-new erg))
	  erg))
    (error nil)))

(defun ar--th-bounds-char-return (beg end &optional orig no-delimiters)
  (when (and beg end
	     (not (eq beg end))
	     (or (eobp)
	         (<= orig end)))
    (if no-delimiters
	(cons (1+ beg) (1- beg))
      (cons beg end))))

(defun ar--th-bounds-list-return (beg end &optional orig no-delimiters)
  ;; (message "%s" no-delimiters)
  (let (erg)
    (when
	(and beg end
	     (not (eq beg end))
	     (or (eobp)
		 (or (<= orig
                         (or (ignore-errors (cadr end))
			     (ignore-errors (cdr end)))))))
      (if no-delimiters
	  (progn
	    (push (car end) erg)
	    (push (cdr beg) erg))
	(push end erg)
	(push beg erg))
      erg)))


(defvar ar-th-bounds-backfix nil
  "starting delimiter pos might need correction from end")

(defun ar-th-bounds (thing &optional no-delimiters)
  "Determine the start and end buffer locations for the THING at point.
  THING is a symbol which specifies the kind entity you want.

  Boolean value NO-DELIMITERS: boundaries are excluded.
  Call THING by his name, i.e. ar-word-atpt etc.


"
  (setq ar-th-bounds-backfix nil)
  (ignore-errors
    (cond ((eq thing 'region)
	   (ignore-errors (cons (region-beginning) (region-end))))
	  ((eq thing 'char)
	   (cons (point) (1+ (point))))
	  (t (save-excursion
	       (save-restriction
		 (let* ((orig (point))
			(beg (funcall (get thing 'beginning-op-at)))
                        (beg_char (if (consp beg) (car beg) beg))
			(end (and beg (goto-char beg_char) (funcall (get thing 'end-op-at)))))
		   (when ar-th-bounds-backfix
		     (message "backfix: %s" ar-th-bounds-backfix)
		     (setq beg ar-th-bounds-backfix))
		   (if (numberp beg)
		       (ar--th-bounds-char-return beg end orig no-delimiters)
		     (ar--th-bounds-list-return beg end orig no-delimiters)))))))))

(defun ar-th-beg (thing &optional no-delimiters)
  "Return beginning position of THING. "
  (condition-case nil
      (let ((bounds (ar-th-bounds thing no-delimiters)))
	(ignore-errors (or (ignore-errors (caar bounds)) (car-safe bounds))))
    (error nil)))

(defun ar-th-end (thing &optional no-delimiters)
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters))
	     (end (or (ignore-errors (car (cdr (cadr bounds))))(ignore-errors (cdr (cadr bounds)))(ignore-errors (cadr bounds)))))
	end)
    (error nil)))

(defun ar-th-gotobeg (thing &optional no-delimiters)
  "Goto char beginning, core function "
  (goto-char (car-safe (car-safe (ar-th-bounds thing no-delimiters)))))

(defun ar-th-gotoend (thing &optional no-delimiters)
  "Goto char end, core function "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters))
	     (end (or (ignore-errors (car (cdr (cadr bounds))))(ignore-errors (cdr (cadr bounds)))(ignore-errors (cadr bounds)))))
	(goto-char end)
	(forward-char -1)
	(cons (point) (1+ (point))))
    (error (concat (format "%s: " thing) "ar-th-gotoend failed"))))

(defun ar-th-length (thing &optional no-delimiters)
  (ignore-errors
    (let* ((bounds (ar-th-bounds thing no-delimiters))
	   (beg (caar bounds))
	   (end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))))
	   (length (- end beg)))
      length)))

(defun ar-th-ratio-base (cla elt &optional beg end ratio)
  (let ((beg
         (cond (beg beg)
               ((use-region-p)
                (region-beginning))
               (t
                (funcall (intern-soft (concat "ar-" (format "%s" elt) "-beginning-position-atpt"))))))
	(end
         (cond (end (copy-marker end))
               ((use-region-p)
                (copy-marker (region-end)))
               (t
                (condition-case nil (copy-marker (funcall (intern-soft (concat "ar-" (format "%s" elt) "-end-position-atpt")))) (error nil))))))
    (ar-th-ratio elt cla beg end ratio)))

(defun ar-th-ratio (thing cla &optional beg end ratio no-delimiters)
  (save-excursion
    (ignore-errors
      (let* (bounds
             (beg (or beg (and (setq bounds (ar-th-bounds thing no-delimiters)) (caar bounds))))
             (end (or end (cadr (cadr bounds))))
             (matchcount 0)
             (erg 0)
             len)
        (goto-char beg)
        (setq erg
              (cond ((member cla ar-atpt-classes)
                     (if (featurep 'xemacs)
                         (string-to-number (string-strip (count-matches (eval cla)) nil "a-z "))
                       (count-matches (concat "[[:" (format "%s" cla) ":]]") (or beg (point-min)) (or end (point-max)))))
                    (t (if (functionp (intern-soft (concat "ar-forward-" (format "%s" cla) "-atpt")))
                           (progn
                             (while (and (< (point) end)
                                         (funcall (intern-soft (concat "ar-forward-" (format "%s" cla) "-atpt"))))
                               (setq matchcount (1+ matchcount)))
                             matchcount)
                         (while (and (< (point) end)
                                     (search-forward cla end t 1))
                           (setq matchcount (1+ matchcount)))
                         matchcount))))
        (when ratio
          (progn
            (setq len (string-to-number (format "%f" (- end beg))))
            (setq erg (/ matchcount len))
            erg))
        erg))))

(defun ar-th-copy (thing &optional no-delimiters)
  (condition-case nil
      (let ((newcopy (ar-th thing no-delimiters)))
	(when newcopy
          (progn
            (unless (string= newcopy (car kill-ring)) (kill-new newcopy))
            newcopy)))
    (error nil)))

(defun ar-th-trim (thing &optional no-delimiters left right)
  "Trims given THING at point.
If boundaries of thing are know, use ‘ar-th-trim-base’ directly. "
  (let* ((bounds (ar-th-bounds thing no-delimiters))
         (beg (or (ignore-errors (caar bounds)) (car-safe bounds)))
         (end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))(ignore-errors (cdr bounds)))))
    (ar-th-trim-base beg end left right)))

(defun ar-th-trim-base (beg end left right)
  "Trim buffer-substring-point.

Arg LEFT-TRIM: trim left
Arg RIGHT-TRIM: trim right. "
  (save-excursion
    (let ((beg (copy-marker beg))
	  (end (copy-marker end))
	  (old-end end))
      (cond ((and left right)
	     (goto-char end)
	     (delete-char -1)
	     (goto-char beg)
	     (delete-char 1)
	     (eq (marker-position end) (- old-end 2)))
	    (right
	     (goto-char end)
	     (delete-char -1)
	     (eq (marker-position end) (- old-end 1)))
	    (left
	     (goto-char beg)
	     (delete-char 1)
	     (eq (marker-position end) (- old-end 1)))
	    (t (goto-char end)
	       (delete-char -1)
	       (goto-char beg)
	       (delete-char 1)
	       (eq (marker-position end) (- old-end 2)))))))

(defun ar-th-trim-left (thing &optional no-delimiters)
  (ar-th-trim thing no-delimiters t))

(defun ar-th-trim-right (thing &optional no-delimiters)
  (ar-th-trim thing no-delimiters nil t))

(defun ar-th-peel (thing &optional no-delimiters)
  "Remove the outer element of an hierarchical form.

\(foo (bar baz)) --> (bar baz)
--^-----------

\[foo [bar baz]] --> [bar baz]
--^-----------

Inspired by stuff like ‘paredit-splice-sexp-killing-backward’;
instead of working ‘-backward’ or ‘-forward’ deletes expression at point.

"
  (let* ((outer (ar-th-bounds thing no-delimiters))
	 (outer-start (caar outer))
	 (outer-end (copy-marker (or (ignore-errors (cadr (cadr outer)))(cdr (cadr outer))(car (cadr outer)))))
	 )
    (when (eq (point) outer-start)(forward-char 1))
    (skip-syntax-forward "^(")
    (forward-sexp)
    (delete-region (point) outer-end)
    (backward-sexp)
    (delete-region (point) outer-start)))

(defun ar-th-comment (thing &optional no-delimiters)
  "Comment or uncomment THING "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters))
	     (beg (caar bounds))
	     (end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds))))))
	(when (and beg end)
	  (goto-char beg)
	  (comment-region beg end)))
    (error nil)))

(defun ar-th-mark (thing &optional bounds beg end no-delimiters)
  " "
  (condition-case nil
      (let* ((bounds (unless (and beg end) (or bounds (ar-th-bounds thing no-delimiters))))
	     (beg (or beg (ignore-errors (caar bounds))))
	     (end (or end (or (ignore-errors (cadr (cadr bounds))) (ignore-errors (cdr (cadr bounds)))))))
	(when (and beg end)
	  (goto-char beg)
	  (push-mark (point) t t)
	  (goto-char end)
	  (exchange-point-and-mark)))
    (error nil)))

;; uses sgml-tag from sgml-mode.el
(defun ar-th-hide (thing &optional beg end no-delimiters)
  "Hide visibility of existing things at point. "
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t) bounds)
    (unless (and beg end)
      (setq bounds (ar-th-bounds thing no-delimiters))
      (setq beg (or (ignore-errors (caar bounds))(car-safe bounds)))
      (setq end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))(ignore-errors (cdr bounds)))))
    (if (and beg end)
        (progn
          (hs-make-overlay beg end 'code)
          (set-buffer-modified-p modified))
      (error (concat "No " (format "%s" thing) " at point!")))))

;;;###autoload
(defun ar-th-show (thing &optional beg end no-delimiters)
  "Remove invisibility of existing things at point. "
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t) bounds)
    (unless (and beg end)
      (setq bounds (ar-th-bounds thing no-delimiters))
      (setq beg (or (ignore-errors (caar bounds))(point-min)))
      (setq end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))(ignore-errors (cdr end))(point-max))))
    (if (and beg end)
        (progn
          (hs-discard-overlays beg end)
          (set-buffer-modified-p modified))
      (error (concat "No " (format "%s" thing) " at point!")))))

(defun ar-th-hide-show (&optional thing beg end no-delimiters)
  "Toggle visibility of existing things at point. "
  (interactive "p")
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t)
        (beg (or beg (and (use-region-p) (region-beginning))))
        (end (or end (and (use-region-p) (region-end))))
        bounds)
    (unless (and beg end)
      (setq bounds (ar-th-bounds thing no-delimiters))
      (setq beg (caar bounds))
      (setq end (cadr (cadr bounds))))
    (if (overlays-in beg end)
        (hs-discard-overlays beg end)
      (hs-make-overlay beg end 'code))
    (set-buffer-modified-p modified)))

(defun ar-th-separate (thing &optional no-delimiters)
  "Optional CHECK is ignored "
  (let* ((bounds (ar-th-bounds thing no-delimiters))
	 (beg (caar bounds))
	 (end (copy-marker (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))))))
    (when beg (goto-char beg)
	  (when (not (looking-back "^[ \t\f\r]*" (line-beginning-position)))
	    (newline ar-newlines-separate-before))
	  (indent-according-to-mode)
	  (goto-char end)
	  (when (not (looking-at "[ \t\f\r]*$"))
	    (newline 1)
	    (indent-according-to-mode)))))

(defun ar-in-delimited-p (char)
  "Return if CHAR is delimiting at point.

Return position if at opening delimiter"
  (let ((orig (point))
        (char (char-to-string char))
	(counter 0)
	erg)
    (save-excursion
      (goto-char (point-min))
      (while (and (search-forward char orig t)
		  (not (ar-escaped)))
	(setq counter (1+ counter))))
    (setq erg (eq 1 (% counter 2)))
    (or erg (and (eq (char-after) char)(point)))))

(defun ar-thing-in-thing (thing-1th thing-2th th-function &optional no-delimiters)
  "Addresses things of 1th kind within the borders of the 2th,
If optional positions BEG-2TH END-2TH are given, works on them instead. "
  (let* ((ar-match-in-comment-p-orig ar-match-in-comment-p)
         (ar-match-in-string-p-orig ar-match-in-string-p)
         (ar-match-in-comment-p 'ignore)
         (ar-match-in-string-p 'ignore)
         (bounds (ar-th-bounds thing-2th))
	 ;; take the inner pos of a delimiter
	 (beg (or
	       (ignore-errors (car (cdr (car-safe bounds))))
	       (ignore-errors (caar bounds))
               (car-safe bounds)))
	 ;; take the inner pos of a delimiter
	 (end (copy-marker (or (ignore-errors (car (car (cdr bounds))))(ignore-errors (car (cdr (cadr bounds))))(ignore-errors (cdr (cadr bounds)))(cdr-safe bounds))))
         ;; ar-scan-whole-buffer
	 (last 1)
	 inner-end done)
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (setq ar-match-in-string-p ar-match-in-string-p-orig)
        (setq ar-match-in-comment-p ar-match-in-comment-p-orig)
        (goto-char beg)
        ;; (if (eq th-function 'ar-th-sort)
        ;;     (ar-th-sort thing-1th nil beg end nil nil nil)
	(when (numberp inner-end) (goto-char inner-end))
	(while
	    (and
	     (not (eobp))
             (or (not done) (< last (point)))
	     (setq last (point))
	     (progn
               (funcall th-function thing-1th)
	       (setq done t)))
	  (unless (eq 'char thing-1th) (setq inner-end (ar-th-forward thing-1th 1))))))))

(defun ar-th-kill (thing &optional no-delimiters)
  " "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters))
	     (beg (or (ignore-errors (caar bounds)) (ignore-errors (car bounds))))
	     (end (and beg (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))(ignore-errors (cadr bounds))(ignore-errors (cdr bounds))))))
	(and beg end
	     (if (eq thing 'comment)
                 (kill-region beg (min (point-max) (1+ end)))
	       (progn
		 (kill-region beg end)
		 t))))
    (error nil)))

(defun ar-th-delete (thing &optional no-delimiters)
  " "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters))
	     (beg (or (ignore-errors (caar bounds)) (ignore-errors (car bounds))))
	     (end (or (ignore-errors (cdr (cadr bounds)))(ignore-errors (cadr bounds))(ignore-errors (cadr (cadr bounds))))))
        (if (eq thing 'comment)
            (delete-region beg (1+ end))
          (delete-region beg end)))
    (error nil)))

(defun ar-th-delete-in-region (thing beg end &optional no-delimiters)
  "Delete THING in region. Delete line, if empty afterwards. "
  (save-excursion
    (goto-char beg)
    (let ((orig (point))
          (end (copy-marker end)))
      (while (progn (ar-th-forward thing) (and (< orig (point)) (< (point) end)))
	(let ((bounds (ar-th-bounds thing no-delimiters)))
	  (delete-region (caar bounds) (cadr (cadr bounds)))
	  (when (and (empty-line-p) (not (eobp)))
	    (delete-region (line-beginning-position) (1+ (line-end-position)))))))))

(defun ar-th-commatize (thing &optional no-delimiters)
  " "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters))
	     (end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds))))))
	(goto-char end)
        (insert ","))
    (error nil)))

(defun ar-th-quote (thing &optional no-delimiters)
  " "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters))
	     (beg (caar bounds)))
        (goto-char beg)
        (insert "'"))
    (error nil)))

;; (defun ar-th-triplequotedq (thing &optional no-delimiters)
;;   " "
;;   (ar-th-delim thing "\"\"\"" "\"\"\"" no-delimiters))

;; (defun ar-th-triplequotesq (thing &optional no-delimiters)
;;   " "
;;   (ar-th-delim thing "'''" "'''" no-delimiters))

;; (defun ar-th-triplebacktick (thing &optional no-delimiters)
;;   " "
;;   (ar-th-delim thing "```" "```" no-delimiters))

(defun ar-th-interactive-backward-form (ap ep)
  (goto-char ep)
  (push-mark ap)
  (exchange-point-and-mark)
  (kill-new (buffer-substring-no-properties ap ep)))

(defun ar-th-forward-function-call (thing arg)
  (let (erg)
    (while (< 0 arg)
      (setq erg (funcall (get thing 'forward-op-at)))
      (setq arg (1- arg)))
    erg))

(defun ar-th-backward-function-call (arg thing)
  (let (erg)
    (while
	(> 0 arg)
      (setq erg (funcall (get thing 'backward-op-at)))
      (setq arg (1+ arg)))
    erg))

(defun ar-th-forward (thing &optional arg)
  "With positive ARG, go to beginning of next THING.

Return position, if successful, nil otherwise.
Move backward with negative argument "
  (let* ((pps (parse-partial-sexp (point-min) (point)))
         (ar-match-in-string-p (nth 3 pps))
         (ar-match-in-comment-p (nth 4 pps))
	 (arg (or arg 1)))
    (if (< 0 arg)
        (funcall (get thing 'forward-op-at))
      (funcall (get thing 'backward-op-at))
      )))

(defun ar-th-un-ml (thing &optional beg end)
  (save-excursion
    (save-restriction
      (when (and beg end)
        (narrow-to-region beg end))
      (let\* ((startstring (eval (intern-soft (concat (format "%s" thing) "-startstring-atpt"))))
              (endstring (concat (eval (intern-soft (concat (format "%s" thing) "-endstring-atpt")))))
              (begstringpos
               (progn
                 (beginning-of-form-base startstring endstring)
                 (if (looking-at startstring)
                     (list (match-beginning 0) (match-end 0))
                   (error "Can't see startstring"))))
              (thisbeg (copy-marker (car begstringpos)))
              thisend)
             (forward-char 1)
             (end-of-form-base startstring endstring)
             (when (looking-back endstring (line-beginning-position))
               (replace-match "")
               (setq thisend (point-marker))
               (delete-region (car begstringpos) (cadr begstringpos))
               (list thisbeg thisend))))
    (widen)))

(defun ar-th-backward (thing &optional arg)
  "Returns beg and end of THING before point as a list. "
  (condition-case nil
      (ar-th-forward thing (- (or arg 1)))
    (error nil)))

(defvar paired-start-pos nil)

(defun ar-th-transpose (thing &optional no-delimiters)
  "Returns position, when called from a program
 end of transposed section. "
  (let* ((pos (point-marker))
         (first (ar-th-bounds thing no-delimiters))
         (pos1 (if (ignore-errors (<= (car first) pos))
                   first
                 (ar-th-bounds thing no-delimiters)))
         (pos2 (progn
                 (ar-th-forward thing no-delimiters)
                 (ar-th-bounds thing no-delimiters)))
         (a (car pos1))
         (b (copy-marker (cdr pos1)))
         (c (car pos2))
         (d (copy-marker (cdr pos2))))
    (transpose-regions a b c d)
    d))

;; credits to sort-subr, sort.el
;; (defun ar-th-sort (thing reverse beg end startkeyfun endkeyfun)
;;   (save-excursion
;;     (save-restriction
;;       (unless (buffer-narrowed-p)(narrow-to-region beg end))
;;       (goto-char (point-min))
;;       (let ((reverse (or reverse nil))
;;             (startkeyfun (or startkeyfun nil))
;;             (endkeyfun (or endkeyfun nil))
;;         (while (not (or (eobp)(stringp (ar-th thing))))
;;           (forward-char 1))
;;         (if (eq thing 'number)
;;           (ar-sort-numbers-subr reverse
;;                       (function (lambda () (if (ar-th-forward thing) (ar-th-gotobeg thing) (goto-char (point-max)))))
;;                       (function (lambda () (ar-th-gotoend thing)(forward-char 1))) startkeyfun endkeyfun)
;;           (sort-subr reverse
;;                       (function (lambda () (if (ar-th-forward thing) (ar-th-gotobeg thing) (goto-char (point-max)))))
;;                       (function (lambda () (ar-th-gotoend thing)(forward-char 1))) startkeyfun endkeyfun)))))))

;; (defun ar-sort-numbers-subr (reverse nextrecfun endrecfun
;;                                      &optional startkeyfun endkeyfun)
;;   "A patched sort-subr. Divides buffer into records and sort them.

;; We divide the accessible portion of the buffer into disjoint pieces
;; called sort records.  A portion of each sort record (perhaps all of
;; it) is designated as the sort key.  The records are rearranged in the
;; buffer in order by their sort keys.  The records may or may not be
;; contiguous.

;; Usually the records are rearranged in order of ascending sort key.
;; If REVERSE is non-nil, they are rearranged in order of descending sort key.
;; The variable ‘sort-fold-case’ determines whether alphabetic case affects
;; the sort order.

;; The next four arguments are functions to be called to move point
;; across a sort record.  They will be called many times from within sort-subr.

;; NEXTRECFUN is called with point at the end of the previous record.
;; It moves point to the start of the next record.
;; It should move point to the end of the buffer if there are no more records.
;; The first record is assumed to start at the position of point when sort-subr
;; is called.

;; ENDRECFUN is called with point within the record.
;; It should move point to the end of the record.

;; STARTKEYFUN moves from the start of the record to the start of the key.
;; It may return either a non-nil value to be used as the key, or
;; else the key is the substring between the values of point after
;; STARTKEYFUN and ENDKEYFUN are called.  If STARTKEYFUN is nil, the key
;; starts at the beginning of the record.

;; ENDKEYFUN moves from the start of the sort key to the end of the sort key.
;; ENDKEYFUN may be nil if STARTKEYFUN returns a value or if it would be the
;; same as ENDRECFUN.

;; PREDICATE is the function to use to compare keys.  If keys are numbers,
;; it defaults to `<', otherwise it defaults to `string<'."
;;   ;; Heuristically try to avoid messages if sorting a small amt of text.
;;   (let ((messages (> (- (point-max) (point-min)) 50000)))
;;     (save-excursion
;;       (if messages (message "Finding sort keys..."))
;;       (let* ((sort-lists (sort-build-lists nextrecfun endrecfun
;; 					   startkeyfun endkeyfun))
;; 	     (old (reverse sort-lists))
;; 	     (case-fold-search sort-fold-case))
;; 	(if (null sort-lists)
;; 	    ()
;; 	  (or reverse (setq sort-lists (nreverse sort-lists)))
;; 	  (if messages (message "Sorting records..."))
;; 	  (setq sort-lists
;; 		(sort sort-lists
;;                       (lambda (a b)
;;                         (< (string-to-number (buffer-substring-no-properties (caar a) (cdar a)))(string-to-number (buffer-substring-no-properties (caar b)(cdar b)))))))
;; 	  (if reverse (setq sort-lists (nreverse sort-lists)))
;; 	  (if messages (message "Reordering buffer..."))
;; 	  (sort-reorder-buffer sort-lists old)))
;;       (if messages (message "Reordering buffer... Done"))))
;;   nil)

(defun ar-th-delim-intern (beg end begstr endstr)
  (let ((end (copy-marker end)))
    (goto-char beg)
    (insert begstr)
    (goto-char end)
    (insert endstr)))

(defun ar-th-delim (thing &optional beg-char end-char no-delimiters)
  "Put delimiters around THING."
  (interactive "*")
  (let* ((bounds (ar-th-bounds thing no-delimiters))
         (beg (or (ignore-errors (caar bounds))(ignore-errors (car bounds))))
         (end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))(ignore-errors (cdr bounds)))))
    (when (and beg end)
      (ar-th-delim-intern beg end beg-char end-char))))

(defun ar-th-base-copy-or (kind arg &optional)
  "Internally used by ‘ar-parentize-or-copy-atpt’ and the like."
  (let* ((expr (format "%s" kind))
	 (arg (if arg (prefix-numeric-value arg) 1))
	 (suffix
	  (when (or (member kind ar-paired-delimit-aktiv)
		    ;; (loop for e in ar-unpaired-delimit-aktiv if (member kind e) return e))
		    (member kind ar-unpaired-delimit-aktiv))
	    (if (string-match "e$" expr)
		"d" "ed")))
	 beg end erg bounds)
    (cond
     ((eq 2 arg)
      (if (use-region-p)
	  (setq erg (funcall (intern-soft (concat "ar-trim- " expr "-in-region-atpt"))))
	(or (setq erg (funcall (intern-soft (concat "ar-trim-" expr suffix "-atpt"))))
	    (funcall (intern-soft (concat "ar-" expr "-" copy-or-alternative "-atpt")) arg))))
     ((eq 4 (prefix-numeric-value arg))
      (if (use-region-p)
	  (setq erg (funcall (intern-soft (concat "ar-" expr "-in-region-atpt"))))
	(or (setq erg (funcall (intern-soft (concat "ar-" expr suffix "-atpt")) arg))
	    (funcall (intern-soft (concat "ar-" expr "-" copy-or-alternative "-atpt"))))))
     ((< arg 0)
      (setq erg (funcall (intern-soft (concat "ar-kill-" expr suffix "-atpt")))))
     ((< 0 arg)
      (or (setq bounds (funcall (intern-soft (concat "ar-bounds-of-" expr suffix "-atpt"))))
	  (setq bounds (funcall (intern-soft (concat "ar-bounds-of-" expr "-" copy-or-alternative "-atpt")))))
      (when bounds
	(setq beg (cond ((ignore-errors (numberp (car-safe bounds)))
			 (car-safe bounds))
			((ignore-errors (caar bounds))
			 (caar bounds))
			(t (car-safe bounds))))
	(setq end (or (ignore-errors (cadr (cadr bounds)))
		      (ignore-errors (cdr (cadr bounds)))(cdr bounds)))
	(when (and beg end)
	  (setq erg (kill-new (buffer-substring-no-properties beg end)))
	  (goto-char beg)
	  (push-mark (point) t t)
	  (goto-char end))))
     ((use-region-p)
      (setq erg (funcall (intern-soft (concat "ar-" expr "-in-region-atpt"))))))
    erg))

(defvar ar-werkstatt-mode-map nil
  "Keymap used in Sh-Werkstatt mode.")


(define-derived-mode werkstatt emacs-lisp-mode "Werk"
  ;; (kill-all-local-variables)
  ;; (setq major-mode 'ar-werkstatt
  ;; mode-name "Sh-Werkstatt")
  (use-local-map ar-werkstatt-mode-map)
  (and ar-werkstatt-hs-minor-mode-p
       (add-hook 'ar-werkstatt-mode-hook 'hs-minor-mode)))
;; ar-insert-delimit-forms-intern ar-paired-delimit-aktiv-raw: start

(defun ar-th-symbol (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "`" "'" no-delimiters))

(defun ar-th-brace (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "{" "}" no-delimiters))

(defun ar-th-bracket (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "[" "]" no-delimiters))

(defun ar-th-lesserangle (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "<" ">" no-delimiters))

(defun ar-th-greaterangle (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing ">" "<" no-delimiters))

(defun ar-th-curvedsinglequote (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "‘" "’" no-delimiters))

(defun ar-th-curveddoublequote (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "“" "”" no-delimiters))

(defun ar-th-parentize (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "(" ")" no-delimiters))
;; ar-insert-delimit-forms-intern ar-paired-delimit-aktiv-raw: end


;; ar-insert-delimit-forms-intern ar-unpaired-delimit-aktiv-raw: start

(defun ar-th-colon (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing ":" ":" no-delimiters))

(defun ar-th-cross (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "+" "+" no-delimiters))

(defun ar-th-doubleslash (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "//" "//" no-delimiters))

(defun ar-th-backslash (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "\\" "\\" no-delimiters))

(defun ar-th-backtick (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "`" "`" no-delimiters))

(defun ar-th-dollar (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "$" "$" no-delimiters))

(defun ar-th-doublequote (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "\"" "\"" no-delimiters))

(defun ar-th-equalize (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "=" "=" no-delimiters))

(defun ar-th-escape (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "\\" "\\" no-delimiters))

(defun ar-th-hash (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "#" "#" no-delimiters))

(defun ar-th-hyphen (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "-" "-" no-delimiters))

(defun ar-th-pipe (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "|" "|" no-delimiters))

(defun ar-th-singlequote (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "'" "'" no-delimiters))

(defun ar-th-slash (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "/" "/" no-delimiters))

(defun ar-th-star (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "*" "*" no-delimiters))

(defun ar-th-tild (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "~" "~" no-delimiters))

(defun ar-th-underscore (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "_" "_" no-delimiters))

(defun ar-th-whitespace (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing " " " " no-delimiters))
;; ar-insert-delimit-forms-intern ar-unpaired-delimit-aktiv-raw: end

;; ar-atpt-data-forms-aktiv start

(defun ar-th-beginendquote (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "\\begin{quote}" "\\end{quote}" no-delimiters))

(defun ar-th-blok (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "{% " " %}" no-delimiters))

(defun ar-th-doublebackslash (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "\\\\" "\\\\" no-delimiters))

(defun ar-th-doublebackslashparen (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "\\\\(" "\\\\)" no-delimiters))

(defun ar-th-doublebacktick (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "``" "``" no-delimiters))

(defun ar-th-triplebacktick (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "```" "```" no-delimiters))

(defun ar-th-backslashparen (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "\\(" "\\)" no-delimiters))

(defun ar-th-slashparen (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "////(" "////)" no-delimiters))

(defun ar-th-triplequotedq (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "\"\"\"" "\"\"\"" no-delimiters))

(defun ar-th-triplequotesq (thing &optional no-delimiters)
  " "
  (interactive "*P")
  (ar-th-delim thing "'''" "\"\"\"\\|'''" no-delimiters))
;; ar-atpt-data-forms-aktiv end


;; ar-insert-thingatpt-syntax-funktionen start

(defun ar-syntax-class-atpt (&optional pos)
  "Return the syntax class part of the syntax at point. "
  (interactive "p")
  (let* ((pos (or pos (point)))
         (erg (logand (car (syntax-after pos)) 65535)))
    (when erg (message "%s" erg)) erg))

(defun syntax-class-bfpt ()
  "Return the syntax class part of the syntax at point. "
  (interactive "p")
  (let ((erg (logand (car (syntax-after (1- (point)))) 65535)))
    (when erg (message "%s" erg)) erg))

(defun ar-syntax-atpt (&optional pos)
  ""
  (interactive)
  (save-excursion
    (when pos
      (goto-char pos))
    (let* ((elt (car (if (featurep 'xemacs)
                         (char-syntax (char-after))
                       (syntax-after (point)))))
           (stax (cond ((eq elt 0) "0 whitespace")
                       ((eq elt 5) "5 close parenthesis")
                       ((eq elt 10) "10 character quote")
                       ((eq elt 1) "1 punctuation")
                       ((eq elt 6) "6 expression prefix")
                       ((eq elt 11) "11 comment-start")
                       ((eq elt 2) "2 word")
                       ((eq elt 7) "7 string quote")
                       ((eq elt 12) "12 comment-end")
                       ((eq elt 3) "3 symbol")
                       ((eq elt 8) "8 paired delimiter")
                       ((eq elt 13) "13 inherit")
                       ((eq elt 4) "4 open parenthesis")
                       ((eq elt 9) "9 escape")
                       ((eq elt 14) "14 generic comment")
                       ((eq elt 15) "15 generic string"))))
      elt)))

;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2019-09/msg00562.html
(defun ar-syntax-class-to-char (syntax-class)
  (aref " .w_()'\"$\/<>@!|" syntax-class))

(defun ar--forward-syntax-class-intern (syntax)
  (skip-syntax-forward
   (char-to-string
    (ar-syntax-class-to-char
     (syntax-class syntax)))))

(defun ar-forward-syntax-class ()
  "Behavior like forward-same-syntax."
  (interactive)
  (ar--forward-syntax-class-intern (syntax-after (point))))

(setq ar-forward-syntax-classes-list (list 0 1 2 3 6))
(defun ar-forward-syntax-classes ()
  "Skip chars forward belonging to syntax-classes ‘ar-forward-syntax-classes-list’"
  (interactive "^p")
  (let ((orig (point))
	done last)
    (while (and (not (eobp))
		(setq last (point))
		(prog1 (not done)
		  (dolist (ele ar-forward-syntax-classes-list)
		    (ar--forward-syntax-class-intern (list ele)))))
      (unless (< last (point))(setq done t)))
    (< orig (point))))

(defun ar-syntax-in-region-atpt (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (let (erg)
      (while (< (point) end)
        (setq erg (concat erg "\n" "\"" (char-to-string (char-after)) "\"" "  is  " (ar-syntax-atpt t)))
        (forward-char 1))
      (message "%s" erg)
      erg)))

(defun syntax-bfpt (&optional arg)
  (interactive "p")
  (let ((stax (syntax-after (1- (point)))))
    (when arg
      (message (format "%s" stax)))
    stax))


(defun ar-beginning-of-indent ()
  "Go to the beginning of a section of equal indent."
  (interactive)
  (let ((indent (current-indentation))
	(last (line-beginning-position)))
    (while (and (not (bobp))
		(progn (forward-line -1)
		       (= indent (current-indentation)))
		(not (empty-line-p))
		(setq last (line-beginning-position))))
    (goto-char last)
    last))

(defun ar--travel-this-indent-backward (&optional indent)
  "Travel current INDENT backward.

With optional INDENT travel bigger or equal indentation"
  (let ((indent (or indent (current-indentation)))
	(last (line-beginning-position)))
    (while (and (not (bobp))
		(progn (forward-line -1)
		       (= indent (current-indentation)))
		(not (empty-line-p))
		(setq last (line-beginning-position))))
    (goto-char last)
    last))

(defun ar-backward-indent ()
  "Go to the beginning of a section of equal indent.

If already at the beginning or before a indent, go to next indent upwards
Returns final position when called from inside section, nil otherwise"
  (interactive)
  (unless (bobp)
    (let (erg)
      (setq erg (ar--travel-this-indent-backward))
      (when erg (goto-char erg))
      erg)))

(defun ar-end-of-indent ()
  "Go to the end of a section of equal indentation."
  (interactive)
  (let ((last (line-end-position))
	(indent (current-indentation)))
    (while (and (not (eobp)) (progn (forward-line 1) (and (not (empty-line-p)) (= indent (current-indentation))))(setq last (line-end-position))))
    (goto-char last)
    (point)))

(defun ar--travel-this-indent-forward (indent)
  "Internal use.

Travel this INDENT forward"
  (let (last)
    (while (and (progn (forward-line 1)
		       (eq indent (current-indentation)))
		(not (empty-line-p))
		(setq last (line-end-position))))
    (when last (goto-char last))
    last))

(defun ar-forward-indent ()
  "Go to the end of a section of equal indentation..

If already at the end, go down to next indent in buffer
Returns final position when called from inside section, nil otherwise"
  (interactive)
  (let (done
	(last (point))
	(orig (point))
	(indent (current-indentation)))
    (while (and (not (eobp)) (not done) (progn (forward-line 1) (back-to-indentation) (or (empty-line-p) (and (<= indent (current-indentation))(< last (point))(setq last (point)))(setq done t))))
      (and (< indent (current-indentation))(setq done t)))
    (if (and last (< orig last))
	(progn (goto-char last)
	       (end-of-line)
	       (skip-chars-backward " \t\r\n\f"))
      (skip-chars-forward " \t\r\n\f")
      (end-of-line)
      (skip-chars-backward " \t\r\n\f"))
    (and (< orig (point))(point))))

(defun ar-sort-indent ()
  (interactive)
  (save-excursion
    (let ((beg (ar-beginning-of-indent))
	  (end (ar-end-of-indent)))
      (when (and beg end)
	(save-restriction
	  (narrow-to-region beg end)
	  (sort-lines nil beg end))))))

(defun ar-mark-indent ()
  (interactive)
  (let ((beg (ar-beginning-of-indent))
	(end (ar-end-of-indent)))
    (goto-char end)
    (set-mark (point))
    (goto-char beg)
    (exchange-point-and-mark)))

(defvar ar-paired-delimited-passiv-raw
  (list
   '(braced "{" "}")
   '(bracketed "[" "]")
   '(lesserangled "<" ">")
   '(greaterangled ">" "<")
   '(curvedsinglequoted "‘" "’")
   '(parentized "(" ")")))

(defvar ar-unpaired-delimited-raw
  (list
   '(backslashed "\\\\")
   '(backticked "`")
   '(coloned ":")
   '(dollared "$")
   '(doublequoted "\\\"")
   '(equalized "=")
   '(hyphened "-")
   '(singlequoted "'")
   '(slashed "/")
   '(stared "*")
   '(underscored "_")
   '(whitespaced " ")))

(defun ar--transform-delimited-new-delimiter (to)
  "Return the new delimiter - either paired or unpaired. "
  (let ((erg))
    (dolist (ele ar-paired-delimited-passiv-raw)
      (when (member to ele)
	(setq erg (cdr ele))
	(message "%s" (car erg))))
    (unless erg
      (dolist (ele ar-unpaired-delimited-raw)
	(when (member to ele)
	  (setq erg (cdr ele))
	  (message "%s" (car erg)))))
    erg))

(defun ar--transform-insert-opening-delimiter-according-to-type (new-delimiter)
  "If a cons, insert car. "
  (if (string-match "\"" (car new-delimiter))
      (insert "\"")
    (insert (car new-delimiter))))

(defun ar--transform-return-closing-delimiter-according-to-type (new-delimiter)
  "Return closing delimiter. "
  (let ((erg (if (< 1 (length new-delimiter))
		 (cadr new-delimiter)
	       (car new-delimiter))))
    (if (string-match "\"" erg)
	"\""
      erg)))

;; (defun ar--transform-insert-opening-delimiter-according-to-type (new-delimiter)
;;   "If a cons, insert car. "
;;   (insert (car new-delimiter)))

;; (defun ar--transform-return-closing-delimiter-according-to-type (new-delimiter)
;;   "Return closing delimiter. "
;;   (if (< 1 (length new-delimiter))
;;       (cadr new-delimiter)
;;     (car new-delimiter)))

(defun ar--transform-delimited-intern (from to)
  "Expects string. "
  (save-restriction
    (let* ((bounds (ignore-errors (funcall (car (read-from-string (concat "ar-bounds-of-" from "-atpt"))))))
	   (end (copy-marker (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds))))))
	   (new-delimiter (ar--transform-delimited-new-delimiter (car (read-from-string to)))))
      (unless bounds (message (concat "ar--transform-delimited-intern: can't see " from)))
      (unless new-delimiter (message (concat "ar--transform-delimited-intern: can't see " to)))
      (goto-char (caar bounds))
      (delete-char 1)
      (ar--transform-insert-opening-delimiter-according-to-type new-delimiter)
      (goto-char end)
      (delete-char -1)
      (insert (ar--transform-return-closing-delimiter-according-to-type new-delimiter)))))


(defvar ar-paired-delimited-passiv-raw
  (list
   '(symboled "`" "'")
   '(braced "{" "}")
   '(bracketed "[" "]")
   '(lesserangled "<" ">")
   '(greaterangled ">" "<")
   '(curvedsinglequoted "‘" "’")
   '(parentized "(" ")")))

(defvar ar-unpaired-delimited-raw
  (list
   '(backslashed "\\\\")
   '(backticked "`")
   '(coloned ":")
   '(dollared "$")
   '(doublequoted "\\\"")
   '(equalized "=")
   '(hyphened "-")
   '(singlequoted "'")
   '(slashed "/")
   '(stared "*")
   '(underscored "_")
   '(whitespaced " ")))

(setq ar-paired-delimit-aktiv-raw
      (list
       '(symbol 96 39)
       '(brace 123 125)
       '(bracket 91 93)
       '(lesserangle 60 62)
       '(greaterangle 62 60)
       '(curvedsinglequote 8216 8217)
       '(curveddoublequote 8220 8221)
       '(parentize 40 41)
       ))

(setq ar-paired-delimit-aktiv
      (list
       'symbol
       'brace
       'bracket
       'lesserangle
       'greaterangle
       'curvedsinglequote
       'curveddoublequote
       'parentize
       ))

(setq ar-atpt-classes
      (list
       'alnum
       'alpha
       'ascii
       'blank
       'cntrl
       'digit
       'graph
       'lower
       'nonascii
       'print
       'punct
       'space
       'upper
       'xdigit
       ))

(setq ar-unpaired-delimit-aktiv-raw
      (list
       '(colon ":")
       '(cross "+")
       '(doubleslash "//")
       '(backslash "\\\\")
       '(backtick "`")
       '(dollar "$")
       '(doublequote "\"")
       '(equalize "=")
       '(escape "\\\\")
       '(hash "#")
       '(hyphen "-")
       '(pipe "|")
       '(singlequote "'")
       '(slash "/")
       '(star "*")
       '(tild "~")
       '(underscore "_")
       '(whitespace " ")
       ))

(setq ar-unpaired-delimit-aktiv
      (list
       'colon
       'cross
       'doubleslash
       'backslash
       'backtick
       'dollar
       'doublequote
       'equalize
       'escape
       'hash
       'hyphen
       'pipe
       'singlequote
       'slash
       'star
       'tild
       'underscore
       'whitespace
       ))

(setq ar-unary-operations
      (list
       'commatize
       'quote
       ))

(setq ar-atpt-data-forms-aktiv-raw
      (list
       '("beginendquote" "\\\\begin{quote}" "\\\\end{quote}" nil 'move 1 nil t nil)
       '("blok" "{% " " %}" nil 'move "1" nil t)
       '("doublebackslash" "\\\\\\\\" "\\\\\\\\" nil 'move "1" nil nil 'ar-escaped)
       '("doublebackslashparen" "\\\\\\\\(" "\\\\\\\\)" nil 'move "1" nil nil 'ar-escaped)
       '("doublebacktick" "``" "``" 'move "1" nil t 'ar-escaped)
       '("triplebacktick" "```" "```" 'move "1" nil t 'ar-escaped)
       '("backslashparen" "\\\\(" "\\\\)" nil 'move "1" nil nil 'ar-escaped)
       '("slashparen" "////(" "////)" nil 'move "1" nil nil 'ar-escaped)
       '("triplequotedq" "\\\"\\\"\\\"" nil 'move 1 nil nil 'ar-escaped)
       '("triplequotesq" "'''" "\\\"\\\"\\\"\\\\|'''" nil 'move 1 nil nil 'ar-escaped)
       ))

(setq ar-atpt-data-forms-aktiv
      (list
       'beginendquote
       'blok
       'doublebackslash
       'doublebackslashparen
       'doublebacktick
       'triplebacktick
       'backslashparen
       'slashparen
       'triplequotedq
       'triplequotesq
       ))

(setq ar-atpt-data-forms-passiv-raw
      (list
       '("beginendquoted" "\\\\begin{quote}" "\\\\end{quote}" nil 'move 1 nil nil nil)
       '("blok" "{% " " %}" nil 'move "1" nil t)
       '("doublebackslashed" "\\\\\\\\" "\\\\\\\\" nil 'move "1" nil nil 'ar-escaped)
       '("doublebackticked" "``" "``" nil 'move "1" nil nil 'ar-escaped)
       '("doubleslashed" "//" "//" nil 'move "1" nil nil 'ar-escaped)
       '("doublebackslashedparen" "\\\\\\\\\\\\\\\\(" "\\\\\\\\\\\\\\\\)" nil 'move "1" nil nil 'ar-escaped)
       '("tabledatap" "<td[^>]*>" "</td>" nil 'move "1" nil nil nil)
       '("backslashedparen" "\\\\\\\\(" "\\\\\\\\)" nil 'move "1" nil nil 'ar-escaped)
       '("slashedparen" "////////(" "////////)" nil 'move "1" nil nil 'ar-escaped)
       '("triplequoteddq" "\\\"\\\"\\\"\\\\|'''" "\\\"\\\"\\\"\\\\|'''" nil 'move 1 nil nil 'ar-escaped)
       '("triplequotedsq" "\\\"\\\"\\\"\\\\|'''" "\\\"\\\"\\\"\\\\|'''" nil 'move 1 nil nil 'ar-escaped)
       '("xslstylesheetp" "<xsl:stylesheet[^<]+>.*$" "</xsl:stylesheet>" nil 'move "1" nil nil nil)
       ))

(setq ar-atpt-data-forms-passiv
      (list
       'beginendquoted
       'blok
       'doublebackslashed
       'doublebackticked
       'doubleslashed
       'doublebackslashedparen
       'tabledatap
       'backslashedparen
       'slashedparen
       'triplequoteddq
       'triplequotedsq
       'xslstylesheetp
       ))

(setq ar-atpt-python-list
      (list
       'py-block
       'py-block-or-clause
       'py-class
       'py-clause
       'py-def-or-class
       'py-def
       'py-expression
       'py-partial-expression
       'py-statement
       'py-string
       ))

(setq ar-atpt-python-quoted-raw
      (list
       '(triplequoted "\"\"\"\\\\|'''")
       '(triplequoteddq "\"\"\"")
       '(triplequotedsq "'''")
       ))

(setq ar-atpt-python-quoted
      (list
       'triplequoted
       'triplequoteddq
       'triplequotedsq
       ))

(setq ar-atpt-expression-list
      (list
       'block
       'block-or-clause
       'char
       'class
       'clause
       'def-or-class
       'def
       'delimited
       'expression
       'partial-expression
       'statement
       'string
       ))

(setq ar-atpt-markup-list
      (list
       'beginendquote
       'blok
       'doublebackslashed
       'doublebackticked
       'doublebackslashedparen
       'doubleslashed
       'doubleslashedparen
       'markup
       'mldata
       'mlattribut
       'mltag
       'slashedparen
       'symbol
       'tabledata
       'xslstylesheet
       'xsltemplate
       ))

(setq ar-paired-delimited-passiv-raw
      (list
       '(braced "{" "}")
       '(symboled "`" "'")
       '(bracketed "[" "]")
       '(lesserangled "<" ">")
       '(greaterangled ">" "<")
       '(curvedsinglequoted "‘" "’")
       '(curveddoublequoted "“" "”")
       '(parentized "(" ")")
       ))

(setq ar-paired-delimited-passiv
      (list
       'braced
       'symboled
       'bracketed
       'lesserangled
       'greaterangled
       'curvedsinglequoted
       'curveddoublequoted
       'parentized
       ))

(setq ar-unpaired-delimited-passiv-raw
      (list
       '(backslashed "\\\\")
       '(backticked "`")
       '(coloned ":")
       '(crossed "+")
       '(dollared "$")
       '(doublequoted "\\\"")
       '(equalized "=")
       '(hashed "#")
       '(hyphened "-")
       '(piped "-")
       '(singlequoted "'")
       '(slashed "/")
       '(stared "*")
       '(tilded "~")
       '(underscored "_")
       '(whitespaced " ")
       ))

(setq ar-unpaired-delimited-passiv
      (list
       'backslashed
       'backticked
       'coloned
       'crossed
       'dollared
       'doublequoted
       'equalized
       'hashed
       'hyphened
       'piped
       'singlequoted
       'slashed
       'stared
       'tilded
       'underscored
       'whitespaced
       ))

(setq ar-atpt-region-only
      (list
       'region
       ))

(setq ar-atpt-rest-list
      (list
       'greateranglednested
       'lesseranglednested
       'buffer
       'char
       'comment
       'csv
       'date
       'delimited
       'email
       'filename
       'filenamenondirectory
       'float
       'function
       'ip
       'isbn
       'line
       'list
       'name
       'number
       'page
       'paragraph
       'phone
       'sentence
       'sexp
       'shstruct
       'symbol
       'url
       'word
       'wordalphaonly
       ))

(setq ar-atpt-major-forms-restricted-list
      (list
       'buffer
       'page
       'paragraph
       'region
       ))

(setq ar-atpt-counts-list
      (list
       'anglednonest
       'greateranglednested
       'lesseranglednested
       'csv
       'line
       'paragraph
       'region
       'sentence
       'string
       'buffer
       ))




(provide 'thingatpt-utils-core)
;;; thingatpt-utils-core.el ends here
