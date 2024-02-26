THING may be a well known form as word, paragraph, but
also a char class as `alnum' or a new defined thing.

For example `ar-alnum-atpt' will return all
alpha-numerical chars below and around cursor as a
string. `ar-bounds-of-alnum-atpt' returns the
borders of that string as a list and so on.

`ar-doublequoted-atpt' will return the string at point,
copied into the kill-ring, enabling yanking it and a
lot of further actions.

So far THING is simply picked up.

Different approach combines copying, deleting with delimiting

if region is active:

(global-set-key [(control c) (\")] 'ar-doublequote-or-copy-atpt)

will provide doublequotes at beginning and end of region.

With negative argument it deletes the doublequoted portion under
point.

Without any argument these functions return as their simplier
counterparts

With universal argument [(control u)] delimiters --i.e. doublequotes, slashes, whatever-- are stripped.

;;;;;;;;;

Use ‘delimited’ in order to fetch or manipulate symbols. The following key-settings may be useful:

  (global-set-key [(meta ?\})] 'ar-delimited2braced-atpt)
  ;; (global-set-key [(control c) (control d) (\\)] 'ar-delimited2backslashed-atpt)
  (global-set-key [(meta ?\\)] 'ar-delimited2dollared-atpt)
  ;; (global-set-key [(control c) (control d) (\})] 'ar-delimited2braced-atpt)
  (global-set-key [(meta ?})] 'ar-delimited2braced-atpt)
  ;; (global-set-key [(control c) (control d) (\])] 'ar-delimited2bracketed-atpt)
  (global-set-key [(meta ?\])] 'ar-delimited2bracketed-atpt)
  ;; (global-set-key [(control c) (control d) (\`)] 'ar-delimited2backticked-atpt)
  (global-set-key [(meta ?`)] 'ar-delimited2backticked-atpt)
  ;; (global-set-key [(control c) (control d) ($)] 'ar-delimited2dollared-atpt)
  (global-set-key [(meta ?$)] 'ar-delimited2dollared-atpt)
  ;; (global-set-key [(control c) (control d) (\")] 'ar-delimited2doublequoted-atpt)
  (global-set-key [(meta ?\")] 'ar-delimited2doublequoted-atpt)
  ;; (global-set-key [(control c) (control d) (=)] 'ar-delimited2equalized-atpt)
  (global-set-key [(meta ?=)] 'ar-delimited2equalized-atpt)
  ;; (global-set-key [(control c) (control d) (-)] 'ar-delimited2hyphened-atpt)
  (global-set-key [(meta ?-)] 'ar-delimited2hyphened-atpt)
  ;; (global-set-key [(control c) (control d) (\))] 'ar-delimited2parentized-atpt)
  (global-set-key [(meta ?\))] 'ar-delimited2parentized-atpt)
  ;; (global-set-key [(control c) (control d) (\')] 'ar-delimited2singlequoted-atpt)
  (global-set-key [(meta ?')] 'ar-delimited2singlequoted-atpt)
  ;; (global-set-key [(control c) (control d) (/)] 'ar-delimited2slashed-atpt)
  (global-set-key [(meta ?/)] 'ar-delimited2slashed-atpt)
  ;; (global-set-key [(control c) (control d) (\ )] 'ar-delimited2whitespaced-atpt)
  (global-set-key [(meta ? )] 'ar-delimited2whitespaced-atpt)
  ;; (global-set-key [(control c) (control d) (_)] 'ar-delimited2underscored-atpt)


;;;;;;;;;

Presently for a given THING the following is
implemented:

ar-THING-atpt
ar-THING-bounds-atpt
ar-THING-beginning-position-atpt
ar-THING-end-position-atpt
ar-THING-beginning-atpt
ar-THING-end-atpt
ar-THING-length-atpt
ar-THING-copy-atpt
ar-THING-kill-atpt
ar-THING-forward-atpt
ar-THING-backward-atpt
ar-THING-transpose-atpt
ar-THING-sort-atpt
ar-THING-check-atpt

Beside of the mentioned above, esists still a couple of
functions, whose use is much less probable:

ar-THING-slash-atpt
ar-THING-double-backslash-atpt
ar-THING-doubleslash-atpt
ar-THING-delete-in-region
ar-blok-THING-atpt
ar-THING-escape-atpt
ar-THING-doublequote-atpt
ar-THING-doubleslash-paren-atpt
ar-THING-slashparen-atpt
ar-THING-dollar-atpt
ar-THING-equalize-atpt
ar-THING-greaterangle-atpt
ar-THING-lesserangle-atpt
ar-THING-backslash-atpt
ar-THING-brace-atpt
ar-THING-bracket-atpt
ar-comment-THING-atpt
ar-commatize-THING-atpt
ar-quote-THING-atpt
ar-THING-hyphen-atpt
ar-THING-mark-atpt
ar-THING-hide-atpt
ar-THING-show-atpt
ar-THING-hide-show-atpt
ar-THING-left-right-singlequote-atpt
ar-THING-parentize-atpt
ar-THING-separate-atpt
ar-THING-singlequote-atpt
ar-THING-trim-atpt
ar-THING-left-trim-atpt
ar-THING-right-trim-atpt
ar-underscore-THING-atpt
ar-whitespace-THING-atpt

;;;;;;;;;

The goal is to have a set of similar forms. For
example, to provide a word with double-quotes around
it, call ar-doublequote-word-atpt. In a similar way you
may double-quote not just a word, but any object
instrumented here as THING. To make parentheses
around it call ar-parentize-word-atpt, etc.

To see other features, maybe try `ar-separate-list-atpt'
or `ar-comment-list-atpt' while point is inside a
list. Try it again with an abstract char-class as
[:alnum:], i.e. try `ar-comment-alnum-atpt',
`ar-brace-alnum-atpt' etc.

Move-functions of this package differ from common
behaviour in such, as `ar-forward-word-atpt' stops
not after THING, but on the last char of
THING.

;;;;;;;;;

THING as a buffer substring is determined by
move-functions specified for thingatpt, called
beginning-op-at and end-op-at. Point is stored
after move, beginning and end delivered as pair: as
consed bounds-of-thing. It's easy to write your own
thing-at-point functions that way. You need the
caller and both move forms:

(defun MY-FORM-atpt (&optional arg)
  " "
  (interactive "p")
  (ar-th 'MY-FORM arg))

(put 'MY-FORM 'beginning-op-at
           (lambda () MY-FORWARD-MOVE-CODE))

(put 'MY-FORM 'end-op-at
     (lambda () MY-BACKWARD-MOVE-CODE))

For example if you want to pick all chars at point
which are written between a string "AAA" and a
"BBB", which may exist as
AAA Luckily detected a lot of things! BBB
After evaluation of
(put 'MY-FORM 'beginning-op-at
     (lambda ()
       (search-backward "AAA" nil 'move 1)
       ;; step chars of search expression back
       (forward-char 3)))

(put 'MY-FORM 'end-op-at
     (lambda ()
       (search-forward "BBB" nil 'move 1)
       (forward-char -3)))
together with the functions definition above, it's ready.
M-x MY-FORM-atpt
(while point inside) you should see:
" Luckily detected a lot of things! "
in the minibuffer.
