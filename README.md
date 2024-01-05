# thing-at-point-utils

[![CircleCI thing-at-point-utils](https://circleci.com/gh/andreas-roehler/thing-at-point-utils.svg?style=svg)](https://app.circleci.com/pipelines/gh/andreas-roehler/thing-at-point-util)s

===


Thing-at-point-utils [![Build Status](https://travis-ci.org/andreas-roehler/thing-at-point-utils.svg?branch=master)](https://travis-ci.org/andreas-roehler/thing-at-point-utils)
===

# ar-forward-sexp, ar- backward-sexp

Similar to ‘forward-sexp’ but, when called from inside a comment or string, match only inside.
Likewise, when called from outside a comment or string, ignore comments or strings for a match.

# thing-at-point-utils
Delivers a set of functions to return, mover over or
manipulate a given THING. 

See USAGE for details

;;;;;;;;;

To see what's implemented, consult contents of
variables at the end of thingatpt-utils-core.el as
`ar-atpt-delimlist', `ar-atpt-delimited-list', etc.

;;;;;;;;;

The idea comes from Mike Williams, author of shipped
thingatpt.el

You might be interested also to visit Drew Adam's
http://www.emacswiki.org/emacs/thingatpt+.el
which predates this approach and was helpful writing it.

In case of trouble or feature requests, please file a
bug report.


