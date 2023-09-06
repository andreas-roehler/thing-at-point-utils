#!/bin/sh

# Author: Andreas Roehler <andreas.roehler@online.de>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# Commentary:

# This script tests functions from ar-mode.el.

# Code:

if [ $1 == e25 ]; then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e26 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e27 ];then
    #  export EMACS="$HOME/emacs-20220306/src/emacs -Q"
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e28 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e29 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e30 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
else
    EMACS=emacs
fi

#  EMACS=emacs
echo "before shift \$EMACS: $EMACS"
shift

echo "\$*: $*"
PDIR=$PWD
echo "\$PWD: $PWD"

TESTDIR=$PWD/test/
export TESTDIR
echo "\$TESTDIR: $TESTDIR"

SETUP1=${TESTDIR}ar-thingatpt-setup-tests.el
SETUP2=${TESTDIR}py-setup-ert-tests.el

FILE1=beg-end.el
FILE2=ar-subr.el
FILE3=thingatpt-utils-map.el
FILE4=thingatpt-utils-core.el
FILE5=thing-at-point-utils.el
FILE6=thingatpt-highlight.el
FILE7=thingatpt-transform-delimited.el
FILE8=thing-unpaired-delimited-list-in-rest-list.el
FILE9=thing-classes-in-rest-list.el
FILE10=thing-delimited-list-in-counts-list.el
FILE11=thing-unpaired-delimited-list-in-delimited-list.el
FILE12=thing-classes-in-delimited-list.el
FILE13=thing-at-point-peel.el
FILE14=thingatpt-count-ratio.el
FILE15=thingatpt-transform-generic-delimited.el
FILE16=translate-paired-delimiters.el
FILE17=thing-rest-list-in-region-only.el
FILE18=thing-data-forms-aktiv-in-rest-list.el
# FILE19=$HOME/arbeit/emacs-lisp/python-modes/gitlab-python-mode/python-mode.el
FILE20=ar-sexp.el
FILE21=ar-comment-lor.el

TEST1=test/thing-at-point-utils-tests.el
TEST2=test/ar-paired-delimit-tests.el
TEST3=test/ar-trim-test.el
TEST4=test/ar-unpaired-delimited-tests.el
TEST5=test/ar-bracketlist-atpt-ert-tests.el
TEST6=test/ar-thing-at-point-utils-forward-tests.el
TEST7=test/ar-translate-paired-delimiters-test.el
TEST8=test/ar-thing-at-point-utils-nodelim-classes-test.el
TEST9=test/ar-tatpt-utils-delimited-tests.el
TEST10=test/ar-list-atpt-brace-ert-tests.el
TEST11=test/ar-paired-delimited-tests.el
TEST12=test/ar-delimited2delimited-tests.el
TEST13=test/ar-separate-tests.el
TEST14=test/ar-list-atpt-parens-ert-tests.el
TEST15=test/ar-thing-atpt-also-delimited-test.el
TEST16=test/ar-thing-atpt-other-delimited-test.el
TEST17=test/ar-thing-atpt-other-position-test.el
TEST18=test/ar-thing-atpt-more-delimited-test.el
TEST19=test/ar-thing-at-point-interactive-tests.el
TEST20=test/ar-thingatpt-utils-comment-or-uncomment-tests.el
TEST21=test/ar-forward-sexp-tests.el

h1 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST1 \
-f ert-run-tests-batch-and-exit
}

h2 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST2 \
-f ert-run-tests-batch-and-exit
}

h3 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
-load $FILE21 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST3 \
-f ert-run-tests-batch-and-exit
}

h4 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST4 \
-f ert-run-tests-batch-and-exit
}

h5 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST5 \
-f ert-run-tests-batch-and-exit
}

h6 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST6 \
-f ert-run-tests-batch-and-exit
}

h7 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST7 \
-f ert-run-tests-batch-and-exit
}

h8 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST8 \
-f ert-run-tests-batch-and-exit
}

h9 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST9 \
-f ert-run-tests-batch-and-exit
}

h10 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST10 \
-f ert-run-tests-batch-and-exit
}

h11 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST11 \
-f ert-run-tests-batch-and-exit
}

h12 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST12 \
-f ert-run-tests-batch-and-exit
}

h13 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST13 \
-f ert-run-tests-batch-and-exit
}

h14 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST14 \
-f ert-run-tests-batch-and-exit
}

h15 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST15 \
-f ert-run-tests-batch-and-exit
}

h16 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST16 \
-f ert-run-tests-batch-and-exit
}

h17 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
-load $FILE21 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST17 \
-f ert-run-tests-batch-and-exit
}

h18 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
-load $FILE21 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST18 \
-f ert-run-tests-batch-and-exit
}

h19 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
-load $FILE21 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST19 \
-f ert-run-tests-batch-and-exit
}

h20 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
-load $FILE21 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST20 \
-f ert-run-tests-batch-and-exit
}

h21 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
-load $FILE21 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST21 \
-f ert-run-tests-batch-and-exit
}

hier () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE5 \
-load $FILE6 \
-load $FILE7 \
-load $FILE8 \
-load $FILE9 \
-load $FILE10 \
-load $FILE11 \
-load $FILE12 \
-load $FILE13 \
-load $FILE14 \
-load $FILE15 \
-load $FILE16 \
-load $FILE17 \
-load $FILE18 \
-load $FILE20 \
-load $FILE21 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST1 \
-load $TEST2 \
-load $TEST3 \
-load $TEST4 \
-load $TEST5 \
-load $TEST6 \
-load $TEST7 \
-load $TEST8 \
-load $TEST9 \
-load $TEST10 \
-load $TEST12 \
-load $TEST13 \
-load $TEST14 \
-load $TEST16 \
-load $TEST17 \
-load $TEST18 \
-load $TEST19 \
-load $TEST20 \
-load $TEST21 \
-f ert-run-tests-batch-and-exit
}

if [ $WERKSTATT -eq 0 ]; then

    [ $HOME/werkstatt/thingatpt-utils-core/ar-subr.el -nt ${PWD}/ar-subr.el ] && echo "cp -u $HOME/werkstatt/thingatpt-utils-core/ar-subr.el $PWD"; cp -u $HOME/werkstatt/thingatpt-utils-core/ar-subr.el $PWD
    [ $HOME/werkstatt/thingatpt-utils-core/beg-end.el -nt ${PWD}/beg-end.el ] && echo "cp -u $HOME/werkstatt/thingatpt-utils-core/beg-end.el $PWD"; cp -u $HOME/werkstatt/thingatpt-utils-core/beg-end.el $PWD
    [ $HOME/werkstatt/thingatpt-utils-core/thingatpt-utils-map.el -nt ${PWD}/thingatpt-utils-map.el ] && echo "cp -u $HOME/werkstatt/thingatpt-utils-core/thingatpt-utils-map.el $PWD"; cp -u $HOME/werkstatt/thingatpt-utils-core/thingatpt-utils-map.el $PWD
    [ $HOME/werkstatt/thingatpt-utils-core/thingatpt-utils-core.el -nt ${PWD}/thingatpt-utils-core.el ] && echo "cp -u $HOME/werkstatt/thingatpt-utils-core/thingatpt-utils-core.el $PWD"; cp -u $HOME/werkstatt/thingatpt-utils-core/thingatpt-utils-core.el $PWD && echo "cp -u $HOME/werkstatt/thingatpt-utils-core/thingatpt-utils-core.el nach $HOME/werkstatt/numbers-at-point"; cp -u $HOME/werkstatt/thingatpt-utils-core/thingatpt-utils-core.el $HOME/werkstatt/numbers-at-point
    [ $HOME/werkstatt/thingatpt-utils-core/ar-thingatpt-basic-definitions.el -nt ${PWD}/ar-thingatpt-basic-definitions.el ] && echo "cp -u $HOME/werkstatt/thingatpt-utils-core/ar-thingatpt-basic-definitions.el $PWD"; cp -u $HOME/werkstatt/thingatpt-utils-core/ar-thingatpt-basic-definitions.el $PWD
    [ $HOME/werkstatt/thingatpt-utils-core/test/ar-thingatpt-setup-tests.el -nt ${PWD}/test/ar-thingatpt-setup-tests.el ] && echo "cp -u $HOME/werkstatt/thingatpt-utils-core/test/ar-thingatpt-setup-tests.el $PWD/test/"; cp -u $HOME/werkstatt/thingatpt-utils-core/test/ar-thingatpt-setup-tests.el $PWD/test/

    while getopts 123456789abcdefghijklmnopqrstuvwxyz option

    do
	case $option in
	    1) echo "Lade \$TEST1: \"$TEST1\"";h1;;
	    2) echo "Lade \$TEST2: \"$TEST2\"";h2;;
	    3) echo "Lade \$TEST3: \"$TEST3\"";h3;;
	    4) echo "Lade \$TEST4: \"$TEST4\"";h4;;
	    5) echo "Lade \$TEST5: \"$TEST5\"";h5;;
	    6) echo "Lade \$TEST6: \"$TEST6\"";h6;;
	    7) echo "Lade \$TEST7: \"$TEST7\"";h7;;
	    8) echo "Lade \$TEST8: \"$TEST8\"";h8;;
	    9) echo "Lade \$TEST9: \"$TEST9\"";h9;;
	    a) echo "Lade \$TEST10: \"$TEST10\"";h10;;
	    b) echo "Lade \$TEST11: \"$TEST11\"";h11;;
	    c) echo "Lade \$TEST12: \"$TEST12\"";h12;;
	    d) echo "Lade \$TEST13: \"$TEST13\"";h13;;
	    e) echo "Lade \$TEST14: \"$TEST14\"";h14;;
	    f) echo "Lade \$TEST15: \"$TEST15\"";h15;;
	    g) echo "Lade \$TEST16: \"$TEST16\"";h16;;
	    h) echo "Lade \$TEST17: \"$TEST17\"";h17;;
	    i) echo "Lade \$TEST18: \"$TEST18\"";h18;;
	    j) echo "Lade \$TEST19: \"$TEST19\"";h19;;
	    k) echo "Lade \$TEST20: \"$TEST20\"";h20;;
	    l) echo "Lade \$TEST21: \"$TEST21\"";h21;;
	    # m) echo "Lade \$TEST22: \"$TEST22\"";h22;;
	    # n) echo "Lade \$TEST14: \"$TEST14\"";h14;;
	    # o) echo "Lade \$TEST15: \"$TEST15\"";h15;;
	    # p) echo "Lade \$TEST16: \"$TEST16\"";h16;;
	    # q) echo "Lade \$TEST17: \"$TEST17\"";h17;;
	    # r) echo "Lade \$TEST18: \"$TEST18\"";h18;;
	    # s) echo "Lade \$TEST19: \"$TEST19\"";h19;;
	    # p) echo "Lade \$TEST12: \"$TEST12\"";h12;;
	    # q) echo "Lade \$TEST13: \"$TEST13\"";h13;;
	    # #  r) echo "Lade \$TEST14: \"$TEST14\"";h14;;
	    # s) echo "Lade \$TEST15: \"$TEST15\"";h15;;
	    # t) echo "Lade \$TEST16: \"$TEST16\"";h16;;
	    # u) echo "Lade \$TEST17: \"$TEST17\"";h17;;
	    # v) echo "Lade \$TEST18: \"$TEST18\"";h18;;
	    # w) echo "Lade \$TEST19: \"$TEST19\"";h19;;
	    x) echo "Lade testumgebung \"ENTFERNT\""; entfernt;;
	    n) echo "Lade testumgebung \"HIER\"";hier;;

	esac
    done

else

    echo "Lade testumgebung \"ENTFERNT\""
    entfernt

fi
