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

if [ $1 == en ]; then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")

elif [ $1 == e25 ]; then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e26 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e27 ];then
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

echo "before shift \$EMACS: $EMACS"
shift

echo "\$*: $*"
PDIR=$PWD
echo "\$PWD: $PWD"

TESTDIR=$PWD/test/
export TESTDIR
echo "\$TESTDIR: $TESTDIR"

echo "\$WERKSTATT: $WERKSTATT"

IFLOCAL=${IFLOCAL:=1}

SETUP1=${TESTDIR}ar-thingatpt-setup-tests.el
SETUP2=${TESTDIR}py-setup-ert-tests.el

echo "\$SETUP1: $SETUP1"

FILE1=ar-beg-end.el
FILE2=ar-subr.el
FILE3=ar-subr-x.el
# FILE4=ar-emacs-generics-start-Zf98zM.el
FILE5=ar-thingatpt-utils-map.el
FILE6=ar-thingatpt-utils-core.el
FILE7=ar-thingatpt-utils.el
FILE8=ar-thingatpt-highlight.el
FILE9=ar-thingatpt-transform-delimited.el
FILE10=ar-thingatpt-unpaired-delimited-list-in-rest-list.el
FILE11=ar-thingatpt-classes-in-rest-list.el
FILE12=ar-thingatpt-delimited-list-in-counts-list.el
FILE13=ar-thingatpt-unpaired-delimited-list-in-delimited-list.el
FILE14=ar-thingatpt-classes-in-delimited-list.el
FILE15=ar-thingatpt-peel.el
FILE16=ar-thingatpt-count-ratio.el
FILE17=ar-thingatpt-transform-generic-delimited.el
FILE18=ar-translate-paired-delimiters.el
FILE19=ar-thingatpt-rest-list-in-region-only.el
FILE20=ar-thingatpt-data-forms-aktiv-in-rest-list.el
FILE21=ar-thingatpt-markup.el
FILE22=ar-sexp.el
FILE23=ar-comment-lor.el
FILE24=ar-tptp-mode.el


TEST1=test/ar-thingatpt-utils-tests.el
TEST2=test/ar-paired-delimit-tests.el
TEST3=test/ar-trim-tests.el
TEST4=test/ar-unpaired-delimited-tests.el
TEST5=test/ar-bracketlist-atpt-tests.el
TEST6=test/ar-thingatpt-utils-forward-tests.el
TEST7=test/ar-translate-paired-delimiters-tests.el
TEST8=test/ar-thingatpt-utils-nodelim-classes-tests.el
TEST9=test/ar-thingatpt-utils-delimited-tests.el
TEST10=test/ar-list-atpt-brace-tests.el
TEST11=test/ar-paired-delimited-tests.el
TEST12=test/ar-delimited2delimited-tests.el
TEST13=test/ar-separate-tests.el
TEST14=test/ar-list-atpt-parens-tests.el
TEST15=test/ar-thingatpt-also-delimited-tests.el
TEST16=test/ar-thingatpt-other-delimited-tests.el
TEST17=test/ar-thingatpt-other-position-tests.el
TEST18=test/ar-thingatpt-more-delimited-tests.el
TEST19=test/ar-thingatpt-interactive-tests.el
TEST20=test/ar-thingatpt-utils-comment-or-uncomment-tests.el
TEST21=test/ar-backward-sexp-tests.el
TEST22=test/ar-forward-sexp-tests.el
TEST23=test/ar-tptp-mode-tests.el
TEST24=test/ar-thingatpt-scala-mode-tests.el
TEST25=test/ar-thingatpt-peel-tests.el

h1 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST21 \
-f ert-run-tests-batch-and-exit
}

h22 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST22 \
-f ert-run-tests-batch-and-exit
}

h23 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST23 \
-f ert-run-tests-batch-and-exit
}

h24 () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
\
-load $SETUP1 \
-load $SETUP2 \
-load $TEST24 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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
-load $TEST20 \
-load $TEST21 \
-load $TEST22 \
-load $TEST23 \
-load $TEST24 \
-f ert-run-tests-batch-and-exit
}

# -load $TEST19 \

entfernt () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"test\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
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
-load $FILE19 \
-load $FILE20 \
-load $FILE21 \
-load $FILE22 \
-load $FILE23 \
-load $FILE24 \
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

if [ $IFLOCAL -eq 0 ]; then

    GEN=$HOME/werkstatt/emacs-generics
    WCO=$HOME/werkstatt/thingatpt-utils-core
    ATPT=$HOME/werkstatt/thing-at-point-utils
    WNA=$HOME/werkstatt/numbers-at-point

    cp -pu ${GEN}/ar-subr.el .
    cp -pu ${GEN}/ar-subr-x.el .
    cp -pu ${GEN}/ar-beg-end.el .
    cp -pu ${GEN}/ar-emacs-generics-start-Zf98zM.el .
    cp -pu ${GEN}/ar-navigate.el .
    cp -pu ${GEN}/ar-emacs-generics-backward-forms.el .
    cp -pu ${GEN}/ar-emacs-generics-forward-forms.el .
    cp -pu ${WCO}/ar-thingatpt-utils-core.el .
    cp -pu ${WCO}/ar-thingatpt-basic-definitions.el .
    cp -pu ${WCO}/test/ar-thingatpt-setup-tests.el .

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
	    m) echo "Lade \$TEST22: \"$TEST22\"";h22;;
            # n unten für alle
	    o) echo "Lade \$TEST14: \"$TEST14\"";h23;;
	    p) echo "Lade \$TEST16: \"$TEST16\"";h24;;
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
