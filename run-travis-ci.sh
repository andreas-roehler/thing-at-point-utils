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

TEST1=test/ar-thing-at-point-utils-setup-tests.el
TEST2=test/thing-at-point-utils-tests.el
TEST3=test/ar-paired-delimit-tests.el
TEST4=test/ar-trim-test.el
TEST5=test/ar-unpaired-delimited-tests.el
TEST7=test/ar-thing-at-point-utils-forward-tests.el
TEST8=test/translate-paired-delimiters-test.el
TEST9=test/ar-thing-at-point-utils-nodelim-classes-test.el
TEST10=test/ar-tatpt-utils-delimited-tests.el
TEST11=test/thingatpt-transform-delimited-test.el
TEST12=test/ar-list-atpt-brace-ert-tests.el
TEST13=test/ar-paired-delimited-tests.el
TEST14=test/ar-peel-tests.el
TEST15=test/delimited2delimited-tests.el
TEST16=test/ar-separate-tests.el
TEST17=test/ar-list-atpt-parens-ert-tests.el
TEST18=test/ar-bracketlist-atpt-ert-tests.el
TEST19=test/ar-thing-atpt-other-test.el
TEST20=test/ar-delimited2delimited-tests.el

# TEST11 and TEST14 not ready

if [ -s emacs24 ]; then
    EMACS=emacs24
else
    EMACS=emacs
fi

echo "\$EMACS: $EMACS"

hier () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"DIR1\"))" \
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
-load $TEST1 \
-load $TEST2 \
-load $TEST3 \
-load $TEST4 \
-load $TEST5 \
-load $TEST7 \
-load $TEST8 \
-load $TEST9 \
-load $TEST10 \
-load $TEST12 \
-load $TEST13 \
-load $TEST15 \
-load $TEST16 \
-load $TEST17 \
-load $TEST18 \
-load $TEST19 \
-f ert-run-tests-batch-and-exit
}

entfernt () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
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
\
-load $TEST1 \
-load $TEST2 \
-load $TEST3 \
-load $TEST4 \
-load $TEST5 \
-load $TEST7 \
-load $TEST8 \
-load $TEST9 \
-load $TEST10 \
-load $TEST12 \
-load $TEST13 \
-load $TEST15 \
-load $TEST16 \
-load $TEST17 \
-load $TEST18 \
-load $TEST19 \
-f ert-run-tests-batch-and-exit
}


# This var is set in my bashrc to 0
ORT=${ORT:-1}

echo "\$ORT: $ORT"

if [ $ORT -eq 0 ]; then

    echo "cp -u $HOME/werkstatt/thingatpt-utils-core/ar-subr.el $PWD"
    cp -u $HOME/werkstatt/thingatpt-utils-core/ar-subr.el $PWD
    echo "cp -u $HOME/werkstatt/thingatpt-utils-core/beg-end.el $PWD"
    cp -u $HOME/werkstatt/thingatpt-utils-core/beg-end.el $PWD
    cp -u $HOME/werkstatt/thingatpt-utils-core/thingatpt-utils-map.el $PWD
    echo "cp -u $HOME/werkstatt/thingatpt-utils-core/thingatpt-utils-map.el $PWD"
    cp -u $HOME/werkstatt/thingatpt-utils-core/thingatpt-utils-core.el $PWD

    echo "cp -u $HOME/werkstatt/thingatpt-utils-core/thingatpt-utils-core.el $PWD"
    hier
    echo "Lade \$DIR6 und \$DIR7"
else
    echo "entfernt"
    echo "Lade testumgebung \"ENTFERNT\""
    entfernt
fi

# -load $FILE3 \
