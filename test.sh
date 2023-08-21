#!/bin/bash

TEST_DIR=./test
SKIP_TESTS=(benchmark)
CLOX=./build/clox
TMP_DIR=./build/test # Removed at the end.

TEST_OUTPUT=$TMP_DIR/test_output
CLOX_OUTPUT=$TMP_DIR/clox_output
TEST_ERRORS=$TMP_DIR/test_errors
CLOX_ERRORS=$TMP_DIR/clox_errors

CLOX_STDERR=$TMP_DIR/clox_stderr

BOLD=$(tput bold)
GOOD=$(tput setaf 2)
BAD=$(tput setaf 1)
MID=$(tput setaf 3)
NORMAL=$(tput sgr0)

# read_test_output <file>
#
# Read expected test script output to stdout.
function read_test_output() {
    grep -oP "// expect: \K.*$" "$1"
}

# read_test_errors <file>
#
# Read expected test script errors to stdout.
# Does not support mixing "// Error ..." and "// [line ..." error declarations.
function read_test_errors() {
    grep -oP "// \K\[line .*$" "$1" |
        sed "s/^\[line \([[:digit:]]*\)\] \(.*\)$/\1\terror\t\2/"
    grep -noP "// \KError .*$" "$1" |
        sed "s/\([[:digit:]]*\):\(.*\)$/\1\terror\t\2/"
}

# read_clox_errors
#
# Read clox stderr output and convert it to a format easier to test.
function read_clox_errors() {
    local LINE
    function next_line() {
        read -r LINE
    }

    while next_line; do
        if [[ $LINE =~ ^\[line.*$ ]]; then
            # Compile error, skip line printout
            echo $LINE | sed "s/^\[line \([[:digit:]]*\)\] \(.*\)$/\1\terror\t\2/"
            next_line
            next_line
        # else
            # Runtime error
        fi
    done
}

# run_test <test_file>
#
# Test against the test file.
# Return 1 if output does not match.
# Return 2 if errors do not match.
function run_test() {
    read_test_output $1 >$TEST_OUTPUT
    read_test_errors $1 >$TEST_ERRORS

    $CLOX "$1" \
        >$CLOX_OUTPUT \
        2>$CLOX_STDERR
    read_clox_errors <$CLOX_STDERR >$CLOX_ERRORS

    if ! diff $TEST_OUTPUT $CLOX_OUTPUT >/dev/null; then
        return 1
    fi

    if ! diff $TEST_ERRORS $CLOX_ERRORS >/dev/null; then
        return 2
    fi
}

FAILED=0
PASSED=0

# do_test <test_path>
#
# Do the test and report the result. The test can be a .lox script or a
# directory of other tests.
function do_test() {
    local TEST_NAME=$1
    local TEST_PATH=$TEST_DIR/$1

    for SKIP_TEST in ${SKIP_TESTS[@]}; do
        if [[ $TEST_NAME = $SKIP_TEST ]]; then
            return
        fi
    done

    if [[ -d $TEST_PATH ]]; then
        for SUBTEST in $(ls $TEST_PATH); do
            do_test $TEST_NAME/$SUBTEST
        done
    else
        echo -n "$BOLD${TEST_NAME%.lox} "

        # We do not support runtime error testing just yet.
        if grep -q "runtime error" $TEST_PATH; then
            echo "${MID}skip$NORMAL"
            return
        fi

        run_test $TEST_PATH
        RESULT=$?

        if [[ $RESULT == 0 ]]; then
            echo "${GOOD}pass$NORMAL"
            PASSED=$((PASSED + 1))
            return
        fi

        echo "${BAD}fail$NORMAL"
        FAILED=$((FAILED + 1))

        if [[ $RESULT == 1 ]]; then
            echo "  Expected output:"
            cat $TEST_OUTPUT
        elif [[ $RESULT == 2 ]]; then
            echo "  Expected errors:"
            cat $TEST_ERRORS
        fi

        if [[ -s $CLOX_OUTPUT ]]; then
            echo "  Clox output:"
            cat $CLOX_OUTPUT
        fi

        if [[ -s $CLOX_ERRORS ]]; then
            echo "  Clox errors:"
            cat $CLOX_ERRORS
        fi
    fi
}

if [[ ! -d $TMP_DIR ]]; then
    mkdir -p $TMP_DIR
fi

for TEST in $(ls $TEST_DIR); do
    do_test $TEST
done

echo "$((FAILED + PASSED)) tests, $PASSED passed, $FAILED failed"
rm -r $TMP_DIR
