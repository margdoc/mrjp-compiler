#!/bin/bash

usage () {
    echo "Usage: $0 <file> [-qu] [--official|--students|--margdoc|--all]"
    echo "  <file>      path to the compiler binary"
    echo "  -q          quiet mode"
    echo "  -u          run uncertain tests"
    echo "  --official  only official's tests"
    echo "  --students  only students' tests"
    echo "  --margdoc   only margdoc's tests"
    echo "  --all       all tests"
}

if [ $# -eq 0 ]; then
    usage
    exit 1
fi


COMPILER=$1
tests_to_skip=( # runtime tests
    "tests/mrjp-tests/gr5/bfs.lat" #?
    "tests/mrjp-tests/bad/runtime/error.lat"
    "tests/mrjp-tests/bad/infinite_loop/infinite_while.lat"
)
uncertain_tests=(
    "tests/margdoc/bad/bad003.lat"
    "tests/margdoc/bad/bad004.lat"
    "tests/margdoc/bad/bad005.lat"
    "tests/margdoc/changing_array_length.lat"
    "tests/margdoc/multidimensional_array.lat"
    "tests/mrjp-tests/bad/semantic/declaration_in_if.lat"
    "tests/mrjp-tests/gr5/lista2Kierunkowa.lat"
    "tests/mrjp-tests/gr5/mergeSort.lat"
    "tests/mrjp-tests/good/hardcore/tail_call_optymization.lat"
)


if [ ! -f $COMPILER ]; then
    echo "File $COMPILER does not exist"
    exit 1
fi

if [ ! -x $COMPILER ]; then
    echo "File $COMPILER is not executable"
    exit 1
fi

everything_ok=1
should_fail_appeared=0
quiet=0
test_group="tests"
run_uncertain_tests=0

for opt in "${@:2}"; do
    case $opt in
        -q)
            quiet=1
            ;;
        --official)
            test_group="tests/official"
            ;;
        --students)
            test_group="tests/mrjp-tests"
            ;;
        --margdoc)
            test_group="tests/margdoc"
            ;;
        --all)
            test_group="tests"
            ;;
        -u)
            run_uncertain_tests=1
            ;;
        --help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown option: $opt"
            usage
            exit 1
            ;;
    esac
done

tmp_file=$(mktemp)

for file in `find $test_group -name '*.lat' | sort`
do
    # if [[ " ${tests_to_skip[@]} " =~ " ${file} " ]]; then
    #     if [ $quiet -eq 0 ]; then
    #         echo -e "\e[33mSkipping runtime test $file\e[0m"
    #     fi
    #     continue
    # fi
    if [[ " ${uncertain_tests[@]} " =~ " ${file} " ]]; then
        if [ $run_uncertain_tests -eq 0 ]; then
            if [ $quiet -eq 0 ]; then
                echo -e "\e[33mSkipping uncertain test $file\e[0m"
            fi
            continue
        fi
    fi

    timeout 2 ./$COMPILER $file &>/dev/null
    EXIT_STATUS=$?
    compiled_file=${file%.lat}

    if [ $EXIT_STATUS -eq 124 ]; then
        if [ $quiet -eq 0 ]; then
            echo -e "Testing $file \033[0;31mTIMEOUT\e[0m"
        fi
        everything_ok=0
        rm -f $compiled_file $compiled_file.s
        continue
    fi

    should_pass=0
    if [ -f ${file%.lat}.output ]; then
        should_pass=1
    fi

    if [ $EXIT_STATUS -ne 0 ]; then
        if [ $should_pass -eq 1 ]; then
            echo -e "Testing $file \033[0;31mERROR (typechecker)\033[0m"
            everything_ok=0
            rm -f $compiled_file $compiled_file.s
            continue
        fi
    fi

    if [ -f $compiled_file.input ]; then
        timeout 2 ./$compiled_file < $compiled_file.input &>$tmp_file
    else
        timeout 2 ./$compiled_file &>$tmp_file
    fi
    EXIT_STATUS=$?
    rm -f $compiled_file $compiled_file.s

    failed=0
    if [ $EXIT_STATUS -eq 124 ]; then
        failed=1
    elif [ $EXIT_STATUS -ne 0 ]; then
        failed=1
    fi

    if [ $should_pass -eq 1 ]; then
        diff -q $tmp_file ${file%.lat}.output &>/dev/null
        if [ $? -ne 0 ]; then
            failed=1
        fi
    fi

    if [ $should_pass -eq 1 ]; then
        if [ $failed -eq 1 ]; then
            echo -e "Testing $file \033[0;31mERROR\033[0m"
            everything_ok=0
            continue
        else
            if [ $quiet -eq 0 ]; then
                echo -e "Testing $file \033[0;32mPASSED\033[0m"
            fi
        fi
    else
        if [ $failed -eq 0 ]; then
            echo -e "Testing $file \033[0;31mERROR (should fail)\033[0m"
            everything_ok=0
            should_fail_appeared=1
            continue
        else
            if [ $quiet -eq 0 ]; then
                echo -e "Testing $file \033[0;32mPASSED (failed)\033[0m"
            fi
        fi
    fi
done

rm $tmp_file

if [ $everything_ok -eq 1 ]; then
    echo -e "\nEVERYTHING \033[0;32mPASSED\033[0m :)"
else
    echo -e "\nSome tests \033[0;31mFAILED\033[0m :c"
    echo "> You don't have to pass all students' and margdoc's tests. If you don't agree with some of them, you can add them to uncertain tests."
fi

if [ $should_fail_appeared -eq 1 ]; then
    echo -e "> \"\033[0;31mERROR (should fail)\033[0m\" means that the test should fail but it passed."
fi
