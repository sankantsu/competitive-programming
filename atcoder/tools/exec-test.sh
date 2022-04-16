#!/bin/bash

usage() {
    echo "usage: "`basename $0`" <executable> <task_id>"
}

if [[ $# -ne 2 ]]; then
    usage
fi

executable=$1
task_id=$2

dir=testcases/"$task_id"
sample_num=$(echo $(ls "$dir" | wc -l)"/2" | bc)
for num in `seq "$sample_num"`; do
    sample_in="$dir"/in-$num.txt
    sample_out="$dir"/out-$num.txt
    echo "testcase $num"
    if diff --strip-trailing-cr <("$executable" < "$sample_in") "$sample_out"; then
        echo "PASSED!"
    fi
done
