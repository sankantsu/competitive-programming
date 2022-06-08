#!/bin/bash

board_size=$1

gen=bin/gen
solve=bin/solve
score=bin/score
check_length=bin/check-length

scores=scores

rm $scores
for i in {1..100}; do
    echo "----------"
    echo "$i th test"
    board=`tempfile`
    ans=`tempfile`
    $gen $board_size >$board
    if time $solve <$board >$ans; then
        :
    else
        echo "error occured!!"
        cat $board >board
        cat $ans >ans
        rm $board $ans
        exit
    fi
    cat $board $ans | $score | tee -a $scores
    rm $board $ans
done
