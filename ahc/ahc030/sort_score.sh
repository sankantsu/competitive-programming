#!/bin/bash
logdir=$1
grep -r 'Score' $logdir | sort -nk 3
