#!/bin/bash

n_testcase=$1

if [[ -z $n_testcase ]]; then
  echo "usage: $0 <n-testcase>"
  exit 1
fi

in_dir="in"
out_dir="out"
log_dir="log"

cargo build --release

mkdir -p $out_dir
mkdir -p $log_dir

n_batch=50

for ((i=0; i<n_testcase; i++)); do
  digit=`printf "%04d" $i`
  in="${in_dir}/$digit.txt"
  out="${out_dir}/$digit.txt"
  log="${log_dir}/$digit.txt"
  ./target/release/ahc033 <$in >$out 2>$log &
  if (((i+1)%n_batch == 0)); then
    echo "Waiting jobs until $i th..."
    wait
  fi
done

echo "Waiting jobs until $i th..."
wait
