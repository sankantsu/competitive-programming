#!/bin/bash

in_dir="in"
out_dir="out"
log_dir="log"

cargo build --release

mkdir -p $out_dir
mkdir -p $log_dir

for i in `seq 0 999`; do
  echo "running $i..."
  digit=`printf "%04d" $i`
  in="${in_dir}/$digit.txt"
  out="${out_dir}/$digit.txt"
  log="${log_dir}/$digit.txt"
  ./target/release/ahc033 <$in >$out 2>$log &
done
wait
