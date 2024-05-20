#!/bin/bash

in_dir="in"
out_dir="out"
log_dir="log"

for i in `seq 0 999`; do
  echo "running $i..."
  digit=`printf "%04d" $i`
  in="${in_dir}/$digit.txt"
  out="${out_dir}/$digit.txt"
  log="${log_dir}/$digit.txt"
  cargo run <$in >$out 2>$log &
  sleep 0.05  # prevent failing lock acqusition
done
wait
