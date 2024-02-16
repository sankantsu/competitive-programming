#!/bin/bash

tester="tools/target/debug/tester"
bin="$1"
m="$2"

if [[ -z "$bin" ]]; then
  echo "usage: run_all.sh <executable>"
  exit 1
fi

if [[ -z "$m" ]]; then
  indir="tools/in"
  outdir="out/default"
  logdir="log/default"
else
  indir="in/$2"
  outdir="out/$2"
  logdir="log/$2"
fi

mkdir -p "${outdir}"
mkdir -p "${logdir}"
for i in `seq 0 99`; do
  echo "Executing $i th testcase ..."
  digit=`printf "%04d" $i`
  infile="${indir}/${digit}.txt"
  outfile="${outdir}/${digit}.txt"
  logfile="${logdir}/${digit}.txt"
  $tester $bin <$infile >$outfile 2>$logfile
done
