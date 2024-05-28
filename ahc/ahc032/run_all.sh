#!/bin/zsh

executable=./a.out

mkdir -p out/
mkdir -p log/
for i in {0000..0299}; do
  in="in/$i.txt"
  out="out/$i.out"
  log="log/$i.log"
  $executable <$in >$out 2>$log &
done
wait
