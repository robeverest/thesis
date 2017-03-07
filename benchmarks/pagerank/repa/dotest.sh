#!/bin/bash
# vim: nospell

set -e

PAGERANK=$(stack exec which -- pagerank)

OUT=out
LINKS='../links-simple-sorted.txt'
TITLES='../titles-sorted.txt'

stack build
mkdir -p $OUT

for THREADS in $(seq 1 24); do
  outputBase=$OUT/out-N$THREADS

  numa_spreadout $THREADS $PAGERANK +RTS -N$THREADS -qa -A30M -RTS -rank-internal $LINKS $TITLES -- --output=$outputBase.html --raw=$outputBase.raw 2>&1 | tee $outputBase.log

  echo -e "\n"
done

