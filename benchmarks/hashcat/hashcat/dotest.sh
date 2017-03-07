#!/bin/bash
# vim: nospell

set -e

HASHCAT=hashcat-cli64.bin

OUT=out

mkdir -p $OUT

for THREADS in $(seq 1 24); do
  outputBase=$OUT/out-N$THREADS

  numa_spreadout $THREADS $HASHCAT -m0 -b -n$THREADS 2>&1 | tee $outputBase.log

  echo -e "\n"
done

