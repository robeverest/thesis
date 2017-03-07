#!/bin/bash
# vim: nospell

set -e

declare -a MATRICES=('../../../../data/matrices/dense2.mtx'
                     '../../../../data/matrices/pdb1HYS.mtx'
                     '../../../../data/matrices/consph.mtx'
                     '../../../../data/matrices/cant.mtx'
                     '../../../../data/matrices/pwtk.mtx'
                     '../../../../data/matrices/rma10.mtx'
                     '../../../../data/matrices/shipsec1.mtx'
                     '../../../../data/matrices/rail4284.mtx'
                     '../../../../data/TSOPF_FS_b300_c2/TSOPF_FS_b300_c2.mtx'
                     '../../../../data/FullChip/FullChip.mtx'
                     '../../../../data/dielFilterV2real/dielFilterV2real.mtx'
                     '../../../../data/Flan_1565/Flan_1565.mtx'
                     '../../../../data/Queen_4147/Queen_4147.mtx'
                     '../../../../data/nlpkkt240/nlpkkt240.mtx'
                     '../../../../data/HV15R/HV15R.mtx'
                    )

THREADS=1
SMVM=$(stack exec which -- smvm-acc)

OUT=out-cpu-N$THREADS

stack build
mkdir -p $OUT

for matrixFile in ${MATRICES[@]}; do
  matrixName=$(basename -s .mtx $matrixFile)
  outputBase=$OUT/$matrixName-N$THREADS

  du -h $matrixFile
  numa_spreadout $THREADS $SMVM $matrixFile --output=$outputBase.html --raw=$outputBase.raw --time-limit=20 +RTS -N$THREADS -qa -A30M -RTS 2>&1 | tee $outputBase.log

  echo -e "\n"
done

