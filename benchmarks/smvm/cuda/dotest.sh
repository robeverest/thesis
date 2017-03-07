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

SMVM=$(stack exec which -- smvm-cuda)

OUT=out

stack build
mkdir -p $OUT

for matrixFile in ${MATRICES[@]}; do
  matrixName=$(basename -s .mtx $matrixFile)
  outputBase=$OUT/$matrixName

  du -h $matrixFile
  $SMVM $matrixFile --output=$outputBase.html --raw=$outputBase.raw 2>&1 | tee $outputBase.log

  echo -e "\n"
done

