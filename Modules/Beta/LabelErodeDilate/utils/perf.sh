#!/bin/bash

REP=5

echo "parabolic label dilate" > para.log
echo "danielsson method" > danielsson.log
echo "maurer method" > maurerws.log
for th in 1 4 8 12 ; do

    for rad in 2 5 10 20; do
        echo $th $rad
        OPTIONS="--repetitions $REP --threads $th -r $rad -i ../images/HarvardOxford-cort-maxprob-thr50-1mm.nii.gz -o /tmp/tt.nii.gz"

        ./labelSetsDilatePerf $OPTIONS >> para.log
        ./labelSetsDilateWSPerf $OPTIONS >> maurerws.log
        ./labelSetsDilateDanielssonPerf $OPTIONS >> danielsson.log

    done

done

for i in para.log danielsson.log maurerws.log ; do
  hd=$(grep "^Iterations" $i | head -1)
  echo $hd > ${i/log/csv}
  ## get lines starting with iterations
  grep "^$REP" $i >> ${i/log/csv}

done
