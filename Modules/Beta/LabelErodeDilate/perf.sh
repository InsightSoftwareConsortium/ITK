#!/bin/bash

REP=10

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