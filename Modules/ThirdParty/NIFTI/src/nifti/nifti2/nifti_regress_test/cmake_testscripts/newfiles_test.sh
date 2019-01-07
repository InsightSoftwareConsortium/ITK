#!/bin/sh

if [ $# -lt 2 ]
then
echo Missing nifti tool and Binary directory name
exit 1
fi

NT=$1
DATA=$2
OUT_DATA=$(dirname ${DATA}) #Need to write to separate directory
cd ${OUT_DATA}

rm -f ${OUT_DATA}/new* ${OUT_DATA}/ncopy*

# test writing various output file types
${NT} -make_im -prefix ${OUT_DATA}/new1.hdr
${NT} -make_im -prefix ${OUT_DATA}/new2.hdr.gz
${NT} -make_im -prefix ${OUT_DATA}/new3.img.gz
${NT} -make_im -prefix ${OUT_DATA}/new4.nii.gz
${NT} -make_im -prefix ${OUT_DATA}/new5.nia

# test reading them
${NT} -copy_im -prefix ${OUT_DATA}/ncopy1.nii -infiles ${OUT_DATA}/new1.hdr
${NT} -copy_im -prefix ${OUT_DATA}/ncopy2.nii -infiles ${OUT_DATA}/new2.hdr.gz
${NT} -copy_im -prefix ${OUT_DATA}/ncopy3.nii -infiles ${OUT_DATA}/new3.img.gz
${NT} -copy_im -prefix ${OUT_DATA}/ncopy4.nii -infiles ${OUT_DATA}/new4.nii.gz
${NT} -copy_im -prefix ${OUT_DATA}/ncopy5.nii -infiles ${OUT_DATA}/new5.nia

# verify that they are all the same
set count = 0
for index in 2 3 4 5 ; do
    diff ${OUT_DATA}/ncopy1.nii ${OUT_DATA}/ncopy$index.nii
    if [ $? -ne 0 ] ; then
        echo "-- failure on test index $index --"
        exit 1
    fi
done
