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

rm -f ${OUT_DATA}/anat1*

if ${NT} -mod_hdr -prefix ${OUT_DATA}/anat1 \
-infiles ${DATA}/anat0.nii \
-mod_field qoffset_x -17.325 -mod_field slice_start 1 \
-mod_field descrip "beer, brats and cheese, mmmmm..."
then
echo ""
else
echo mod_field failed
exit 1
fi

if ${NT} -diff_hdr -infiles ${DATA}/anat0.nii ${OUT_DATA}/anat1.nii
then
echo diff_hdr failed '(no difference seen)!'
exit 1
else
echo ""
fi

if ${NT} -add_afni_ext "wow, my first extension" \
           -add_afni_ext "look, my second.." \
           -overwrite -infiles ${OUT_DATA}/anat1.nii
then
echo ""
else
echo add_afni_ext failed
exit 1
fi

if ${NT} -disp_exts -infiles ${DATA}/anat0.nii ${OUT_DATA}/anat1.nii
then
echo ""
else
echo disp_exts failed
exit 1
fi


if ${NT} -diff_hdr -infiles ${DATA}/anat0.nii ${OUT_DATA}/anat1.nii
then
echo diff_hdr failed '(no difference seen)!'
exit 1
else
echo ""
fi

exit 0
