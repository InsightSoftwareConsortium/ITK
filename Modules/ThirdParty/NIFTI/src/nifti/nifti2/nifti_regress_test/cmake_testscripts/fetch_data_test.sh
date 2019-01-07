#!/bin/sh
#
# arg[1] is TESTING_BINARY_DIR
if [ $# -lt 1 ]
then
echo Missing Binary directory name
exit 1
fi

## ensure that a clean download can occur
#  by preventing downloads with .1 .2 .3 suffix
#  if a previous tgz file is present
rm -f nifti_regress_data.tgz*

if cd $1
then
echo working in `pwd`
else
echo can\'t cd to $1
exit 1
fi

if wget -nd http://nifti.nimh.nih.gov/pub/dist/data/nifti_regress_data.tgz
then
  echo wget succeeded
else
  ## Note:  On MacOSX, wget is not installed by default, but curl is
  ##        so use this fall back strategy when wget can not be found
  if curl -O http://nifti.nimh.nih.gov/pub/dist/data/nifti_regress_data.tgz
  then
    echo curl succeeded
  else
    echo Both curl and wget failed
    exit 1
  fi
fi

if tar xzvf nifti_regress_data.tgz
then
echo ""
else
echo failed tar xzvf nifti_regress_data.tgz
exit 1
fi

if rm -f nifti_regress_data.tgz*
then
echo ""
else
echo can\'t remove ../nifti_regress_data.tgz
exit 1
fi

exit 0
