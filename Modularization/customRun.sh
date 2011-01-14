#!/bin/bash

# Excute the modularization python scripts.
# This scripts is only for developer's convinience of running the modularization.

# Run the scripts from the Modularization directory of the monolithic ITK.
# Need to feed the path variable:  HeadOfModularITKTree.


HeadOfModularITKTree=$1


if [ $# -ne 1 ]
then
  echo "Usage: ./customRun.sh  [path of modular ITK] "
  exit
fi

######################################################################
HeadOfMonolithicITKTree='..' # This is the origin ITK dir
logs=$HeadOfModularITKTree/logs

# excute the modulizer.py with the default "clean up  the modular ITK tree" option: 'y'
./modulizer.py  $HeadOfMonolithicITKTree $HeadOfModularITKTree


# dealing with itk-common
./specialModuleFix.py  $HeadOfMonolithicITKTree $HeadOfModularITKTree


# handling data ( Testing/data and Examples/data)
if [ ! -d $HeadOfModularITKTree/data ];then
  cp -r ../Testing/Data $HeadOfModularITKTree/data
fi

if [ ! -d $HeadOfModularITKTree/Examples ];then
  cp -r ../Examples  $HeadOfModularITKTree/Examples
fi

grep -v Wrapping $logs/newFiles.log | \
grep -v Utilities | \
grep -v Validation | \
grep -v Examples | \
grep -v Testing | \
grep -v Review | \
grep -v CMake | \
grep -v Documentation | \
grep -v Modularization | \
tee  $logs/filesToClassify.log

wc Manifest.txt
wc $logs/filesToClassify.log
wc $logs/missingFiles.log
