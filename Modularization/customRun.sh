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


# create an index table for searching modules, used by search function in modulizerHelpers.py
cat Manifest.txt |grep -v '^#' |sed 's/[^ ]* * *//' |sort|uniq|sort -k 1 >ModulePathTable.txt

# copy the utility modules to desitnation
if [ ! -d $HeadOfModularITKTree/Utilities ];then
  git clone http://itk.org/tmp/modularITKSupport.git $HeadOfModularITKTree/Utilities
fi
# copy the cmake codes to destnation
if [ ! -d $HeadOfModularITKTree ];then
  mkdir $HeadOfModularITKTree
fi
cp -r ./modularITKCMake/* $HeadOfModularITKTree/




# excute the modulizer.py with the default "clean up  the modular ITK tree" option: 'y'
./modulizer.py  $HeadOfMonolithicITKTree $HeadOfModularITKTree


# dealing with itk-common
./specialModuleFix.py  $HeadOfMonolithicITKTree $HeadOfModularITKTree


# handling data ( Testing/data and Examples/Data)
if [ ! -d $HeadOfModularITKTree/Data ];then
  cp -r ../Testing/Data $HeadOfModularITKTree/Data
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
grep -v vnl | \
tee  $logs/filesToClassify.log

wc Manifest.txt
wc $logs/filesToClassify.log
wc $logs/missingFiles.log
