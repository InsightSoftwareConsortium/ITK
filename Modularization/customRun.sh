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

#clean up the destination dir
if [ -d $HeadOfModularITKTree ]; then
   rm -rf $HeadOfModularITKTree
fi

# create an index table for searching modules, used by search function in modulizerHelpers.py
export PATH=/usr/bin:$PATH # for  using sort in cygwin
cat Manifest.txt |grep -v '^#' |sed 's/[^ ]* * *//' |sort|uniq|sort -k 1 >ModulePathTable.txt

# copy the utility modules to desitnation
if [ ! -d $HeadOfModularITKTree/ITK/Utilities ];then
  git clone http://itk.org/tmp/modularITKSupport.git $HeadOfModularITKTree/ITK/Utilities
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

cat $logs/newFiles.log  |grep -v Utilities |grep -v CMakeLists.txt |grep -v Modularization |grep -v Header  >$logs/filesNeedsClassify.txt
wc Manifest.txt
wc $logs/missingFiles.log
wc $logs/newFiles.log
