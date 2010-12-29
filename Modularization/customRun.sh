#!/bin/bash

# Excute the modularization python scripts.
# This scripts is only for developer's convinience of running modularization repteatly.

# To run the script, first modify the following two variables:  HeadOfModularITKTree
# and HeadOfMonolithicITKTree.


################ path variables needs to be modified  ###############

HeadOfMonolithicITKTree='..' # This is the origin ITK dir
HeadOfModularITKTree=/tmp/src/modularITK  # This dir will be cleaned up every time running the script, so be carful not setting it to wrong dirs.

######################################################################




logs=$HeadOfModularITKTree/logs

# excute the modulizer.py with the default "clean up  the modular ITK tree" option: 'y'
./modulizer.py  $HeadOfMonolithicITKTree $HeadOfModularITKTree y


#dealing with itk-common
./specialModuleFix.py  $HeadOfMonolithicITKTree $HeadOfModularITKTree


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
