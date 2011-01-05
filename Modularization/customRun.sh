#!/bin/bash

# Excute the modularization python scripts.
# This scripts is only for developer's convinience of running the modularization.

# Run the scripts from the Modularization directory of the monolithic ITK.
# Need to set the path variable:  HeadOfModularITKTree.

################   output path needs to be specified   ###############
# This directory will be cleaned up first (if exisits) when running the script,
# so be carful not setting it to wrong dirs.
HeadOfModularITKTree=/media/work/src/modularITK
######################################################################

HeadOfMonolithicITKTree='..' # This is the origin ITK dir
logs=$HeadOfModularITKTree/logs


# excute the modulizer.py with the default "clean up  the modular ITK tree" option: 'y'
./modulizer.py  $HeadOfMonolithicITKTree $HeadOfModularITKTree y


# dealing with itk-common
./specialModuleFix.py  $HeadOfMonolithicITKTree $HeadOfModularITKTree


# handling data ( Testing/data and Examples/data)
cp -r ../Testing/Data $HeadOfModularITKTree/data
cp -r ../Examples  $HeadOfModularITKTree/Examples


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
