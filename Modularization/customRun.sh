#!/bin/bash

# This script needs customization to run.
# It is for the developer's convinience of running the modularization scripts repeatly.

HeadOfMonolithicITKTree=/media/work/src/ITK
HeadOfModularITKTree=/media/work/src/ModularITK/modularITK
# modify according to your paths
./modulizer.py  $HeadOfMonolithicITKTree $HeadOfModularITKTree y

#./testFinder.py    \
#/media/work/src/ITK

#cat ManifestOfITKTests.txt  >> Manifest.txt

#dealing with itk-common
./specialModuleFix.py   ./modulizer.py  $HeadOfMonolithicITKTree $HeadOfModularITKTree


grep -v Wrapping logs/newFiles.log | \
grep -v Utilities | \
grep -v Validation | \
grep -v Examples | \
grep -v Testing | \
grep -v Review | \
grep -v CMake | \
grep -v Documentation | \
tee  logs/filesToClassify.log

wc Manifest.txt
wc logs/filesToClassify.log
wc logs/missingFiles.log
