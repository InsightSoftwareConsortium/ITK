#!/bin/bash
# \author Hans J. Johnson
#
# Script to process a directory to replace
# outdated macro names with their modern
# conformant names

function ReplaceCXXString()
{
oldstring="$1"
newstring="$2"

# NOTE: Skip processing this file
# NOTE: Skip processing the Migration directory in ITK
git grep -l "${oldstring}" | \
  fgrep -v Migration | \
  fgrep -v ITKv5Preparation | \
  fgrep -v itkMacro.h | \
  fgrep -v itk_compiler_detection.h | fgrep -v CMakeLists.txt |fgrep -v .cmake | \
  xargs sed -i '' -e "s/${oldstring}/${newstring}/g"

git add -A
git commit -m"COMP: Deprecate use itkGetObjectMacro

git grep -l \"${oldstring}\" | \
  fgrep -v itk_compiler_detection.h | fgrep -v CMakeLists.txt |fgrep -v .cmake | \
  xargs sed -i '' -e \"s/${oldstring}/${newstring}/g\"

For a full description of the rational for this change
see:

commit 08df6afeb32e363104bf1e7a043f4b38d524c364
Author: Hans Johnson <hans-johnson@uiowa.edu>
Date:   Sun Dec 2 14:07:01 2012 -0600
ENH: Get function accessible from const objects


"

echo "WARNING:  This script is not intended to be bullet-proof."
echo "WARNING:  Please carefully review all changes made to ensure proper behavior."
echo "WARNING:  Auto replacements were for itkGetConstObjectMacro, but"
echo "          itkGetModifiableObjectMacro is needed in some cases."
echo "          manual fixing may be required."
}

ReplaceCXXString itkGetObjectMacro itkGetConstObjectMacro
