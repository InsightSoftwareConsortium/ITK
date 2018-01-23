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
  fgrep -v ReplaceOutdatedMacroNames.sh | \
  fgrep -v Migration | \
  fgrep -v itk_compiler_detection.h | fgrep -v CMakeLists.txt |fgrep -v .cmake | \
  xargs sed -i '' -e "s/${oldstring}/${newstring}/g"

git add -A
git commit -m"COMP:  Use C++11 ${newstring} directly

git grep -l \"${oldstring}\" | \
  fgrep -v itk_compiler_detection.h | fgrep -v CMakeLists.txt |fgrep -v .cmake | \
  xargs sed -i '' -e \"s/${oldstring}/${newstring}/g\"
"

echo "WARNING:  This script is not intended to be bullet-proof."
echo "WARNING:  Please carefully review all changes made to ensure proper behavior."
}

ReplaceCXXString ITK_NOEXCEPT_OR_THROW ITK_NOEXCEPT
ReplaceCXXString ITK_HAS_CXX11_STATIC_ASSERT ITK_COMPILER_CXX_STATIC_ASSERT
ReplaceCXXString ITK_DELETE_FUNCTION ITK_DELETED_FUNCTION
ReplaceCXXString ITK_HAS_CPP11_ALIGNAS ITK_COMPILER_CXX_ALIGNAS
