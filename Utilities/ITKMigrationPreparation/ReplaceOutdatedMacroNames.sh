#!/bin/bash
# \author Hans J. Johnson

#% NAME
#%     ReplaceOutdatedMacroNames.sh - replace outdated ITK C++11 macros
#%
#% SYNOPSIS
#%     ReplaceOutdatedMacroNames.sh
#%     ReplaceOutdatedMacroNames.sh -h | --help
#%
#% DESCRIPTION
#%     Replace each outdated ITK compatibility macro, such as ITK_NULLPTR,
#%     ITK_OVERRIDE, and ITK_CONSTEXPR, with its modern name or the C++11
#%     keyword it stands for, in the tracked files at or below the current
#%     directory. After each macro the script stages every change in the
#%     work tree, commits it with a message starting "COMP: Use C++11", and
#%     prints a warning to review the changes. The script is not meant to be
#%     bullet-proof.
#%
#%     Paths containing Migration, CMakeLists.txt, .cmake, or
#%     itk_compiler_detection.h are skipped, as is this script.
#%
#% OPTIONS
#%     -h, --help
#%         Print this help and exit.
#%
#% IMPLEMENTATION
#%     Requires git and sed. The script calls sed -i with an empty argument
#%     in the BSD style; GNU sed still edits the files but reports that it
#%     cannot read a file with an empty name.
#%
#%     Replacement is plain text with no word boundaries, so longer names
#%     that contain a macro name are changed too. Because ITK_NOEXCEPT is
#%     replaced before ITK_NOEXCEPT_EXPR, the latter becomes noexcept_EXPR.
#%     Each commit uses git add -A, so other changes in the work tree are
#%     committed with the first replacement. A macro with no remaining uses
#%     still reaches git commit, which fails with nothing to commit; such
#%     failures do not stop the script.
#%
#% EXAMPLES
#%     ./Utilities/ITKMigrationPreparation/ReplaceOutdatedMacroNames.sh
#%         Replace the macros in an ITK checkout, run from its top, with one
#%         commit per macro.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.

usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

case "${1-}" in
  -h | --help)
    usage
    exit 0
    ;;
esac

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

ReplaceCXXString ITK_NOEXCEPT_OR_THROW             ITK_NOEXCEPT
ReplaceCXXString ITK_HAS_CXX11_STATIC_ASSERT       ITK_COMPILER_CXX_STATIC_ASSERT
ReplaceCXXString ITK_DELETE_FUNCTION               ITK_DELETED_FUNCTION
ReplaceCXXString ITK_HAS_CPP11_ALIGNAS             ITK_COMPILER_CXX_ALIGNAS
ReplaceCXXString ITK_ALIGNAS                       alignas
ReplaceCXXString ITK_ALIGNOF                       alignof
ReplaceCXXString ITK_CONSTEXPR                     constexpr
ReplaceCXXString ITK_EXTERN_TEMPLATE               extern
ReplaceCXXString ITK_FINAL                         final
ReplaceCXXString ITK_NOEXCEPT                      noexcept
ReplaceCXXString ITK_NOEXCEPT_EXPR                 noexcept
ReplaceCXXString ITK_NULLPTR                       nullptr
ReplaceCXXString ITK_OVERRIDE                      override
ReplaceCXXString ITK_STATIC_ASSERT                 static_assert
ReplaceCXXString ITK_THREAD_LOCAL                  thread_local
