#!/bin/bash

# \author Hans J. Johnson

#% NAME
#%     cmakeToLowerCase.sh - convert upper-case CMake commands to lower case
#%
#% SYNOPSIS
#%     cmakeToLowerCase.sh
#%     cmakeToLowerCase.sh -h | --help
#%
#% DESCRIPTION
#%     Rewrite every upper-case CMake command name followed by an opening
#%     parenthesis, such as SET( or IF (, in lower case, in every tracked
#%     bootstrap, *.cmake, *.cmake.in, CMakeLists.txt, and *.wrap file at or
#%     below the current directory. Old CMake versions required upper-case
#%     commands; command names later became case-insensitive, and lower case
#%     is now the preferred style.
#%
#%     The script then stages every change in the work tree and commits it
#%     with the message "STYLE: Convert CMake-language commands to lower
#%     case".
#%
#% OPTIONS
#%     -h, --help
#%         Print this help and exit.
#%
#% IMPLEMENTATION
#%     Requires git, cmake, and GNU sed installed as gsed. The list of
#%     command names comes from the cmake found on PATH. Run from the top of
#%     the git work tree to cover every file.
#%
#%     The match does not know CMake syntax, so matching upper-case text in
#%     strings and comments is converted too.
#%
#%     The script writes convert.sed in the current directory and removes it
#%     only when the edit succeeds. The commit step runs regardless and uses
#%     git add -A, so any other change in the work tree, including a leftover
#%     convert.sed, is committed with it. The commit message file is left in
#%     /tmp.
#%
#% EXAMPLES
#%     ./Utilities/ITKMigrationPreparation/cmakeToLowerCase.sh
#%         Convert the commands in an ITK checkout and commit the result.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Utilities/Maintenance/HowToCreateTheCMakeCaseConversion.txt:
#%         A manual vim-based method for the same conversion.
#%     Utilities/ITKMigrationPreparation/cmakeRemoveBlockEndCruft.sh:
#%         Empty the arguments of CMake block-end commands.

usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

case "${1-}" in
  -h | --help)
    usage
    exit 0
    ;;
esac

this_script_name=$(basename $0)
CMTMSG=$(mktemp -q /tmp/${this_script_name}.XXXXXX)

cat > ${CMTMSG} << EOF
STYLE: Convert CMake-language commands to lower case

Ancient CMake versions required upper-case commands.  Later command names
became case-insensitive.  Now the preferred style is lower-case.
EOF

#Run the following shell code:
## NOTE: MUST USE GNU compliant version of sed
cmake --help-command-list \
| grep -v "cmake version" \
| while read c; do
    echo 's/\b'"$(echo $c | tr '[:lower:]' '[:upper:]')"'\(\s*\)(/'"$c"'\1(/g'
  done >convert.sed \
&& git ls-files -z -- bootstrap '*.cmake' '*.cmake.in' '*CMakeLists.txt' '*.wrap' \
   | xargs -0 gsed -i -f convert.sed \
&& rm convert.sed

git add -A && git commit --file ${CMTMSG}
