#!/bin/bash
# \author Hans J. Johnson

#% NAME
#%     cmakeRemoveBlockEndCruft.sh - empty the arguments of CMake block ends
#%
#% SYNOPSIS
#%     cmakeRemoveBlockEndCruft.sh
#%     cmakeRemoveBlockEndCruft.sh -h | --help
#%
#% DESCRIPTION
#%     Rewrite else(...), endif(...), endforeach(...), endfunction(...),
#%     endmacro(...), and endwhile(...) as the same command with empty
#%     parentheses, in every tracked bootstrap, *.cmake, *.cmake.in,
#%     CMakeLists.txt, and *.wrap file at or below the current directory.
#%     Old versions of CMake required these block-end commands to repeat the
#%     arguments of the command that opened the block; that style is no
#%     longer preferred.
#%
#%     The script then stages every change in the work tree and commits it
#%     with the message "STYLE: Remove CMake-language block-end command
#%     arguments".
#%
#% OPTIONS
#%     -h, --help
#%         Print this help and exit.
#%
#% IMPLEMENTATION
#%     Requires git and GNU sed installed as gsed. Run from the top of the
#%     git work tree to cover every file.
#%
#%     The match runs to the last closing parenthesis on the line, so a
#%     trailing comment that contains parentheses is deleted too. Only the
#%     first block-end command on each line is rewritten.
#%
#%     The script writes convert.sed in the current directory and removes it
#%     only when the edit succeeds. The commit step runs regardless and uses
#%     git add -A, so any other change in the work tree, including a leftover
#%     convert.sed, is committed with it. The commit message file is left in
#%     /tmp.
#%
#% EXAMPLES
#%     ./Utilities/ITKMigrationPreparation/cmakeRemoveBlockEndCruft.sh
#%         Rewrite the block ends in an ITK checkout and commit the result.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Utilities/ITKMigrationPreparation/cmakeToLowerCase.sh:
#%         Convert CMake commands to lower case.

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
STYLE: Remove CMake-language block-end command arguments

Ancient versions of CMake required else(), endif(), and similar block
termination commands to have arguments matching the command starting the block.
This is no longer the preferred style.
EOF

# NOTE: MUST USE GNU compliant version of sed
# Run the following shell code:

for c in else endif endforeach endfunction endmacro endwhile; do
    echo 's/\b'"$c"'\(\s*\)(.\+)/'"$c"'\1()/'
done >convert.sed \
&& git ls-files -z -- bootstrap '*.cmake' '*.cmake.in' '*CMakeLists.txt' '*.wrap' \
   | xargs -0 gsed -i -f convert.sed \
&& rm convert.sed

#   | fgrep  -V '^(thirdparty|Utilities/cm|Source/kwsys/)' \
#   | fgrep  -V 'Tests/CMakeTests/While-Endwhile-' \

git add -A && git commit --file ${CMTMSG}
