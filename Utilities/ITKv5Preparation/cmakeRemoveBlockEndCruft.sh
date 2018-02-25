#!/bin/bash
# \author Hans J. Johnson

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
