#!/bin/bash

# \author Hans J. Johnson

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
