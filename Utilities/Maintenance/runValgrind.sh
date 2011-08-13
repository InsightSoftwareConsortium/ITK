#!/bin/bash
#==========================================================================
#
#   Copyright Insight Software Consortium
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

usage() {

cat << EOF
  How to use this script:

0)  Use Linux or Mac, install
     - valgrind
     - xsltproc

1)  Compile ITK with RelWithDebInfo CMAKE_BUILD_TYPE

2)  cd into  the ITK binary build directory

3)  From the TOP of the binary directory type the "ctest" expression that
set(DOC  "
    selects the tests that you want to perform memory checking on.

For example:

    ctest  -R   itkHDF5ImageIOTest  -N

This will print the tests selected by the regular expression but not run the
tests (-N option).

4)  Type the path to this script in the ITK source tree and add the select expression
    from step 3 above.

For example:

   ~/src/ITK/Utilities/Maintenance/runValgrind.sh    \
      -R itkHDF5ImageIOTest

This will run the selected tests under valgrind and generate HTML that can be
opened with your favorite browser.  The HTML is written to ./memcheck_index.html
To open:

  In Linux, you can do      firefox  ./memcheck_index.html
  In Mac,   you can do      open     ./memcheck_index.html
EOF

}

if test "$1" == "-h" -o "$1" == "--help"; then
  usage
  exit 1
fi

# Remove results from prior run.
find . -name DynamicAnalysis.xml -delete

ctest -D ExperimentalMemCheck $*

memcheck_xml=$(find . -name DynamicAnalysis.xml | xargs ls -t1 | head -n 1)

u=$(cd "$(echo "$0"|sed 's/[^/]*$//')"; pwd)

tests=$(xsltproc "${u}/DynamicAnalysisGetTests.xsl" "$memcheck_xml")
if test -z "$tests"; then
  echo
  echo "No memory leaks were detected :-)."
  exit
fi

for test in  $tests
do
  sed "s/@TESTNAME@/${test}/" "${u}/DynamicAnalysisFile.xsl" > test.xsl
  xsltproc test.xsl "${memcheck_xml}" > "${test}.html"
done
rm test.xsl

xsltproc "${u}/DynamicAnalysis.xsl" "${memcheck_xml}" > memcheck_index.html

cp "${u}/stylish.css" ./

cat << EOF

To open:

  In Linux, you can do      firefox  ./memcheck_index.html
  In Mac,   you can do      open     ./memcheck_index.html
EOF
