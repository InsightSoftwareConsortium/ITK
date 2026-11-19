#!/bin/bash
#==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

#% NAME
#%     runValgrind.sh - run ITK tests under valgrind and report as HTML
#%
#% SYNOPSIS
#%     runValgrind.sh [<ctest options>...]
#%     runValgrind.sh -h | --help
#%
#% DESCRIPTION
#%     Run the selected tests of an ITK build under the CTest memory checker
#%     (ctest -D ExperimentalMemCheck) and turn its DynamicAnalysis.xml into
#%     HTML pages. If the newest DynamicAnalysis.xml lists no tests, the
#%     script prints "No memory leaks were detected" and writes no pages.
#%
#%     Otherwise it writes, in the current directory, memcheck_index.html,
#%     one <test>.html for each test listed, and a copy of stylish.css.
#%     Open memcheck_index.html in a browser to read the report.
#%
#% OPTIONS
#%     <ctest options>
#%         Passed to ctest unquoted, usually -R <regex> to select tests.
#%         Check the selection first with ctest -R <regex> -N, which lists
#%         the tests without running them.
#%     -h, --help
#%         Print this help and exit with status 1. Only the first argument
#%         is checked.
#%
#% IMPLEMENTATION
#%     Needs valgrind, xsltproc, and ctest, on Linux or macOS. Build ITK with
#%     CMAKE_BUILD_TYPE RelWithDebInfo and run from the top of the build
#%     directory. Every DynamicAnalysis.xml under the current directory is
#%     deleted before the run. The XSL files and stylish.css are read from
#%     the directory holding this script, and a temporary test.xsl is
#%     written to the current directory and removed.
#%
#% EXAMPLES
#%     ~/src/ITK/Utilities/Maintenance/runValgrind.sh -R itkHDF5ImageIOTest
#%         Check itkHDF5ImageIOTest for memory errors.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Utilities/Maintenance/DynamicAnalysis.xsl:
#%         Stylesheet for the index page.
#%     Utilities/Maintenance/DynamicAnalysisFile.xsl:
#%         Stylesheet for the per-test pages.

usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
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
