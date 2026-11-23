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
#%     computeCodeCoverageLocally.sh - report code coverage of all ITK tests
#%
#% SYNOPSIS
#%     computeCodeCoverageLocally.sh
#%     computeCodeCoverageLocally.sh -h | --help
#%
#% DESCRIPTION
#%     Run every ITK test with ctest in a coverage build and write an HTML
#%     code coverage report for the whole toolkit to the current directory.
#%     Coverage counters are reset first. Files whose paths match KWStyle,
#%     test, ThirdParty, or /usr are left out of the report.
#%
#% OPTIONS
#%     -h, --help
#%         Print this help and exit.
#%
#% IMPLEMENTATION
#%     Needs lcov (with genhtml), ctest, and the gcc compiler, on Linux or
#%     macOS. Build ITK for coverage first:
#%
#%         CMAKE_BUILD_TYPE=Debug
#%         CMAKE_CXX_FLAGS:STRING=-g -O0 -fprofile-arcs -ftest-coverage
#%         CMAKE_C_FLAGS:STRING=-g -O0 -fprofile-arcs -ftest-coverage
#%
#%     Run from the top of the build directory. The script writes app.info,
#%     app.info2, index.html, and the other report files there, and
#%     overwrites any earlier report.
#%
#% EXAMPLES
#%     cd ITK-build
#%     ../ITK/Utilities/Maintenance/computeCodeCoverageLocally.sh
#%         Run all tests and write the report; open ./index.html to view it.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Utilities/Maintenance/computeCodeCoverageLocallyForOneTest.sh:
#%         Coverage from selected tests only.

usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

case "${1-}" in
  -h | --help)
    usage
    exit 0
    ;;
esac

#==========================================================================

lcov --directory . --zerocounters
ctest
lcov --directory . --capture --output-file app.info
lcov --remove app.info '*KWStyle*' '*test*'  '*ThirdParty*' '/usr/*' --output-file  app.info2
genhtml app.info2
echo "To view results on Linux, type firefox ./index.html"
echo "To view results on Mac, type open ./index.html"
