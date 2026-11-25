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
#%     computeCodeCoverageLocallyForOneTest.sh - report coverage of chosen tests
#%
#% SYNOPSIS
#%     computeCodeCoverageLocallyForOneTest.sh MODULE_DIR CTEST_ARGS...
#%     computeCodeCoverageLocallyForOneTest.sh -h | --help
#%
#% DESCRIPTION
#%     Run the ITK tests selected by CTEST_ARGS in a coverage build, and
#%     write an HTML code coverage report for MODULE_DIR into MODULE_DIR.
#%     Coverage counters of the whole build are reset first, so the report
#%     shows only what the selected tests executed. Coverage data is
#%     collected from MODULE_DIR only. Files whose paths match test,
#%     ThirdParty, or /usr are left out of the report.
#%
#%     With no argument the script prints this help and exits with
#%     status 0.
#%
#% OPTIONS
#%     MODULE_DIR
#%         Build directory of the module to report on, relative to the top
#%         of the build directory.
#%     CTEST_ARGS
#%         Arguments passed to ctest to select the tests, such as -R REGEX.
#%         To check the selection without running the tests, try them first
#%         as ctest CTEST_ARGS -V -N.
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
#%     app.info2, index.html, and the other report files in MODULE_DIR, and
#%     overwrites any earlier report there. CTEST_ARGS are split on spaces,
#%     so an argument cannot contain a space.
#%
#% EXAMPLES
#%     computeCodeCoverageLocallyForOneTest.sh ./Modules/IO/HDF5 \
#%         -R itkHDF5ImageIOTest
#%         Run itkHDF5ImageIOTest and write the report to
#%         ./Modules/IO/HDF5/index.html.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Utilities/Maintenance/computeCodeCoverageLocally.sh:
#%         Coverage from all tests.

usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

case "${1-}" in
  -h | --help)
    usage
    exit 0
    ;;
esac

if [[ $# == 0 ]]
then
usage

else

#==========================================================================
lcov --directory . --zerocounters
destinationdir=$1
shift
ctest $*
cd $destinationdir
lcov --directory . --capture --output-file app.info
lcov --remove app.info '*test*'  '*ThirdParty*' '/usr/*' --output-file  app.info2
genhtml app.info2
echo "To view results on Linux, type firefox "$destinationdir"/index.html"
echo "To view results on Mac, type open "$destinationdir"/index.html"
fi
