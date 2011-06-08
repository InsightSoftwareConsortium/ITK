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
if [[ $# == 0 || $1 == "--help" ]]
then
echo "                                                                        "
echo "  How to use this script   "
echo "   "
echo "   0)  Use Linux or Mac   "
echo "   "
echo "   1)  Add the CMake flags:   "
echo "   "
echo "       CMAKE_CXX_FLAGS:STRING=-g -O0  -fprofile-arcs -ftest-coverage   "
echo "       CMAKE_C_FLAGS:STRING= -g -O0  -fprofile-arcs -ftest-coverage   "
echo "   "
echo "   2)  Compile ITK for Debug   "
echo "   "
echo "                     CMAKE_BUILD_TYPE  Debug   "
echo "   "
echo "   3)  From the binary directory type the "ctest" expression that select the   "
echo "       tests that you want use to generate coverage (Ideally this should only   "
echo "       need to be the unit tests of the class in question).   "
echo "   "
echo "       For example:   "
echo "   "
echo "                ctest  -R   itkHDF5ImageIOTest   -V   -N   "
echo "   "
echo "       This will print to the console the command line instructions needed to   "
echo "       run the tests (-V option), but without running the tests (-N option).   "
echo "   "
echo "   4)  From the binary directory type the path to this script in the ITK   "
echo "       source tree and add the selection expression that you put after     "
echo "       ctest in numeral (3) above.   "
echo "   "
echo "       More specifically, for example:   "
echo "   "
echo "        computeCodeCoverageLocallyForOneTest.sh -R  itkHDF5ImageIOTest   "
echo "   "
echo "   "
echo "       This will run the selected tests in ITK and generate code coverage for   "
echo "       the entire toolkit, but only from the test that you have selected. The   "
echo "       code coverage report will be generated in HTML can be opened with your   "
echo "       favorite browser.                                                        "
echo "       For example, "
echo "    "
echo "          In Linux, you can do        firefox  ./index.html     "
echo "          In Mac,   you can do        open     ./index.html     "
echo "    "

else

#==========================================================================
lcov --directory . --zerocounters
ctest $*
lcov --directory . --capture --output-file app.info
lcov --remove app.info '*test*'  '*ThirdParty*' '/usr/*' --output-file  app.info2
genhtml app.info2

fi
