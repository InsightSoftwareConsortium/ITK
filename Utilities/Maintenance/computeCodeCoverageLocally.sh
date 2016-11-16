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
if [[ $1 == "--help" ]]
then
echo "      "
echo "  How to use this script      "
echo "      "
echo "   0)  Use Linux or Mac, "
echo "        - install lcov"
echo "        - use the gcc compiler"
echo "      "
echo "   1)  Add the CMake flags:      "
echo "      "
echo "       CMAKE_CXX_FLAGS:STRING=-g -O0  -fprofile-arcs -ftest-coverage     "
echo "       CMAKE_C_FLAGS:STRING= -g -O0  -fprofile-arcs -ftest-coverage      "
echo "      "
echo "   2)  Compile ITK for Debug      "
echo "      "
echo "                     CMAKE_BUILD_TYPE  Debug      "
echo "      "
echo "   3)  From the TOP of the binary directory type the path to this script "
echo "       in the ITK source tree.  This will run all tests in ITK and generate "
echo "       code coverage for the entire toolkit.  The code coverage report will "
echo "       be generated in HTML and can be opened with your favorite browser.     "
echo "    "
echo "       For example, "
echo "    "
echo "          In Linux, you can do        firefox  ./index.html     "
echo "          In Mac,   you can do        open     ./index.html     "
echo "    "

else

#==========================================================================

lcov --directory . --zerocounters
ctest
lcov --directory . --capture --output-file app.info
lcov --remove app.info '*KWStyle*' '*test*'  '*ThirdParty*' '/usr/*' --output-file  app.info2
genhtml app.info2
echo "To view results on Linux, type firefox ./index.html"
echo "To view results on Mac, type open ./index.html"

fi
