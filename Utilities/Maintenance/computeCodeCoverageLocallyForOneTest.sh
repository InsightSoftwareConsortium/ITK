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
#
#  How to use this script
#
#   0)  Use Linux or Mac
#
#   1)  Add the CMake flags:
#
#       CMAKE_CXX_FLAGS:STRING=-g -O0  -fprofile-arcs -ftest-coverage
#       CMAKE_C_FLAGS:STRING= -g -O0  -fprofile-arcs -ftest-coverage
#
#   2)  Compile ITK for Debug
#
#                     CMAKE_BUILD_TYPE  Debug
#
#   3)  From the binary directory type the "ctest" expression that select the
#       tests that you want use to generate coverage (Ideally this should only
#       need to be the unit tests of the class in question).
#
#       For example:
#
#                ctest  -R   itkHDF5ImageIOTest   -V   -N
#
#       This will print to the console the command line instructions needed to
#       run the tests (-V option), but without running the tests (-N option).
#
#   4)  From the binary directory type the path to this script in the ITK
#       source tree and add the selection expression that you put in front
#       of ctest in numeral (3) above.
#
#       More specifically, for example:
#
#        computeCodeCoverageLocallyForOneTest.sh -R  itkHDF5ImageIOTest
#
#
#       This will run the selected tests in ITK and generate code coverage for
#       the entire toolkit, but only from the test that you have selected. The
#       code coverage report will be generated in HTML and will be presented
#       with Firefox.
#
#==========================================================================

lcov --directory . --zerocounters
ctest $*
lcov --directory . --capture --output-file app.info
lcov --remove app.info '*test*'  '*ThirdParty*' '/usr/*' --output-file  app.info2
genhtml app.info2
firefox ./index.html
