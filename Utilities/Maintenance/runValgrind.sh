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
#   1)  Compile ITK for Debug
#
#   2)  cd into  the ITK binary build directory
#
#   3)  type the "ctest" expression that select
#       the test that you want to check with Valgrind
#
#       for example:
#
#          ctest  -R   itkHDF5ImageIOTest   -V   -N
#
#       This will print to the console the command line instructions needed to
#       run the tests (-V option), but without running the tests (-N option).
#
#   4)  Type the path to this script in the ITK source tree and then put in
#       front of it the expression that you get from step (3)
#
#       for example:
#
#          ~/src/ITK/Utilities/Maintenance/runValgrind.sh    \
#                $HOME/bin/ITK/Debug/bin/ITK-IO-HDF5TestDriver \
#                "itkHDF5ImageIOTest"   \
#                "$HOME/bin/ITK/Debug/Testing/Temporary"
#
#      At this point, the test will be run under the control of Valgrind
#      and the combined text output of both the test and the Valgrind analysis
#      will be printed in the console. For convienience, the same output is being
#      copied to a file in the /tmp directory. More precisely to:
#
#                        /tmp/itkValgrindReport.txt
#
#
#==============================================================================

#
#  Find the path to the Valgrind suppressions file.
#
basenameOfScript=`basename $0`
directoryOfScript=`dirname $0`
directoryOfValgrindSuppressions=$directoryOfScript/../../CMake
pathToValgrindSuppressionsFile=$directoryOfValgrindSuppressions/InsightValgrind.supp

#
#  Run the test under Valgrind control
#
echo "RUNNING " $*
valgrind \
 --sim-hints=lax-ioctls \
 --trace-children=yes \
 -v \
 --tool=memcheck \
 --leak-check=yes \
 --show-reachable=yes \
 --suppressions=$pathToValgrindSuppressionsFile \
  $* |& tee /tmp/itkValgrindReport.txt
