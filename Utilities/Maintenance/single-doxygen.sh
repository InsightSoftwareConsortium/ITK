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
# How to use this script
#
# 0) Use Linux or Mac
#
# 1) run cmake
#
# 2) TURN ON BUILD_DOCUMENTATION
#
# 3) cd into the ITK binary build directory
#
# 4) example:
#
# * for a single file:
#
# $ ~/src/ITK/Utilities/Maintenance/single-doxygen.sh ~/src/ITK/Modules/Core/Common/include/itkObject.h
#
# * for a module:
#
# $ ~/src/ITK/Utilities/Maintenance/single-doxygen.sh ~/src/ITK/Modules/Core/Common/include/

#
# 4) firefox temp/html/index.html

FILE=$1

sed "s/INPUT_SINGLE_FILE/"${FILE//\//\\/}"/g" single-doxygen.config > temp-single-doxygen.config

doxygen temp-single-doxygen.config

rm temp-single-doxygen.config
