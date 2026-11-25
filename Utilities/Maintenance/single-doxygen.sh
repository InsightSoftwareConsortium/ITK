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
#%     single-doxygen.sh - run Doxygen on one file or directory
#%
#% SYNOPSIS
#%     single-doxygen.sh <path>
#%     single-doxygen.sh -h | --help
#%
#% DESCRIPTION
#%     Build Doxygen documentation for a single header or a single
#%     directory, to check its documentation without building the
#%     documentation of all of ITK. The script copies single-doxygen.config
#%     from the current directory to temp-single-doxygen.config, replacing
#%     each INPUT_SINGLE_FILE in it with <path>, runs doxygen on the copy,
#%     and deletes the copy.
#%
#% OPTIONS
#%     <path>
#%         The header file or directory to document.
#%     -h, --help
#%         Print this help and exit.
#%
#% IMPLEMENTATION
#%     Needs doxygen and Linux or macOS. Run from a directory holding
#%     single-doxygen.config. Nothing in the ITK source tree creates that
#%     file, so it must be provided. Output is written where the config
#%     file says.
#%
#% EXAMPLES
#%     ~/ITK/Utilities/Maintenance/single-doxygen.sh \
#%       ~/ITK/Modules/Core/Common/include/itkObject.h
#%         Document one header.
#%
#%     ~/ITK/Utilities/Maintenance/single-doxygen.sh \
#%       ~/ITK/Modules/Core/Common/include/
#%         Document the headers of one module.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Documentation/docs/contributing/document_itk.md:
#%         How ITK classes are documented.

usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

case "${1-}" in
  -h | --help)
    usage
    exit 0
    ;;
esac

FILE=$1

sed "s/INPUT_SINGLE_FILE/"${FILE//\//\\/}"/g" single-doxygen.config > temp-single-doxygen.config

doxygen temp-single-doxygen.config

rm temp-single-doxygen.config
