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
#%     UpdateTestingMacrosNames.sh - add the ITK_ prefix to testing macros
#%
#% SYNOPSIS
#%     UpdateTestingMacrosNames.sh
#%     UpdateTestingMacrosNames.sh -h | --help
#%
#% DESCRIPTION
#%     Rename the ITK testing macros to their ITK_ prefixed names, for
#%     example TEST_SET_GET_VALUE to ITK_TEST_SET_GET_VALUE. The macros are
#%     EXERCISE_BASIC_OBJECT_METHODS, TRY_EXPECT_EXCEPTION,
#%     TRY_EXPECT_NO_EXCEPTION, TEST_EXPECT_TRUE_STATUS_VALUE,
#%     TEST_EXPECT_TRUE, TEST_EXPECT_EQUAL_STATUS_VALUE, TEST_EXPECT_EQUAL,
#%     TEST_SET_GET, TEST_SET_GET_VALUE, TEST_SET_GET_NULL_VALUE, and
#%     TEST_SET_GET_BOOLEAN. Only whole words are replaced, so a name that
#%     already has the prefix is left alone. Only the first argument is
#%     checked for -h or --help; other arguments are ignored.
#%
#% OPTIONS
#%     -h, --help
#%         Print this help and exit with status 1.
#%
#% IMPLEMENTATION
#%     Edits every .h, .c, .cpp, .hxx, and .cxx file under the current
#%     directory in place. Needs GNU sed. Nothing is committed.
#%
#% EXAMPLES
#%     cd ITKFoo && ../ITK/Utilities/Maintenance/UpdateTestingMacrosNames.sh
#%         Rename the macros in a remote module checked out beside ITK.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Utilities/Maintenance/ApplyScriptToRemotes.sh:
#%         Apply a script to the remote modules of an ITK source tree.


# Utility functions
usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

die() {
  echo "$@" 1>&2; exit 1
}

while test $# -gt 0;
do
  opt="$1";
  case "$opt" in
    "-h"|"--help")
      shift;
      help=true
      break;;
    *)
      break;;
  esac
done

if test $help; then
  usage
  exit 1
fi

prefix='ITK_'

declare -a old_strings=("EXERCISE_BASIC_OBJECT_METHODS" "TRY_EXPECT_EXCEPTION" \
 "TRY_EXPECT_NO_EXCEPTION" "TEST_EXPECT_TRUE_STATUS_VALUE" "TEST_EXPECT_TRUE" \
 "TEST_EXPECT_EQUAL_STATUS_VALUE" "TEST_EXPECT_EQUAL" "TEST_SET_GET" \
 "TEST_SET_GET_VALUE" "TEST_SET_GET_NULL_VALUE" "TEST_SET_GET_BOOLEAN")

# Do the replacement
for old_str in "${old_strings[@]}"; do
  find . -type f \( -name '*.h' -o -name '*.c' -o -name '*.cpp' \
  -o -name '*.hxx' -o -name '*.cxx' \) \
  -exec sed -i -e "s/\b$old_str\b/$prefix$old_str/g" {} \;
done
