#!/usr/bin/env bash
#=============================================================================
# Copyright 2023 NumFOCUS
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#=============================================================================
# This script adapted from the KWSys utilities

#% NAME
#%     cmake-format.bash - format ITK CMake files with cmake-format
#%
#% SYNOPSIS
#%     cmake-format.bash [--cmake-format TOOL] MODE [--]
#%     cmake-format.bash -h | --help
#%
#% DESCRIPTION
#%     Run cmake-format in place on the CMake files selected by MODE. Only
#%     files whose git attribute hooks.style includes cmakeformat are
#%     formatted, and paths containing ThirdParty, ExternalData.cmake, or
#%     Modules/Remote are skipped. Formatting uses
#%     Utilities/CMakeFormat/config.yaml. Nothing is staged or committed.
#%
#%     With no MODE the script prints this help and exits with status 0,
#%     but only after it has found a cmake-format tool. An unknown option
#%     or an extra argument prints this help and exits with status 1.
#%
#% OPTIONS
#%     --amend
#%         Format files added or modified by HEAD.
#%     --cached
#%         Format files staged for commit.
#%     --modified
#%         Format files modified from HEAD.
#%     --last
#%         Format files modified from HEAD~1.
#%     --tracked
#%         Format every file tracked by git.
#%     --cmake-format TOOL
#%         Use TOOL in place of the cmake-format found on PATH.
#%     -h, --help
#%         Print this help and exit.
#%
#%     When several modes are given, the last one wins.
#%
#% IMPLEMENTATION
#%     Needs git and cmake-format. Run from the root of the ITK source tree:
#%     the file list and the configuration path are relative to it. If no
#%     tool is found, the script prints an error and exits with status 1.
#%
#% EXAMPLES
#%     Utilities/Maintenance/cmake-format.bash --modified
#%         Format locally modified files.
#%     Utilities/Maintenance/cmake-format.bash --cached
#%         Format files staged for commit.
#%     Utilities/Maintenance/cmake-format.bash --amend
#%         Format files modified by the most recent commit.
#%     Utilities/Maintenance/cmake-format.bash --tracked
#%         Format all files.
#%     git filter-branch \
#%       --tree-filter "Utilities/Maintenance/cmake-format.bash --tracked" \
#%       main..
#%         Format every commit of the current topic.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Utilities/CMakeFormat/config.yaml:
#%         The cmake-format configuration.
#%     Utilities/Maintenance/clang-format.bash:
#%         The same interface for C++ sources.

usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

die() {
    echo "$@" 1>&2; exit 1
}

#-----------------------------------------------------------------------------

# Parse command-line arguments.
cmake_format=''
mode=''
while test "$#" != 0; do
    case "$1" in
    --amend) mode="amend" ;;
    --cached) mode="cached" ;;
    --cmake-format) shift; cmake_format="$1" ;;
    -h | --help) usage; exit 0 ;;
    --modified) mode="modified" ;;
    --last) mode="last" ;;
    --tracked) mode="tracked" ;;
    --) shift ; break ;;
    -*) usage 1>&2; exit 1 ;;
    *) break ;;
    esac
    shift
done
test "$#" = 0 || { usage 1>&2; exit 1; }

# Find a default tool.
tools='
  cmake-format
'
if test "x$cmake_format" = "x"; then
    for tool in $tools; do
        if type -p "$tool" >/dev/null; then
            cmake_format="$tool"
            break
        fi
    done
fi

# Verify that we have a tool.
if ! type -p "$cmake_format" >/dev/null; then
    echo "Unable to locate a 'cmake-format' tool."
    exit 1
fi

# Select listing mode.
case "$mode" in
    '')       usage; exit 0 ;;
    amend)    git_ls='git diff-tree  --diff-filter=AM --name-only HEAD -r --no-commit-id' ;;
    cached)   git_ls='git diff-index --diff-filter=AM --name-only HEAD --cached' ;;
    modified) git_ls='git diff-index --diff-filter=AM --name-only HEAD' ;;
    last)     git_ls='git diff-index --diff-filter=AM --name-only HEAD~1' ;;
    tracked)  git_ls='git ls-files' ;;
    *) die "invalid mode: $mode" ;;
esac

# List files as selected above.
$git_ls |

  # Select sources with our attribute.
  git check-attr --stdin hooks.style         |
  grep -e 'hooks.style: .*cmakeformat'       |
  grep -v 'ThirdParty'                       |
  grep -v 'ExternalData.cmake'               |
  grep -v 'Modules/Remote'                   |
  sed -n 's/:[^:]*:[^:]*$//p'                |
  # Update sources in-place.
  tr '\n' '\0'                               |
  xargs -0 "$cmake_format" -c ./Utilities/CMakeFormat/config.yaml -i
