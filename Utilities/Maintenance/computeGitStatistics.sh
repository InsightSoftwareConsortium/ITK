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
#%     computeGitStatistics.sh - generate git history statistics with gitstats
#%
#% SYNOPSIS
#%     computeGitStatistics.sh SOURCE_TREE OUTPUT_DIR
#%     computeGitStatistics.sh -h | --help
#%
#% DESCRIPTION
#%     Run gitstats on the git repository SOURCE_TREE and write its
#%     statistics pages to OUTPUT_DIR. Up to 300 authors are listed. The
#%     arguments are not checked.
#%
#% OPTIONS
#%     SOURCE_TREE
#%         The ITK git repository to analyze.
#%     OUTPUT_DIR
#%         Directory for the generated files.
#%     -h, --help
#%         Print this help and exit with status 1.
#%
#% IMPLEMENTATION
#%     Written for Linux; needs gitstats on PATH.
#%
#% EXAMPLES
#%     computeGitStatistics.sh ~/src/ITK /tmp/ITKGitStats
#%         Write statistics for ~/src/ITK to /tmp/ITKGitStats.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Utilities/Maintenance/CodeSwarmWithGource.sh:
#%         Render the git history as a video.

usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

if test "$1" == "-h" -o "$1" == "--help"; then
  usage
  exit 1
fi

gitstats -c max_authors=300  $1   $2
