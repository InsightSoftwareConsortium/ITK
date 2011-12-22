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

usage() {

cat << EOF
  How to use this script:

1)  Use Linux, install

     - gitstat

2) Run this script and pass as arguments

     a) The source three of the ITK Git repository
     b) An output directory to put the generated files

For example:

  computeGitStatistics.sh  ~/src/ITK /tmp/ITKGitStats

EOF

}

if test "$1" == "-h" -o "$1" == "--help"; then
  usage
  exit 1
fi

gitstats -c max_authors=300  $1   $2
