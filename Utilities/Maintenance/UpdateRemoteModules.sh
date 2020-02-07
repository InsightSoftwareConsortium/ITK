#!/bin/bash

#==========================================================================
#
#   Copyright NumFOCUS
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


# This script updates the git commit hashes of all successful remote modules.
#
# The script updates the git commit hashes in ITK's Modules/Remote
# '*.remote.cmake' files for the remote modules whose CI build status is
# successful.


# Utility functions
usage() {
cat << EOF
Usage: $0

Use this script to update the git commit hashes of all successful remote
modules.

The script updates the git commit hashes in ITK's Modules/Remote
'*.remote.cmake' files for the remote modules whose CI build status is
successful.
EOF
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

# Make sure we are inside the repository
cd "${BASH_SOURCE%/*}" &&

remote_modules_path=$(cd ../../Modules/Remote && pwd)

updated=()
if [ $# -ge 1 ] && [ $1 = "-ongreen" ]; then
  ongreen=1
  failing=()
else
  ongreen=0
fi

# Loop over the remote modules' CMake files
for filename in ${remote_modules_path}/*.cmake; do
  echo -n "."

  # Get the current commit hash
  curr_commit=$(grep -v "\s*#" $filename | grep -m 1 -o "GIT_TAG \(\w\|\.\|\-\|_\)*")
  curr_commit=${curr_commit/*GIT_TAG /}

  # Read the git repository information
  repository=$(grep -v "\s*#" $filename | grep -m 1 -o "GIT_REPOSITORY \${git_protocol}://github.com/\(\w\|\-\|_\|/\)*")
  repository=${repository/*GIT_REPOSITORY \${git_protocol\}:\/\/github.com\//}

  # Get the latest git commit hash of the remote module.
  # Remotes will usually not be tagged.
  latest_commit=$(git ls-remote git://github.com/$repository refs/heads/master)
  latest_commit=${latest_commit/[[:space:]]refs\/heads\/master/}

  if [ $curr_commit = $latest_commit ]; then
    continue
  fi

  if [ $ongreen -eq 1 ]; then
    # Get the repository status
    # Be careful of rate limiting (https://developer.github.com/v3/#rate-limiting)
    # alternatively using Azure: https://docs.microsoft.com/en-us/azure/data-factory/monitor-programmatically#rest-api
    status=$(curl -s https://api.github.com/repos/$repository/commits/$latest_commit/status | grep -m 1 -o "\"state\": \"\w*\"")
    if [ "$status" != "\"state\": \"success\"" ]; then
      failing+=($repository)
      continue
    fi
  fi

  # Search and replace the latest commit hash in the CMake file
  ex -sc "%s/${curr_commit}/${latest_commit}/g|x" $filename
  updated+=($repository)

done

echo -ne "\n"

if [ -n "$updated" ]; then
  echo -e "Modules \033[1;32mupdated\033[0m:"
  for module in ${updated[@]}; do
    echo $module
  done
else
  echo "All modules are up to date"
fi

if [ $ongreen -eq 1 ] && [ -n "$failing" ]; then
  echo -e "Modules \033[1;31mfailing\033[0m:"
  for module in ${failing[@]}; do
    echo $module
  done
fi
