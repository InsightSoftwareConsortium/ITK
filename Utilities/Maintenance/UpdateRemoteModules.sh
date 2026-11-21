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

# NAME
#     UpdateRemoteModules.sh - updates the git commit hashes of ITK's remote modules
#
# SYNOPSIS
#     UpdateRemoteModules.sh [-ongreen]
#     UpdateRemoteModules.sh -h | --help
#
# DESCRIPTION
#     This script updates the git commit hash in each of ITK's remote module
#     files (Modules/Remote/*.remote.cmake) to the latest commit on that
#     module's master or main branch. It edits those files in place and does
#     not commit the changes, so the result should be reviewed and committed
#     afterwards. To find each latest commit, it contacts GitHub over SSH.
#
#     Only -h, --help and -ongreen are recognized, and only as the first
#     argument. Any other argument is ignored and the update runs, so use
#     --help to read this text without modifying any files.
#
#     When it finishes, the script prints the modules it updated in green, or a
#     message saying that all modules are up to date. With -ongreen, it also
#     prints the failing modules it skipped in red.
#
# OPTIONS
#     -h, --help
#         Print the usage summary and exit without making any changes.
#
#     -ongreen
#         Update a module only if its latest commit has a successful CI status
#         on GitHub. Modules with any other status are left unchanged and
#         listed as failing. Without this option, every module with a newer
#         commit is updated and no status is checked.
#
# IMPLEMENTATION
#     The script first defines its usage() text and checks whether the first
#     argument asks for help, in which case it prints the usage and stops. It
#     then moves to its own directory and locates Modules/Remote. For each
#     .cmake file there, it reads the current commit hash and the GitHub
#     repository, asks GitHub for the latest commit on master (or main if there
#     is no master), and skips the module if the two hashes match. If the script
#     runs with -ongreen, it will also query the commit's status through the
#     GitHub API and skip the module if the status is not "success". Otherwise,
#     it replaces the old hash with the new one in the .cmake file. Once changes
#     are done, users should review changes then commit the .cmake file.
#
# EXAMPLES
#     ./Utilities/Maintenance/UpdateRemoteModules.sh --help
#         Print the usage text without changing anything.
#
#     ./Utilities/Maintenance/UpdateRemoteModules.sh
#         Update every remote module that has a newer commit.
#
#     ./Utilities/Maintenance/UpdateRemoteModules.sh -ongreen
#         Update modules whose latest commit has a successful status.
#
# SEE ALSO
#     - Release.md (Documentation/Maintenance/Release.md):
#         Describes the remote module procedure this script belongs to.
#     - UpdateRequiredITKVersionInRemoteModules.sh (same directory):
#         Updates the required ITK version in a remote module.


# Utility functions
usage() {
cat << EOF
NAME
  UpdateRemoteModules.sh - updates the git commit hashes of ITK's remote modules

SYNOPSIS
  UpdateRemoteModules.sh [-ongreen]
  UpdateRemoteModules.sh -h | --help

DESCRIPTION
  This script updates the git commit hash in each of ITK's remote module
  files (Modules/Remote/*.remote.cmake) to the latest commit on that
  module's master or main branch. It edits those files in place and does
  not commit the changes, so the result should be reviewed and committed
  afterwards. To find each latest commit, it contacts GitHub over SSH.

  Only -h, --help and -ongreen are recognized, and only as the first
  argument. Any other argument is ignored and the update runs, so use
  --help to read this text without modifying any files.

  When it finishes, the script prints the modules it updated in green, or a
  message saying that all modules are up to date. With -ongreen, it also
  prints the failing modules it skipped in red.

OPTIONS
  -h, --help
      Print the usage summary and exit without making any changes.

  -ongreen
      Update a module only if its latest commit has a successful CI status
      on GitHub. Modules with any other status are left unchanged and
      listed as failing. Without this option, every module with a newer
      commit is updated and no status is checked.

IMPLEMENTATION
  The script first defines its usage() text and checks whether the first
  argument asks for help, in which case it prints the usage and stops. It
  then moves to its own directory and locates Modules/Remote. For each
  .cmake file there, it reads the current commit hash and the GitHub
  repository, asks GitHub for the latest commit on master (or main if there
  is no master), and skips the module if the two hashes match. If the script
  runs with -ongreen, it will also query the commit's status through the
  GitHub API and skip the module if the status is not "success". Otherwise,
  it replaces the old hash with the new one in the .cmake file. Once changes
  are done, users should review changes then commit the .cmake file.

EXAMPLES
  ./Utilities/Maintenance/UpdateRemoteModules.sh --help
      Print the usage text without changing anything.

  ./Utilities/Maintenance/UpdateRemoteModules.sh
      Update every remote module that has a newer commit.

  ./Utilities/Maintenance/UpdateRemoteModules.sh -ongreen
      Update modules whose latest commit has a successful status.

SEE ALSO
  - Release.md (Documentation/Maintenance/Release.md):
      Describes the remote module procedure this script belongs to.
  - UpdateRequiredITKVersionInRemoteModules.sh (same directory):
      Updates the required ITK version in a remote module.
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
  echo $filename

  # Get the current commit hash
  curr_commit=$(grep -v "\s*#" $filename | grep -m 1 -o "GIT_TAG \(\w\|\.\|\-\|_\)*")
  curr_commit=${curr_commit/*GIT_TAG /}

  # Read the git repository information
  repository=$(grep -v "\s*#" $filename | grep -m 1 -o "GIT_REPOSITORY \https://github.com/\(\w\|\-\|_\|/\)*")
  repository=${repository/*GIT_REPOSITORY https:\/\/github.com\//}

  # Get the latest git commit hash of the remote module.
  # Remotes will usually not be tagged.
  latest_commit=$(git ls-remote git@github.com:$repository refs/heads/master)
  latest_commit=${latest_commit/[[:space:]]refs\/heads\/master/}

  if [ -z "$latest_commit" ]; then
    # check for a branch named main
    latest_commit=$(git ls-remote git@github.com:$repository refs/heads/main)
    latest_commit=${latest_commit/[[:space:]]refs\/heads\/main/}
    if [ -z "$latest_commit" ]; then
      echo "$repository has neither branch named master nor main"
      continue
    fi
  fi

  if [ "$curr_commit" = "$latest_commit" ]; then
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
  # echo "Updating $repository from '${curr_commit}' to '${latest_commit}'" # debug
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
