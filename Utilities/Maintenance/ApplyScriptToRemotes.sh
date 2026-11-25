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
#%     ApplyScriptToRemotes.sh - run a script on remote modules and commit
#%
#% SYNOPSIS
#%     ApplyScriptToRemotes.sh SCRIPT FEATURE_BRANCH COMMIT_MESSAGE
#%     ApplyScriptToRemotes.sh -h | --help
#%
#% DESCRIPTION
#%     Apply SCRIPT to the source code of the remote modules listed in
#%     Modules/Remote/*.cmake, commit the result on FEATURE_BRANCH in each
#%     module, and offer to push each commit to the user's GitHub fork.
#%
#%     A remote module is selected when the GIT_TAG in its .cmake file equals
#%     the latest commit on the main branch of its GitHub repository. The
#%     code that would also require a successful CI status exists but is not
#%     enabled, so CI status is not checked.
#%
#%     The parser expects the module name on the same line as
#%     itk_fetch_module(, so the current Modules/Remote/*.cmake files, which
#%     put the name on the next line, give an empty name and no module is
#%     selected; ITK is still
#%     configured, and no module is changed.
#%
#%     ITK is configured once with every selected module turned on, which
#%     fetches the modules into Modules/Remote. In each module the script
#%     checks out main, creates FEATURE_BRANCH from origin/main (or checks
#%     out an existing FEATURE_BRANCH), runs SCRIPT, stages tracked files
#%     with the extensions .txt, .cmake, .rst, .py, .yml, .c, .cxx, .h,
#%     .hxx, and .wrap, and commits them with COMMIT_MESSAGE. It then asks
#%     whether to push the branch. Afterward, open the pull requests by hand.
#%
#%     With a missing argument the script prints this help and exits with
#%     status 1.
#%
#% OPTIONS
#%     SCRIPT
#%         Command to run from the top directory of each module. Give an
#%         absolute path, or a command on PATH.
#%     FEATURE_BRANCH
#%         Branch to create or reuse in each module, and to push.
#%     COMMIT_MESSAGE
#%         Commit message, following the ITK commit message conventions.
#%     -h, --help
#%         Print this help and exit.
#%
#% IMPLEMENTATION
#%     Needs git, curl, cmake, and network access to github.com. The script
#%     asks for a GitHub user name and pushes over SSH to
#%     git@github.com:USER/REPOSITORY.git, so an SSH key for that account
#%     and a fork of each module are needed.
#%
#%     The script changes to its own directory, so it may be run from
#%     anywhere, but the ITK source tree must be in a directory named ITK.
#%     It creates the build directory ITK-build beside the source tree and
#%     deletes it at the end. The fetched modules, their branches, and
#%     their commits stay in Modules/Remote.
#%
#%     The extensions are expanded by the shell in each module's top
#%     directory before git sees them. When a top-level file has the
#%     extension, only top-level files with it are staged; otherwise git
#%     matches the extension in every subdirectory.
#%
#% EXAMPLES
#%     ./Utilities/Maintenance/ApplyScriptToRemotes.sh \
#%         /path/to/fix.sh fix-branch "STYLE: Apply fix"
#%         Run fix.sh in each selected module and commit on fix-branch.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     CONTRIBUTING.md:
#%         Commit message conventions.
#%     Utilities/Maintenance/UpdateRemoteModules.sh:
#%         Update the remote module GIT_TAG entries.

# Utility functions
usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

die() {
  echo "$@" 1>&2; exit 1
}

# Parse arguments
help=false
script=""
feature_branch=""
commit_message=""
while test $# -gt 0;
do
  opt="$1";
  case "$opt" in
    "-h"|"--help")
      usage
      exit 1;;
    *)
      break;;
  esac
done

script="$1"
feature_branch="$2"
commit_message="$3"

if test "${script}" = "" || test "${feature_branch}" = "" || test "${commit_message}" = "" || $help; then
  usage
  exit 1
fi

# Make sure we are inside the repository
cd "${BASH_SOURCE%/*}" &&

remote_modules_path='../../Modules/Remote'

# Ask for GitHub username and password once
echo "Please provide your GitHub (https://github.com)"
read -p "username: " username

remotes=()

function list_candidate_remotes() {
  if [ $# -ge 1 ] && [ $1 = "-ongreen" ]; then
    ongreen=1
    failing=()
  else
    ongreen=0
  fi

  # Loop over the remote modules' CMake files
  for filename in ${remote_modules_path}/*.cmake; do
    echo -n "."

    # Get the current module name
    module_name=$(grep -v "\s*#" $filename | grep -m 1 -o "itk_fetch_module(\(\w\|\.\|\-\|_\)*")
    module_name=${module_name/*itk_fetch_module(/}

    # Get the current commit hash
    curr_commit=$(grep -v "\s*#" $filename | grep -m 1 -o "GIT_TAG \(\w\|\.\|\-\|_\)*")
    curr_commit=${curr_commit/*GIT_TAG /}

    # Read the git repository information
    repository=$(grep -v "\s*#" $filename | grep -m 1 -o "GIT_REPOSITORY \https://github.com/\(\w\|\-\|_\|/\)*")
    repository=${repository/*GIT_REPOSITORY \${git_protocol\}:\/\/github.com\//}

    # Get the latest git commit hash of the remote module.
    # Remotes will usually not be tagged.
    latest_commit=$(git ls-remote https://github.com/$repository refs/heads/main)
    latest_commit=${latest_commit/[[:space:]]refs\/heads\/main/}

    # Skip remotes whose current commit in ITK differs from the latest
    if [ $curr_commit != $latest_commit ]; then
      continue
    fi

    # Get the remotes' build status
    if [ $ongreen -eq 1 ]; then
      # Get the repository status
      # Be careful of rate limiting (https://developer.github.com/v3/#rate-limiting)
      # alternatively using Azure: https://docs.microsoft.com/en-us/azure/data-factory/monitor-programmatically#rest-api
      status=$(curl -s https://api.github.com/repos/$repository/commits/$latest_commit/status | grep -m 1 -o "\"state\": \"\w*\"")
      if [ "$status" != "\"state\": \"success\"" ]; then
        continue
      fi
    fi

    remotes+=($module_name)

  done
}

function configure_itk_remotes() {
  mkdir ../../../ITK-build
  cd ../../../ITK-build

  cmake_remote_flags=()

  # Switch on all remotes that have a success build status
  for remote in ${remotes[@]}; do
    cmake_remote_flags+=(-D'Module_'$remote':BOOL=ON' )
  done

  cmake ${cmake_remote_flags[@]} ../ITK
}

function apply_script_and_push_remotes() {
  cd ../ITK/Modules/Remote

  # Loop over remotes and apply the changes
  for remote in ${remotes[@]}; do
    echo -e $remote

    cd $remote

    repository_basename=$(basename -s .git `git config --get remote.origin.url`)

    git checkout main || git checkout main
    git checkout -b $feature_branch origin/main || git checkout -b $feature_branch origin/main || git checkout $feature_branch
    $script

    # Add the files, adding filters if necessary, and redirecting stdout and
    # stderr to /dev/null in order to avoid a too verbose output and
    # displaying error messages if files with some given extensions are not
    # found.
    git add -u *.txt > /dev/null 2>&1
    git add -u *.cmake > /dev/null 2>&1
    git add -u *.rst > /dev/null 2>&1
    git add -u *.py > /dev/null 2>&1
    git add -u *.yml > /dev/null 2>&1
    git add -u *.c > /dev/null 2>&1
    git add -u *.cxx > /dev/null 2>&1
    git add -u *.h > /dev/null 2>&1
    git add -u *.hxx > /dev/null 2>&1
    git add -u *.wrap > /dev/null 2>&1

    # Commit and push to the feature branch
    git commit -m "$commit_message"

    read -ep "Push $feature_branch changes to git@github.com:$username/$repository_basename.git? [y/n]" dopush
    if [ $dopush = 'y' ]; then
      git push --quiet git@github.com:$username/$repository_basename.git $feature_branch
    fi

    cd ..
  done
}

echo -e "\nChecking remote modules build status..."
list_candidate_remotes
echo -e "\nDone checking remote modules build status."

echo -e "Configuring ITK with succeeding remote modules..."
configure_itk_remotes
echo -e "Done configuring ITK with succeeding remote modules."

echo -e "Applying script to remotes..."
apply_script_and_push_remotes
echo -e "Done applying script to remotes."

rm -Rf ../../../ITK-build

echo -e "Visit the remote repositories and open the corresponding pull requests."
