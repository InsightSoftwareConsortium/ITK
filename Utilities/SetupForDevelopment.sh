#!/usr/bin/env bash
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

# Run this script to set up your local repository for development with Git.

cd "${BASH_SOURCE%/*}/.." &&
Utilities/GitSetup/setup-user && echo &&
Utilities/GitSetup/setup-hooks && echo &&
Utilities/GitSetup/setup-git-aliases && echo &&
(Utilities/GitSetup/setup-upstream ||
 echo 'Failed to setup origin.  Run this again to retry.') && echo &&
(Utilities/GitSetup/setup-github ||
 echo 'Failed to setup GitHub.  Run this again to retry.') && echo &&
(Utilities/GitSetup/setup-girder-api-key ||
 echo 'Failed to setup the Girder API key.  Run this again to retry.') && echo &&
Utilities/GitSetup/tips &&
Utilities/GitSetup/github-tips

# Rebase master by default
git config rebase.stat true
git config branch.master.rebase true

# Disable old Gerrit hooks
hook=$(git config --get hooks.GerritId) &&
if "$hook"; then
	echo '
ITK has migrated from Gerrit to GitHub for code reviews.

Disabling the GerritId hook that adds a "Change-Id" footer to commit
messages for interaction with Gerrit. Also, removing the "gerrit" and "stage" remote.' &&
	git config hooks.GerritId false
	git config --get remote.gerrit.url > /dev/null && \
          git remote remove gerrit
	git config --get remote.stage.url > /dev/null && \
          git remote remove stage
fi

# Blame should ignore bulk style changes by default
git config blame.ignoreRevsFile .git-blame-ignore-revs

# Style hook configuration
git config hooks.KWStyle.conf "Utilities/KWStyle/ITK.kws.xml"
git config hooks.KWStyle.overwriteRulesConf "Utilities/KWStyle/ITKOverwrite.txt"

# Make sure we are inside the repository.
cd "$(echo "$0"|sed 's/[^/]*$//')"/..

if test -d .git/.git; then
  die "The directory '.git/.git' exists, indicating a configuration error.

Please 'rm -rf' this directory."
fi

# Check to make sure we have a new enough git.
git_required_major_version=1
git_required_minor_version=6
git_required_release_version=6
git_required_bugfix_version="" # Use "" if there is no bugfix version.
echo "Checking Git version..."
git_version=$(git --version | awk '{print $3}')
declare -a git_version_arr
git_version_arr=(`echo ${git_version//./ }`)
insufficient_version() {
  if test -z "${git_required_bugfix_version}"; then
    git_required_version="${git_required_major_version}.${git_required_minor_version}.${git_required_release_version}"
  else
    git_required_version="${git_required_major_version}.${git_required_minor_version}.${git_required_release_version}.${git_required_bugfix_version}"
  fi
  die "Insufficient Git version.

Detected version was
  ${git_version}
and the minimum required version is
  ${git_required_version}"
}
if test ${git_version_arr[0]} -lt $git_required_major_version; then
  insufficient_version
elif test ${git_version_arr[0]} -eq $git_required_major_version; then
  if test ${git_version_arr[1]} -lt $git_required_minor_version; then
    insufficient_version
  elif test ${git_version_arr[1]} -eq $git_required_minor_version; then
    if test ${git_version_arr[2]} -lt $git_required_release_version; then
      insufficient_version
    elif test ${git_version_arr[2]} -eq $git_required_release_version; then
      if test -n "${git_required_bugfix_version}" -a \
         -n "${git_version_arr[3]}" -o \
         ${git_version_arr[3]} -lt $git_required_bugfix_version; then
        insufficient_version
      fi
    fi
  fi
fi
echo -e "Git version $git_version is OK.\n"


# Record the version of this setup so Hooks/pre-commit can check it.
SetupForDevelopment_VERSION=9
git config hooks.SetupForDevelopment ${SetupForDevelopment_VERSION}
