#!/usr/bin/env bash
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


# Run this script to set up development with git.

die() {
	echo 'failure during git development setup' 1>&2
	echo '------------------------------------' 1>&2
	echo '' 1>&2
	echo "$@" 1>&2
	exit 1
}

# Make sure we are inside the repository.
cd "$(echo "$0"|sed 's/[^/]*$//')"/..

if test -d .git/.git; then
  die "The directory '.git/.git' exists, indicating a configuration error.

Please 'rm -rf' this directory."
fi

echo "Downloading submodules..."
git submodule update --init || die "Failure downloading submodules."
echo -e "Done.\n"

# add an "upstream" remote to make easier to maintain a fork outside of itk.org,
# with an origin which is not itk.org
if [ "`git config remote.origin.url`" != "git://itk.org/ITK.git" ]; then
  echo "Setting up upstream remote..."
  if ! git config remote.upstream.url > /dev/null ; then
    git remote add upstream git://itk.org/ITK.git
    git remote set-url --push upstream git@itk.org:ITK.git
    echo "Done"
  else
    echo "upstream is already configured."
  fi
  echo
fi

cd Utilities/DevelopmentSetupScripts

echo "Setting up git hooks..."
./SetupHooks.sh || exit 1
echo

echo "Setting up Gerrit..."
./SetupGerrit.sh || exit 1
echo

echo "Setting up the topic stage..."
./SetupTopicStage.sh || exit 1
echo

echo "Setting up useful Git aliases..."
./SetupGitAliases.sh || exit 1
echo

echo "Suggesting git tips..."
./GitTips.sh || exit 1
echo
