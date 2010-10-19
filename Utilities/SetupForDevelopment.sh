#!/usr/bin/env bash
#=============================================================================
# Copyright (c) 2010 Insight Software Consortium. All rights reserved.
# See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even
# the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
# PURPOSE.  See the above copyright notices for more information.
#=============================================================================


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
