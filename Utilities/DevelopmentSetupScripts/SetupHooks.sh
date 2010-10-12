#!/usr/bin/env bash
#=============================================================================
# Copyright (c) 2010 Insight Software Consortium. All rights reserved.
# See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even
# the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
# PURPOSE.  See the above copyright notices for more information.
#=============================================================================


# Run this script to set up the git hooks for committing changes to ITK.
# For more information, see:
#   http://www.itk.org/Wiki/ITK/Git#Hooks
#   http://www.itk.org/Wiki/Git/Hooks


die() {
	echo 'failure during hook setup' 1>&2
	echo '-------------------------' 1>&2
	echo '' 1>&2
	echo "$@" 1>&2
	exit 1
}

u=$(cd "$(echo "$0"|sed 's/[^/]*$//')"; pwd)
cd "$u/../../.git/hooks"

# We need to have a git repository to do a pull.
if ! test -d ./.git; then
  git init || die "Could not run git init."
fi

# Grab the hooks.
# Use the local hooks if possible.
echo "Pulling the hooks..."
if GIT_DIR=.. git for-each-ref refs/remotes/origin/hooks 2>/dev/null | \
  grep -q '\<refs/remotes/origin/hooks$'; then
  git pull .. remotes/origin/hooks
else
  git pull http://public.kitware.com/ITK.git hooks || die "Downloading the hooks failed."
fi
cd ../..

# Set up uncrustify hook.
echo "Setting up the uncrustify hook..."
git config hooks.uncrustify.conf "Utilities/Maintenance/uncrustify_itk.cfg"

# Set up KWStyle hook.
echo "Setting up the KWStyle hook..."
git config hooks.KWStyle.conf "Utilities/KWStyle/ITK.kws.xml.in"
git config hooks.KWStyle.overwriteRulesConf "Utilities/KWStyle/ITKOverwrite.txt"
git config hooks.KWStyle true

echo "Done."
