#!/usr/bin/env bash
#=============================================================================
# Copyright (c) 2010 Insight Software Consortium. All rights reserved.
# See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even
# the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
# PURPOSE.  See the above copyright notices for more information.
#=============================================================================


# Run this script to set up the git repository to push to the Gerrit code review
# system.


die() {
	echo 'failure during Gerrit setup' 1>&2
	echo '---------------------------' 1>&2
	echo '' 1>&2
	echo "$@" 1>&2
	exit 1
}

# Make sure we are inside the repository.
cd "$(echo "$0"|sed 's/[^/]*$//')"

if git config remote.gerrit.url >/dev/null; then
  echo "Gerrit was already configured."
else
  cat << EOF
Gerrit is a code review system that works with Git.

In order to use Gerrit, an account must be registered at the review site:

  http://review.source.kitware.com/p/ITK

In order to register you need an OpenID

  http://openid.net/get-an-openid/

EOF
  read -ep "Enter your gerrit user [$USER]: " gu
  if [ "$gu" == "" ]; then
   # Use current user name.
   gu=$USER
  fi
  echo -e "\nConfiguring 'gerrit' remote with user '$gu'..."
  git remote add gerrit $gu@review.source.kitware.com:ITK || \
    die "Could not add gerrit remote."
  cat << EOF

For more information on Gerrit usage, see

  http://www.itk.org/Wiki/ITK/Gerrit
  http://www.itk.org/Wiki/ITK/Gerrit/Primer
EOF
fi

echo -e "\nFetching from gerrit..."
git fetch gerrit || die "Could not fetch gerrit remote."

echo "Done."
