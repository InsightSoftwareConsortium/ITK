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

echo -e "\nConfiguring GerritId hook..."
if git config hooks.GerritId >/dev/null; then
  echo "GerritId hook already configured."
else
    cat << EOF
This hook automatically add a "Change-Id" footer to commit messages
to make interaction with Gerrit easier.
To disable this feature, run

  git config hooks.GerritId false

EOF
  git config hooks.GerritId true
  echo "Done."
fi
