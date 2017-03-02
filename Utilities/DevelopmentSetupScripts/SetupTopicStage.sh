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


# Run this script to set up the topic stage for pushing changes.

egrep-q() {
  egrep "$@" >/dev/null 2>/dev/null
}

die() {
  echo 'Failure during topic stage setup.' 1>&2
  echo '---------------------------------' 1>&2
  echo '' 1>&2
  echo "$@" 1>&2
  exit 1
}

# Make sure we are inside the repository.
cd "$(echo "$0"|sed 's/[^/]*$//')"

if git config remote.stage.url >/dev/null; then
  echo "Topic stage remote was already configured."
else
  echo "Configuring the topic stage remote..."
  git remote add stage https://itk.org/stage/ITK.git || \
    die "Could not add the topic stage remote."
  git config remote.stage.pushurl git@itk.org:stage/ITK.git
fi

echo -e "\nNote: push access is *NOT* required to get patches merged. Choose No (N) if you are unsure."
read -ep "Do you want to test push access to itk.org (only experienced contributors have push access)? [y/N]: " access
if [ "$access" == "y" ] || [ "$access" == "Y" ]; then

  echo "Configuring push urls..."
  if ! git config remote.origin.url | egrep-q "://itk.org/ITK.git"; then
    git config remote.origin.pushurl git@itk.org:ITK.git
  fi

  echo -e "Done.\n"

  # We will have the private key corresponding the public key at itk.org at
  # ~/.ssh/id_git_itk.  This allows the developer to keep a single public key
  # on file with the server across multiple machines.
  if ! egrep-q 'Host itk\.org' ~/.ssh/config; then
    echo "Configuring the IdentityFile for itk.org to be ~/.ssh/id_git_itk..."
    mkdir -p ~/.ssh
    chmod og-rwx ~/.ssh
    echo "Host itk.org" >> ~/.ssh/config
    echo "  IdentityFile=~/.ssh/id_git_itk" >> ~/.ssh/config
    chmod 600 ~/.ssh/config
  fi
  if ! test -e ~/.ssh/id_git_itk; then
    if test -f ~/.ssh/id_rsa; then
      # Take care of the common case.
      pushd ~/.ssh >/dev/null
      ln -s id_rsa id_git_itk
      popd >/dev/null
      cat << EOF

Assuming ~/.ssh/id_rsa is the private key corresponding to the public key given
for the 'git' user at itk.org.  If this is not the case, please place the
appropriate private key at ~/.ssh/id_git_itk.

EOF
      read -e -n 1 -p "Press any key to continue..."
    else
      cat << EOF

Please place the private key corresponding to the public key registered at
itk.org in '~/.ssh/id_git_itk'.

EOF
      read -e -n 1 -p "Press any key to continue..."
    fi
  fi
  echo "Testing ssh capabilities..."
  git ls-remote git@itk.org:stage/ITK.git refs/heads/master || \
    die "SSH test to git@itk.org failed. You may need to request access at:

https://www.kitware.com/Admin/SendPassword.cgi

Note that push access to the stage/ITK is separate to Gerrit.
"

  echo "Test successful! ITK push access confirmed. Summary of project access:"
  echo
  # This command queries gitolite for your access privileges
  ssh git@itk.org info

fi

echo "Done."
