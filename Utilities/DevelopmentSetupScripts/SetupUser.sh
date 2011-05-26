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


# Run this script to set up basic user information.


die() {
  echo 'Failure during user information setup.' 1>&2
  echo '--------------------------------------' 1>&2
  echo '' 1>&2
  echo "$@" 1>&2
  exit 1
}

setup_user() {
  read -ep "Please enter your full name, such as 'John Doe': " name
  echo "Setting name to '$name'"
  git config user.name "$name"
  read -ep "Please enter your email address, such as 'john@gmail.com': " email
  echo "Setting email address to '$email'"
  git config user.email "$email"
}

# Added some logic to introduce yourself to Git.
gitName=$(git config user.name)
gitEmail=$(git config user.email)
if [ "$gitName" == "" ] || [ "$gitEmail" == "" ]; then
  setup_user
fi

# Loop until the user is happy with the authorship information
for (( ; ; ))
do
  # Display the final user information.
  gitName=$(git config user.name)
  gitEmail=$(git config user.email)
  echo "Your commits will have the following author:

  $gitName <$gitEmail>
"
  read -ep "Is the author name and email address above correct? [Y/n] " correct
  if [ "$correct" == "n" ] || [ "$correct" == "N" ]; then
    setup_user
  else
    break
  fi
done
