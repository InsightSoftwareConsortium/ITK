#!/bin/bash

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


# Make sure we are inside the repository
cd "${BASH_SOURCE%/*}" &&

remote_modules_path=$(cd ../../Modules/Remote && pwd)

git_repository_tag_label='GIT_REPOSITORY'
git_tag_label='GIT_TAG'

# Loop over the remote modules' CMake files
for filename in ${remote_modules_path}/*.cmake; do

  # Get the current commit hash
  curr_commit_str=($(grep $git_tag_label $filename))
  curr_commit_hash=${curr_commit_str[1]}

  # Read the git repository information
  repository_str=$(grep $git_repository_tag_label $filename)

  repository_arr=($(echo $repository_str | tr "/" " "))
  organization=${repository_arr[-2]}
  repository_name=${repository_arr[-1]}

  # Get the latest git commit hash of the remote module.
  # Remotes will usually not be tagged.
  latest_commit_hash=$(git ls-remote git://github.com/$organization/$repository_name | \
  grep refs/heads/master | cut -f 1)

  # Sed the the latest commit hash in the CMake file
  sed -i "s/${curr_commit_hash}/${latest_commit_hash}/g" $filename

done
