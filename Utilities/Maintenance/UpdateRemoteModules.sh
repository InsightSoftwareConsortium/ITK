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

# Loop over the remote modules' CMake files
for filename in ${remote_modules_path}/*.cmake; do

  # Get the current commit hash
  curr_commit=$(grep -m 1 -o "GIT_TAG \(\w\|\.\|\-\|_\)*" $filename)
  curr_commit=${curr_commit/GIT_TAG /}

  # Read the git repository information
  repository=$(grep -m 1 -o "^\s*GIT_REPOSITORY \${git_protocol}://github.com/\(\w\|\.\|\-\|_\|/\)*" $filename)
  repository=${repository/*GIT_REPOSITORY \${git_protocol\}:\/\/github.com\//}
  repository=${repository/.git/}

  # Get the latest git commit hash of the remote module.
  # Remotes will usually not be tagged.
  latest_commit=$(git ls-remote git://github.com/$repository refs/heads/master)
  latest_commit=${latest_commit/	refs\/heads\/master/}

  if [ $curr_commit = $latest_commit ]; then
    echo "$repository is already up to date"
    continue
  fi

  if [ ${#@} -eq 1 ] && [ $1 = "-ongreen" ]; then
    # Get the repository status
    # Be careful of rate limiting (https://developer.github.com/v3/#rate-limiting)
    status=$(curl -s https://api.github.com/repos/$repository/commits/$latest_commit/status | grep -m 1 "state")
    if [ "$status" != "  \"state\": \"success\"," ]; then
      echo "$repository's latest build was unsuccessful, skipping file"
      continue
    fi
  fi

  # Search and replace the latest commit hash in the CMake file
  ex -sc "%s/${curr_commit}/${latest_commit}/g|x" $filename

  echo "$repository was updated to latest commit"

done
