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


program_name=$0

function usage {
  echo "Usage: ${program_name} "
  echo "  param1 Azure pipelines file path"
  echo "  param2 Setup.py file path"
  exit 1
}

# Display usage
if [ ${#@} != 2 ]; then
  usage
fi

azure_pipelines_ci_filename=$1
python_setup_filename=$2

# Get the latest ITK git tag
latest_git_tag=$(git ls-remote --tags --sort="v:refname" \
git://github.com/InsightSoftwareConsortium/ITK.git | tail -n1 | \
sed 's/.*\///; s/\^{}//')

# Azure pipeline CI file
git_tag_label='ITKGitTag: '

# Read the ITK git tag information
curr_git_str=($(grep $git_tag_label $filename))
curr_git_tag=${curr_git_str[1]}

# Sed the latest ITK git tag in the Azure pipelines config file
sed -i "s/${curr_git_tag}/${latest_git_tag}/g" $azure_pipelines_ci_filename

# Python setup file

# Strip the "v" prefix
latest_git_tag=${latest_git_tag:1}

git_install_req_tag_label='\install_requires=\['

# Read the ITK install requirement git tag information
git_install_req_tag_str=($(grep -A1 -P ${git_install_req_tag_label}$ $filename))
git_install_req_tag=${git_install_req_tag_str[1]}

git_install_req_tag_arr=($(echo $git_install_req_tag | tr "=" " "))
curr_git_tag=${git_install_req_tag_arr[-1]}

# Sed the latest ITK git tag in the Python setup file
sed -i "s/${curr_git_tag}/${latest_git_tag}/g" $python_setup_filename
