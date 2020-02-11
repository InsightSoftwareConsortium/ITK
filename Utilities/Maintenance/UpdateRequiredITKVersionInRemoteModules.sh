#!/bin/bash

#==========================================================================
#
#   Copyright NumFOCUS
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


# This script, given the location of the 'azure-pipelines.yml' and 'setup.py'
# files of a remote module, updates the required ITK version to its latest in
# them.
#
# Since the required ITK version update is a major change, the Python package
# of the remote module is updated to a new major version.


# Utility functions
usage() {
cat << EOF
Usage: $0 <azure_pipelines_ci_filename> <python_setup_filename>

Use this script to update the required ITK version to its latest in the
'azure-pipelines.yml' and 'setup.py' files of a remote module.

Since the required ITK version update is a major change, the
Python package of the remote module is updated to a new major version.
EOF
}

die() {
  echo "$@" 1>&2; exit 1
}

# Parse arguments
help=false
folder_id=""
while test $# -gt 0;
do
  opt="$1";
  case "$opt" in
    "-h"|"--help")
      shift;
      help=true
      break;;
    *)
      break;;
  esac
done

azure_pipelines_ci_filename=$1
python_setup_filename=$2

if test "${azure_pipelines_ci_filename}" = "" || test "${python_setup_filename}" = "" || $help; then
  usage
  exit 1
fi

# Get the latest ITK git tag
latest_git_tag=$(git ls-remote --tags \
git://github.com/InsightSoftwareConsortium/ITK.git | \
awk -F/ '{ print $3 }' | egrep -v 'a0|b0|rc|}' | \
sort -n - | tail -n1)

# Azure pipeline CI file
git_tag_label='ITKGitTag: '

# Read the ITK git tag information
current_git_str=($(grep $git_tag_label $azure_pipelines_ci_filename))
current_git_tag=${current_git_str[1]}

# Sed the latest ITK git tag in the Azure pipelines config file
sed -i "s/${git_tag_label}${current_git_tag}/${git_tag_label}${latest_git_tag}/g" $azure_pipelines_ci_filename


# Get the latest ITK Python git tag
latest_git_tag=$(git ls-remote --tags \
git://github.com/InsightSoftwareConsortium/ITKPythonBuilds.git | \
awk -F/ '{ print $3 }' | egrep -v 'a0|b0|rc|}' | \
sort -n - | tail -n1)

python_git_tag_label='ITKPythonGitTag: '

# Read the ITK git tag information
current_git_str=($(grep $python_git_tag_label $azure_pipelines_ci_filename))
current_git_tag=${current_git_str[1]}

# Sed the latest ITK git tag in the Azure pipelines config file
sed -i "s/${python_git_tag_label}${current_git_tag}/${python_git_tag_label}${latest_git_tag}/g" $azure_pipelines_ci_filename

# Python setup file

# Strip the "v" prefix
latest_git_tag=${latest_git_tag:1}

git_install_req_tag_label='\install_requires=\['

# Read the ITK install requirement git tag information
git_install_req_tag_str=($(grep -A1 -P ${git_install_req_tag_label}$ $python_setup_filename))
git_install_req_tag=${git_install_req_tag_str[1]}

git_install_req_tag_arr=($(echo $git_install_req_tag | tr "=" " "))
current_git_tag=${git_install_req_tag_arr[-1]}

# Sed the latest ITK git tag in the Python setup file
sed -i "s/${current_git_tag}/${latest_git_tag}'/g" $python_setup_filename

pckg_version_label='version'

# Read the module Python package version tag information
pckg_version_tag_str=($(grep -A1 -P ${pckg_version_label} $python_setup_filename))

# Strip the ending comma
pckg_version_tag_str=${pckg_version_tag_str::-1}

pckg_version_tag_arr=($(echo $pckg_version_tag_str | tr "=" " "))

pckg_version=${pckg_version_tag_arr[1]}

# Strip the inverted commas
pckg_version=$(echo $pckg_version | tr -d "\'")

pckg_version_arr=($(echo $pckg_version | tr "." " "))
pckg_patch_version=${pckg_version_arr[2]}
new_pckg_patch_version=$((pckg_patch_version + 1))

# Update to a new major version
new_pckg_version="${pckg_version_tag_str::-2}${new_pckg_patch_version}'"

sed -i "s/${pckg_version_tag_str}/${new_pckg_version}/g" $python_setup_filename
