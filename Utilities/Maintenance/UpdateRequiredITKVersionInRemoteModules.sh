#!/bin/bash

#==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/


#% NAME
#%     UpdateRequiredITKVersionInRemoteModules.sh - require the latest ITK
#%
#% SYNOPSIS
#%     UpdateRequiredITKVersionInRemoteModules.sh <azure_pipelines_ci_filename>
#%                                                <python_setup_filename>
#%     UpdateRequiredITKVersionInRemoteModules.sh -h | --help
#%
#% DESCRIPTION
#%     Update a remote module to require the latest ITK release. In the Azure
#%     Pipelines file, the ITKGitTag value becomes the latest ITK tag and the
#%     ITKPythonGitTag value becomes the latest ITKPythonBuilds tag. In the
#%     Python setup file, the ITK version read from the line after
#%     install_requires=[ is replaced, wherever it appears in the file, by the
#%     latest ITKPythonBuilds tag without its leading v, and the third number of
#%     the package version is increased by one. The intent is a new major
#%     version of the package, but the code changes the third number, and
#%     assumes it is a single digit.
#%
#%     The latest tag is the one that sorts last after removing tags that
#%     contain a0, b0, or rc. The sort is a plain text sort, so v5.10.0
#%     sorts before v5.9.0. Both files are edited in place.
#%
#% OPTIONS
#%     <azure_pipelines_ci_filename>
#%         The module's azure-pipelines.yml.
#%     <python_setup_filename>
#%         The module's setup.py.
#%     -h, --help
#%         Print this help and exit with status 1. The help is also
#%         printed, with status 1, when either file name is missing.
#%
#% IMPLEMENTATION
#%     Needs network access to github.com (git ls-remote of the ITK and
#%     ITKPythonBuilds repositories), GNU grep (for -P), and bash 4.2 or
#%     later. Nothing is committed.
#%
#% EXAMPLES
#%     UpdateRequiredITKVersionInRemoteModules.sh azure-pipelines.yml \
#%       setup.py
#%         Run from the top of a remote module checkout.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Documentation/Maintenance/Release.md:
#%         The release step that runs this script on the remote modules.
#%     Utilities/Maintenance/ApplyScriptToRemotes.sh:
#%         Apply a script to the remote modules of an ITK source tree.


# Utility functions
usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
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
https://github.com/InsightSoftwareConsortium/ITK.git | \
awk -F/ '{ print $3 }' | egrep -v 'a0|b0|rc|}' | \
sort -n - | tail -n1)

# Azure pipeline CI file
git_tag_label='ITKGitTag: '

# Read the ITK git tag information
current_git_str=($(grep $git_tag_label $azure_pipelines_ci_filename))
current_git_tag=${current_git_str[1]}

# Sed the latest ITK git tag in the Azure pipelines config file
sed -i.bak -e "s/${git_tag_label}${current_git_tag}/${git_tag_label}${latest_git_tag}/g" $azure_pipelines_ci_filename
rm "${azure_pipelines_ci_filename}.bak"


# Get the latest ITK Python git tag
latest_git_tag=$(git ls-remote --tags \
https://github.com/InsightSoftwareConsortium/ITKPythonBuilds.git | \
awk -F/ '{ print $3 }' | egrep -v 'a0|b0|rc|}' | \
sort -n - | tail -n1)

python_git_tag_label='ITKPythonGitTag: '

# Read the ITK git tag information
current_git_str=($(grep $python_git_tag_label $azure_pipelines_ci_filename))
current_git_tag=${current_git_str[1]}

# Sed the latest ITK git tag in the Azure pipelines config file
sed -i.bak -e "s/${python_git_tag_label}${current_git_tag}/${python_git_tag_label}${latest_git_tag}/g" $azure_pipelines_ci_filename
rm "${azure_pipelines_ci_filename}.bak"

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
sed -i.bak -e "s/${current_git_tag}/${latest_git_tag}'/g" $python_setup_filename
rm "${python_setup_filename}.bak"

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

sed -i.bak -e "s/${pckg_version_tag_str}/${new_pckg_version}/g" $python_setup_filename
rm "${python_setup_filename}.bak"
