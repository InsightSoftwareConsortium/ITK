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
#==========================================================================

#% NAME
#%     BumpITKVersionInCI.sh - set the ITK version in remote module CI files
#%
#% SYNOPSIS
#%     BumpITKVersionInCI.sh
#%     BumpITKVersionInCI.sh -h | --help
#%
#% DESCRIPTION
#%     Update the ITK version used by the continuous integration pipelines
#%     of an ITK remote module. The script asks for the ITK C++ commit tag
#%     and the ITK Python wheel tag, then edits in place every file under
#%     the current directory whose path contains ".yml":
#%
#%     - itk-git-tag is set to the C++ tag.
#%     - itk-python-git-tag (as a list item) and itk-wheel-tag are set to
#%       the wheel tag.
#%     - python-version lists become [311, 312], and python-version-minor
#%       lists become [11, 12], whatever the tags entered.
#%     - set PATH="C:\P\grep;%PATH%" loses its quotes, for Windows builds.
#%
#%     These edits move a module from the CI of ITK before 6.0.0 to the
#%     current one. Nothing is committed.
#%
#% OPTIONS
#%     -h, --help
#%         Print this help and exit.
#%
#% IMPLEMENTATION
#%     Run from the root of the remote module checkout. The search includes
#%     hidden directories such as .git and .github. The script needs GNU
#%     sed, because it calls sed -i -r without a backup suffix.
#%
#% EXAMPLES
#%     cd ITKMyModule && ../ITK/Utilities/Maintenance/BumpITKVersionInCI.sh
#%         Prompt for the tags and update the module's CI files.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Utilities/Maintenance/UpdateRequiredITKVersionInRemoteModules.sh:
#%         Update the required ITK version of a remote module.

usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

case "${1-}" in
  -h | --help)
    usage
    exit 0
    ;;
esac

# Get the new ITK version
read -ep "Please enter the ITK C++ commit tag >> " itkGitTag
echo $itkGitTag
read -ep "Please enter the ITK Python wheel tag >> " itkWheelTag
echo $itkWheelTag

# Update ITK version in CI .yml pipeline files.
# Updating from old CI (ITK < 6.0.0) requires a few changes:
# - Update ITK and ITK Python tags;
# - Deprecate all python versions less than 3.11
# - make packages for python 3.11 and python 3.12 (no sabi) to be compatible with VTKGlue module
# - Bug fix for PATH string in Windows builds
find . -type f | \
    fgrep ".yml" | \
    tr \\n \\0 | xargs -0 sed -i -r \
	-e "s#itk-git-tag: .*#itk-git-tag: \"$itkGitTag\"#g" \
	-e "s#- itk-python-git-tag: .*#- itk-python-git-tag: \"$itkWheelTag\"#g" \
	-e "s#itk-wheel-tag: .*#itk-wheel-tag: \"$itkWheelTag\"#g" \
	-e 's#python-version: \[.*#python-version: [311, 312]#g' \
	-e 's#python-version-minor: \[.*#python-version-minor: [11, 12]#g' \
        -e 's#set PATH="C:\\P\\grep;%PATH%"#set PATH=C:\\P\\grep;%PATH%#g'
