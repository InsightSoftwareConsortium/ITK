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

# Script to automate ITK version upgrade ("bump") in ITK remote module
# continuous integration pipelines.

# Get the new ITK version
read -ep "Please enter the ITK C++ commit tag >> " itkGitTag
echo $itkGitTag
read -ep "Please enter the ITK Python wheel tag >> " itkWheelTag
echo $itkWheelTag

# Update ITK version in CI .yml pipeline files.
# Updating from old CI (ITK < 5.2.0) requires a few changes:
# - Update ITK and ITK Python tags;
# - Deprecate Python 3.6 and add Python 3.10 wheels
# - Bug fix for PATH string in Windows builds
find . -type f | \
    fgrep ".yml" | \
    tr \\n \\0 | xargs -0 sed -i -r \
	-e "s#itk-git-tag: .*#itk-git-tag: \"$itkGitTag\"#g" \
	-e "s#- itk-python-git-tag: .*#- itk-python-git-tag: \"$itkWheelTag\"#g" \
	-e "s#itk-wheel-tag: .*#itk-wheel-tag: \"$itkWheelTag\"#g" \
	-e 's#python-version: \[.*#python-version: [37, 38, 39, 310]#g' \
	-e 's#python-version-minor: \[.*#python-version-minor: [7, 8, 9, 10]#g' \
        -e 's#set PATH="C:\\P\\grep;%PATH%"#set PATH=C:\\P\\grep;%PATH%#g'
