#!/bin/bash

program_name=$0

function usage {
  echo "Usage: ${program_name} "
  echo "  param1 Azure pipelines file path"
  exit 1
}

# Display usage
if [ ${#@} != 1 ]; then
  usage
fi

filename=$1

# Get the latest ITK git tag
latest_git_tag=$(curl -s "https://api.github.com/repos/InsightSoftwareConsortium/ITK/tags" | jq '.[0].name' )

git_tag_label='ITKGitTag: '

# Read the ITK git tag information
curr_git_str=($(grep $git_tag_label $filename))
curr_git_tag=${curr_git_str[1]}

# Sed the tag in the CMake file
sed -i "s/${curr_git_tag}/${latest_git_tag}/g" $filename
