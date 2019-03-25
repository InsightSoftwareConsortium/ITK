#!/bin/bash

program_name=$0

function usage {
  echo "Usage: ${program_name} "
  echo "  param1 Setup.py file path"
  exit 1
}

# Display usage
if [ ${#@} != 1 ]; then
  usage
fi

filename=$1

# Get the latest ITK git tag
latest_git_tag=$(curl -s "https://api.github.com/repos/InsightSoftwareConsortium/ITK/tags" | jq '.[0].name' )

# Strip the inverted commas
latest_git_tag=$(echo $latest_git_tag | tr -d '"')

# Strip the "v" prefix
latest_git_tag=${latest_git_tag:1}

git_install_req_tag_label='\install_requires=\['

# Read the ITK install requirement git tag information
git_install_req_tag_str=($(grep -A1 -P ${git_install_req_tag_label}$ $filename))
git_install_req_tag=${git_install_req_tag_str[1]}

git_install_req_tag_arr=($(echo $git_install_req_tag | tr "=" " "))
curr_git_tag=${git_install_req_tag_arr[-1]}

# Sed the tag in the CMake file
sed -i "s/${curr_git_tag}/${latest_git_tag}/g" $filename
