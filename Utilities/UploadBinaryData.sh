#!/usr/bin/env bash
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

# Utility functions
print_help() {
cat << EOF
Usage: $0 [--folder-id <folder_id>] <binary/file/path/in/repo/1> [<binary/file/path/in/repo/2> ...]


Use this script to upload binary testing data for ITK and related projects.

Binary data, e.g. test input images, are not stored in Git because they
will bloat a Git repository's size.


To use the script:

1. Sign up for an account at https://data.kitware.com
2. Place the binary file at the desired location in the Git repository.
3. Run this script, and pass in the binary file(s) as arguments to script.
4. In test/CMakeLists.txt, use the itk_add_test macro and reference the file
   path with \`DATA\` and braces, e.g.: DATA{<Relative/Path/To/Source/Tree/File>}.
5. Re-build ITK, and the testing data will be downloaded into the build tree.


If the Git `girder.api-key` config or `GIRDER_API_KEY` environmental variable
is not set, a prompt will appear for your username and password. The API key
can be created in the data.kitware.com user account web browser interface.

The script will authenticate to data.kitware.com, upload the file to your
user account's Public folder, and create a *.sha512 CMake ExternalData
content link file. To specify a different folder, use the --folder flag.
After the content link has been created, add the *.sha512 file to your
git commit. The binary file will be removed from the source tree following
upload.
EOF
}

die() {
  echo "$@" 1>&2; exit 1
}

json_field() {
  local key=$1
  local json=$2
  echo $json | awk 'BEGIN { FS="\""; RS="," }; { if ($2 == "'$key'") {print $4} }'
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
        "-f"|"--folder-id")
          shift;
          folder_id="$1"
          shift;;
        *)
          break;;
   esac
done
binary_files="$@"

if test "${binary_files}" = "" || $help; then
  print_help
  exit 1
fi


# Check for required dependencies
if ! type curl > /dev/null; then
  die "Please install the curl executable."
fi
if ! type wc > /dev/null; then
  die "Please install the wc executable."
fi


# Authenticate
token=""
git_config_api_key=$(git config --get girder.api-key || echo '')
if test -n "${git_config_api_key}"; then
  token_response=$(curl -s -X POST --header 'Content-Length: 0' --header 'Content-Type: application/json' --header 'Accept: application/json' "https://data.kitware.com/api/v1/api_key/token?key=${git_config_api_key}&duration=1" || die "Could not retrieve token from API key.")
  token=$(json_field "token" "${token_response}")
fi
if test -z "${token}" -a -n "${GIRDER_API_KEY}"; then
  token_response=$(curl -s -X POST --header 'Content-Length: 0' --header 'Content-Type: application/json' --header 'Accept: application/json' "https://data.kitware.com/api/v1/api_key/token?key=${GIRDER_API_KEY}&duration=1" || die "Could not retrieve token from API key.")
  token=$(json_field "token" "${token_response}")
fi
if test -z "${token}"; then
  if ! type base64 > /dev/null; then
    die "Please install the base64 executable."
  fi
  echo "Please provide your"
  echo ""
  echo "    https://data.kitware.com"
  echo ""
  read -p "username: " username
  read -p "password: " -s password
  basic_content=$(echo -n "${username}:${password}" | base64)
  token_response=$(curl -s -X GET --header "Girder-Authorization: Basic ${basic_content}" --header 'Accept: */*' --header 'Host: data.kitware.com' --header 'Referer: https://data.kitware.com' 'https://data.kitware.com/api/v1/user/authentication' || die "Could not retrieve token from username / password.")
  token=$(json_field "token" "${token_response}")
fi
if test -z "${token}"; then
  die "Could not authenticate to https://data.kitware.com"
fi


# Get user / folder
user_id_response=$(curl -s -X GET --header 'Accept: application/json' --header "Girder-Token: ${token}" 'https://data.kitware.com/api/v1/user/me' || die 'Could not get user id.')
user_id=$(json_field "_id" "${user_id_response}")

if test -z "$folder_id"; then
  folder_id_response=$(curl -s -X GET --header 'Accept: application/json' --header "Girder-Token: ${token}" "https://data.kitware.com/api/v1/folder?parentType=user&parentId=${user_id}&name=Public&limit=3&sort=lowerName&sortdir=1" || die 'Could not get folder id.')
  folder_id=$(json_field "_id" "${folder_id_response}")
fi


# Upload files and create content links
generated_content_links=""
md5_content_link_conflicts=""
for binary_file in $binary_files; do
  if test ! -e $binary_file; then
    die "$binary_file does not exist."
  fi
  item_name=$(basename "$binary_file")

  create_item_response=$(curl -s -X POST --header 'Content-Length: 0' --header 'Content-Type: application/x-www-form-urlencoded' --header 'Accept: application/json' --header "Girder-Token: ${token}" "https://data.kitware.com/api/v1/item?folderId=${folder_id}&name=${item_name}&description=ITK%20testing%20data%20uploaded%20by%20ITK%2FUtilities%2FUploadBinaryData.sh&reuseExisting=true" || die 'Could not create item.')
  item_id=$(json_field "_id" "${create_item_response}")

  size=$(wc -c < "$binary_file" | sed 's/^[[:space:]]*//')
  echo "Uploading ${item_name}..."
  upload_file_response=$(curl -X POST --data-binary "@${binary_file}" --header 'Content-Type: application/json' --header 'Accept: application/json' --header "Girder-Token: ${token}" "https://data.kitware.com/api/v1/file?parentType=item&parentId=${item_id}&name=${item_name}&size=${size}" || die 'Could not upload file.')
  file_id=$(json_field "_id" "${upload_file_response}")

  curl -s -X GET --output "${binary_file}.sha512" --header 'Accept: text/plain' --header "Girder-Token: ${token}" "https://data.kitware.com/api/v1/file/${file_id}/hashsum_file/sha512" || die 'Could not get file sha512sum.'
  generated_content_links="$generated_content_links ${binary_file}.sha512"
  if type cmake > /dev/null; then
    cmake_sha512sum=$(cmake -E sha512sum "$binary_file" 2> /dev/null)
    # If sufficient CMake version, ...
    if test $? -eq 0; then
      local_sha512=$(echo $cmake_sha512sum | awk '{print $1}')
      remote_sha512=$(cat "${binary_file}.sha512")
      if test "$local_sha512" != "$remote_sha512"; then
        die "Local file hash does not match uploaded file hash."
      fi
    fi
  fi

  md5_content_link="${binary_file}.md5"
  if test -e "$md5_content_link"; then
    md5_content_link_conflicts="$md5_content_link_conflicts $md5_content_link"
  fi
  rm $binary_file
done


# Recommend next steps
cat << EOF

Testing data upload complete.

Now run:

  git add --$generated_content_links

EOF
if test -n "$md5_content_link_conflicts"; then
cat << EOF
and:

  git rm --$md5_content_link_conflicts
EOF
fi
