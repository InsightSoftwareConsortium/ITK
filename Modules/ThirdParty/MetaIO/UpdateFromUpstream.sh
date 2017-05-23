#!/usr/bin/env bash

# We can optionally pass in the desired repository and branch. For example,
#
# ./Modules/ThirdParty/MetaIO/UpdateFromUpstream.sh \
#   https://github.com/Kitware/MetaIO.git \
#   pull/30/head:large-uncompression

thirdparty_module_name='MetaIO'

set -x
if [[ $# -ge 1 ]]; then
  upstream_git_url=$1
else
  upstream_git_url='https://github.com/Kitware/MetaIO.git'
fi
if [[ $# -ge 2 ]]; then
  upstream_git_branch=$2
else
  upstream_git_branch='master'
fi

snapshot_author_name='MetaIO Maintainers'
snapshot_author_email='metaio@itk.org'

snapshot_redact_cmd=''
snapshot_relative_path='src/MetaIO'
snapshot_paths='
  src
  License.txt
  '

source "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/UpdateThirdPartyFromUpstream.sh"
update_from_upstream
