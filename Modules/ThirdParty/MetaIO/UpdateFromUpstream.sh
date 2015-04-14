#!/usr/bin/env bash

thirdparty_module_name='MetaIO'

upstream_git_url='https://github.com/Kitware/MetaIO.git'
upstream_git_branch='master'

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
