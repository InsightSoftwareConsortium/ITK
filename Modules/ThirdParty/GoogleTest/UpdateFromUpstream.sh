#!/usr/bin/env bash

thirdparty_module_name='GoogleTest'

upstream_git_url='https://github.com/google/googletest.git'
upstream_git_branch='master'

github_compare=true

snapshot_author_name='GoogleTest Upstream'
snapshot_author_email='googletestframework@googlegroups.com'

snapshot_redact_cmd=''
snapshot_relative_path='src/itkgoogletest'
snapshot_paths='
CMakeLists.txt
googletest/CMakeLists.txt
googletest/cmake
googletest/src
googletest/include
'

source "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/UpdateThirdPartyFromUpstream.sh"
update_from_upstream
