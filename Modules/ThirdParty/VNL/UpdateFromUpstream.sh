#!/usr/bin/env bash

thirdparty_module_name='VNL'

upstream_git_url='https://github.com/vxl/vxl.git'
upstream_git_branch='master'

snapshot_author_name='VXL Maintainers'
snapshot_author_email='vxl-maintainers@lists.sourceforge.net'

snapshot_redact_cmd=''
snapshot_relative_path='src/vxl'
snapshot_paths='
      CMakeLists.txt
      config/cmake
      core/CMakeLists.txt
      core/testlib
      core/vnl
      core/vxl_config.h.in
      core/vxl_copyright.h
      core/vxl_version.h
      v3p/CMakeLists.txt
      v3p/netlib
      vcl
'

source "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/UpdateThirdPartyFromUpstream.sh"
update_from_upstream
