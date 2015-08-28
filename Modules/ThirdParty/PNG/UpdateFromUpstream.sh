#!/usr/bin/env bash

thirdparty_module_name='PNG'

upstream_git_url='git://git.code.sf.net/p/libpng/code'
upstream_git_branch='v1.0.12beta1'

snapshot_author_name='LIBPNG Upstream'
snapshot_author_email='png-mng-implement@lists.sourceforge.net'

snapshot_redact_cmd=''
snapshot_relative_path='src/itkpng'
snapshot_paths='
  png*.c
  png*.h
  LICENSE
  '

source "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/UpdateThirdPartyFromUpstream.sh"
update_from_upstream
