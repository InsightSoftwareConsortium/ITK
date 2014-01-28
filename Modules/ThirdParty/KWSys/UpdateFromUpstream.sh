#!/usr/bin/env bash

thirdparty_module_name='KWSys'

upstream_git_url='http://public.kitware.com/KWSys.git'
upstream_git_branch='master'

snapshot_author_name='KWSys Robot'
snapshot_author_email='kwrobot@kitware.com'

snapshot_redact_cmd=''
snapshot_relative_path='src/KWSys'

source "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/UpdateThirdPartyFromUpstream.sh"
update_from_upstream
