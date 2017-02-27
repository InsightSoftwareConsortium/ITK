#!/usr/bin/env bash

thirdparty_module_name='KWIML'

upstream_git_url='https://gitlab.kitware.com/utils/kwiml.git'
upstream_git_branch='master'

snapshot_author_name='KWIML Upstream'
snapshot_author_email='kwrobot@kitware.com'

snapshot_redact_cmd=''
snapshot_relative_path='src/itkkwiml'

source "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/UpdateThirdPartyFromUpstream.sh"
update_from_upstream
