#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name='VXL'
readonly ownership='VXL Maintainers <vxl-maintainers@lists.sourceforge.net>'
readonly subtree="Modules/ThirdParty/VNL/src/vxl"
# TODO: revert to canonical 'https://github.com/vxl/vxl.git' / 'master' once
# the integration branch lands upstream; this PR is a preview sync against
# the integration tip so ITK CI exercises the pending VXL changes.
readonly repo="https://github.com/hjmjohnson/vxl.git"
readonly tag="integration/all-open-prs-2026-05-10"
readonly shortlog=false
readonly exact_tree_match=false
readonly paths="
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
"

extract_source () {
    git_archive
    pushd "${extractdir}/${name}-reduced"
    rm v3p/netlib/triangle*
    rm -rf v3p/netlib/toms
    rm v3p/netlib/examples/showme.c
    echo "* -whitespace" > .gitattributes
    popd
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
