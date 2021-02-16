#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name='VXL'
readonly ownership='VXL Maintainers <vxl-maintainers@lists.sourceforge.net>'
readonly subtree="Modules/ThirdParty/VNL/src/vxl"
readonly repo="https://github.com/vxl/vxl.git"
readonly tag="master"
readonly shortlog=false
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
    rm v3p/netlib/examples/showme.c
    echo "* -whitespace" > .gitattributes
    popd
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
