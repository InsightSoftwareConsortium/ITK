#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name='VXL'
readonly ownership='Insight Software Consortium Maintainers <https://discourse.itk.org/>'
readonly subtree="Modules/ThirdParty/VNL/src/vxl"
readonly repo="https://github.com/InsightSoftwareConsortium/vxl.git"
readonly tag="for/itk-vxl-master-7829892" # 2026-06-10 (7829892: vnl_math:: deprecation campaign + sqrteps 0x1p-26)
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
    echo "* -whitespace" > .gitattributes
    popd
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
