#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="nifti"
readonly ownership="NIFTI Upstream <nifti@github.com>"
readonly subtree="Modules/ThirdParty/NIFTI/src/nifti"
readonly repo="https://github.com/NIFTI-Imaging/nifti_clib.git"
readonly tag="master"
readonly shortlog=false
readonly paths="
CMakeLists.txt
CTestConfig.cmake
LICENSE
README
cmake
znzlib
niftilib
nifti2
"

extract_source () {
    git_archive
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
