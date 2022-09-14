#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="GoogleTest"
readonly ownership="GoogleTest Upstream <googletestframework@googlegroups.com>"
readonly subtree="Modules/ThirdParty/GoogleTest/src/itkgoogletest"
readonly repo="https://github.com/google/googletest.git"
readonly tag="release-1.12.1"
readonly shortlog=false
readonly paths="
CMakeLists.txt
googletest/CMakeLists.txt
googletest/cmake
googletest/src
googletest/include
"

extract_source () {
    git_archive
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
