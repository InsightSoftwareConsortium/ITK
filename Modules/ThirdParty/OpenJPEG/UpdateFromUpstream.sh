#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="openjpeg"
readonly ownership="OpenJPEG Upstream <kwrobot@kitware.com>"
readonly subtree="Modules/ThirdParty/OpenJPEG/src/$name"
readonly repo="https://github.com/uclouvain/openjpeg.git"
readonly tag="v2.5.3"
readonly paths="
CMakeLists.txt
README.md
LICENSE

cmake/EnsureFileInclude.cmake
cmake/TestLargeFiles.cmake
cmake/TestFileOffsetBits.c
cmake/TestLargeFiles.c.cmake.in

src/lib/CMakeLists.txt
src/lib/openjp2/CMakeLists.txt
src/lib/openjp2/*.c
src/lib/openjp2/*.h
src/lib/openjp2/*.cmake.in
"

extract_source () {
    git_archive
    pushd "${extractdir}/${name}-reduced"
    echo "* -whitespace" >> .gitattributes
    echo "" >> src/lib/openjp2/mqc.c
    echo "" >> src/lib/openjp2/t1_ht_luts.h
    popd
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
