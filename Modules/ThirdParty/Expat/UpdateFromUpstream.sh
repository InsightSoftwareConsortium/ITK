#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="expat"
readonly ownership="Expat Upstream <kwrobot@kitware.com>"
readonly subtree="Modules/ThirdParty/Expat/src/itk$name"
readonly repo="https://github.com/libexpat/libexpat.git"
readonly tag="R_2_7_2"
readonly paths="
expat/CMakeLists.txt
expat/ConfigureChecks.cmake
expat/COPYING
expat/expat_config.h.cmake

expat/lib/*.h
expat/lib/*.c
"

extract_source () {
    git_archive
    pushd "${extractdir}/${name}-reduced"
    echo "* -whitespace" >> .gitattributes
    mv expat/* .
    rmdir expat
    fromdos ConfigureChecks.cmake CMakeLists.txt expat_config.h.cmake
    chmod a-x ConfigureChecks.cmake CMakeLists.txt expat_config.h.cmake
    popd
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
