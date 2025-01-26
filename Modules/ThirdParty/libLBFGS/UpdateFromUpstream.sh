#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="libLBFGS"
readonly ownership="libLBFGS Upstream <kwrobot@kitware.com>"
readonly subtree="Modules/ThirdParty/libLBFGS/src/itklbfgs"
readonly repo="https://github.com/chokkan/liblbfgs.git"
readonly tag="v1.10"
readonly paths="
COPYING
include/*.h
lib/*.c
lib/*.h
"

extract_source () {
    git_archive
    pushd "${extractdir}/${name}-reduced"
    echo "* -whitespace" >> .gitattributes
    popd
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
