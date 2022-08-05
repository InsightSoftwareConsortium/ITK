#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="DoubleConversion"
readonly ownership="Google double-conversion Maintainers <floitsch@google.com>"
readonly subtree="Modules/ThirdParty/DoubleConversion/src/double-conversion"
readonly repo="https://github.com/google/double-conversion"
readonly tag="master"
readonly paths="
double-conversion/*.h
double-conversion/*.cc
"


extract_source () {
    git_archive
    pushd "${extractdir}/${name}-reduced"
    mv double-conversion/* .
    rmdir double-conversion
    echo "* -whitespace" >> .gitattributes
    popd
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
