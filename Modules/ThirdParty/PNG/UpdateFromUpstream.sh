#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="png"
readonly ownership="LIBPNG Upstream <png-mng-implement@lists.sourceforge.net>"
readonly subtree="Modules/ThirdParty/PNG/src/itk$name"
readonly repo="git://git.code.sf.net/p/libpng/code"
readonly tag='v1.6.44' # Sept 12, 2024
readonly paths="
png*.c
png*.h
arm/*
LICENSE
scripts/pnglibconf.h.prebuilt
"


extract_source () {
    git_archive
    pushd "${extractdir}/${name}-reduced"
    echo "* -whitespace" >> .gitattributes
    popd
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
