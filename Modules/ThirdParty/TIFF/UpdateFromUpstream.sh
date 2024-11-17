#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="tiff"
readonly ownership="Tiff Upstream <kwrobot@kitware.com>"
readonly subtree="Modules/ThirdParty/TIFF/src/itk$name"
readonly repo="https://gitlab.com/libtiff/libtiff.git"
readonly tag="v4.0.3"
readonly paths="
COPYRIGHT
README

libtiff/libtiff.def
libtiff/libtiff.map
libtiff/tif_config.h.in
libtiff/*.c
libtiff/*.h

port/dummy.c
port/getopt.c
port/libport.h
"

extract_source () {
    git_archive
    pushd "${extractdir}/${name}-reduced"
    echo "* -whitespace" >> .gitattributes
    rm -vf libtiff/*.vc.h libtiff/*.wince.h libtiff/mkspans.c libtiff/tif_vms.c libtiff/tif_wince.c
    popd
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
