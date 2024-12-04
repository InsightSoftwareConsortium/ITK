#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="jpeg-turbo"
readonly ownership="libjpeg-turbo Upstream <kwrobot@kitware.com>"
readonly subtree="Modules/ThirdParty/JPEG/src/itk$name"
readonly repo="https://github.com/libjpeg-turbo/libjpeg-turbo.git"
readonly tag="3.0.4"
readonly paths="
j*.c
j*.h
jconfig.h.in
jconfigint.h.in
jversion.h.in

LICENSE.md
README.ijg
README.md
"

extract_source () {
    git_archive
    pushd "${extractdir}/${name}-reduced"
    echo "* -whitespace" >> .gitattributes
    echo "README.md conflict-marker-size=8" >> .gitattributes
    popd
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
