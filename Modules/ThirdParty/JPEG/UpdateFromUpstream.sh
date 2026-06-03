#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="jpeg-turbo"
readonly ownership="libjpeg-turbo Upstream <kwrobot@kitware.com>"
readonly subtree="Modules/ThirdParty/JPEG/src/itk$name"
readonly repo="https://github.com/libjpeg-turbo/libjpeg-turbo.git"
readonly tag="3.1.4"
readonly exact_tree_match=false
readonly paths="
src/j*.c
src/j*.h
src/jconfig.h.in
src/jconfigint.h.in
src/jversion.h.in

LICENSE.md
README.ijg
README.md
"

extract_source () {
    git_archive
    pushd "${extractdir}/${name}-reduced"
    # libjpeg-turbo 3.1.0 and later use a src/ subdirectory relocate to top-level structure.
    mv src/* .
    rmdir src
    echo "* -whitespace" >> .gitattributes
    echo "README.md conflict-marker-size=8" >> .gitattributes
    popd
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
