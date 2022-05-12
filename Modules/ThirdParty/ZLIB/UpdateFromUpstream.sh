#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="zlib-ng"
readonly ownership="Zlib-ng Upstream <kwrobot@kitware.com>"
readonly subtree="Modules/ThirdParty/ZLIB/src/itk$name"
readonly repo="https://github.com/zlib-ng/zlib-ng.git"
readonly tag="develop"
readonly paths="
CMakeLists.txt
cmake/*
arch/*
*.c
*.h

gzread.c.in
zconf.h.in
zlib.h.in
zlib_name_mangling.h.in
zlib_name_mangling.h.empty
zlib.pc.cmakein

zlib.map
win32/zlibcompat.def.in
win32/zlib1.rc
win32/zlib.def.in

.gitattributes
README.md
INDEX.md
"


extract_source () {
    git_archive
    pushd "${extractdir}/${name}-reduced"
    echo "* -whitespace" >> .gitattributes
    popd
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
