#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="zlib"
readonly ownership="Zlib Upstream <kwrobot@kitware.com>"
readonly subtree="Modules/ThirdParty/ZLIB/src/itk$name"
readonly repo="https://gitlab.kitware.com/third-party/zlib.git"
readonly tag="for/itk-v1.2.11-2017-01-15-cacf7f1d"
readonly paths="
CMakeLists.txt

adler32.c
compress.c
crc32.c
deflate.c
gzclose.c
gzlib.c
gzread.c
gzwrite.c
infback.c
inffast.c
inflate.c
inftrees.c
trees.c
uncompr.c
zutil.c

crc32.h
deflate.h
gzguts.h
inffast.h
inffixed.h
inflate.h
inftrees.h
trees.h
zlib.h
zutil.h

zconf.h.cmakein

win32/zlib1.rc
win32/DLL_FAQ.txt

.gitattributes
ChangeLog
README
README.kitware.md
FAQ
INDEX
"

extract_source () {
    git_archive
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
