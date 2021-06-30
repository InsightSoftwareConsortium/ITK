#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="zlib-ng"
readonly ownership="Zlib-ng Upstream <kwrobot@kitware.com>"
readonly subtree="Modules/ThirdParty/ZLIB/src/itk$name"
readonly repo="https://github.com/zlib-ng/zlib-ng.git"
readonly tag="2.0.5"
readonly paths="
CMakeLists.txt

cmake/toolchain-powerpc64le.cmake
cmake/test-tools.cmake
cmake/toolchain-sparc64.cmake
cmake/detect-arch.c
cmake/test-compress.cmake
cmake/run-and-redirect.cmake
cmake/toolchain-powerpc.cmake
cmake/toolchain-s390x.cmake
cmake/toolchain-mingw-i686.cmake
cmake/detect-arch.cmake
cmake/toolchain-aarch64.cmake
cmake/toolchain-arm.cmake
cmake/run-and-compare.cmake
cmake/detect-sanitizer.cmake
cmake/detect-install-dirs.cmake
cmake/detect-coverage.cmake
cmake/toolchain-powerpc64.cmake
cmake/toolchain-mingw-x86_64.cmake

arch/s390/dfltcc_common.c
arch/s390/dfltcc_deflate.c
arch/s390/dfltcc_inflate.h
arch/s390/README.md
arch/s390/dfltcc_inflate.c
arch/s390/dfltcc_common.h
arch/s390/dfltcc_detail.h
arch/s390/Makefile.in
arch/s390/dfltcc_deflate.h
arch/.gitignore
arch/generic/Makefile.in
arch/power/adler32_power8.c
arch/power/Makefile.in
arch/power/power.c
arch/power/power.h
arch/power/slide_hash_power8.c
arch/x86/slide_avx.c
arch/x86/INDEX.md
arch/x86/insert_string_sse.c
arch/x86/chunkset_avx.c
arch/x86/crc_folding.c
arch/x86/slide_sse.c
arch/x86/compare258_avx.c
arch/x86/Makefile.in
arch/x86/chunkset_sse.c
arch/x86/x86.h
arch/x86/compare258_sse.c
arch/x86/crc_folding.h
arch/x86/adler32_ssse3.c
arch/x86/adler32_avx.c
arch/x86/x86.c
arch/arm/armfeature.c
arch/arm/insert_string_acle.c
arch/arm/adler32_neon.c
arch/arm/Makefile.in
arch/arm/arm.h
arch/arm/slide_neon.c
arch/arm/ctzl.h
arch/arm/chunkset_neon.c
arch/arm/crc32_acle.c

adler32.c
chunkset.c
compare258.c
compress.c
crc32.c
crc32_comb.c
deflate.c
deflate_fast.c
deflate_medium.c
deflate_quick.c
deflate_slow.c
functable.c
gzlib.c
gzread.c
gzwrite.c
infback.c
inffast.c
inflate.c
inftrees.c
insert_string.c
trees.c
uncompr.c
zutil.c

adler32_p.h
chunkset_tpl.h
crc32_comb_tbl.h
crc32_p.h
crc32_tbl.h
deflate.h
deflate_p.h
fallback_builtins.h
functable.h
gzguts.h
inffast.h
inffixed_tbl.h
inflate.h
inflate_p.h
inftrees.h
insert_string_tpl.h
match_tpl.h
trees.h
trees_emit.h
trees_tbl.h
zbuild.h
zendian.h
zlib.h
zlib-ng.h
zutil.h
zutil_p.h

zlib.pc.cmakein
zconf-ng.h.in
zlib.pc.in
zconf.h.in
Makefile.in

win32/zlib-ng1.rc
win32/Makefile.arm
win32/Makefile.a64
win32/Makefile.msc
win32/zlibcompat.def
win32/zlib1.rc
win32/zlib-ng.def
win32/zlib.def

tools/maketrees.c
tools/makefixed.c
tools/config.sub
tools/makecrct.c

.gitattributes
README.md
INDEX.md
"


extract_source () {
    git_archive
    pushd "${extractdir}/${name}-reduced"
    echo "* -whitespace" > .gitattributes
    chmod u+x test/pkgcheck.sh
    chmod u+x tools/codecov-upload.sh
    popd
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
