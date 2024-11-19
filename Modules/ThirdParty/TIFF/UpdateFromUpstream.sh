#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="tiff"
readonly ownership="Tiff Upstream <kwrobot@kitware.com>"
readonly subtree="Modules/ThirdParty/TIFF/src/itk$name"
readonly repo="https://gitlab.com/libtiff/libtiff.git"
readonly tag="v4.7.0"
readonly paths="
CMakeLists.txt
README.md
LICENSE.md

cmake/AutotoolsCompat.cmake
cmake/CXXLibrary.cmake
cmake/CompilerChecks.cmake
cmake/DeflateCodec.cmake
cmake/FindCMath.cmake
cmake/IncludeChecks.cmake
cmake/InternalCodecs.cmake
cmake/JBIGCodec.cmake
cmake/JPEGCodec.cmake
cmake/LERCCodec.cmake
cmake/LZMACodec.cmake
cmake/LargeFileSupport.cmake
cmake/LibraryFeatures.cmake
cmake/LinkerChecks.cmake
cmake/OpenGLChecks.cmake
cmake/PixarLogCodec.cmake
cmake/ProcessorChecks.cmake
cmake/SymbolChecks.cmake
cmake/TypeSizeChecks.cmake
cmake/WebPCodec.cmake
cmake/WindowsSupport.cmake
cmake/ZSTDCodec.cmake

libtiff/CMakeLists.txt
libtiff/libtiff.def
libtiff/*.c
libtiff/*.h
libtiff/tif_config.h.cmake.in
libtiff/tiffconf.h.cmake.in
libtiff/tiffvers.h.cmake.in

port/CMakeLists.txt
port/dummy.c
port/getopt.c
port/libport.h
port/libport_config.h.cmake.in
"

extract_source () {
    git_archive
    pushd "${extractdir}/${name}-reduced"
    echo "* -whitespace" >> .gitattributes
    rm -vf libtiff/*.vc.h libtiff/*.wince.h
    chmod a-x libtiff/CMakeLists.txt port/CMakeLists.txt
    popd
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
