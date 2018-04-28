#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="HDF5"
readonly ownership="HDF5 Maintainers <hdf5-maintainers@hdfgroup.org>"
readonly subtree="Modules/ThirdParty/HDF5/src/itkhdf5"
readonly repo="https://bitbucket.hdfgroup.org/scm/hdffv/hdf5.git"
readonly tag="hdf5_1_8_17"
readonly shortlog=false
readonly paths="
   ACKNOWLEDGMENTS
   CTestConfig.cmake
   README.txt
   config/cmake
   config/cmake_ext_mod
   CMakeLists.txt
   CMakeInstallation.cmake
   CMakeFilters.cmake
   COPYING
   src
   c++/CMakeLists.txt
   c++/COPYING
   c++/src
   hl/CMakeLists.txt
   hl/COPYING
   hl/src
   hl/c++/CMakeLists.txt
   hl/c++/COPYING
   hl/c++/src
   UserMacros.cmake
"

extract_source () {
    git_archive
    pushd "$extractdir/$name-reduced"
    sed -i '1 s| /bin/sh|/bin/sh|' c++/src/h5c++.in
    chmod a+x c++/src/h5c++.in
    chmod a-x config/cmake/scripts/*
    rm -v config/cmake/scripts/CTestScript.cmake
    rm -v config/cmake/scripts/HDF518config.cmake
    rm -v c++/src/C2Cppfunction_map.mht
    rm -v c++/src/C2Cppfunction_map.htm
    rm -v src/.indent.pro
    find -name Makefile.am -delete
    find -name Makefile.in -delete
    find -name "*.lnt" -delete
    echo "* -whitespace" > .gitattributes
    popd
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
