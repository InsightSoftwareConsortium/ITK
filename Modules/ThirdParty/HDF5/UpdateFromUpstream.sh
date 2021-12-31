#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="HDF5"
readonly ownership="HDF5 Maintainers <hdf5-maintainers@hdfgroup.org>"
readonly subtree="Modules/ThirdParty/HDF5/src/itkhdf5"
readonly repo="https://github.com/HDFGroup/hdf5.git"
readonly tag="hdf5-1_12_1"
readonly shortlog=false
readonly paths="
   ACKNOWLEDGMENTS
   CTestConfig.cmake
   README.txt
   config/cmake
   config/cmake_ext_mod
   config/clang-warnings
   config/gnu-warnings
   config/intel-warnings
   CMakeLists.txt
   CMakeInstallation.cmake
   CMakeFilters.cmake
   COPYING
   src
   c++/CMakeLists.txt
   c++/src
   hl/CMakeLists.txt
   hl/src
   hl/c++/CMakeLists.txt
   hl/c++/src
   UserMacros.cmake
   .clang-format
"

extract_source () {
    git_archive
    pushd "$extractdir/$name-reduced"
    sed -i.bak -e '1 s| /bin/sh|/bin/sh|' c++/src/h5c++.in config/cmake/libh5cc.in
    rm c++/src/h5c++.in.bak config/cmake/libh5cc.in.bak
    chmod a+x c++/src/h5c++.in config/cmake/libh5cc.in
    chmod a-x README.txt
    chmod a-x config/cmake/CTestScript.cmake
    chmod a-x config/cmake/HDF5_Examples_options.cmake
    chmod a-x config/cmake/libhdf5.pc.in
    chmod a-x config/cmake/scripts/*
    chmod a+x config/cmake/scripts/HPC

    sed s/$'\r'$// < config/cmake/scripts/CTestScript.cmake > output.txt
    cat output.txt > config/cmake/scripts/CTestScript.cmake
    rm output.txt

    sed s/$'\r'$// < config/cmake/scripts/HDF5config.cmake > output.txt
    cat output.txt > config/cmake/scripts/HDF5config.cmake
    rm output.txt

    rm -v config/cmake/scripts/HDF518config.cmake
    rm -v c++/src/C2Cppfunction_map.mht
    rm -v c++/src/C2Cppfunction_map.htm
    rm -v src/.indent.pro
    find . -name Makefile.am -delete
    find . -name Makefile.in -delete
    find . -name "*.lnt" -delete
    echo "* -whitespace" > .gitattributes
    popd
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"

echo "Don't forget to manually update the mangling as described in ${BASH_SOURCE%/*}/src/itkhdf5/src/itk_hdf5_mangle.h"
