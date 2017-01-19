#!/bin/sh

# This script is used to download and reduce upstream HDF5 for inclusion in
# the ITK source tree with a Git subtree merge. Edit the variables 'url',
# 'v', and 'r' before running. See the README-ITK.txt file in this directory
# for more information.

set -x
set -o

url=https://svn.hdfgroup.uiuc.edu/hdf5/branches/hdf5_1_8_17
v=1.8.17
r=30218
paths="
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
date=$(svn log -q -c$r $url |
      sed -n "/^r/ {s/[^|]*|[^|]*|//;p;}")
svn export -r$r $url hdf5-$v-r$r &&
mkdir hdf5-$v-r$r-reduced &&
(cd hdf5-$v-r$r && tar c $paths) |
(cd hdf5-$v-r$r-reduced && tar x)
chmod -x hdf5-$v-r$r-reduced/config/cmake/scripts/*
rm hdf5-$v-r$r-reduced/config/cmake/scripts/CTestScript.cmake
rm hdf5-$v-r$r-reduced/config/cmake/scripts/HDF518config.cmake
rm hdf5-$v-r$r-reduced/config/cmake_ext_mod/CheckTypeSize.cmake
chmod +x hdf5-$v-r$r-reduced/c++/src/h5c++.in
echo "r$r date: $date"
