#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="GDCM"
readonly ownership="GDCM Upstream <gdcm-developers@lists.sourceforge.net>"
readonly subtree="Modules/ThirdParty/GDCM/src/gdcm"
readonly repo="https://github.com/malaterre/GDCM.git"
readonly tag="release"
readonly shortlog=false
readonly paths="
  .gitattributes
  CMake/UseCopyright.cmake
  CMake/InstallMacros.cmake
  CMake/COPYING-CMAKE-SCRIPTS
  CMakeLists.txt
  Copyright.txt
  Source
  Utilities/CMakeLists.txt
  Utilities/gdcmcharls
  Utilities/gdcm_charls.h
  Utilities/gdcm_expat.h
  Utilities/gdcmext
  Utilities/gdcmjpeg
  Utilities/gdcm_ljpeg12.h
  Utilities/gdcm_ljpeg16.h
  Utilities/gdcm_ljpeg8.h
  Utilities/gdcmmd5
  Utilities/gdcm_md5.h
  Utilities/gdcm_openjpeg.h
  Utilities/gdcmopenjpeg
  Utilities/gdcmrle
  Utilities/gdcmutfcpp
  Utilities/gdcmuuid
  Utilities/gdcm_uuid.h
  Utilities/gdcm_zlib.h
  Utilities/socketxx
"

extract_source () {
    git_archive
    pushd "${extractdir}/${name}-reduced"
    popd
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
