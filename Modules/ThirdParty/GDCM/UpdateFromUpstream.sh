#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="GDCM"
readonly ownership="GDCM Upstream <gdcm-developers@lists.sourceforge.net>"
readonly subtree="Modules/ThirdParty/GDCM/src/gdcm"
readonly repo="https://github.com/InsightSoftwareConsortium/GDCM.git"
readonly tag="for/itk-gdcm-master-8ba2cce" # 2026-11-12 (master 2cd05d132 + malaterre/GDCM#228 #235 #238 #243 #244 #245 #246 + ITK overlay)
readonly shortlog=false
readonly exact_tree_match=false
readonly paths="
  .gitattributes
  .codespellrc
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
  Utilities/gdcmrle
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
