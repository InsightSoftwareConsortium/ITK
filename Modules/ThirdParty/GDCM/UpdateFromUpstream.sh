#!/usr/bin/env bash

thirdparty_module_name='GDCM'

upstream_git_url='http://git.code.sf.net/p/gdcm/gdcm.git'
upstream_git_branch='master'

snapshot_author_name='GDCM Upstream'
snapshot_author_email='gdcm-developers@lists.sourceforge.net'

snapshot_redact_cmd=''
snapshot_relative_path='src/gdcm'
snapshot_paths='
  CMake/UseCopyright.cmake
  CMake/InstallMacros.cmake
  CMake/COPYING-CMAKE-SCRIPTS
  CMakeLists.txt
  Copyright.txt
  Source
  Utilities/C99
  Utilities/CMakeLists.txt
  Utilities/gdcmcharls
  Utilities/gdcm_charls.h
  Utilities/gdcm_expat.h
  Utilities/gdcmjpeg
  Utilities/gdcm_ljpeg12.h
  Utilities/gdcm_ljpeg16.h
  Utilities/gdcm_ljpeg8.h
  Utilities/gdcmmd5
  Utilities/gdcm_md5.h
  Utilities/gdcm_openjpeg2.h
  Utilities/gdcm_openjpeg.h
  Utilities/gdcmopenjpeg-v2
  Utilities/gdcmrle
  Utilities/gdcmutfcpp
  Utilities/gdcmuuid
  Utilities/gdcm_uuid.h
  Utilities/gdcm_zlib.h
  Utilities/socketxx
  '

source "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/UpdateThirdPartyFromUpstream.sh"
update_from_upstream
