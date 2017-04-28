#!/usr/bin/env bash

thirdparty_module_name='MINC'

upstream_git_url='https://github.com/BIC-MNI/libminc.git'
upstream_git_branch='develop'

snapshot_author_name='Vladimir S. FONOV'
snapshot_author_email='vladimir.fonov@gmail.com'

snapshot_redact_cmd='chmod u-x volume_io/Geometry/tensors.c'
snapshot_relative_path='src/libminc'
snapshot_paths='
./ChangeLog
./volume_io
./libsrc
./libsrc/minc_compat.h
./libcommon/minc_config.h
./libcommon/minc_config.c
./libsrc/minc_format_convert.c
./libsrc/value_conversion.c
./libsrc/hdf_convenience.c
./libcommon/read_file_names.c
./libsrc/minc_simple.c
./libcommon/time_stamp.h
./libsrc/hdf_convenience.h
./libsrc/type_limits.h
./libcommon/read_file_names.h
./libsrc/minc_varlists.h
./libcommon/restructure.h
./libsrc/nd_loop.h
./libcommon/restructure.c
./libsrc/minc_simple.h
./libsrc/strdup.c
./libsrc/minc_useful.h
./libsrc/minc_convenience.c
./libsrc/minc_basic.h
./libsrc/minc_compat.c
./libcommon/time_stamp.c
./libsrc/voxel_loop.h
./libsrc/minc_routines.h
./libsrc/minc_private.h
./libsrc/netcdf_convenience.c
./libsrc/voxel_loop.c
./libsrc/dim_conversion.c
./libsrc/minc_format_convert.h
./libsrc/nd_loop.c
./libcommon/ParseArgv.h
./libcommon/minc_error.c
./libcommon/minc_error.h
./libsrc/minc_structures.h
./libsrc/minc.h
./libsrc/image_conversion.c
./libcommon/ParseArgv.c
./libcommon/minc_common_defs.h
./COPYING
./UseLIBMINC.cmake.in
./NEWS
./AUTHORS
./libsrc2/m2util.c
./libsrc2/minc2_defs.h
./libsrc2/minc2_api.h
./libcommon/minc2_error.h
./libsrc2/grpattr.c
./libsrc2/hyper.c
./libsrc2/minc_compat2.h
./libcommon/minc2_error.c
./libsrc2/record.c
./libsrc2/volume.c
./libsrc2/datatype.c
./libsrc2/volprops.c
./libsrc2/valid.c
./libsrc2/convert.c
./libsrc2/free.c
./libsrc2/dimension.c
./libsrc2/label.c
./libsrc2/minc2.h
./libsrc2/minc2_private.h
./libsrc2/slice.c
./libsrc2/minc2_structs.h
./CMakeLists.txt
./nifti
./LIBMINCConfig.cmake.in
./README.release
./INSTALL
./config.h.cmake
./README
'

source "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/UpdateThirdPartyFromUpstream.sh"
update_from_upstream