#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
#############################################################################################
####  Change default configuration of options in config/cmake/cacheinit.cmake file        ###
####  format: set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DXXX:YY=ZZZZ")                 ###
#############################################################################################
if (DEFINED MPI)
    # maximum parallel processor count for build and test       ####
    set (MAX_PROC_COUNT 8)
endif()
#############################################################################################
### options to run test scripts in batch commands
set (LOCAL_BATCH_SCRIPT_NAME "ctest.qsub")
set (LOCAL_BATCH_SCRIPT_PARALLEL_NAME "ctest.qsub")
if (DEFINED KNL)
  ### some additions and alternatives to cross compile on haswell for knl
  set (COMPUTENODE_HWCOMPILE_MODULE "craype-mic-knl")
  set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DCMAKE_TOOLCHAIN_FILE:STRING=config/toolchain/crayle.cmake")
endif ()
set (LOCAL_BATCH_SCRIPT_COMMAND "qsub")
set (LOCAL_BATCH_TEST "TRUE")
set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DLOCAL_BATCH_TEST:BOOL=ON")
set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DLOCAL_BATCH_SCRIPT_NAME:STRING=${LOCAL_BATCH_SCRIPT_NAME}")
set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DLOCAL_BATCH_SCRIPT_PARALLEL_NAME:STRING=${LOCAL_BATCH_SCRIPT_PARALLEL_NAME}")
set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DMPIEXEC_EXECUTABLE:STRING=aprun")
# Option to suppress writing job statistics; to avoid issues with h5diff comparisons.
set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DMPIEXEC_PREFLAGS:STRING=-q")
set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DMPIEXEC_NUMPROC_FLAG:STRING=-n")
set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DMPIEXEC_MAX_NUMPROCS:STRING=6")
set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DACCOUNT_ID:STRING=${LOCAL_BATCH_SCRIPT_ARGS}")

#############################################################################################
#############################################################################################
