#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

# CMake settings for HDF5 Developer mode builds

# Set CMake C++ flags based off of Debug build flags
set (CMAKE_CXX_FLAGS_DEVELOPER ${CMAKE_CXX_FLAGS_DEBUG} CACHE STRING
  "Flags used by the C++ compiler during developer builds." FORCE
)

# Set CMake C flags based off of Debug build flags. Add in -Og
# option to disable some GCC optimizations that might affect
# debugging negatively and also include some GCC compiler passes
# that collect debugging information
set (CMAKE_C_FLAGS_DEVELOPER "${CMAKE_C_FLAGS_DEBUG} -Og" CACHE STRING
  "Flags used by the C compiler during developer builds." FORCE
)

# Set CMake binary linker flags based off of Debug binary linker flags
set (CMAKE_EXE_LINKER_FLAGS_DEVELOPER ${CMAKE_EXE_LINKER_FLAGS_DEBUG}
  CACHE STRING "Flags used for linking binaries during developer builds."
  FORCE
)

# Set CMake shared library linker flags based off of Debug shared library
# linker flags
set (CMAKE_SHARED_LINKER_FLAGS_DEVELOPER ${CMAKE_SHARED_LINKER_FLAGS_DEBUG}
  CACHE STRING "Flags used by the shared libraries linker during developer builds."
  FORCE
)

mark_as_advanced (
  CMAKE_CXX_FLAGS_DEVELOPER
  CMAKE_C_FLAGS_DEVELOPER
  CMAKE_EXE_LINKER_FLAGS_DEVELOPER
  CMAKE_SHARED_LINKER_FLAGS_DEVELOPER
)

#-----------------------------------------------------------------------------
# Define various HDF5 macros for debugging the library
#-----------------------------------------------------------------------------

# Enable debugging of various HDF5 modules
set (HDF5_ENABLE_DEBUG_APIS ON CACHE BOOL "Turn on extra debug output in all packages" FORCE)

# HDF5 module debug definitions for debug code which either isn't
# currently integrated with HDF5_ENABLE_DEBUG_APIS, or which isn't
# well integrated with HDF5's H5DEBUG(X) (where 'X' is a package
# letter) system. This type of debug code usually always prints output
# to stdout, regardless of whether debugging for its particular module
# has been requested via the HDF5_DEBUG environment variable. Therefore,
# we don't automatically enable this debug code, but allow developers
# to quickly add those definitions into their build here, without
# needing to hack up source files.
option (HDF5_ENABLE_DEBUG_H5AC_DIRTY_BYTES "Enable printing of H5AC module dirty bytes information" OFF)
mark_as_advanced (HDF5_ENABLE_DEBUG_H5AC_DIRTY_BYTES)
if (HDF5_ENABLE_DEBUG_H5AC_DIRTY_BYTES)
  list (APPEND HDF5_DEBUG_APIS H5AC_DEBUG_DIRTY_BYTES_CREATION)
endif ()

option (HDF5_ENABLE_DEBUG_H5FA "Enable debugging of H5FA module" OFF)
mark_as_advanced (HDF5_ENABLE_DEBUG_H5FA)
if (HDF5_ENABLE_DEBUG_H5FA)
  list (APPEND HDF5_DEBUG_APIS H5FA_DEBUG)
endif ()

option (HDF5_ENABLE_DEBUG_H5FD_ALLOC "Enable debugging of H5FD module allocation code" OFF)
mark_as_advanced (HDF5_ENABLE_DEBUG_H5FD_ALLOC)
if (HDF5_ENABLE_DEBUG_H5FD_ALLOC)
  list (APPEND HDF5_DEBUG_APIS H5FD_ALLOC_DEBUG)
endif ()

option (HDF5_ENABLE_DEBUG_H5FL "Enable debugging of H5FL module" OFF)
mark_as_advanced (HDF5_ENABLE_DEBUG_H5FL)
if (HDF5_ENABLE_DEBUG_H5FL)
  list (APPEND HDF5_DEBUG_APIS H5FL_DEBUG)
endif ()

option (HDF5_ENABLE_DEBUG_H5FS "Enable debugging of H5FS module" OFF)
mark_as_advanced (HDF5_ENABLE_DEBUG_H5FS)
if (HDF5_ENABLE_DEBUG_H5FS)
  list (APPEND HDF5_DEBUG_APIS H5FS_DEBUG)
endif ()

option (HDF5_ENABLE_DEBUG_H5FS_SINFO "Enable debugging of H5FS module section info" OFF)
mark_as_advanced (HDF5_ENABLE_DEBUG_H5FS_SINFO)
if (HDF5_ENABLE_DEBUG_H5FS_SINFO)
  list (APPEND HDF5_DEBUG_APIS H5FS_SINFO_DEBUG)
endif ()

option (HDF5_ENABLE_DEBUG_H5MF_AGGR "Enable debugging of H5MF module aggregation code" OFF)
mark_as_advanced (HDF5_ENABLE_DEBUG_H5MF_AGGR)
if (HDF5_ENABLE_DEBUG_H5MF_AGGR)
  list (APPEND HDF5_DEBUG_APIS H5MF_AGGR_DEBUG)
endif ()

option (HDF5_ENABLE_DEBUG_H5MF_ALLOC "Enable debugging of H5MF module allocation code" OFF)
mark_as_advanced (HDF5_ENABLE_DEBUG_H5MF_ALLOC)
if (HDF5_ENABLE_DEBUG_H5MF_ALLOC)
  list (APPEND HDF5_DEBUG_APIS H5MF_ALLOC_DEBUG)
endif ()

option (HDF5_ENABLE_DEBUG_H5MF_ALLOC_MORE "Enable extra debugging of H5MF module allocation code" OFF)
mark_as_advanced (HDF5_ENABLE_DEBUG_H5MF_ALLOC_MORE)
if (HDF5_ENABLE_DEBUG_H5MF_ALLOC_MORE)
  list (APPEND HDF5_DEBUG_APIS H5MF_ALLOC_DEBUG_MORE)
endif ()

option (HDF5_ENABLE_DEBUG_H5MF_ALLOC_DUMP "Enable printing of debugging info for H5MF module allocation code" OFF)
mark_as_advanced (HDF5_ENABLE_DEBUG_H5MF_ALLOC_DUMP)
if (HDF5_ENABLE_DEBUG_H5MF_ALLOC_DUMP)
  list (APPEND HDF5_DEBUG_APIS H5MF_ALLOC_DEBUG_DUMP)
endif ()

option (HDF5_ENABLE_DEBUG_H5R "Enable debugging of H5R module" OFF)
mark_as_advanced (HDF5_ENABLE_DEBUG_H5R)
if (HDF5_ENABLE_DEBUG_H5R)
  list (APPEND HDF5_DEBUG_APIS H5R_DEBUG)
endif ()

option (HDF5_ENABLE_DEBUG_H5S_HYPER "Enable debugging of H5S hyperslab code" OFF)
mark_as_advanced (HDF5_ENABLE_DEBUG_H5S_HYPER)
if (HDF5_ENABLE_DEBUG_H5S_HYPER)
  list (APPEND HDF5_DEBUG_APIS H5S_HYPER_DEBUG)
endif ()

option (HDF5_ENABLE_DEBUG_H5T_REF "Enable debugging of H5T module reference code" OFF)
mark_as_advanced (HDF5_ENABLE_DEBUG_H5T_REF)
if (HDF5_ENABLE_DEBUG_H5T_REF)
  list (APPEND HDF5_DEBUG_APIS H5T_REF_DEBUG)
endif ()

# HDF5 module debug definitions for debug code which may add
# considerable amounts of overhead when enabled and is usually
# only useful for specific circumstances rather than general
# developer use.
option (HDF5_ENABLE_DEBUG_H5B "Enable debugging of H5B module" OFF)
mark_as_advanced (HDF5_ENABLE_DEBUG_H5B)
if (HDF5_ENABLE_DEBUG_H5B)
  list (APPEND HDF5_DEBUG_APIS H5B_DEBUG)
endif ()

option (HDF5_ENABLE_DEBUG_H5B2 "Enable debugging of H5B2 module" OFF)
mark_as_advanced (HDF5_ENABLE_DEBUG_H5B2)
if (HDF5_ENABLE_DEBUG_H5B2)
  list (APPEND HDF5_DEBUG_APIS H5B2_DEBUG)
endif ()

option (HDF5_ENABLE_DEBUG_H5C_SANITY_CHECKS "Enable full sanity checking in H5C module" OFF)
mark_as_advanced (HDF5_ENABLE_DEBUG_H5C_SANITY_CHECKS)
if (HDF5_ENABLE_DEBUG_H5C_SANITY_CHECKS)
  list (APPEND HDF5_DEBUG_APIS H5C_DO_SANITY_CHECKS)
  list (APPEND HDF5_DEBUG_APIS H5C_DO_SLIST_SANITY_CHECKS)
  list (APPEND HDF5_DEBUG_APIS H5C_DO_TAGGING_SANITY_CHECKS)
  list (APPEND HDF5_DEBUG_APIS H5C_DO_EXTREME_SANITY_CHECKS)

  # See note in H5Cprivate.h about this #define
  # list (APPEND HDF5_DEBUG_APIS H5C_DO_MEMORY_SANITY_CHECKS=1)
endif ()

option (HDF5_ENABLE_DEBUG_H5FL_TRACK "Enable tracking of free list allocations" OFF)
mark_as_advanced (HDF5_ENABLE_DEBUG_H5FL_TRACK)
if (HDF5_ENABLE_DEBUG_H5FL_TRACK)
  list (APPEND HDF5_DEBUG_APIS H5FL_TRACK)

  # Free list tracking requires the codestack functionality
  set (HDF5_ENABLE_CODESTACK ON CACHE BOOL "Enable the function stack tracing (for developer debugging)." FORCE)
else ()
  unset (HDF5_ENABLE_CODESTACK CACHE)
endif ()

option (HDF5_ENABLE_DEBUG_H5FS_ASSERT "Enable extra debugging of H5FS module" OFF)
mark_as_advanced (HDF5_ENABLE_DEBUG_H5FS_ASSERT)
if (HDF5_ENABLE_DEBUG_H5FS_ASSERT)
  list (APPEND HDF5_DEBUG_APIS H5FS_DEBUG_ASSERT)
endif ()

# If HDF5 free list debugging wasn't specifically enabled, disable
# free lists entirely for developer build modes, as they can
# make certain types of issues (like references to stale pointers)
# much more difficult to debug
if (NOT HDF5_ENABLE_DEBUG_H5FL AND NOT HDF5_ENABLE_DEBUG_H5FL_TRACK)
  list (APPEND HDF5_DEVELOPER_DEFS H5_NO_FREE_LISTS)
endif ()

# Enable strict checking of the file format
list (APPEND HDF5_DEVELOPER_DEFS H5_STRICT_FORMAT_CHECKS)
