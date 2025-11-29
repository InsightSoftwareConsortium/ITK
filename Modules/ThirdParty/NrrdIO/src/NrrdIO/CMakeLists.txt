#
# NrrdIO: C library for NRRD file IO (with optional compressions)
# Copyright (C) 2009--2025  University of Chicago
# Copyright (C) 2005--2008  Gordon Kindlmann
# Copyright (C) 1998--2004  University of Utah
#
# This software is provided 'as-is', without any express or implied
# warranty.  In no event will the authors be held liable for any
# damages arising from the use of this software.
#
# Permission is granted to anyone to use this software for any
# purpose, including commercial applications, and to alter it and
# redistribute it freely, subject to the following restrictions:
#
# 1. The origin of this software must not be misrepresented; you must
#    not claim that you wrote the original software. If you use this
#    software in a product, an acknowledgment in the product
#    documentation would be appreciated but is not required.
#
#   2. Altered source versions must be plainly marked as such, and must
#    not be misrepresented as being the original software.
#
# 3. This notice may not be removed or altered from any source distribution.

# NrrdIO can be used in bigger applications (e.g. ITK) to add support for the
# "Nearly Raw Raster Data" NRRD format: https://teem.sf.net/nrrd/format.html
# See ./0-README.txt for more information about how these source files are
# generated automatically from a checkout of Teem. The use of Teem source
# files (with modifications and relicensing) is why "TEEM" shows up in some
# compilation flags, but our job here is to build NrrdIO aka NRRDIO, hence
# the NrrdIO/NRRDIO naming of CMake things that are externally visible.

# CMake Version 3.12 came out July 17, 2018
# https://www.kitware.com/cmake-3-12-0-available-for-download/
# No need to copy Teem's use of 3.25 because we don't need "try_compile" since
# here the AIR_EXISTS macro always expands into a airExists() function call.
# 3.12 improved find_package() (including its use for ZLIB and BZip2).
cmake_minimum_required(VERSION 3.12...3.31)
project(NrrdIO
        DESCRIPTION "NrrdIO: C library for NRRD file IO (with optional compressions)"
        LANGUAGES C)

# Set NRRDIO_PREFIX manually when configuring with cmake
# so that the generated library does not conflict with the
# public name of the Teem library. By default this variable
# is empty and the library name defaults to the project name
# of NrrdIO.
set(NRRDIO_LIBRARY_NAME ${NRRDIO_PREFIX}${PROJECT_NAME})

set(nrrdio_SRCS
    754.c
    accessors.c
    array.c
    arraysNrrd.c
    axis.c
    biffbiff.c
    biffmsg.c
    comment.c
    defaultsNrrd.c
    encoding.c
    encodingAscii.c
    encodingBzip2.c
    encodingGzip.c
    encodingHex.c
    encodingRaw.c
    encodingZRL.c
    endianAir.c
    endianNrrd.c
    enum.c
    enumsNrrd.c
    format.c
    formatEPS.c
    formatNRRD.c
    formatPNG.c
    formatPNM.c
    formatText.c
    formatVTK.c
    gzio.c
    keyvalue.c
    methodsNrrd.c
    miscAir.c
    mop.c
    parseAir.c
    parseNrrd.c
    preamble.c
    read.c
    reorder.c
    sampleIO.c
    sane.c
    simple.c
    string.c
    subset.c
    write.c
)

# Turn on TEEM_BUILD when compiling .c files so that on Windows
# the proper dll export def's are used.
add_definitions(-DTEEM_BUILD=1)

# Configure (and learn how to find) NrrdIO.h
configure_file(${NrrdIO_SOURCE_DIR}/NrrdIO.h.in ${NrrdIO_BINARY_DIR}/NrrdIO.h)
include_directories( ${NrrdIO_BINARY_DIR} )

if ( NOT "${NRRDIO_PREFIX}" STREQUAL "ITK" ) # <-- ITK START_REMOVE
## Override configuring zlib in ../CMakeLists.txt

option(NRRDIO_USE_ZLIB "Enable support for ZLIB compression" ON)
if(NRRDIO_USE_ZLIB)
  # Possibly turn on usage of zlib compression (requires linking with libz)
  # (i.e., programs compiled with ${NRRDIO_LIBRARY_NAME}NrrdIO must also be compiled with zlib)
  add_definitions(-DTEEM_ZLIB=1)
  find_package(ZLIB)
  list(APPEND NRRDIO_COMPRESSION_LIBRARIES ZLIB::ZLIB)
endif()

option(NRRDIO_USE_BZIP2 "Enable support for BZIP2 compression" OFF)
if(NRRDIO_USE_BZIP2)
  # Possibly turn on usage of bzip2 compression (requires linking with bz2)
  # (i.e., programs compiled with ${NRRDIO_LIBRARY_NAME}NrrdIO must also be compiled with bz2)
  add_definitions(-DTEEM_BZIP2=1)
  find_package(BZip2)
  list(APPEND NRRDIO_COMPRESSION_LIBRARIES BZip2::BZip2)
endif()

# Build the library
add_library(${NRRDIO_LIBRARY_NAME} ${nrrdio_SRCS} )
# Link optional compression libs
if(NRRDIO_USE_ZLIB OR NRRDIO_USE_BZIP2)
  target_link_libraries(${NRRDIO_LIBRARY_NAME} PRIVATE ${NRRDIO_COMPRESSION_LIBRARIES})
endif()

else() # <-- ITK START_REPLACE
## These are ITK-specific
  # Build the library
  add_library(${NRRDIO_LIBRARY_NAME} ${nrrdio_SRCS} )
  target_link_libraries(${NRRDIO_LIBRARY_NAME} PRIVATE ${NRRDIO_COMPRESSION_LIBRARIES})
  if(NOT BUILD_SHARED_LIBS)
   set(TEEM_STATIC 1)
  endif()

  set(MANGLE_PREFIX itk)
  configure_file(${NrrdIO_SOURCE_DIR}/itk_NrrdIO_mangle.h.in
    ${NrrdIO_BINARY_DIR}/itk_NrrdIO_mangle.h
    @ONLY
  )

  if(ITK_LIBRARY_PROPERTIES)
    set_target_properties(${NRRDIO_LIBRARY_NAME} PROPERTIES ${ITK_LIBRARY_PROPERTIES})
  endif()

  install(TARGETS ${NRRDIO_LIBRARY_NAME}
    EXPORT ${ITK3P_INSTALL_EXPORT_NAME}
    RUNTIME DESTINATION ${ITK3P_INSTALL_RUNTIME_DIR} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK3P_INSTALL_LIBRARY_DIR} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK3P_INSTALL_ARCHIVE_DIR} COMPONENT Development
    )

  install(FILES ${NrrdIO_BINARY_DIR}/NrrdIO.h ${NrrdIO_BINARY_DIR}/itk_NrrdIO_mangle.h
    DESTINATION ${ITK3P_INSTALL_INCLUDE_DIR} # TODO: itk_NrrdIO.h #include "itkNrrdIO/NrrdIO.h"
    COMPONENT Development)

  if(CMAKE_COMPILER_IS_GNUCXX)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-strict-aliasing")
  endif()
endif() # <-- ITK END_REPLACE
