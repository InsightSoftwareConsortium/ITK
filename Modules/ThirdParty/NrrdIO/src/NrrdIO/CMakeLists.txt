#
# NrrdIO: C library for NRRD file IO (with optional compressions)
# Copyright (C) 2009--2026  University of Chicago
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
cmake_minimum_required(VERSION 3.12)
project(NrrdIO
        DESCRIPTION "NrrdIO: C library for NRRD file IO (with optional compressions)"
        LANGUAGES C)

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

set(NRRDIO_COMPRESSION_LIBRARIES "")
option(NRRDIO_USE_ZLIB "Enable support for ZLIB compression" ON)
if(NRRDIO_USE_ZLIB)
  # Possibly turn on usage of zlib compression (requires linking with libz)
  # (i.e., programs compiled with ITKNrrdIO must also be compiled with zlib)
  add_definitions(-DTEEM_ZLIB=1)
  find_package(ZLIB)
  list(APPEND NRRDIO_COMPRESSION_LIBRARIES ZLIB::ZLIB)
endif()

option(NRRDIO_USE_BZIP2 "Enable support for BZIP2 compression" OFF)
if(NRRDIO_USE_BZIP2)
  # Possibly turn on usage of bzip2 compression (requires linking with bz2)
  # (i.e., programs compiled with ITKNrrdIO must also be compiled with bz2)
  add_definitions(-DTEEM_BZIP2=1)
  find_package(BZip2)
  list(APPEND NRRDIO_COMPRESSION_LIBRARIES BZip2::BZip2)
endif()

# Build the library
add_library(NrrdIO ${nrrdio_SRCS} )

# Link optional compression libs
if(NRRDIO_USE_ZLIB OR NRRDIO_USE_BZIP2)
  target_link_libraries(NrrdIO PRIVATE ${NRRDIO_COMPRESSION_LIBRARIES})
endif()

## These are ITK-specific
#target_link_libraries(NrrdIO ${ITK_ZLIB_LIBRARIES} )
#install_targets(/lib/InsightToolkit ITKNrrdIO)
#install_files(/include/InsightToolkit/Utilities/NrrdIO "(\\.h)$")
