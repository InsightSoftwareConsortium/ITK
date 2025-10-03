#
# NrrdIO: stand-alone code for basic nrrd functionality
# Copyright (C) 2013, 2012, 2011, 2010, 2009  University of Chicago
# Copyright (C) 2008, 2007, 2006, 2005  Gordon Kindlmann
# Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998  University of Utah
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
#
cmake_minimum_required(VERSION 3.20...3.31)
project(ITK3P_NrrdIO)
include_regular_expression("^.*.h$")

#
# This CMake file configures the NrrdIO library build.  NrrdIO
# is used by Insight/Code/IO/itkNrrdIO for reading/writing
# "Nearly Raw Raster Data" within the open-source Teem software
# package. See http://teem.sourceforge.net for more information.
#

SET(nrrdio_SRCS
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
read.c
reorder.c
sane.c
simple.c
string.c
subset.c
write.c
)
# Turn on TEEM_BUILD so that the proper dll export def's are
# used on windows builds.
ADD_DEFINITIONS(-DTEEM_BUILD=1)

if(NOT BUILD_SHARED_LIBS)
 set(TEEM_STATIC 1)
endif()

# Configure (and learn how to find) NrrdIO.h
CONFIGURE_FILE(${ITK3P_NrrdIO_SOURCE_DIR}/NrrdIO.h.in
  ${ITK3P_NrrdIO_BINARY_DIR}/NrrdIO.h)
INCLUDE_DIRECTORIES(
  ${ITK3P_NrrdIO_BINARY_DIR}
)

set(MANGLE_PREFIX itk)
configure_file(${ITK3P_NrrdIO_SOURCE_DIR}/itk_NrrdIO_mangle.h.in
  ${ITK3P_NrrdIO_BINARY_DIR}/itk_NrrdIO_mangle.h
  @ONLY)

#-----------------------------------------------------------------------------
# Find platform-specific differences in the handling of IEEE floating point
# special values.

include(${CMAKE_CURRENT_SOURCE_DIR}/CMake/TestQnanhibit.cmake)
test_qnanhibit(QNANHIBIT_VALUE ${CMAKE_CURRENT_SOURCE_DIR}/CMake)
if(QNANHIBIT_VALUE)
   set(QNANHIBIT 1 CACHE INTERNAL "The 22nd bit of 32-bit floating-point quiet NaN.")
else()
   set(QNANHIBIT 0 CACHE INTERNAL "The 22nd bit of 32-bit floating-point quiet NaN.")
endif()

#The QNANHIBIT variable is configured by the root level CMakeLists.txt
if(QNANHIBIT)
  add_definitions(-DTEEM_QNANHIBIT=1)
else()
  add_definitions(-DTEEM_QNANHIBIT=0)
endif()


# Possibly turn on usage of zlib compression (requires linking with libz)
# (i.e., programs compiled with ITKNrrdIO must also be compiled with zlib)
add_definitions(-DTEEM_ZLIB=1)

add_library(ITKNrrdIO ${nrrdio_SRCS} )

if(UNIX)
  target_link_libraries(ITKNrrdIO ${ITKZLIB_LIBRARIES} m)
else()
  target_link_libraries(ITKNrrdIO ${ITKZLIB_LIBRARIES})
endif()

if(ITK_LIBRARY_PROPERTIES)
  set_target_properties(ITKNrrdIO PROPERTIES ${ITK_LIBRARY_PROPERTIES})
endif()

install(TARGETS ITKNrrdIO
  EXPORT ${ITK3P_INSTALL_EXPORT_NAME}
  RUNTIME DESTINATION ${ITK3P_INSTALL_RUNTIME_DIR} COMPONENT RuntimeLibraries
  LIBRARY DESTINATION ${ITK3P_INSTALL_LIBRARY_DIR} COMPONENT RuntimeLibraries
  ARCHIVE DESTINATION ${ITK3P_INSTALL_ARCHIVE_DIR} COMPONENT Development
  )

install(FILES ${ITK3P_NrrdIO_BINARY_DIR}/NrrdIO.h ${ITK3P_NrrdIO_BINARY_DIR}/itk_NrrdIO_mangle.h
  DESTINATION ${ITK3P_INSTALL_INCLUDE_DIR} # TODO: itk_NrrdIO.h #include "itkNrrdIO/NrrdIO.h"
  COMPONENT Development)

if(CMAKE_COMPILER_IS_GNUCXX)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-strict-aliasing")
endif()
