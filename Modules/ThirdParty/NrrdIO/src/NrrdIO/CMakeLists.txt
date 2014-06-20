PROJECT(ITK3P_NrrdIO)
INCLUDE_REGULAR_EXPRESSION("^.*.h$")

#
# This CMake file configures the NrrdIO library build.  NrrdIO
# is used by Insight/Code/IO/itkNrrdIO for reading/writing
# "Nearly Raw Raster Data" within the open-source Teem software
# package. See http://teem.sourceforge.net for more information.
#

SET(nrrdio_SRCS 754.c mop.c array.c parseAir.c dio.c sane.c endianAir.c
string.c enum.c miscAir.c biffbiff.c biffmsg.c accessors.c defaultsNrrd.c
enumsNrrd.c arraysNrrd.c methodsNrrd.c reorder.c axis.c simple.c comment.c
keyvalue.c endianNrrd.c parseNrrd.c gzio.c read.c write.c format.c
formatNRRD.c encoding.c encodingRaw.c encodingAscii.c encodingHex.c
encodingGzip.c subset.c encodingBzip2.c formatEPS.c formatPNG.c
formatPNM.c formatText.c formatVTK.c )

# Turn on TEEM_BUILD and TEEM_STATIIC so that the proper dll export
# def's are used on windows builds.
ADD_DEFINITIONS(-DTEEM_BUILD)

if(NOT BUILD_SHARED_LIBS)
 set(TEEM_STATIC 1)
endif()

# Configure (and learn how to find) NrrdIO.h
CONFIGURE_FILE(${ITK3P_NrrdIO_SOURCE_DIR}/NrrdIO.h.in
  ${ITK3P_NrrdIO_BINARY_DIR}/NrrdIO.h)
INCLUDE_DIRECTORIES(
  ${ITK3P_NrrdIO_BINARY_DIR}
)

#-----------------------------------------------------------------------------
# Find platform-specific differences in the handling of IEEE floating point
# special values.

include(${CMAKE_CURRENT_SOURCE_DIR}/CMake/TestQnanhibit.cmake)
test_qnanhibit(QNANHIBIT_VALUE ${CMAKE_CURRENT_SOURCE_DIR}/CMake)
if(QNANHIBIT_VALUE)
   set(QNANHIBIT 1 CACHE INTERNAL "The 22nd bit of 32-bit floating-point quiet NaN.")
else(QNANHIBIT_VALUE)
   set(QNANHIBIT 0 CACHE INTERNAL "The 22nd bit of 32-bit floating-point quiet NaN.")
endif(QNANHIBIT_VALUE)

#The QNANHIBIT variable is configured by the root level CMakeLists.txt
IF(QNANHIBIT)
  ADD_DEFINITIONS(-DTEEM_QNANHIBIT=1)
ELSE(QNANHIBIT)
  ADD_DEFINITIONS(-DTEEM_QNANHIBIT=0)
ENDIF(QNANHIBIT)

#DirectIO is the fast way to do multi-gigabyte I/O and currently only available
#for SGI platforms.  Use of DirectIO is enabled manually for now.
#OPTION(USE_DIRECTIO "Use DirectIO for Nrrd file IO.  Only valid on SGI systems." 0)
#MARK_AS_ADVANCED(USE_DIRECTIO)
#IF(USE_DIRECTIO)
#  ADD_DEFINITIONS(-DTEEM_DIO=1)
#ELSE(USE_DIRECTIO)
  ADD_DEFINITIONS(-DTEEM_DIO=0)
#ENDIF(USE_DIRECTIO)

# Tell NRRD to enable nrrdEncodingGzip
ADD_DEFINITIONS(-DTEEM_ZLIB=1)

ADD_LIBRARY(ITKNrrdIO ${nrrdio_SRCS} )
TARGET_LINK_LIBRARIES(ITKNrrdIO ${ITKZLIB_LIBRARIES})

IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(ITKNrrdIO PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)

INSTALL(TARGETS ITKNrrdIO
  EXPORT ${ITK3P_INSTALL_EXPORT_NAME}
  RUNTIME DESTINATION ${ITK3P_INSTALL_RUNTIME_DIR} COMPONENT RuntimeLibraries
  LIBRARY DESTINATION ${ITK3P_INSTALL_LIBRARY_DIR} COMPONENT RuntimeLibraries
  ARCHIVE DESTINATION ${ITK3P_INSTALL_ARCHIVE_DIR} COMPONENT Development
  )

INSTALL(FILES ${ITK3P_NrrdIO_BINARY_DIR}/NrrdIO.h itk_NrrdIO_mangle.h
  DESTINATION ${ITK3P_INSTALL_INCLUDE_DIR} # TODO: itk_NrrdIO.h #include "itkNrrdIO/NrrdIO.h"
  COMPONENT Development)

IF(CMAKE_COMPILER_IS_GNUCXX)
  SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-strict-aliasing")
ENDIF(CMAKE_COMPILER_IS_GNUCXX)
