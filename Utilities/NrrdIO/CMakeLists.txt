PROJECT(ITKNrrdIO)
INCLUDE_REGULAR_EXPRESSION("^.*.h$")

#
# This CMake file configures the NrrdIO library build.  NrrdIO
# is used by Insight/Code/IO/itkNrrdIO for reading/writing 
# "Nearly Raw Raster Data" within the open-source Teem software 
# package. See http://teem.sourceforge.net for more information.
#

SET(nrrdio_SRCS 754.c mop.c array.c parseAir.c dio.c sane.c endianAir.c 
string.c enum.c miscAir.c biffbiff.c accessors.c defaultsNrrd.c enumsNrrd.c 
arraysNrrd.c methodsNrrd.c reorder.c axis.c simple.c comment.c 
keyvalue.c endianNrrd.c parseNrrd.c gzio.c read.c write.c format.c 
formatNRRD.c encoding.c encodingRaw.c encodingAscii.c encodingHex.c 
encodingGzip.c subset.c encodingBzip2.c formatEPS.c formatPNG.c 
formatPNM.c formatText.c formatVTK.c )

# Turn on TEEM_BUILD so that the proper dll export def's are 
# used on windows builds.
ADD_DEFINITIONS(-DTEEM_BUILD=1)

# Set compiler flags for endian-ness.
IF(CMAKE_WORDS_BIGENDIAN)
  ADD_DEFINITIONS(-DTEEM_ENDIAN=4321)
ELSE(CMAKE_WORDS_BIGENDIAN)
  ADD_DEFINITIONS(-DTEEM_ENDIAN=1234)
ENDIF(CMAKE_WORDS_BIGENDIAN)

# Set compiler flags for 32 or 64 bit architecture (based on the size
# of a void pointer).
IF(CMAKE_SIZEOF_VOID_P MATCHES  8)
  ADD_DEFINITIONS(-DTEEM_32BIT=0)
ELSE(CMAKE_SIZEOF_VOID_P MATCHES 8)
  ADD_DEFINITIONS(-DTEEM_32BIT=1)
ENDIF(CMAKE_SIZEOF_VOID_P MATCHES 8)

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
TARGET_LINK_LIBRARIES(ITKNrrdIO ${ITK_ZLIB_LIBRARIES} )

IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(ITKNrrdIO PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)

IF(NOT ITK_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS ITKNrrdIO
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development
    )
ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)

IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  FILE(GLOB __files "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
  INSTALL(FILES ${__files}
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24}/Utilities/NrrdIO
    COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)

