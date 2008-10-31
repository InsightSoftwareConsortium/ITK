#
#  ITKIOReview: library for new ImageIO classes
#
SET(ITKIOReview_SRCS
  itkBruker2DSEQImageIO.cxx
  itkBruker2DSEQImageIOFactory.cxx
  itkPhilipsRECImageIO.cxx
  itkPhilipsRECImageIOFactory.cxx
  itkPhilipsPAR.cxx
  itkVoxBoCUBImageIO.cxx
  itkVoxBoCUBImageIOFactory.cxx
  )
 
ADD_LIBRARY(ITKIOReview ${ITK_REVIEW_BUILD_TYPE} ${ITKIOReview_SRCS})
TARGET_LINK_LIBRARIES (ITKIOReview ITKIO ITKCommon itkvnl)
IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(ITKIOReview PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)
IF(NOT ITK_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS ITKIOReview
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)



IF(ITK_USE_TRANSFORM_IO_FACTORIES)
# Sources of non-templated classes.
SET(ITKTransformIOReview_SRCS
  itkTransformFileReaderWithFactory.cxx
  itkTransformFileWriterWithFactory.cxx
  itkMatlabTransformIO.cxx
  itkMatlabTransformIOFactory.cxx
  itkTransformIOBase.cxx
  itkTransformIOFactory.cxx
  itkTxtTransformIO.cxx
  itkTxtTransformIOFactory.cxx
  )

ADD_LIBRARY(ITKTransformIOReview ${ITK_REVIEW_BUILD_TYPE} ${ITKTransformIOReview_SRCS})
TARGET_LINK_LIBRARIES (ITKTransformIOReview ITKCommon itkvnl)
IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(ITKTransformIOReview PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)
IF(NOT ITK_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS ITKTransformIOReview
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)

ENDIF(ITK_USE_TRANSFORM_IO_FACTORIES)


#
#   Begin of optional ITKReview library setup
#
# So far we do not have non-templated classes in this directory.
# Whenever we add non-templated classes, then the following lines
# should be moved out of the comments.
#
# and ... when the non-templated classes are moved out of this
# directory, these lines must be commented out again if the library
# becomes empty.
#
# ADD_LIBRARY(ITKReview ${ITK_REVIEW_BUILD_TYPE} ${ITKReview_SRCS})
# TARGET_LINK_LIBRARIES (ITKReview ITKCommon itkvnl)
# IF(ITK_LIBRARY_PROPERTIES)
#   SET_TARGET_PROPERTIES(ITKReview PROPERTIES ${ITK_LIBRARY_PROPERTIES})
# ENDIF(ITK_LIBRARY_PROPERTIES)
# IF(NOT ITK_INSTALL_NO_LIBRARIES)
#   INSTALL(TARGETS ITKReview
#     RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
#     LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
#     ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
# ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)
#
#
#   End of optional ITKReview library setup
#


IF(ITK_USE_MINC2 AND MINC2_FOUND)
SET(ITKMINC2_SRCS
  itkMINC2ImageIO.cxx
  itkMINC2ImageIOFactory.cxx
  )

ADD_LIBRARY(ITKIOMINC2 ${ITK_REVIEW_BUILD_TYPE} ${ITKMINC2_SRCS})
TARGET_LINK_LIBRARIES (ITKIOMINC2 ITKIO ${MINC2_LIB} ${HDF5_LIB} ${NETCDF_LIB})
IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(ITKIOMINC2 PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)
IF(NOT ITK_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS ITKIOMINC2
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)



ENDIF(ITK_USE_MINC2 AND MINC2_FOUND)


# Build ITKQuadEdgeMesh library

ADD_LIBRARY(ITKQuadEdgeMesh ${ITK_REVIEW_BUILD_TYPE}
  itkQuadEdge.cxx
  )
IF(NOT ITK_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS ITKQuadEdgeMesh
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)


TARGET_LINK_LIBRARIES(ITKQuadEdgeMesh ITKCommon)

IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(ITKQuadEdgeMesh PROPERTIES
    ${ITK_LIBRARY_PROPERTIES}
    )
ENDIF(ITK_LIBRARY_PROPERTIES)


# install the development files
IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  FILE(GLOB __files1 "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
  FILE(GLOB __files2 "${CMAKE_CURRENT_SOURCE_DIR}/*.txx")
  INSTALL(FILES ${__files1} ${__files2}
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24}/Review
    COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)
# Warn the user about the license issues and lack of stability of the API
# of the code in this directory
SET(msg "Attention: You have chosen to use the files in the Review directory.")
SET(msg "${msg} The Copyright of these files has not been cleared up, and the coding")
SET(msg "${msg} style and API of the classes in the Review directory may change")
SET(msg "${msg} drastically as it get code reviews from the developers.")
SET(msg "${msg} Please set ITK_USE_REVIEW to OFF if you don't want to use these files.")

# display the message during the setup
MESSAGE("${msg}")

# and also display it during the install
INSTALL(CODE "MESSAGE(\"${msg}\")")
