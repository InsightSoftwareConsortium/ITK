#
#  ITKIOReview: library for new ImageIO classes
#
set(ITKIOReview_SRCS
  itkBruker2DSEQImageIO.cxx
  itkBruker2DSEQImageIOFactory.cxx
  itkPhilipsRECImageIO.cxx
  itkPhilipsRECImageIOFactory.cxx
  itkPhilipsPAR.cxx
  itkVoxBoCUBImageIO.cxx
  itkVoxBoCUBImageIOFactory.cxx
  itkGaborKernelFunction.cxx
  itkMRCHeaderObject.cxx
  itkMRCImageIO.cxx
  itkMRCImageIOFactory.cxx
  itkStreamingImageIOBase.cxx
  itkVTKImageIO2.cxx
  itkVTKImageIO2Factory.cxx
  itkJPEG2000ImageIO.cxx
  itkJPEG2000ImageIOFactory.cxx
  )

include_directories(${ITK_SOURCE_DIR}/Utilities/openjpeg)

add_library(ITKIOReview ${ITK_REVIEW_BUILD_TYPE} ${ITKIOReview_SRCS})
target_link_libraries(ITKIOReview  ITKIO itkopenjpeg)
if(ITK_LIBRARY_PROPERTIES)
  set_target_properties(ITKIOReview PROPERTIES ${ITK_LIBRARY_PROPERTIES})
endif(ITK_LIBRARY_PROPERTIES)
if(NOT ITK_INSTALL_NO_LIBRARIES)
  install(TARGETS ITKIOReview
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
endif(NOT ITK_INSTALL_NO_LIBRARIES)

if(ITK_USE_TRANSFORM_IO_FACTORIES)
# Sources of non-templated classes.
set(ITKTransformIOReview_SRCS
  itkTransformFileReaderWithFactory.cxx
  itkTransformFileWriterWithFactory.cxx
  itkMatlabTransformIO.cxx
  itkMatlabTransformIOFactory.cxx
  itkTransformIOBase.cxx
  itkTransformIOFactory.cxx
  itkTxtTransformIO.cxx
  itkTxtTransformIOFactory.cxx
  )

add_library(ITKTransformIOReview ${ITK_REVIEW_BUILD_TYPE} ${ITKTransformIOReview_SRCS})
target_link_libraries(ITKTransformIOReview ITKCommon)
if(ITK_LIBRARY_PROPERTIES)
  set_target_properties(ITKTransformIOReview PROPERTIES ${ITK_LIBRARY_PROPERTIES})
endif(ITK_LIBRARY_PROPERTIES)
if(NOT ITK_INSTALL_NO_LIBRARIES)
  install(TARGETS ITKTransformIOReview
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
endif(NOT ITK_INSTALL_NO_LIBRARIES)

endif(ITK_USE_TRANSFORM_IO_FACTORIES)


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
# add_library(ITKReview ${ITK_REVIEW_BUILD_TYPE} ${ITKReview_SRCS})
# target_link_libraries(ITKReview ITKCommon itkvnl)
# if(ITK_LIBRARY_PROPERTIES)
#   set_target_properties(ITKReview PROPERTIES ${ITK_LIBRARY_PROPERTIES})
# endif(ITK_LIBRARY_PROPERTIES)
# if(NOT ITK_INSTALL_NO_LIBRARIES)
#   install(TARGETS ITKReview
#     RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
#     LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
#     ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
# endif(NOT ITK_INSTALL_NO_LIBRARIES)
#
#
#   End of optional ITKReview library setup
#


if(ITK_USE_MINC2 AND MINC2_FOUND)
set(ITKMINC2_SRCS
  itkMINC2ImageIO.cxx
  itkMINC2ImageIOFactory.cxx
  )

add_library(ITKIOMINC2 ${ITK_REVIEW_BUILD_TYPE} ${ITKMINC2_SRCS})
target_link_libraries(ITKIOMINC2 ITKIO ${MINC2_LIB} ${HDF5_LIB} ${NETCDF_LIB})
if(ITK_LIBRARY_PROPERTIES)
  set_target_properties(ITKIOMINC2 PROPERTIES ${ITK_LIBRARY_PROPERTIES})
endif(ITK_LIBRARY_PROPERTIES)
if(NOT ITK_INSTALL_NO_LIBRARIES)
  install(TARGETS ITKIOMINC2
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
endif(NOT ITK_INSTALL_NO_LIBRARIES)



endif(ITK_USE_MINC2 AND MINC2_FOUND)


# Build ITKQuadEdgeMesh library

add_library(ITKQuadEdgeMesh ${ITK_REVIEW_BUILD_TYPE}
  itkQuadEdge.cxx
  )
if(NOT ITK_INSTALL_NO_LIBRARIES)
  install(TARGETS ITKQuadEdgeMesh
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
endif(NOT ITK_INSTALL_NO_LIBRARIES)


target_link_libraries(ITKQuadEdgeMesh ITKCommon)

if(ITK_LIBRARY_PROPERTIES)
  set_target_properties(ITKQuadEdgeMesh PROPERTIES
    ${ITK_LIBRARY_PROPERTIES}
    )
endif(ITK_LIBRARY_PROPERTIES)


# install the development files
if(ITK_USE_FLAT_DIRECTORY_INSTALL)
  set( ITK_INSTALL_INCLUDE_DIR_CM24_REVIEW ${ITK_INSTALL_INCLUDE_DIR_CM24})
else(ITK_USE_FLAT_DIRECTORY_INSTALL)
  set( ITK_INSTALL_INCLUDE_DIR_CM24_REVIEW ${ITK_INSTALL_INCLUDE_DIR_CM24}/Review)
endif(ITK_USE_FLAT_DIRECTORY_INSTALL)


if(NOT ITK_INSTALL_NO_DEVELOPMENT)
  file(GLOB __files1 "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
  file(GLOB __files2 "${CMAKE_CURRENT_SOURCE_DIR}/*.txx")
  install(FILES ${__files1} ${__files2}
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24_REVIEW}
    COMPONENT Development)
endif(NOT ITK_INSTALL_NO_DEVELOPMENT)
# Warn the user about the license issues and lack of stability of the API
# of the code in this directory
set(msg "Attention: You have chosen to use the files in the Review directory.")
set(msg "${msg} The Copyright of these files has not been cleared up, and the coding")
set(msg "${msg} style and API of the classes in the Review directory may change")
set(msg "${msg} drastically as it get code reviews from the developers.")
set(msg "${msg} Please set ITK_USE_REVIEW to OFF if you don't want to use these files.")

# display the message during the setup
message("${msg}")

# and also display it during the install
install(CODE "message(\"${msg}\")")
