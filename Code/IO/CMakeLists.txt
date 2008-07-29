SET(ITKIO_SRCS
  itkNiftiImageIO.cxx
  itkNiftiImageIOFactory.cxx
  itkAnalyzeImageIO.cxx
  itkAnalyzeImageIOFactory.cxx
  itkArchetypeSeriesFileNames.cxx
  itkBioRadImageIO.cxx
  itkBioRadImageIOFactory.cxx
  itkBMPImageIO.cxx
  itkBMPImageIOFactory.cxx
  itkBrains2HeaderBase.cxx
  itkBrains2HeaderFactory.cxx
  itkBrains2IPLHeaderInfo.cxx
  itkBrains2MaskHeaderInfo.cxx
  itkBrains2MaskImageIO.cxx
  itkBrains2MaskImageIOFactory.cxx
  itkDICOMImageIO2.cxx
  itkDICOMImageIO2Factory.cxx
  itkDICOMSeriesFileNames.cxx
  itkDicomImageIOFactory.cxx
  itkGDCMImageIO.cxx
  itkGDCMImageIOFactory.cxx
  itkGDCMSeriesFileNames.cxx
  itkGE4ImageIO.cxx
  itkGE4ImageIOFactory.cxx
  itkGE5ImageIO.cxx
  itkGE5ImageIOFactory.cxx
  itkGEAdwImageIO.cxx
  itkGEAdwImageIOFactory.cxx
  itkGiplImageIO.cxx
  itkGiplImageIOFactory.cxx
  itkIOCommon.cxx
  itkIPLCommonImageIO.cxx
  itkIPLFileNameList.cxx
  itkImageIOBase.cxx
  itkImageIOFactory.cxx
  itkImageIORegion.cxx
  itkJPEGImageIO.cxx
  itkJPEGImageIOFactory.cxx
  itkLSMImageIO.cxx
  itkLSMImageIOFactory.cxx
  itkMetaImageIO.cxx
  itkMetaImageIOFactory.cxx
  itkNrrdImageIO.cxx
  itkNrrdImageIOFactory.cxx
  itkNumericSeriesFileNames.cxx
  itkPNGImageIO.cxx
  itkPNGImageIOFactory.cxx
  itkPolygonGroupSpatialObjectXMLFile.cxx
  itkRegularExpressionSeriesFileNames.cxx
  itkSiemensVisionImageIO.cxx
  itkSiemensVisionImageIOFactory.cxx
  itkStimulateImageIO.cxx
  itkStimulateImageIOFactory.cxx
  itkTIFFImageIO.cxx
  itkTIFFImageIOFactory.cxx
  itkVTKImageIO.cxx
  itkVTKImageIOFactory.cxx
  itkXMLFile.cxx
  itkMetaArrayReader.cxx
  itkMetaArrayWriter.cxx
)

IF( NOT ITK_USE_TRANSFORM_IO_FACTORIES )
SET(ITKIO_SRCS
  ${ITKIO_SRCS}
  itkTransformFileReader.cxx
  itkTransformFileWriter.cxx
  )
ENDIF( NOT ITK_USE_TRANSFORM_IO_FACTORIES )

# Add ITKIO library
ADD_LIBRARY(ITKIO ${ITKIO_SRCS})

# Enable warnings for trunctation of 64bits to 32bits in Mac
IF (APPLE)
  IF(CMAKE_COMPILER_IS_GNUCXX)
    IF(ITK_USE_64BITS_APPLE_TRUNCATION_WARNING)
      SET_TARGET_PROPERTIES(ITKIO PROPERTIES COMPILE_FLAGS "-Wshorten-64-to-32")
    ENDIF(ITK_USE_64BITS_APPLE_TRUNCATION_WARNING)
  ENDIF(CMAKE_COMPILER_IS_GNUCXX)
ENDIF (APPLE)

TARGET_LINK_LIBRARIES(ITKIO
ITKCommon
ITKNrrdIO
itkjpeg8
itkjpeg12
itkjpeg16
${ITK_GDCM_LIBRARIES}
${ITK_PNG_LIBRARIES}
${ITK_ZLIB_LIBRARIES}
${ITK_TIFF_LIBRARIES}
ITKSpatialObject
ITKMetaIO
ITKDICOMParser
ITKEXPAT
ITKniftiio)

IF( ITK_USE_TRANSFORM_IO_FACTORIES )
    TARGET_LINK_LIBRARIES(ITKIO ITKTransformIOReview)
ENDIF( ITK_USE_TRANSFORM_IO_FACTORIES )

IF(CMAKE_COMPILER_IS_GNUCXX)
  SET_SOURCE_FILES_PROPERTIES(itkTestDriver.cxx PROPERTIES COMPILE_FLAGS -w)
  SET_SOURCE_FILES_PROPERTIES(itkPolygonGroupSpatialObjectXMLFile.cxx PROPERTIES COMPILE_FLAGS -w)
  SET_SOURCE_FILES_PROPERTIES(itkJPEGImageIO.cxx PROPERTIES COMPILE_FLAGS -w)
  SET_SOURCE_FILES_PROPERTIES(itkPNGImageIO.cxx PROPERTIES COMPILE_FLAGS -w)
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

# Avoid anachronism warnings by Sun compilers
INCLUDE(${ITK_SOURCE_DIR}/CMake/itkCheckCXXAcceptsFlags.cmake)
itkCHECK_CXX_ACCEPTS_FLAGS("-features=no%anachronisms" SUN_COMPILER)
IF(SUN_COMPILER)
  SET_SOURCE_FILES_PROPERTIES(itkXMLFile.cxx PROPERTIES COMPILE_FLAGS -w)
ENDIF(SUN_COMPILER)

# itkTestDriver is built even when BUILD_TESTING is FALSE to:
# + allow to run WrapITK tests without the other of the ITK tests
# + be used by external code like contributions to the insight journal
ADD_EXECUTABLE(itkTestDriver itkTestDriver.cxx) 
TARGET_LINK_LIBRARIES(itkTestDriver ITKIO)
IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  INSTALL(TARGETS itkTestDriver
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)
SET(ITK_TEST_DRIVER "${EXECUTABLE_OUTPUT_PATH}/itkTestDriver" 
    CACHE INTERNAL "itkTestDriver path to be used by subprojects")

IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(ITKIO PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)

IF(NOT ITK_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS ITKIO
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)

IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  FILE(GLOB __files1 "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
  FILE(GLOB __files2 "${CMAKE_CURRENT_SOURCE_DIR}/*.txx")
  INSTALL(FILES ${__files1} ${__files2}
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24}/IO
    COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)
