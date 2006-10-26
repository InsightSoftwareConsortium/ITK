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
  itkTransformFileReader.cxx
  itkTransformFileWriter.cxx
  itkVTKImageIO.cxx
  itkVTKImageIOFactory.cxx
  itkXMLFile.cxx
)

# Add ITKIO library
ADD_LIBRARY(ITKIO ${ITKIO_SRCS})

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

ADD_EXECUTABLE(itkTestDriver itkTestDriver.cxx) 
TARGET_LINK_LIBRARIES(itkTestDriver ITKIO)
INSTALL(TARGETS itkTestDriver
  RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT Development)
SET(ITK_TEST_DRIVER "${EXECUTABLE_OUTPUT_PATH}/itkTestDriver" CACHE INTERNAL "itkTestDriver path to be used by subprojects")

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
