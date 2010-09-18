set(ITKIO_SRCS
  itkNiftiImageIO.cxx
  itkNiftiImageIOFactory.cxx
  itkAnalyzeImageIO.cxx
  itkAnalyzeImageIOFactory.cxx
  itkArchetypeSeriesFileNames.cxx
  itkBioRadImageIO.cxx
  itkBioRadImageIOFactory.cxx
  itkBMPImageIO.cxx
  itkBMPImageIOFactory.cxx
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

if( NOT ITK_USE_TRANSFORM_IO_FACTORIES )
set(ITKIO_SRCS
  ${ITKIO_SRCS}
  itkTransformFileReader.cxx
  itkTransformFileWriter.cxx
  )
endif( NOT ITK_USE_TRANSFORM_IO_FACTORIES )

# Add ITKIO library
add_library(ITKIO ${ITKIO_SRCS})

# Enable warnings for trunctation of 64bits to 32bits in Mac
if(APPLE)
  if(CMAKE_COMPILER_IS_GNUCXX)
    if(ITK_USE_64BITS_APPLE_TRUNCATION_WARNING)
      set_target_properties(ITKIO PROPERTIES COMPILE_FLAGS "-Wshorten-64-to-32")
    endif(ITK_USE_64BITS_APPLE_TRUNCATION_WARNING)
  endif(CMAKE_COMPILER_IS_GNUCXX)
endif(APPLE)

target_link_libraries(ITKIO
ITKNrrdIO
${ITK_GDCM_LIBRARIES}
${ITK_PNG_LIBRARIES}
${ITK_JPEG_LIBRARIES}
${ITK_TIFF_LIBRARIES}
ITKSpatialObject
ITKMetaIO
ITKDICOMParser
ITKEXPAT
ITKniftiio)

if( ITK_USE_TRANSFORM_IO_FACTORIES )
    target_link_libraries(ITKIO ITKTransformIOReview)
endif( ITK_USE_TRANSFORM_IO_FACTORIES )

if(CMAKE_COMPILER_IS_GNUCXX)
  set_source_files_properties(itkTestDriver.cxx PROPERTIES COMPILE_FLAGS -w)
  set_source_files_properties(itkPolygonGroupSpatialObjectXMLFile.cxx PROPERTIES COMPILE_FLAGS -w)
  set_source_files_properties(itkJPEGImageIO.cxx PROPERTIES COMPILE_FLAGS -w)
  set_source_files_properties(itkPNGImageIO.cxx PROPERTIES COMPILE_FLAGS -w)
endif(CMAKE_COMPILER_IS_GNUCXX)

# Avoid anachronism warnings by Sun compilers
include(${ITK_SOURCE_DIR}/CMake/itkCheckCXXAcceptsFlags.cmake)
itkCHECK_CXX_ACCEPTS_FLAGS("-features=no%anachronisms" SUN_COMPILER)
if(SUN_COMPILER)
  set_source_files_properties(itkXMLFile.cxx PROPERTIES COMPILE_FLAGS -w)
endif(SUN_COMPILER)

# itkTestDriver is built even when BUILD_TESTING is FALSE to:
# + allow to run WrapITK tests without the other of the ITK tests
# + be used by external code like contributions to the insight journal
add_executable(itkTestDriver itkTestDriver.cxx)
target_link_libraries(itkTestDriver ITKIO)
if(NOT ITK_INSTALL_NO_DEVELOPMENT)
  install(TARGETS itkTestDriver
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT Development)
endif(NOT ITK_INSTALL_NO_DEVELOPMENT)
set(ITK_TEST_DRIVER "${EXECUTABLE_OUTPUT_PATH}/itkTestDriver"
    CACHE INTERNAL "itkTestDriver path to be used by subprojects")

if(ITK_LIBRARY_PROPERTIES)
  set_target_properties(ITKIO PROPERTIES ${ITK_LIBRARY_PROPERTIES})
endif(ITK_LIBRARY_PROPERTIES)

if(NOT ITK_INSTALL_NO_LIBRARIES)
  install(TARGETS ITKIO
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
endif(NOT ITK_INSTALL_NO_LIBRARIES)

if(ITK_USE_FLAT_DIRECTORY_INSTALL)
  set( ITK_INSTALL_INCLUDE_DIR_CM24_IO ${ITK_INSTALL_INCLUDE_DIR_CM24})
else(ITK_USE_FLAT_DIRECTORY_INSTALL)
  set( ITK_INSTALL_INCLUDE_DIR_CM24_IO ${ITK_INSTALL_INCLUDE_DIR_CM24}/IO)
endif(ITK_USE_FLAT_DIRECTORY_INSTALL)

if(NOT ITK_INSTALL_NO_DEVELOPMENT)
  file(GLOB __files1 "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
  file(GLOB __files2 "${CMAKE_CURRENT_SOURCE_DIR}/*.txx")
  install(FILES ${__files1} ${__files2}
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24_IO}
    COMPONENT Development)
endif(NOT ITK_INSTALL_NO_DEVELOPMENT)
