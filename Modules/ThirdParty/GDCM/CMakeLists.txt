project(ITKGDCM)
set(ITKGDCM_THIRD_PARTY 1)

if(ITK_USE_SYSTEM_GDCM)
  set(ITKGDCM_SYSTEM_INCLUDE_DIRS ${GDCM_INCLUDE_DIRS})
  set(ITKGDCM_SYSTEM_LIBRARY_DIRS ${GDCM_LIBRARY_DIRS})
  set(ITKGDCM_LIBRARIES gdcmDICT gdcmMSFF)
  set(ITKGDCM_NO_SRC 1)

  # When this module is loaded by an app, load GDCM too.
  set(ITKGDCM_EXPORT_CODE_INSTALL "
set(GDCM_DIR \"${GDCM_DIR}\")
find_package(GDCM REQUIRED)
")
  set(ITKGDCM_EXPORT_CODE_BUILD "
if(NOT ITK_BINARY_DIR)
  set(CMAKE_MODULE_PATH \"${CMAKE_CURRENT_SOURCE_DIR}/CMake\" \${CMAKE_MODULE_PATH})

  set(GDCM_DIR \"${GDCM_DIR}\")
  find_package(GDCM REQUIRED)
endif()
")

else()
  set(ITKGDCM_INCLUDE_DIRS
    ${ITKGDCM_BINARY_DIR}
    ${ITKGDCM_BINARY_DIR}/src/gdcm/Source/Common
    ${ITKGDCM_SOURCE_DIR}/src/gdcm/Source/MediaStorageAndFileFormat
    ${ITKGDCM_SOURCE_DIR}/src/gdcm/Source/DataDictionary
    ${ITKGDCM_SOURCE_DIR}/src/gdcm/Source/Common
    ${ITKGDCM_SOURCE_DIR}/src/gdcm/Source/InformationObjectDefinition
    ${ITKGDCM_SOURCE_DIR}/src/gdcm/Source/MessageExchangeDefinition
    ${ITKGDCM_SOURCE_DIR}/src/gdcm/Source/DataStructureAndEncodingDefinition
    )
  include(CheckIncludeFile)
  check_include_file("stdint.h"       GDCM_HAVE_STDINT_H)
  if(NOT GDCM_HAVE_STDINT_H)
    if( WIN32 )
      if(MSVC OR "x${CMAKE_CXX_COMPILER_ID}x" MATCHES "Intel")
        list(APPEND ITKGDCM_INCLUDE_DIRS
          ${ITKGDCM_SOURCE_DIR}/src/gdcm/Utilities/C99)
      endif()
    endif()
  endif()
  set(ITKGDCM_LIBRARIES  gdcmDICT gdcmMSFF )
  set(CHARLS_LIBRARY_PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS FALSE)
  set(GDCM_LIBRARY_PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS FALSE)
  set(JPEG_LIBRARY_PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS FALSE)
  set(OPENJPEG_LIBRARY_PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS FALSE)
  set(SOCKETXX_LIBRARY_PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS FALSE)
endif()

itk_module_impl()
