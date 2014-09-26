project(ITKGDCM)
set(ITKGDCM_THIRD_PARTY 1)

if(ITK_USE_SYSTEM_GDCM)
  set(ITKGDCM_SYSTEM_INCLUDE_DIRS
    ${GDCM_INCLUDE_DIRS}
    )
  set(ITKGDCM_SYSTEM_LIBRARY_DIRS
    ${GDCM_LIBRARY_DIRS}
    )
  set(ITKGDCM_LIBRARIES gdcmDICT gdcmMSFF )
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
      if(MSVC OR "${CMAKE_CXX_COMPILER_ID}" MATCHES "Intel")
        list(APPEND ITKGDCM_INCLUDE_DIRS
          ${ITKGDCM_SOURCE_DIR}/src/gdcm/Utilities/C99)
      endif()
    endif()
  endif()
  set(ITKGDCM_LIBRARIES  gdcmDICT gdcmMSFF )
endif()

itk_module_impl()
