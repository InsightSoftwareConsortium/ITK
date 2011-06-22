project(ITK-GDCM)
set(ITK-GDCM_THIRD_PARTY 1)

if(ITK_USE_SYSTEM_GDCM)
  set(ITK-GDCM_SYSTEM_INCLUDE_DIRS
    ${GDCM_INCLUDE_DIRS}
    )
  set(ITK-GDCM_INCLUDE_DIRS ${GDCM_INCLUDE_DIRS})
  set(ITK-GDCM_LIBRARIES ITK-GDCM)
else()
  set(ITK-GDCM_INCLUDE_DIRS
    ${ITK-GDCM_BINARY_DIR}
    ${ITK-GDCM_BINARY_DIR}/src/gdcm/Source/Common
    ${ITK-GDCM_SOURCE_DIR}/src/gdcm/Source/MediaStorageAndFileFormat
    ${ITK-GDCM_SOURCE_DIR}/src/gdcm/Source/DataDictionary
    ${ITK-GDCM_SOURCE_DIR}/src/gdcm/Source/Common
    ${ITK-GDCM_SOURCE_DIR}/src/gdcm/Source/InformationObjectDefinition
    ${ITK-GDCM_SOURCE_DIR}/src/gdcm/Source/MessageExchangeDefinition
    ${ITK-GDCM_SOURCE_DIR}/src/gdcm/Source/DataStructureAndEncodingDefinition
    )
  include(CheckIncludeFile)
  check_include_file("stdint.h"       GDCM_HAVE_STDINT_H)
  if(NOT GDCM_HAVE_STDINT_H)
    if( WIN32 )
      if(MSVC OR "${CMAKE_CXX_COMPILER_ID}" MATCHES "Intel")
        list(APPEND ITK-GDCM_INCLUDE_DIRS
          ${ITK-GDCM_SOURCE_DIR}/src/gdcm/Utilities/C99)
      endif()
    endif()
  endif()
  set(ITK-GDCM_LIBRARIES  gdcmDICT gdcmMSFF )
endif()

itk_module_impl()
