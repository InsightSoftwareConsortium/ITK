cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(ITK-GDCM)
find_package(itk-base 4.0 EXACT REQUIRED)

option(ITK_USE_SYSTEM_GDCM "Use an outside build of GDCM." OFF)
mark_as_advanced(ITK_USE_SYSTEM_GDCM)

if(ITK_USE_SYSTEM_GDCM)
  find_package(GDCM REQUIRED)
  set(ITK-GDCM_INCLUDE_DIRS
    ${GDCM_INCLUDE_DIR}
    )
  set(ITK-GDCM_LIBRARIES  gdcmDICT gdcmMSFF )
  set(itk-module-config-install "
list(APPEND ITK-GDCM_LIBRARY_DIRS \"${GDCM_LIBRARY_DIR}\")
list(APPEND ITK-GDCM_INCLUDE_DIRS \"${GDCM_INCLUDE_DIRS}\")
")
  set(itk-module-config-build "
list(APPEND ITK-GDCM_LIBRARIES \"${GDCM_LIBRARIES})
list(APPEND ITK-GDCM_LIBRARY_DIRS \"${GDCM_LIBRARY_DIR}\")
")
  set(itk-module-no-src 1)
else()
  set(ITK-GDCM-target  gdcmDICT)
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
  check_include_file("stdint.h"       GDCM_HAVE_STDINT_H)
  if(NOT GDCM_HAVE_STDINT_H)
    # for MSVC only gdcm has a stdint.h that works for that
    # compiler found in Utilities/C99
    if(MSVC)
      list(APPEND ITK-GDCM_INCLUDE_DIRS
        ${ITK-GDCM_SOURCE_DIR}/src/gdcm/Utilities/C99)
    endif()
  endif()
  set(ITK-GDCM_LIBRARIES  gdcmDICT gdcmMSFF )
endif()

itk_module_impl()
