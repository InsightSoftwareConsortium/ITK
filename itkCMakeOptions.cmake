#
# This file can be included by other projects that use or depend on Insight.
# It sets up many default parameters and include paths.
#

# set project include directories
INCLUDE_DIRECTORIES(
${ITK_SOURCE_DIR}/Code/Algorithms
${ITK_SOURCE_DIR}/Code/BasicFilters
${ITK_SOURCE_DIR}/Code/Common 
${ITK_SOURCE_DIR}/Code/Numerics
${ITK_SOURCE_DIR}/Code/IO 
${ITK_SOURCE_DIR}/Code/Numerics/FEM
${ITK_SOURCE_DIR}/Code/Numerics/Statistics
${ITK_SOURCE_DIR}/Code/SpatialObject
${ITK_SOURCE_DIR}/Utilities/png
${ITK_SOURCE_DIR}/Utilities/zlib
${ITK_SOURCE_DIR}/Utilities/MetaIO
${ITK_SOURCE_DIR}/Utilities/MetaIO/SpatialObject
${ITK_SOURCE_DIR}/Utilities/DICOMParser
${ITK_BINARY_DIR} 
)

OPTION(VXL_FROM_UTILITIES "Use new vxl from utilities" 0)

IF(VXL_FROM_UTILITIES)
 INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Utilities/vxl/vcl
                      ${ITK_BINARY_DIR}/Utilities/vxl/vcl
                      ${ITK_SOURCE_DIR}/Utilities/vxl)
  IF(WIN32)
    IF(NOT CYGWIN)
      INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Utilities/vxl/vcl/config.win32)
    ENDIF(NOT CYGWIN)
  ENDIF(WIN32)
ELSE(VXL_FROM_UTILITIES)
  INCLUDE_DIRECTORIES(
  ${ITK_BINARY_DIR}/Code/Numerics/vxl 
  ${ITK_SOURCE_DIR}/Code/Numerics/vxl 
  ${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl 
  ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl 
  )
ENDIF(VXL_FROM_UTILITIES)


# use the distributed version of SGI's experimental C++ Standard Library
IF (NOT ITK_USE_SYSTEM_STDLIB)
  INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Utilities/stdlib)
  LINK_DIRECTORIES(${ITK_BINARY_DIR}/Utilities/stdlib)
ENDIF (NOT ITK_USE_SYSTEM_STDLIB)

# set link libraries for project
IF(UNIX)
  LINK_LIBRARIES(${CMAKE_THREAD_LIBS} ${CMAKE_DL_LIBS} -lm)
ENDIF(UNIX)
