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
${ITK_BINARY_DIR}/Code/Numerics/vxl 
${ITK_SOURCE_DIR}/Code/Numerics/vxl 
${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl 
${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl 
${ITK_SOURCE_DIR}/Code/SpatialObject
${ITK_SOURCE_DIR}/Utilities/png
${ITK_SOURCE_DIR}/Utilities/zlib
${ITK_SOURCE_DIR}/Utilities/MetaIO
${ITK_BINARY_DIR} 
)

# use the distributed version of SGI's experimental C++ Standard Library
IF (NOT ITK_USE_SYSTEM_STDLIB)
  INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Utilities/stdlib)
  LINK_DIRECTORIES(${ITK_BINARY_DIR}/Utilities/stdlib)
ENDIF (NOT ITK_USE_SYSTEM_STDLIB)

# set link libraries for project
IF(UNIX)
  LINK_LIBRARIES(${CMAKE_THREAD_LIBS} ${CMAKE_DL_LIBS})
ENDIF(UNIX)

IF(CMAKE_COMPILER_IS_GNUCXX)
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ftemplate-depth-50")
  IF(APPLE)
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-long-double")
  ENDIF(APPLE)
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

# force the use of ansi cxx flags (i.e. -LANG:std on sgi )
IF("x${CMAKE_ANSI_CXXFLAGS}" MATCHES "^x.*[^ ]")
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${CMAKE_ANSI_CXXFLAGS}")
ENDIF("x${CMAKE_ANSI_CXXFLAGS}" MATCHES "^x.*[^ ]")
IF("x${CMAKE_ANSI_CFLAGS}" MATCHES "^x.*[^ ]")
  SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${CMAKE_ANSI_CFLAGS}")
ENDIF("x${CMAKE_ANSI_CFLAGS}" MATCHES "^x.*[^ ]")

IF("x${CMAKE_TEMPLATE_FLAGS}" MATCHES "^x.*[^ ]")
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${CMAKE_TEMPLATE_FLAGS}")
ENDIF("x${CMAKE_TEMPLATE_FLAGS}" MATCHES "^x.*[^ ]")

IF(CMAKE_USE_PTHREADS)
    ADD_DEFINITIONS(-D_PTHREADS)
ENDIF(CMAKE_USE_PTHREADS)
IF(WIN32)
    ADD_DEFINITIONS(-DNOMINMAX)
ENDIF(WIN32)

