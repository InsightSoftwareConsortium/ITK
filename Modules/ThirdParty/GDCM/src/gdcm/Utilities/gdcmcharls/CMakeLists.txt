cmake_minimum_required(VERSION 2.8.7)

if(NOT CHARLS_NAMESPACE)
  set(CHARLS_NAMESPACE "CHARLS")
  set(CHARLS_STANDALONE 1)
endif()
# In all cases:
string(TOLOWER ${CHARLS_NAMESPACE} CHARLS_LIBRARY_NAME)

project(${CHARLS_NAMESPACE} CXX)

#-----------------------------------------------------------------------------
# CHARLS version number
set(CHARLS_MAJOR_VERSION 1)
set(CHARLS_MINOR_VERSION 1)
set(CHARLS_BUILD_VERSION 0)
set(CHARLS_VERSION
      "${CHARLS_MAJOR_VERSION}.${CHARLS_MINOR_VERSION}.${CHARLS_BUILD_VERSION}")

#-----------------------------------------------------------------------------
#set (EXECUTABLE_OUTPUT_PATH ${CHARLS_BINARY_DIR}/bin CACHE PATH "Single output directory for building all executables.")
#set (LIBRARY_OUTPUT_PATH ${CHARLS_BINARY_DIR}/bin CACHE PATH "Single output directory for building all libraries.")
#mark_as_advanced(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)

if(NOT CHARLS_INSTALL_BIN_DIR)
  set(CHARLS_INSTALL_BIN_DIR "bin")
endif()
if(NOT CHARLS_INSTALL_LIB_DIR)
  set(CHARLS_INSTALL_LIB_DIR "lib")
endif()
if(NOT CHARLS_INSTALL_INCLUDE_DIR)
  set(CHARLS_INSTALL_INCLUDE_DIR "include")
endif()

#set(MANGLE_PREFIX ${JPEGLS_LIBRARY_NAME})
#configure_file(${CMAKE_CURRENT_SOURCE_DIR}/jpegls_mangle.h.in
#               ${CMAKE_CURRENT_BINARY_DIR}/jpegls_mangle.h
#               @ONLY )



# When user specify build type do not override settings:
if(NOT CMAKE_BUILD_TYPE)
  # The following compiler option are only meant for GCC:
  if(CMAKE_COMPILER_IS_GNUCC)
    #set(CMAKE_CXX_FLAGS "-Wall -O3 -g" )
  endif()
endif()

#option(charls_BUILD_SHARED_LIBS "Build CharLS with shared libraries." OFF)
#set(BUILD_SHARED_LIBS ${charls_BUILD_SHARED_LIBS})

if(WIN32)
  if(BUILD_SHARED_LIBS)
    add_definitions(-DCHARLS_DLL)
  else()
    add_definitions(-DCHARLS_STATIC)
  endif()
endif()

add_library(${CHARLS_LIBRARY_NAME} header.cpp  interface.cpp  jpegls.cpp
  jpegmarkersegment.cpp  jpegstreamwriter.cpp
  )
set_target_properties( ${CHARLS_LIBRARY_NAME} PROPERTIES
                              VERSION ${CHARLS_MAJOR_VERSION}.${CHARLS_MINOR_VERSION}
                              SOVERSION ${CHARLS_MAJOR_VERSION})


# Install library
if(NOT CHARLS_INSTALL_NO_LIBRARIES)
  # Runtime
  install(TARGETS ${CHARLS_LIBRARY_NAME}
    EXPORT ${GDCM_TARGETS_NAME}
    RUNTIME DESTINATION ${CHARLS_INSTALL_BIN_DIR} COMPONENT Applications
    LIBRARY DESTINATION ${CHARLS_INSTALL_LIB_DIR} COMPONENT Libraries ${NAMELINK_SKIP}
    ARCHIVE DESTINATION ${CHARLS_INSTALL_LIB_DIR} COMPONENT DebugDevel
  )
  #Development
  if(NAMELINK_ONLY)
    install(TARGETS ${CHARLS_LIBRARY_NAME}
    EXPORT ${GDCM_TARGETS_NAME}
      LIBRARY DESTINATION ${CHARLS_INSTALL_LIB_DIR} COMPONENT DebugDevel ${NAMELINK_ONLY}
    )
  endif()
endif()

if(NOT CHARLS_INSTALL_NO_DEVELOPMENT)
  file(GLOB header_files "*.h")
  install(FILES ${header_files}
    DESTINATION ${CHARLS_INSTALL_INCLUDE_DIR} COMPONENT Headers
  )
endif()
