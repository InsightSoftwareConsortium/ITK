include(ExternalProject)

set( KWStyle_SOURCE_DIR ${CMAKE_BINARY_DIR}/KWStyle )
set( KWStyle_DIR ${CMAKE_BINARY_DIR}/KWStyle-build )
set( KWSTYLE_EXECUTABLE ${KWStyle_DIR}/KWStyle)

if(CMAKE_BUILD_TYPE)
  set(_build_configuration_arg -DCMAKE_BUILD_TYPE=Release)
endif()

configure_file("${ITK_CMAKE_DIR}/ITKKWStyleConfig.cmake.in"
               "${CMAKE_CURRENT_BINARY_DIR}/ITKKWStyleConfig.cmake" @ONLY)

# XXX Implementation of the itk_download_attempt_check macro copied from the
#     ITK main CMakeLists.txt. This allows external modules to use the logic
#     which is not defined when building against an ITK build tree.
#     Equivalent to "itk_download_attempt_check(KWStyle)".
if(ITK_FORBID_DOWNLOADS)
  message(SEND_ERROR "Attempted to download KWStyle when ITK_FORBID_DOWNLOADS is ON")
endif()
set(CMAKE_CXX_COMPILER_LAUNCHER_FLAG -DCMAKE_CXX_COMPILER_LAUNCHER:FILEPATH=${CMAKE_CXX_COMPILER_LAUNCHER})
set(CMAKE_C_COMPILER_LAUNCHER_FLAG -DCMAKE_C_COMPILER_LAUNCHER:FILEPATH=${CMAKE_C_COMPILER_LAUNCHER})
if(NOT TARGET KWStyle)
ExternalProject_add(KWStyle
  GIT_REPOSITORY "${git_protocol}://github.com/Kitware/KWStyle.git"
  GIT_TAG d0d656e67f95129421e47825301a056f245cfecf
  UPDATE_COMMAND ""
  DOWNLOAD_DIR ${KWStyle_SOURCE_DIR}
  SOURCE_DIR ${KWStyle_SOURCE_DIR}
  BINARY_DIR ${KWStyle_DIR}
  INSTALL_DIR ${KWStyle_DIR}
  LOG_DOWNLOAD 1
  LOG_UPDATE 0
  LOG_CONFIGURE 0
  LOG_BUILD 0
  LOG_TEST 0
  LOG_INSTALL 0
  CMAKE_GENERATOR ${gen}
  CMAKE_ARGS
    -DCMAKE_MAKE_PROGRAM:FILEPATH=${CMAKE_MAKE_PROGRAM}
    -DCMAKE_CXX_COMPILER:STRING=${CMAKE_CXX_COMPILER}
    ${CMAKE_CXX_COMPILER_LAUNCHER_FLAG}
    -DCMAKE_C_COMPILER:STRING=${CMAKE_C_COMPILER}
    ${CMAKE_C_COMPILER_LAUNCHER_FLAG}
    -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
    -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
    ${_build_configuration_arg}
    -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
    -DBUILD_TESTING:BOOL=OFF
  INSTALL_COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/ITKKWStyleConfig.cmake
  )
endif()
