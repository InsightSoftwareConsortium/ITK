cmake_minimum_required(VERSION 3.10.2 FATAL_ERROR)
foreach(p
    ## Only policies introduced after the cmake_minimum_required
    ## version need to explicitly be set to NEW.
    CMP0070 #3.10.0 Define ``file(GENERATE)`` behavior for relative paths.
    CMP0071 #3.10.0 Let ``AUTOMOC`` and ``AUTOUIC`` process ``GENERATED`` files.
    )
  if(POLICY ${p})
    cmake_policy(SET ${p} NEW)
  endif()
endforeach()

if(NOT ITK_SOURCE_DIR)
  include(itk-module-init.cmake)
endif()
project(ITKVideoBridgeOpenCV)

set(ITKVideoBridgeOpenCV_LIBRARIES ITKVideoBridgeOpenCV)

# When this module is loaded by an app, load OpenCV too.
set(ITKVideoBridgeOpenCV_EXPORT_CODE_INSTALL "
set(OpenCV_DIR \"${OpenCV_DIR}\")
find_package(OpenCV REQUIRED)
")
set(ITKVideoBridgeOpenCV_EXPORT_CODE_BUILD "
if(NOT ITK_BINARY_DIR)
  set(OpenCV_DIR \"${OpenCV_DIR}\")
  find_package(OpenCV REQUIRED)
endif()
")

set(ITKVideoBridgeOpenCV_SYSTEM_INCLUDE_DIRS ${OpenCV_INCLUDE_DIRS})
set(ITKVideoBridgeOpenCV_SYSTEM_LIBRARY_DIRS ${OpenCV_LIB_DIR})

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  include(ITKModuleExternal)
else()
  itk_module_impl()
endif()
