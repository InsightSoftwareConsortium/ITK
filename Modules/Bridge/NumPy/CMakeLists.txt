cmake_minimum_required(VERSION 3.16.3)
project(ITKBridgeNumPy)

# Don't set SYSTEM_INCLUDE_DIRS for Python - handle includes separately
# to avoid CMake errors about source-prefixed paths in INTERFACE properties
set(ITKBridgeNumPy_SYSTEM_INCLUDE_DIRS "")

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  include(ITKModuleExternal)
else()
  itk_module_impl()
endif()

# Add Python includes using BUILD_INTERFACE generator expression
# This ensures they're only used during build, not in exports
if(Python3_INCLUDE_DIRS AND TARGET ITKBridgeNumPyModule)
  target_include_directories(ITKBridgeNumPyModule
    SYSTEM
    INTERFACE
      $<BUILD_INTERFACE:${Python3_INCLUDE_DIRS}>
  )
endif()
