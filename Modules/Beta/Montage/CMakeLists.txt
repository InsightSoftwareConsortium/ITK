cmake_minimum_required(VERSION 3.16.3)

if(CMAKE_CXX_STANDARD EQUAL "98" )
   message(FATAL_ERROR "CMAKE_CXX_STANDARD:STRING=98 is not supported in ITK version 5 and greater.")
endif()

if(NOT CMAKE_CXX_STANDARD)
  set(CMAKE_CXX_STANDARD 17) # Supported values are ``17``, ``20``, and ``23``.
endif()
if(NOT CMAKE_CXX_STANDARD_REQUIRED)
  set(CMAKE_CXX_STANDARD_REQUIRED ON)
endif()
if(NOT CMAKE_CXX_EXTENSIONS)
  set(CMAKE_CXX_EXTENSIONS OFF)
endif()

project(Montage)

set(Montage_LIBRARIES Montage)

# Suppress warnings about potentially uninstantiated static members
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  include(CheckCXXCompilerFlag)
  CHECK_CXX_COMPILER_FLAG("-Wno-undefined-var-template" COMPILER_HAS_NO_UNDEFINED_VAR_TEMPLATE)
  if(COMPILER_HAS_NO_UNDEFINED_VAR_TEMPLATE)
    set(CMAKE_CXX_FLAGS "-Wno-undefined-var-template ${CMAKE_CXX_FLAGS}")
  endif()
endif()

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  include(ITKModuleExternal)
else()
  set(ITK_DIR ${CMAKE_BINARY_DIR})
  itk_module_impl()
endif()

itk_module_examples()
