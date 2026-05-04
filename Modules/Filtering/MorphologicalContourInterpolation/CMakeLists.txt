cmake_minimum_required(VERSION 3.16.3)

if(POLICY CMP0135)
  cmake_policy(SET CMP0135 NEW)
endif()

if(CMAKE_CXX_STANDARD EQUAL "11")
  message(
    FATAL_ERROR
    "CMAKE_CXX_STANDARD:STRING=11 is not supported in ITK version 5.3 and greater."
  )
endif()

if(NOT CMAKE_CXX_STANDARD)
  if(WASI OR EMSCRIPTEN)
    set(CMAKE_CXX_STANDARD 20) # Supported values are ``14``, ``17``, and ``20``.
  else()
    set(CMAKE_CXX_STANDARD 17) # Supported values are ``14``, ``17``, and ``20``.
  endif()
endif()
if(NOT CMAKE_CXX_STANDARD_REQUIRED)
  set(CMAKE_CXX_STANDARD_REQUIRED ON)
endif()
if(NOT CMAKE_CXX_EXTENSIONS)
  set(CMAKE_CXX_EXTENSIONS OFF)
endif()

project(MorphologicalContourInterpolation)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  include(ITKModuleExternal)
  if(WASI OR EMSCRIPTEN)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    add_subdirectory(wasm)
  endif()
else()
  set(ITK_DIR ${CMAKE_BINARY_DIR})
  itk_module_impl()
endif()

itk_module_examples()
