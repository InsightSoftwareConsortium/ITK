project(ITKTextureFeatures)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
endif()

# This are checks specific to the example content in this module and can be
# removed.
include(CheckIncludeFileCXX)
check_include_file_cxx(
  "random"
  _have_random
)
if(NOT _have_random)
  message(FATAL_ERROR "This module requires the C++11 \"random\" header.")
endif()
include(ITK_CheckCXXCompilerFlag)
itk_check_cxx_compiler_flag("-Wno-c++1z-extensions" _have_cxx1z_extensions_warning)
if(_have_cxx1z_extensions_warning)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-c++1z-extensions")
endif()

set(ITKTextureFeatures_LIBRARIES ITKTextureFeatures)
if(NOT ITK_SOURCE_DIR)
  include(ITKModuleExternal)
else()
  itk_module_impl()
endif()
