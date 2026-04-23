cmake_minimum_required(VERSION 3.16.3)

project(
  AnisotropicDiffusionLBR
  VERSION
    5.1.0 #Version should track with ITK
  LANGUAGES
    CXX
)

# Header only module, no libraries set(AnisotropicDiffusionLBR_LIBRARIES AnisotropicDiffusionLBR)

if(NOT ITK_SOURCE_DIR)
  find_package(ITK 5.1 REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  include(ITKModuleExternal)
else()
  set(ITK_DIR ${CMAKE_BINARY_DIR})
  itk_module_impl()
endif()

# itk_module_examples() is intentionally omitted: the upstream
# examples/ directory is not part of the whitelist-based ingest
# (see Modules/Filtering/AnisotropicDiffusionLBR/README.md), so
# expanding this call under BUILD_EXAMPLES=ON would fail at CMake
# configure time looking for a non-existent subdirectory.
