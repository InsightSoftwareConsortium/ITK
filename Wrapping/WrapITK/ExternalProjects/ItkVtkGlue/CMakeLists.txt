cmake_minimum_required(VERSION 2.4)
if(COMMAND CMAKE_POLICY)
  cmake_policy(SET CMP0003 NEW)
endif(COMMAND CMAKE_POLICY)

project(ItkVtkGlue)

option(BUILD_WRAPPERS "Wrap library" OFF)
option(INSTALL_DEVEL_FILES "Install C++ headers" ON)

find_package(ITK REQUIRED)
find_package(VTK REQUIRED)

include_directories(${VTK_INCLUDE_DIRS} ${ITK_INCLUDE_DIRS} ${ItkVtkGlue_SOURCE_DIR}/src)
link_directories(${VTK_LIBRARY_DIRS} ${ITK_LIBRARY_DIRS})

if(BUILD_WRAPPERS)
   add_subdirectory(Wrapping)
endif(BUILD_WRAPPERS)

# install devel files
if(INSTALL_DEVEL_FILES)
  foreach(f itkImageToVTKImageFilter.h itkImageToVTKImageFilter.txx itkVTKImageToImageFilter.h itkVTKImageToImageFilter.txx)
    install_files(/include/InsightToolkit/BasicFilters FILES ${CMAKE_CURRENT_SOURCE_DIR}/src/${f})
  endforeach(f)
endif(INSTALL_DEVEL_FILES)

enable_testing()

