cmake_minimum_required(VERSION 3.16.3)
project(ITKBridgeNumPy)

# Only set SYSTEM_INCLUDE_DIRS if Python is outside the source directory
# CMake does not allow INTERFACE properties to contain paths prefixed in the source directory
set(ITKBridgeNumPy_SYSTEM_INCLUDE_DIRS "")
if(Python3_INCLUDE_DIRS)
  foreach(_py_inc_dir ${Python3_INCLUDE_DIRS})
    # Check if Python include directory is outside the source tree
    get_filename_component(_py_inc_real "${_py_inc_dir}" REALPATH)
    # Use CMAKE_SOURCE_DIR which is the ITK source directory when building as part of ITK
    file(RELATIVE_PATH _relpath "${CMAKE_SOURCE_DIR}" "${_py_inc_real}")
    
    # Determine if directory is outside the source tree:
    # 1. Empty _relpath means directory equals CMAKE_SOURCE_DIR (inside source)
    # 2. If _relpath is still absolute (Windows: different drive), it's outside source
    # 3. If _relpath starts with "..", it's outside the source tree
    # Pattern matches: ".." at start, followed by "/" or "\" (path separator) or end of string
    set(_is_outside_source FALSE)
    if(_relpath)
      if(IS_ABSOLUTE "${_relpath}")
        # Path is on different drive/root (Windows-specific), outside source tree
        set(_is_outside_source TRUE)
      elseif(_relpath MATCHES "^\\.\\.([/\\\\]|$)")
        # Path starts with "..", outside source tree
        set(_is_outside_source TRUE)
      endif()
    endif()
    
    if(_is_outside_source)
      # Directory is outside source directory, safe to add to SYSTEM_INCLUDE_DIRS
      # This will be added to both BUILD_INTERFACE and INSTALL_INTERFACE
      list(APPEND ITKBridgeNumPy_SYSTEM_INCLUDE_DIRS "${_py_inc_dir}")
    else()
      # Directory is within source directory or equals source directory (e.g., .pixi/envs)
      # Use include_directories() to add for build-time compilation only
      # This affects all targets in this module but won't propagate to INSTALL_INTERFACE
      include_directories("${_py_inc_dir}")
    endif()
  endforeach()
endif()

if(NOT ITK_SOURCE_DIR)
  find_package(ITK REQUIRED)
  list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  include(ITKModuleExternal)
else()
  itk_module_impl()
endif()
