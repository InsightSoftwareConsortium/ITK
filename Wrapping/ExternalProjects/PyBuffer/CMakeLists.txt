project(PyBuffer)
include(CPack)
cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

find_package(WrapITK REQUIRED)

# Deal with numarray package  Options
set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}")
find_package( NUMARRAY )

include_directories( ${PYTHON_NUMARRAY_INCLUDE_DIR} )

itk_wrap_module("BufferConversion" Python)

  set(WRAPPER_LIBRARY_DEPENDS Base)
  set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})
    set(WRAPPER_SWIG_LIBRARY_FILES ${WRAPPER_SWIG_LIBRARY_FILES} "${CMAKE_CURRENT_SOURCE_DIR}/itkPyBuffer.swg")

  itk_auto_load_submodules()
itk_end_wrap_module()

WRAP_ITK_BINDINGS_INSTALL("/Python/itkExtras/" "${CMAKE_CURRENT_SOURCE_DIR}/PyBufferExtras.py")
