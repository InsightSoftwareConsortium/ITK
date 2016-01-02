project(ITKReview)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/CMake ${CMAKE_MODULE_PATH})
set(ITKReview_LIBRARIES ITKReview)

itk_module_impl()

if(ITK_WRAP_PYTHON)
  if(PYTHON_VERSION_STRING VERSION_GREATER 3.0 AND "${VTK_VERSION}" VERSION_LESS 7.0.0)
    message(FATAL_ERROR "The ITK Review module needs VTK >= 7 to be wrapped with Python 3.")
  endif()
endif()
