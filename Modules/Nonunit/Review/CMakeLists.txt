project(ITKReview)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/CMake ${CMAKE_MODULE_PATH})
set(ITKReview_LIBRARIES ITKReview)

itk_module_impl()

if(ITK_WRAP_PYTHON)
  if(PYTHON_VERSION_STRING VERSION_GREATER 3.0)
    message(FATAL_ERROR "The ITK Review can not be wrapped with Python 3 because it uses VTK.")
  endif()
endif()
