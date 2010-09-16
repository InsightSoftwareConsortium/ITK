find_package(WrapITK REQUIRED)

BEGIN_WRAPPER_LIBRARY(${PROJECT_NAME})
set(WRAPPER_SWIG_LIBRARY_FILES ${WRAPPER_SWIG_LIBRARY_FILES} "${CMAKE_CURRENT_SOURCE_DIR}/itkvtk.swg")
set(WRAPPER_LIBRARY_DEPENDS Base SimpleFilters)

WRAPPER_LIBRARY_CREATE_WRAP_FILES()
WRAPPER_LIBRARY_CREATE_LIBRARY()

if(WRAP_ITK_PYTHON)
  # I can't manage to put that code in Python subdir :-(
  set(VTK_VERSION "${VTK_MAJOR_VERSION}.${VTK_MINOR_VERSION}.${VTK_BUILD_VERSION}")
  set(MIN_ITK_VER "5.0.0")
  if("${VTK_VERSION}" STRLESS "${MIN_ITK_VER}")
    target_link_libraries(_${WRAPPER_LIBRARY_NAME}Python vtkImagingPython ITKCommon ITKBasicFilters)
  else("${VTK_VERSION}" STRLESS "${MIN_ITK_VER}")
    target_link_libraries(_${WRAPPER_LIBRARY_NAME}Python vtkImagingPythonD ITKCommon ITKBasicFilters)
  endif("${VTK_VERSION}" STRLESS "${MIN_ITK_VER}")

  add_subdirectory(Python)
endif(WRAP_ITK_PYTHON)


