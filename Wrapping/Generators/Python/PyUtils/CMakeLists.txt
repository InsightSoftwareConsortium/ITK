###############################################################################
# Add the ITKPythonSupport library which contains the itkPyCommand class.

itk_wrap_module("ITKPyUtils" Python)

set(WRAPPER_LIBRARY_DEPENDS ITKCommon)
set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

set(HEADER_FILES
  "${CMAKE_CURRENT_LIST_DIR}/itkPyCommand.h"
  "${CMAKE_CURRENT_LIST_DIR}/itkPyImageFilter.h"
)

set(WRAPPER_LIBRARY_CXX_SOURCES "${CMAKE_CURRENT_LIST_DIR}/itkPyCommand.cxx" ${HEADER_FILES})

itk_auto_load_and_end_wrap_submodules()
