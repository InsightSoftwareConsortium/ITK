###############################################################################
# Add the ITKPythonSupport library which contains the itkPyCommand class.

BEGIN_WRAPPER_LIBRARY("ITKPyUtils" Python)

SET(WRAPPER_LIBRARY_DEPENDS Base)
SET(WRAPPER_LIBRARY_LINK_LIBRARIES ITKCommon)

SET(WRAPPER_LIBRARY_CXX_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/itkPyCommand.cxx") 

WRAPPER_LIBRARY_CREATE_WRAP_FILES()
WRAPPER_LIBRARY_CREATE_LIBRARY()

