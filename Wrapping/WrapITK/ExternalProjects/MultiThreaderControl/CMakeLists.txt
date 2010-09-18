project(MultiThreaderControl)
find_package(WrapITK REQUIRED)

BEGIN_WRAPPER_LIBRARY("MultiThreaderControl")

set(WRAPPER_LIBRARY_DEPENDS Base)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ITKCommon)

if(WIN32)
  include_directories( ${PROJECT_SOURCE_DIR} )
  set(WRAPPER_DEFAULT_INCLUDE "itkMultiThreaderStub.h" ${WRAPPER_DEFAULT_INCLUDE})
endif(WIN32)

WRAPPER_LIBRARY_CREATE_WRAP_FILES()
WRAPPER_LIBRARY_CREATE_LIBRARY()


enable_testing()

add_subdirectory(Python)

