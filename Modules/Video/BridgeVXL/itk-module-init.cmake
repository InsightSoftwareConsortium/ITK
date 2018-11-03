if(NOT ITK_USE_SYSTEM_VXL)
  message( FATAL_ERROR "System installed VXL is required for ITKVideoBridgeVXL.")
endif()
find_package(VXL 2.0.0 REQUIRED)

if (VXL_FOUND)
  include( ${VXL_CMAKE_DIR}/UseVXL.cmake )
  message("ITK_USE_SYSTEM_VXL is ON; Use an outside build of VXL. ")
endif()
