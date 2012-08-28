find_package ( VXL REQUIRED )

if (VXL_FOUND)
  set(ITK_USE_SYSTEM_VXL ON CACHE BOOL "System installed VXL is required for ITKVideoBridgeVXL." FORCE)
  include( ${VXL_CMAKE_DIR}/UseVXL.cmake )
  message("ITK_USE_SYSTEM_VXL is ON; Use an outside build of VXL. ")
endif()
