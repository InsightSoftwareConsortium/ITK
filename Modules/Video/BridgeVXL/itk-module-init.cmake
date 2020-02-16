if(ITK_USE_SYSTEM_VXL)
  find_package(VXL 2.0.2 REQUIRED)

  if (VXL_FOUND)
    include( ${VXL_CMAKE_DIR}/UseVXL.cmake )
    message("ITK_USE_SYSTEM_VXL is ON; Use an outside build of VXL. ")
  endif()
endif()
