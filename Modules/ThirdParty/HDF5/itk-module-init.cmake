option(ITK_USE_SYSTEM_HDF5 "Use an outside build of HDF5." ${ITK_USE_SYSTEM_LIBRARIES})
mark_as_advanced(ITK_USE_SYSTEM_HDF5)

if(ITK_USE_SYSTEM_HDF5)
  if(ITK_BUILD_SHARED_LIBS)
    find_package(HDF5 QUIET NO_MODULE COMPONENTS CXX C shared)
  else()
    find_package(HDF5 QUIET NO_MODULE COMPONENTS CXX C static)
  endif()

  if(NOT HDF5_FOUND)
    find_package(HDF5 REQUIRED COMPONENTS CXX C)
  endif()
endif()
