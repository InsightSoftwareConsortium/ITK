project(ITK-HDF5)
set(ITK-HDF5_THIRD_PARTY 1)

option(ITK_USE_SYSTEM_HDF5 "Use an outside build of HDF5." OFF)
mark_as_advanced(ITK_USE_SYSTEM_HDF5)

if(ITK_USE_SYSTEM_HDF5)
  find_package(HDF5 REQUIRED C CXX)
  set(ITK-HDF5_SYSTEM_INCLUDE_DIRS
    ${ITK-HDF5_BINARY_DIR}/src # itk_hdf5.h and itk_H5Cpp.h
    ${HDF5_INCLUDE_DIRS}
    )
  set(ITK-HDF5_LIBRARIES ${HDF5_LIBRARIES})
  set(ITK-HDF5_NO_SRC 1)
else()
  set(ITK-HDF5_INCLUDE_DIRS
    ${ITK-HDF5_SOURCE_DIR}/src
    ${ITK-HDF5_BINARY_DIR}/src
    )
  set(ITK-HDF5_LIBRARIES itkhdf5_cpp itkhdf5)
endif()

configure_file(src/itk_hdf5.h.in src/itk_hdf5.h)
configure_file(src/itk_H5Cpp.h.in src/itk_H5Cpp.h)

itk_module_impl()

install(FILES ${ITK-HDF5_BINARY_DIR}/src/itk_hdf5.h
              ${ITK-HDF5_BINARY_DIR}/src/itk_H5Cpp.h
  DESTINATION ${ITK-HDF5_INSTALL_INCLUDE_DIR})
