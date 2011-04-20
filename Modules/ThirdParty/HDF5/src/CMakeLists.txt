set(HDF5_EXTERNALLY_CONFIGURED ON)
set(HDF5_EXTERNAL_LIB_PREFIX "itk")
set(HDF5_EXPORTED_TARGETS ${ITK-HDF5-targets})
set(HDF5_BUILD_CPP_LIB 1)
set(HDF5_INSTALL_LIBRARY_DIR ${ITK-HDF5_INSTALL_LIBRARY_DIR})
set(HDF5_INSTALL_ARCHIVE_DIR ${ITK-HDF5_INSTALL_ARCHIVE_DIR})
set(HDF5_INSTALL_RUNTIME_DIR ${ITK-HDF5_INSTALL_RUNTIME_DIR})
set(HDF5_INSTALL_INCLUDE_DIR ${ITK-HDF5_INSTALL_INCLUDE_DIR}/itkhdf5)

set(HDF5_DISABLE_COMPILER_WARNINGS OFF CACHE INTERNAL "Disable HDF5 warnings")
set(HDF5_ENABLE_Z_LIB_SUPPORT ON CACHE INTERNAL "Enable HDF5 Zlib Filters")

# Setup all necessary overrides for zlib so that HDF5 uses our
# internally compiled zlib rather than any other version
if(HDF5_ENABLE_Z_LIB_SUPPORT)
  # We must tell the main HDF5 library that it depends on our zlib
  set(ZLIB_INCLUDE_DIRS "${ITK-ZLIB_INCLUDE_DIRS}")
  set(ZLIB_LIBRARIES "${ITK-ZLIB_LIBRARIES}")
  set(H5_ZLIB_HEADER "itk_zlib.h")
endif(HDF5_ENABLE_Z_LIB_SUPPORT)

add_subdirectory(itkhdf5)

foreach(lib
    itkhdf5
    itkhdf5_cpp
    )
  itk_module_target_name(${lib})
  itk_module_target_export(${lib})
endforeach()
