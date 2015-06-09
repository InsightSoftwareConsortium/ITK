project(ITKIOMINC)
set(ITKIOMINC_LIBRARIES ITKIOMINC)

if(NOT ITK_USE_SYSTEM_HDF5)
  include_directories(
    "${ITKHDF5_SOURCE_DIR}/src/itkhdf5"
    "${ITKHDF5_SOURCE_DIR}/src/itkhdf5/src"
    "${ITKHDF5_BINARY_DIR}/src/itkhdf5"
    )
endif()

itk_module_impl()
