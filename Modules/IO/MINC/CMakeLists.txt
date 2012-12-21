project(ITKIOMINC)
set(ITKIOMINC_LIBRARIES ITKIOMINC)

IF(NOT ITK_USE_SYSTEM_HDF5)
  include_directories(
    "${ITKHDF5_SOURCE_DIR}/src/itkhdf5"
    "${ITKHDF5_BINARY_DIR}/src/itkhdf5"
    )
ENDIF()

itk_module_impl()
