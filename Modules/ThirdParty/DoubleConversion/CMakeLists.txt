project(ITKDoubleConversion)
set(ITKDoubleConversion_THIRD_PARTY 1)

option(
  ITK_USE_SYSTEM_DOUBLECONVERSION
  "Use an outside build of double-conversion"
  OFF
)

mark_as_advanced(ITK_USE_SYSTEM_DOUBLECONVERSION)

if(ITK_USE_SYSTEM_DOUBLECONVERSION)
  set(_double_conversion_compatible 3.1)
  find_package(double-conversion ${_double_conversion_compatible} REQUIRED)
  set(ITKDoubleConversion_LIBRARIES "double-conversion::double-conversion")
  set(ITKDoubleConversion_NO_SRC 1)

  set(
    ITKDoubleConversion_EXPORT_CODE_INSTALL
    "
find_package(double-conversion ${_double_conversion_compatible} REQUIRED)
"
  )
  set(
    ITKDoubleConversion_EXPORT_CODE_BUILD
    "
if(NOT ITK_BINARY_DIR)
  find_package(double-conversion ${_double_conversion_compatible} REQUIRED)
endif()
"
  )
else()
  set(
    ITKDoubleConversion_INCLUDE_DIRS
    ${ITKDoubleConversion_SOURCE_DIR}/src
    ${ITKDoubleConversion_BINARY_DIR}/src/double-conversion
  )
  set(ITKDoubleConversion_LIBRARIES itkdouble-conversion)
endif()

itk_module_impl()
