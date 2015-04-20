project(ITKDoubleConversion)
set(ITKDoubleConversion_THIRD_PARTY 1)

option(ITK_USE_SYSTEM_DOUBLECONVERSION
  "Use an outside build of double-conversion" OFF)

mark_as_advanced(ITK_USE_SYSTEM_DOUBLECONVERSION)

if(ITK_USE_SYSTEM_DOUBLECONVERSION)
  find_package(double-conversion NO_MODULE)
  set(ITKDoubleConversion_SYSTEM_INCLUDE_DIRS
    ${double-conversion_INCLUDE_DIRS})
  set(ITKDoubleConversion_LIBRARIES
    "${double-conversion_LIBRARIES}")
else()
  set(ITKDoubleConversion_INCLUDE_DIRS
    ${ITKDoubleConversion_SOURCE_DIR}/src/double-conversion
    ${ITKDoubleConversion_BINARY_DIR}/src/double-conversion)
  set(ITKDoubleConversion_LIBRARIES itkdouble-conversion)
endif()

itk_module_impl()
