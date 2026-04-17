project(ITKDoubleConversion)
set(ITKDoubleConversion_THIRD_PARTY 1)

option(
  ITK_USE_SYSTEM_DOUBLECONVERSION
  "Use an outside build of double-conversion"
  OFF
)

mark_as_advanced(ITK_USE_SYSTEM_DOUBLECONVERSION)

if(ITK_USE_SYSTEM_DOUBLECONVERSION)
  find_package(double-conversion 3.1.6 REQUIRED)
  # Use the imported target directly; CMake resolves it per-platform
  # (Linux: .so, Windows: .lib import library). Do not use
  # get_target_property(... LOCATION), which yields the .dll on Windows
  # and produces LNK1107 ("invalid or corrupt file") at link time.
  # INTERFACE_INCLUDE_DIRECTORIES propagates through the target, so no
  # explicit ITKDoubleConversion_INCLUDE_DIRS assignment is required.
  set(ITKDoubleConversion_LIBRARIES double-conversion::double-conversion)

  # Ensure downstream consumers of the installed ITKConfig have the
  # double-conversion:: imported target before ITKTargets.cmake is
  # processed (which references it in INTERFACE_LINK_LIBRARIES).
  set(
    ITKDoubleConversion_EXPORT_CODE_INSTALL
    "find_package(double-conversion 3.1.6 REQUIRED)"
  )
  set(
    ITKDoubleConversion_EXPORT_CODE_BUILD
    "
    if(NOT ITK_BINARY_DIR)
      find_package(double-conversion 3.1.6 REQUIRED)
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
