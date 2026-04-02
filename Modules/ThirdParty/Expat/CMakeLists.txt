project(ITKExpat)
set(ITKExpat_THIRD_PARTY 1)

option(
  ITK_USE_SYSTEM_EXPAT
  "Use system-installed expat"
  ${ITK_USE_SYSTEM_LIBRARIES}
)
mark_as_advanced(ITK_USE_SYSTEM_EXPAT)

if(ITK_USE_SYSTEM_EXPAT)
  find_package(EXPAT REQUIRED)
  set(
    ITKExpat_INCLUDE_DIRS
    ${ITKExpat_BINARY_DIR} # <- for itk_expat.h
  )
  set(ITKExpat_SYSTEM_INCLUDE_DIRS "${EXPAT_INCLUDE_DIR}")
  set(ITKExpat_LIBRARIES "${EXPAT_LIBRARY}")
  set(ITKExpat_NO_SRC 1)
else()
  set(
    ITKExpat_INCLUDE_DIRS
    ${ITKExpat_BINARY_DIR} # <- for itk_expat.h
    ${ITKExpat_BINARY_DIR}/src/itkexpat # <- for expat_config.h itk_expat_mangle.h
    ${ITKExpat_SOURCE_DIR}/src # <- for itkexpat/lib/expat.h in build tree
  )
  set(ITKExpat_LIBRARIES ITKEXPAT)
  set(
    ITK_LIBRARY_PROPERTIES
    WINDOWS_EXPORT_ALL_SYMBOLS
    FALSE
  )
  set(
    EXPAT_GE
    ON
    CACHE BOOL
    "Define to make general entity parsing functionality available"
    FORCE
  )
  set(
    XML_GE
    1
    CACHE STRING
    "Define to make general entity parsing functionality available"
    FORCE
  )
endif()

configure_file(itk_expat.h.in ${ITKExpat_BINARY_DIR}/itk_expat.h)
# Remove stale itk_expat.h from pre-bfc8e07d build trees that generated it
# into the src/ subdirectory.  A leftover copy can shadow the current file
# and confuse incremental builds.
file(REMOVE ${ITKExpat_BINARY_DIR}/src/itk_expat.h)
if(NOT ITK_USE_SYSTEM_EXPAT AND NOT ITK_FUTURE_LEGACY_REMOVE)
  # Configure a file with the name <expat.h> as was the default in ITKv5 and earlier
  # so that "#include <expat.h>" finds the itk mangled symbols during linkage stage.
  # This is only needed to preserve behaviors of ITKv5 and earlier. ITKv6 and greater
  # should #include "itk_expat.h" to get the correctly mangled symbols.
  configure_file(itkv5_expat.h.in ${ITKExpat_BINARY_DIR}/expat.h)
else()
  # Remove file if configuration changes occur
  file(REMOVE ${ITKExpat_BINARY_DIR}/expat.h)
endif()

itk_module_impl()
