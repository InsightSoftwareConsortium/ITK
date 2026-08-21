project(ITKNrrdIO)
set(ITKNrrdIO_THIRD_PARTY 1)

set(
  ITKNrrdIO_INCLUDE_DIRS
  ${ITKNrrdIO_BINARY_DIR}/src/NrrdIO
  ${ITKNrrdIO_SOURCE_DIR}/src/NrrdIO
)
set(ITKNrrdIO_LIBRARIES ITKNrrdIO)
set(
  ITK_LIBRARY_PROPERTIES
  WINDOWS_EXPORT_ALL_SYMBOLS
  FALSE
)

if(UNIX)
  # ITKNrrdIO's exported link interface names Threads::Threads.
  set(
    ITKNrrdIO_EXPORT_CODE_INSTALL
    "
find_package(Threads REQUIRED)
"
  )
  set(
    ITKNrrdIO_EXPORT_CODE_BUILD
    "
if(NOT ITK_BINARY_DIR)
  find_package(Threads REQUIRED)
endif()
"
  )
endif()

itk_module_impl()
