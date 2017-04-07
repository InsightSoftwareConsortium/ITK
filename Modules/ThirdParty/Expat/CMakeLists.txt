project(ITKExpat)
set(ITKExpat_THIRD_PARTY 1)

option(ITK_USE_SYSTEM_EXPAT "Use system-installed expat" ${ITK_USE_SYSTEM_LIBRARIES})
mark_as_advanced(ITK_USE_SYSTEM_EXPAT)

if(ITK_USE_SYSTEM_EXPAT)
  find_package(EXPAT REQUIRED)
  set(ITKExpat_INCLUDE_DIRS ${ITKExpat_BINARY_DIR}/src )
  set(ITKExpat_SYSTEM_INCLUDE_DIRS "${EXPAT_INCLUDE_DIR}")
  set(ITKExpat_LIBRARIES "${EXPAT_LIBRARY}")
  set(ITKExpat_NO_SRC 1)
else()
  set(ITKExpat_INCLUDE_DIRS
  ${ITKExpat_BINARY_DIR}/src/expat
  ${ITKExpat_SOURCE_DIR}/src/expat
  )
set(ITKExpat_LIBRARIES ITKEXPAT)
set(ITK_LIBRARY_PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS FALSE)
endif()

itk_module_impl()
