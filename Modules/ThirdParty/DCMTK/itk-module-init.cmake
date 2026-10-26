## Only present and build DCMTK options if ITKIODCMTK is requested

option(ITK_USE_SYSTEM_DCMTK "Use an outside build of DCMTK." OFF)

if(ITK_USE_SYSTEM_DCMTK)
  # Use local FindDCMTK.cmake.
  list(INSERT CMAKE_MODULE_PATH 0 "${CMAKE_CURRENT_LIST_DIR}/CMake")
  # 3.7.0 matches the vendored DCMTK and guarantees DCMTK::-namespaced targets.
  find_package(DCMTK 3.7.0 REQUIRED NO_MODULE)
else()
  # Change default from OFF to ON for portability.
  option(
    DCMTK_ENABLE_BUILTIN_OFICONV_DATA
    "Embed oficonv data files into oficonv library"
    ON
  )
endif()
