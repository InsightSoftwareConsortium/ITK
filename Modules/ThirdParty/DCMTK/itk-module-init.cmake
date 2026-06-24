## Only present and build DCMTK options if ITKIODCMTK is requested

option(ITK_USE_SYSTEM_DCMTK "Use an outside build of DCMTK." OFF)

if(ITK_USE_SYSTEM_DCMTK)
  # Use local FindDCMTK.cmake.
  list(INSERT CMAKE_MODULE_PATH 0 "${CMAKE_CURRENT_LIST_DIR}/CMake")
  find_package(DCMTK REQUIRED NO_MODULE)
else()
  # Change default from OFF to ON for portability.
  option(
    DCMTK_ENABLE_BUILTIN_OFICONV_DATA
    "Embed oficonv data files into oficonv library"
    ON
  )
  # ICU creates problems on macOS and Windows, so it is disabled by default;
  # DCMTK's in-scope configuration selects builtin oficonv otherwise.
  if(NOT DEFINED DCMTK_USE_ICU)
    option(DCMTK_USE_ICU "Use system ICU for DCMTK charset conversion" OFF)
  endif()
  if(DCMTK_USE_ICU)
    if(ITK_USE_SYSTEM_ICU)
      find_package(
        ICU
        REQUIRED
        COMPONENTS
          uc
          data
      )
      set(ITKDCMTK_ICU_LIBRARIES ${ICU_LIBRARIES})
    endif()
  endif()
endif()
