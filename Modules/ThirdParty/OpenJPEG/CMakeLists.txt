project(ITKOpenJPEG)
set(ITKOpenJPEG_THIRD_PARTY 1)

option(
  ITK_USE_SYSTEM_OPENJPEG
  "Use an outside build of OpenJPEG."
  ${ITK_USE_SYSTEM_LIBRARIES}
)
mark_as_advanced(ITK_USE_SYSTEM_OPENJPEG)

if(ITK_USE_SYSTEM_OPENJPEG)
  # OpenJPEG does not install an OpenJPEGConfigVersion.cmake file, so CMake
  # cannot enforce a version requirement via find_package.
  find_package(OpenJPEG REQUIRED)

  # We expect this to be a target with the proper include dirs and link libraries already set, but if not, we will fall back to the old variables:
  set(ITKOpenJPEG_LIBRARIES ${OPENJPEG_LIBRARIES})

  set(ITKOpenJPEG_INCLUDE_DIRS ${ITKOpenJPEG_BINARY_DIR}/src)

  set(ITKOpenJPEG_NO_SRC 1)

  set(
    ITKOpenJPEG_EXPORT_CODE_INSTALL
    "
  find_package(OpenJPEG REQUIRED)
"
  )
  set(
    ITKOpenJPEG_EXPORT_CODE_BUILD
    "
  if(NOT ITK_BINARY_DIR)
    find_package(OpenJPEG REQUIRED)
  endif()
"
  )
else()
  set(
    ITKOpenJPEG_INCLUDE_DIRS
    ${ITKOpenJPEG_BINARY_DIR}/src
    ${ITKOpenJPEG_SOURCE_DIR}/src
    ${ITKOpenJPEG_BINARY_DIR}/src/openjpeg/src/lib/openjp2
    ${ITKOpenJPEG_SOURCE_DIR}/src/openjpeg/src/lib/openjp2
  )
  set(ITKOpenJPEG_LIBRARIES itkopenjpeg)
  set(
    ITK_LIBRARY_PROPERTIES
    WINDOWS_EXPORT_ALL_SYMBOLS
    FALSE
  )
endif()

configure_file(src/itk_openjpeg.h.in src/itk_openjpeg.h)

itk_module_impl()

install(
  FILES
    ${ITKOpenJPEG_BINARY_DIR}/src/itk_openjpeg.h
  DESTINATION ${ITKOpenJPEG_INSTALL_INCLUDE_DIR}
  COMPONENT Development
)
