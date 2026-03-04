project(ITKFFTW)
set(ITKFFTW_THIRD_PARTY 1)

# ITK_USE_FFTWD -- use double precision FFTW
option(ITK_USE_FFTWD "Use double precision FFTW" ON)
mark_as_advanced(ITK_USE_FFTWD)

# ITK_USE_FFTWF -- use single precision FFTW
option(ITK_USE_FFTWF "Use single precision FFTW" ON)
mark_as_advanced(ITK_USE_FFTWF)

if(NOT ITK_USE_FFTWD AND NOT ITK_USE_FFTWF)
  message(
    FATAL_ERROR
    "At least one of ITK_USE_FFTWD or ITK_USE_FFTWF must be ON"
  )
endif()

set(msg "ATTENTION: You have enabled the use of FFTW.")
set(msg "${msg} This library is distributed under a GPL license.")
set(msg "${msg} By enabling this option, the ITK libraries binary")
set(msg "${msg} that is built will be covered by a GPL license")
set(
  msg
  "${msg} and so will any executable that is linked against these libraries."
)
message(STATUS "${msg}")

set(ITKFFTW_LIBRARIES "")

if(ITK_USE_SYSTEM_FFTW)
  # When using system FFTW, find_package is called in itk-module-init.cmake
  # The FindFFTW3.cmake module sets up FFTW3:: namespaced targets
  set(ITKFFTW_INCLUDE_DIRS ${FFTW3_INCLUDE_DIRS})

  # Create ITK:: namespace aliases for the FFTW3:: targets
  if(ITK_USE_FFTWF)
    if(TARGET FFTW3::fftw3f)
      list(APPEND ITKFFTW_LIBRARIES FFTW3::fftw3f)
    endif()

    if(TARGET FFTW3::fftw3f_threads)
      list(APPEND ITKFFTW_LIBRARIES FFTW3::fftw3f_threads)
    endif()
  endif()

  if(ITK_USE_FFTWD)
    if(TARGET FFTW3::fftw3)
      list(APPEND ITKFFTW_LIBRARIES FFTW3::fftw3)
    endif()

    if(TARGET FFTW3::fftw3_threads)
      list(APPEND ITKFFTW_LIBRARIES FFTW3::fftw3_threads)
    endif()
  endif()

  set(
    ITKFFTW_EXPORT_CODE_INSTALL
    "
set(ITK_USE_FFTWD \"${ITK_USE_FFTWD}\")
set(ITK_USE_FFTWF \"${ITK_USE_FFTWF}\")
list(APPEND CMAKE_MODULE_PATH \"\${ITK_CMAKE_DIR}\")
find_package(FFTW3 REQUIRED)
"
  )
  set(
    ITKFFTW_EXPORT_CODE_BUILD
    "
if(NOT ITK_BINARY_DIR)
  set(ITK_USE_FFTWD \"${ITK_USE_FFTWD}\")
  set(ITK_USE_FFTWF \"${ITK_USE_FFTWF}\")
  list(APPEND CMAKE_MODULE_PATH \"${CMAKE_CURRENT_LIST_DIR}/CMake\")
  find_package(FFTW3 REQUIRED)
  list(REMOVE_AT CMAKE_MODULE_PATH 0)
endif()
"
  )

  set(ITKFFTW_NO_SRC 1)

  install(
    FILES
      CMake/FindFFTW3.cmake
    DESTINATION ${ITK_INSTALL_PACKAGE_DIR}
    COMPONENT Development
  )
else()
  # Build FFTW using FetchContent
  include(${ITKFFTW_SOURCE_DIR}/CMake/itkFetchFFTW.cmake)

  set(ITKFFTW_INCLUDE_DIRS ${ITKFFTW_BINARY_DIR}/src)

  # Create IMPORTED INTERFACE targets with FFTW3:: namespace for export compatibility
  # IMPORTED targets (unlike ALIAS targets) can be exported in ITKTargets.cmake
  if(ITK_USE_FFTWF)
    list(
      APPEND
      ITKFFTW_LIBRARIES
      FFTW3::fftw3f
      FFTW3::fftw3f_threads
    )
    add_library(FFTW3::fftw3f INTERFACE IMPORTED GLOBAL)
    target_link_libraries(FFTW3::fftw3f INTERFACE fftw3f)
    add_library(FFTW3::fftw3f_threads INTERFACE IMPORTED GLOBAL)
    target_link_libraries(FFTW3::fftw3f_threads INTERFACE fftw3f_threads)
  endif()
  if(ITK_USE_FFTWD)
    list(
      APPEND
      ITKFFTW_LIBRARIES
      FFTW3::fftw3
      FFTW3::fftw3_threads
    )
    add_library(FFTW3::fftw3 INTERFACE IMPORTED GLOBAL)
    target_link_libraries(FFTW3::fftw3 INTERFACE fftw3)
    add_library(FFTW3::fftw3_threads INTERFACE IMPORTED GLOBAL)
    target_link_libraries(FFTW3::fftw3_threads INTERFACE fftw3_threads)
  endif()

  set(
    ITKFFTW_EXPORT_CODE_INSTALL
    "
set(ITK_USE_FFTWD \"${ITK_USE_FFTWD}\")
set(ITK_USE_FFTWF \"${ITK_USE_FFTWF}\")
set(ITK_USE_SYSTEM_FFTW OFF)

"
  )
  set(
    ITKFFTW_EXPORT_CODE_BUILD
    "
if(NOT ITK_BINARY_DIR)
  set(ITK_USE_FFTWD \"${ITK_USE_FFTWD}\")
  set(ITK_USE_FFTWF \"${ITK_USE_FFTWF}\")
  set(ITK_USE_SYSTEM_FFTW OFF)

endif()
"
  )
endif()

itk_module_impl()

configure_file(src/itk_fftw.h.in src/itk_fftw.h)
install(
  FILES
    ${ITKFFTW_BINARY_DIR}/src/itk_fftw.h
  DESTINATION ${ITKFFTW_INSTALL_INCLUDE_DIR}
  COMPONENT Development
)

message(STATUS "ITKFFTW_LIBRARIES: ${ITKFFTW_LIBRARIES}")
