# Test if ITK is built with FFTW. FFTWD is recommended and required for the
# tests with elastic or curvature regularisation to pass.
if(ITK_USE_FFTWF AND NOT ITK_USE_FFTWD)
  message(
    WARNING
    " ITK is built with ITK_USE_FFTWF but ITK_USE_FFTWD is"
    " recommended for use with Module_VariationalRegistration."
    " Tests with elastic or curvature regularisation will fail."
  )
endif()

set(VariationalRegistration_SRC VariationalRegistrationMain.cxx)

if(WIN32 AND MSVC)
  set(
    VariationalRegistration_SRC
    VariationalRegistrationMain.cxx
    win32_compatibility/getopt.c
  )
endif()

set(
  _VariationalRegistration_example_link_libraries
  ITKCommon
  ITKIOImageBase
  ITKImageFilterBase
  ITKSmoothing
  ITKFFT
  ITKFiniteDifference
  ITKDisplacementField
  ITKRegistrationCommon
  ITKMathematicalMorphology
  ITKBinaryMathematicalMorphology
  ITKIOGDCM
  ITKIOMeta
  ITKIOJPEG
  ITKIOPNG
  ITKIOTIFF
  ITKIOBMP
  ITKIOVTK
  ITKIONRRD
  ITKIOGIPL
  ITKIONIFTI
)

add_executable(VariationalRegistration ${VariationalRegistration_SRC})
target_link_libraries(
  VariationalRegistration
  ${_VariationalRegistration_example_link_libraries}
)
target_include_directories(
  VariationalRegistration
  PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/../include
)
if(WIN32 AND MSVC)
  target_include_directories(
    VariationalRegistration
    PRIVATE
      ${CMAKE_CURRENT_LIST_DIR}/win32_compatibility
  )
endif()

add_executable(VariationalRegistration2D ${VariationalRegistration_SRC})
target_link_libraries(
  VariationalRegistration2D
  ${_VariationalRegistration_example_link_libraries}
)
target_include_directories(
  VariationalRegistration2D
  PRIVATE
    ${CMAKE_CURRENT_LIST_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/../include
)
if(WIN32 AND MSVC)
  target_include_directories(
    VariationalRegistration2D
    PRIVATE
      ${CMAKE_CURRENT_LIST_DIR}/win32_compatibility
  )
endif()
target_compile_definitions(VariationalRegistration2D PRIVATE USE_2D_IMPL)
