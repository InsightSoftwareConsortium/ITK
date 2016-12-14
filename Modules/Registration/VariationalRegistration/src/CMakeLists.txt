include_directories("${VariationalRegistration_SOURCE_DIR}/src")

# Test if ITK is built with FFTW. FFTWD is recommended and required for the
# tests with elastic or curvature regularisation to pass.
if(ITK_USE_FFTWF)
  message(
    WARNING
    " ITK is built with ITK_USE_FFTWF but ITK_USE_FFTWD is"
    " recommended for use with Module_VariationalRegistration."
    " Tests with elastic or curvature regularisation will fail."
  )
endif(ITK_USE_FFTWF)

set(VariationalRegistration_SRC VariationalRegistrationMain.cxx)

if(WIN32) # Check if we are on Windows
  if(MSVC) # Check if we are using the Visual Studio compiler
    include_directories(
      "${VariationalRegistration_SOURCE_DIR}/src/win32_compatibility"
    )
    set(
      VariationalRegistration_SRC
      VariationalRegistrationMain.cxx
      win32_compatibility/getopt.c
    )
  endif()
endif()

add_executable(VariationalRegistration ${VariationalRegistration_SRC})
target_link_libraries(
  VariationalRegistration
  ${VariationalRegistration_LIBRARIES}
)

add_executable(VariationalRegistration2D ${VariationalRegistration_SRC})
target_link_libraries(
  VariationalRegistration2D
  ${VariationalRegistration_LIBRARIES}
)
set_target_properties(
  VariationalRegistration2D
  PROPERTIES
    COMPILE_FLAGS
      -DUSE_2D_IMPL
)
