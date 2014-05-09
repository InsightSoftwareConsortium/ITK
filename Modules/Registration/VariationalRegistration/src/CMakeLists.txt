include_directories("${VariationalRegistration_SOURCE_DIR}/src")

# Test if ITK is built with FFTW. FFTWD is recommended and required for the
# tests with elastic regularisation to pass.
if(ITK_USE_FFTWD OR ITK_USE_FFTWF)
  if(ITK_USE_FFTWF)
    MESSAGE(WARNING " ITK is built with ITK_USE_FFTWF but ITK_USE_FFTWD is"
                    " recommended for use with Module_VariationalRegistration."
                    " Tests with elastic regularisation will fail.")
  endif(ITK_USE_FFTWF)

  add_executable(VariationalRegistration VariationalRegistrationMain.cxx )
  target_link_libraries(VariationalRegistration ${VariationalRegistration_LIBRARIES})

  add_executable(VariationalRegistration2D VariationalRegistrationMain.cxx)
  target_link_libraries(VariationalRegistration2D ${VariationalRegistration_LIBRARIES})
  set_target_properties(VariationalRegistration2D PROPERTIES COMPILE_FLAGS -DUSE_2D_IMPL )
else()
  MESSAGE(ERROR " Module_VariationalRegistration requires FFTW."
                " Please set ITK_USE_FFTWD to ON.")
endif()
