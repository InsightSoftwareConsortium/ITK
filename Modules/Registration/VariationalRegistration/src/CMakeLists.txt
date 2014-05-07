include_directories("${VariationalRegistration_SOURCE_DIR}/src")

if( ITK_USE_FFTWD OR ITK_USE_FFTWF )

  add_executable(itkVariationalRegistration VariationalRegistrationMain.cxx )
  target_link_libraries(itkVariationalRegistration ${VariationalRegistration_LIBRARIES})

  add_executable(itkVariationalRegistration2D VariationalRegistrationMain.cxx)
  target_link_libraries(itkVariationalRegistration2D ${VariationalRegistration_LIBRARIES})
  set_target_properties(itkVariationalRegistration2D PROPERTIES COMPILE_FLAGS -DUSE_2D_IMPL )

else()
  MESSAGE(ERROR " Module VariationalRegistration requires FFTW.")
endif()
