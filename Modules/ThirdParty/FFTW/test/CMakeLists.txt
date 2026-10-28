itk_module_test()

if(NOT ITK_USE_MKL AND NOT ITK_USE_CUFFTW)
  set(ITKFFTWGTests itkFFTWModuleGTest.cxx)
  creategoogletestdriver(ITKFFTW "${ITKFFTW-Test_LIBRARIES}" "${ITKFFTWGTests}")
endif()
