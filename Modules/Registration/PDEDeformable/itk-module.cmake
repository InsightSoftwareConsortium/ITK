set(DOCUMENTATION "This module contains classes for deformable image
registration based on intensity differences by solving the PDE, optical flow
problem.  This includes Thirion's popular \"demons\" algorithm.")


set(_FFTW_DEPENDS )
if( ITK_USE_FFTWF OR ITK_USE_FFTWD )
  set(_FFTW_DEPENDS "ITKFFT")
endif()

itk_module(ITKPDEDeformableRegistration
  ENABLE_SHARED
  DEPENDS
    ${_FFTW_DEPENDS}
    ITKRegistrationCommon
    ITKFiniteDifference
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
