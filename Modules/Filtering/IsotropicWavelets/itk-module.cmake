set(
  DOCUMENTATION
  "An ITK-based implementation of Wavelet transforms using isotropic wavelets.

A more detailed description can be found in the Insight Journal article::

Cerdan, P.H. \"ITK Wavelet Module\".
  http://hdl.handle.net/10380/3558
  September, 2016.
"
)

if(ITKVtkGlue_ENABLED)
  itk_module(
    IsotropicWavelets
    DEPENDS
      ITKImageFunction
      ITKFFT
      ITKRegistrationCommon
      ITKConvolution
    TEST_DEPENDS
      ITKTestKernel
      ITKVtkGlue
    EXCLUDE_FROM_DEFAULT
    DESCRIPTION "${DOCUMENTATION}"
  )
else()
  itk_module(
    IsotropicWavelets
    DEPENDS
      ITKImageFunction
      ITKFFT
      ITKRegistrationCommon
      ITKConvolution
    TEST_DEPENDS
      ITKTestKernel
    EXCLUDE_FROM_DEFAULT
    DESCRIPTION "${DOCUMENTATION}"
  )
endif()
