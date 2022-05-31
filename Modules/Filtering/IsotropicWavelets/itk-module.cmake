set(
  DOCUMENTATION
  "An ITK-based implementation of Wavelet transforms using isotropic wavelets.

A more detailed description can be found in the Insight Journal article::

Cerdan, P.H. \"ITK Wavelet Module\".
  https://hdl.handle.net/10380/3558
  September, 2016.
"
)

set(VTKGlueModule "")
if(ITKVtkGlue_ENABLED)
  set(VTKGlueModule ITKVtkGlue)
endif()

itk_module(
  IsotropicWavelets
  DEPENDS
    ITKImageFunction
    ITKFFT
    ITKRegistrationCommon
    ITKConvolution
    ITKImageFrequency
  TEST_DEPENDS
    ITKTestKernel
    ${VTKGlueModule}
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
  DESCRIPTION "${DOCUMENTATION}"
)
