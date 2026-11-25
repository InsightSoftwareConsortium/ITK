set(
  DOCUMENTATION
  "Fast total variation methods for image denoising, deconvolution, and
related applications. The itkProxTVImageFilter wraps the proxTV library
(Modules/ThirdParty/proxTV) for 2D and 3D images."
)

itk_module(
  TotalVariation
  DEPENDS
    ITKCommon
    ITKImageFilterBase
    ITKEigen3
    ITKproxTV
  COMPILE_DEPENDS
    ITKImageSources
  TEST_DEPENDS
    ITKTestKernel
    ITKMetaIO
  DESCRIPTION "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
