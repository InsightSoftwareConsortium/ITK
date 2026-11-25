itk_wrap_module(AdaptiveDenoising)

set(
  WRAPPER_SUBMODULE_ORDER
  itkVarianceImageFilter
  itkNonLocalPatchBasedImageFilter
  itkAdaptiveNonLocalMeansDenoisingImageFilter
)

itk_auto_load_submodules()
itk_end_wrap_module()
