itk_wrap_module(ITKThresholding)

set(WRAPPER_SUBMODULE_ORDER
  itkHistogramThresholdCalculator
  itkHistogramThresholdImageFilter
)
itk_auto_load_submodules()
itk_end_wrap_module()
