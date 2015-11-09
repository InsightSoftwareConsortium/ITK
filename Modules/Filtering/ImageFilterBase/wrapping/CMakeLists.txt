itk_wrap_module(ITKImageFilterBase)

set(WRAPPER_SUBMODULE_ORDER
  itkRecursiveSeparableImageFilter
  itkFlatStructuringElement
  itkKernelImageFilter
  itkMovingHistogramImageFilterBase
)
itk_auto_load_submodules()
itk_end_wrap_module()
