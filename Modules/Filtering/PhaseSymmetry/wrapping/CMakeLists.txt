itk_wrap_module(PhaseSymmetryFilter)

set(
  WRAPPER_SUBMODULE_ORDER
  itkButterworthFilterFreqImageSource
  itkLogGaborFreqImageSource
  itkPhaseSymmetryImageFilter
  itkSinusoidImageSource
  itkSinusoidSpatialFunction
  itkSteerableFilterFreqImageSource
)

itk_auto_load_submodules()
itk_end_wrap_module()
