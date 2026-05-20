itk_wrap_module(PhaseSymmetry)

set(
  WRAPPER_SUBMODULE_ORDER
  itkSinusoidSpatialFunction
  itkSinusoidImageSource
  itkSteerableFilterFreqImageSource
  itkButterworthFilterFreqImageSource
  itkLogGaborFreqImageSource
  itkPhaseSymmetryImageFilter
)

itk_auto_load_submodules()
itk_end_wrap_module()
