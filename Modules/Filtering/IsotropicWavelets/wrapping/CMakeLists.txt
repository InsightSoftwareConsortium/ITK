itk_wrap_module(IsotropicWavelets)
set(
  WRAPPER_SUBMODULE_ORDER
  itkMonogenicSignalFrequencyImageFilter
  itkVectorInverseFFTImageFilter
)
itk_auto_load_submodules()
itk_end_wrap_module()
