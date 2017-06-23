itk_wrap_module(TextureFeatures)

set(
  WRAPPER_SUBMODULE_ORDER
  itkCoocurenceTextureFeaturesImageFilter
  itkRunLengthTextureFeaturesImageFilter
)

itk_auto_load_submodules()
itk_end_wrap_module()
