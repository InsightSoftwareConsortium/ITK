itk_wrap_module(ITKVideoCore)

set(WRAPPER_SUBMODULE_ORDER
  itkTemporalDataObject
  itkVideoStream
)

itk_auto_load_submodules()

itk_end_wrap_module()
