itk_wrap_module(ITKVideoCore)

set(WRAPPER_SUBMODULE_ORDER
  itkTemporalRegion
  itkRingBuffer
  itkTemporalDataObject
  itkVideoStream
)

itk_auto_load_and_end_wrap_submodules()
