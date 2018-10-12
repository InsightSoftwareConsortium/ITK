itk_wrap_module(LabelErodeDilate)

set(WRAPPER_SUBMODULE_ORDER
   itkLabelSetDilateImageFilter
   itkLabelSetErodeImageFilter
   itkLabelSetMorphBaseImageFilter)

itk_auto_load_submodules()
itk_end_wrap_module()
