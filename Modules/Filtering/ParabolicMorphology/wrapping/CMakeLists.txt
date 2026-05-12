itk_wrap_module(ParabolicMorphology)

set(
  WRAPPER_SUBMODULE_ORDER
  itkMorphologicalDistanceTransformImageFilter
  itkMorphologicalSignedDistanceTransformImageFilter
  itkParabolicCloseImageFilter
  itkParabolicDilateImageFilter
  itkParabolicErodeImageFilter
  itkParabolicOpenImageFilter
)

itk_auto_load_submodules()
itk_end_wrap_module()
