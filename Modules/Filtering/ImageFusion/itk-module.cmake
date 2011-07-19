set(DOCUMENTATION "This module contains classes made to visually
enhance the images, mostly by adding them artificial colors")

itk_module(ITKImageFusion
  DEPENDS
    ITKBinaryMathematicalMorphology
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
