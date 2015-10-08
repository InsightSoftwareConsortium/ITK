set(DOCUMENTATION "This module contains multiple implementations of distance
map filters. They include the implementations of Danielsson and Maurer, as
well as other distance concepts such as Hausdorff and Chamfer distances.")

itk_module(ITKDistanceMap
  COMPILE_DEPENDS
    ITKBinaryMathematicalMorphology
    ITKImageLabel
    ITKNarrowBand
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}")
