set(DOCUMENTATION "This module contains multiple implementations of distance
map filters. They include the implementations of Danielsson and Maurer, as
well as other distance concepts such as Hausdorff and Chamfer distances.")

itk_module(ITK-DistanceMap
  DEPENDS
    ITK-ImageIntensity
    ITK-MathematicalMorphology
    ITK-NarrowBand
    ITK-ImageLabel
  TEST_DEPENDS
    ITK-TestKernel
  DESCRIPTION
    "${DOCUMENTATION}")
