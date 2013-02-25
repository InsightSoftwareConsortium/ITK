set(DOCUMENTATION "This module contains filters that perform label voting, i.e.
they count the number of pixels with a given label within a neighborhood and
determine the output pixel based on the count.  The operations on label images
are similar to filtering on scalar images.  See also \\\\ref
ITKBinaryMathematicalMorphology, \\\\ref ITKConnectedComponents, and \\\\ref
ITKLabelMap.")

itk_module(ITKLabelVoting
  DEPENDS
    ITKThresholding
  TEST_DEPENDS
    ITKTestKernel
    ITKMetaIO
    ITKDoubleConversion
  DESCRIPTION
    "${DOCUMENTATION}"
)
