set(
  DOCUMENTATION
  "FixedPointInverseDisplacementField takes a Displacement field as input and
computes the Displacement field that is its inverse. If the input Displacement
field was mapping coordinates from a space A into a space B, the output of
this filter will map coordinates from the space B into the space A.

To compute the inverse of the given Displacement field, the fixed point algorithm by
Mingli Chen, Weiguo Lu, Quan Chen, Knneth J. Ruchala and Gusavo H. Olivera
described in the paper
\"A simple fixed-point approach to invert a Displacement field\",
Medical Physics, vol. 35, issue 1, p. 81,
is applied.

author Marcel Lüthi, Computer Science Department, University of Basel
"
)

itk_module(
  FixedPointInverseDisplacementField
  DEPENDS
    ITKCommon
    ITKImageGrid
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "${DOCUMENTATION}"
)
