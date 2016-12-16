set(DOCUMENTATION "The classes in this module are intended to describe the
concept of a linear path in N-Dimensional space. They can be used to represent
contours in 2D images, or curves in 3D space. These classes also include the
concept of iterators, polylines, and smooth approximations to paths.")

itk_module(ITKPath
  PRIVATE_DEPENDS
    ITKCommon
  COMPILE_DEPENDS
    ITKImageFunction
  TEST_DEPENDS
    ITKTestKernel
    ITKImageIntensity
    ITKSmoothing
    ITKImageFeature
  DESCRIPTION
    "${DOCUMENTATION}"
)

# Extra test dependencies on ITKImageIntensity, ITKSmoothing, and ITKImageFeature are introduced by itkOrthogonalSwath2DPathFilterTest.
