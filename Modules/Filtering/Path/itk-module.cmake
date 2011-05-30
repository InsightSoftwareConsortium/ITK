  set(DOCUMENTATION "The classes in this module are intendeded to describe the
concept of a linear path in N-Dimensional space. They can be used to represent
contours in 2D images, or curves in 3D space. These classes also include the
concept of iterators, polylines, and smooth approximations to paths.")

itk_module(ITK-Path DEPENDS ITK-ImageFilterBase ITK-ImageFunction TEST_DEPENDS ITK-TestKernel ITK-ImageIntensity ITK-Smoothing ITK-ImageFeature  DESCRIPTION "${DOCUMENTATION}")
#extra test dependecy (ITK-ImageIntensity, ITK-Smoothing and ITK-ImageFeature) is introduced by itkOrthogonalSwath2DPathFilterTest.
