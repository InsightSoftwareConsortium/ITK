set(DOCUMENTATION "This module contains filters that perform pixel-wise
operations on the intensities of images. In particular you will find here
filters that compute trigonometric operations on pixel values, intensity
rescaling, exponentials, conversions between complex and reals, and filters
that combine multiple images into images of multiple components, as well as
filters that compute single scalar images from images of multiple components.")

itk_module(ITK-ImageIntensity DEPENDS ITK-ImageFilterBase ITK-ImageAdaptors ITK-ImageStatistics ITK-ImageGrid ITK-Path TEST_DEPENDS ITK-TestKernel ITK-SpatialObjects ITK-DistanceMap DESCRIPTION "${DOCUMENTATION}")
#Extra dependency of ITK-SpatialObjects is introduced by itkPolylineMaskImageFilterTest.
#Extra dependency of ITK-SpatialObjects is introduced by itkModulusImageFilterTest.
