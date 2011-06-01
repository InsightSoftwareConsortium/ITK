set(DOCUMENTATION "This module contains classes that compute statistics on
images, or use those statistics to perform image operations. In particular you
will find here: how to calculate image moments, minimum maximum values,
projections, principal components analysis (PCA) for shape modeling,
computation of label statistics and image accumulation.")

itk_module(ITK-ImageStatistics DEPENDS ITK-ImageFilterBase ITK-Statistics  ITK-SpatialObjects ITK-ImageCompose TEST_DEPENDS ITK-ImageIntensity ITK-TestKernel ITK-Thresholding ITK-ImageLabel DESCRIPTION "${DOCUMENTATION}")
#extra test dependency on Thresholding is introduced by itkNormalizedCorrelationImageFilterTest;
#extra test dependency on ImageLabel is introduced by itkBinaryProjectionImageFilterTest.
