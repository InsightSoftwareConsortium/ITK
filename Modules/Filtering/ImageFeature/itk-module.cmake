itk_module(ITK-ImageFeature DEPENDS ITK-ImageIntensity ITK-Smoothing ITK-ImageGradient ITK-SpatialObjects TEST_DEPENDS ITK-TestKernel ITK-Thresholding)
#extra test dependency on ITK-Thresholding is introduced by itkHoughTransform2DCirclesImageTest.
