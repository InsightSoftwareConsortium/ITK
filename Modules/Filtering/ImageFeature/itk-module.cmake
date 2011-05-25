set(DOCUMENTATION "This module contains classes that compute image features. In
particular you will find here: Canny edge detection, Sobel, ZeroCrossings,
Hough transform for lines and circles, Hessian filters, Vesselness, and
Fractional anisotropy for tensor images.")

itk_module(ITK-ImageFeature DEPENDS ITK-ImageIntensity ITK-Smoothing ITK-ImageGradient ITK-SpatialObjects TEST_DEPENDS ITK-TestKernel ITK-Thresholding DESCRIPTION "${DOCUMENTATION}")
#extra test dependency on ITK-Thresholding is introduced by itkHoughTransform2DCirclesImageTest.
