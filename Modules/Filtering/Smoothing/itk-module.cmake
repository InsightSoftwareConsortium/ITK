set(DOCUMENTATION "This module includes the most common image smoothing
filters. For example, Gaussian and Median filters. You may also find it
interesting to look at the AnisotropicSmoothing group of filters.")

itk_module(ITK-Smoothing DEPENDS ITK-ImageFilterBase ITK-ImageFunction TEST_DEPENDS ITK-TestKernel DESCRIPTION "${DOCUMENTATION}")
