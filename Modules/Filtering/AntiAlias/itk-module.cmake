set(DOCUMENTATION "This module contains classes for applying antialiasing
filters to images. This is typically done to smooth the outcome of
segmentations, previous to extracting contours or surfaces from them. Note that
you can also achieve effects similar to anti-aliasing by using a binary
segmentation as input to a level set filter.")

itk_module(ITK-AntiAlias DEPENDS ITK-LevelSets ITK-CurvatureFlow TEST_DEPENDS ITK-TestKernel DESCRIPTION "${DOCUMENTATION}")
