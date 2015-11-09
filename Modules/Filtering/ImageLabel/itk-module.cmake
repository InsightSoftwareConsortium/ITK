set(DOCUMENTATION "This module contains filters intended to operate on images
of labels. Such images are typically the output of segmentation and
classification filters, such as watersheds or statistical classifiers. The
filters in this module compute label statistics and perform operations between
labels.")

itk_module(ITKImageLabel
  COMPILE_DEPENDS
    ITKImageIntensity
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
