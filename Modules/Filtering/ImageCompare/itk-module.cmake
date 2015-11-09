set(DOCUMENTATION "This modules provides classes for comparing images. They are
typically used for comparing two images before and after registration, to
perform regression testing or to compare the outcome of segmentations. In
particular you will find here: checkerboard filter, absolute value differences,
similarity index and STAPLE.")

itk_module(ITKImageCompare
  COMPILE_DEPENDS
    ITKImageFilterBase
    ITKImageIntensity
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
