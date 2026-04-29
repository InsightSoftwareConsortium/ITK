set(
  DOCUMENTATION
  "Morphological erosion and dilation for label images. Label collisions are
handled consistently and operations execute in approximately constant time
with respect to structuring-element size. Only circular, spherical, and
hyperspherical structuring elements are supported."
)

itk_module(
  LabelErodeDilate
  DEPENDS
    ITKIOImageBase
  TEST_DEPENDS
    ITKImageGrid
    ITKTestKernel
    ITKSmoothing
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "${DOCUMENTATION}"
)
