set(DOCUMENTATION "This module groups image sources which generate an image")

itk_module(ITKImageSources
  DEPENDS
    ITKImageFunction
  TEST_DEPENDS
    ITKTestKernel
    ITKImageIntensity
  DESCRIPTION
    "${DOCUMENTATION}"
)

# the ITKIntensity is only needed for the
# VectorIndexSelectionCastImageFilter for the PhysicalPointImageSourceTest
