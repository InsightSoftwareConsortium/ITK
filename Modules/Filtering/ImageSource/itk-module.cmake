set(DOCUMENTATION "This module groups image sources which generate an image")

itk_module(ITKImageSource
  DEPENDS
    ITKImageFunction
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
