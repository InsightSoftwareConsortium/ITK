set(DOCUMENTATION "This module contains filters that combine several images
into a single output image. For example, take several input scalar images and
package them into an output image of multiple components.")

itk_module(ITKImageCompose
  COMPILE_DEPENDS
    ITKImageFilterBase
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
