set(DOCUMENTATION "This module contains filters related to the use of spatial
functions. The spatial function classes implement the concept of defining
regions of space, typically as masks. You will find here the filter that can
take a spatial function, and rasterize it into the grid of an image.")

itk_module(ITK-SpatialFunction
  DEPENDS
    ITK-ImageFunction
    ITK-ImageFilterBase
  TEST_DEPENDS
    ITK-TestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
