set(DOCUMENTATION 
"This module contains classes for mathematical morphology 
on label images using circular/spherical/hyperspherical 
structuring elements. It uses parabolic structuring 
functions to do these operations efficiently
and handles label collisions" )
itk_module( LabelErodeDilate
  DEPENDS
  ITKIOImageBase
  TEST_DEPENDS
  ITKImageGrid
  ITKTestKernel
  ITKSmoothing
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION
  "${DOCUMENTATION}"
)
