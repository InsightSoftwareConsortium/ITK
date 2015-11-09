set(DOCUMENTATION "This module contains classes related to segmentation of
biological cells.  It has classes to represent cells' shape, color, and growth
state.  It also has classes to represent a cell genome, whose expression is
modeled by differential equations.")

itk_module(ITKBioCell
  ENABLE_SHARED
  DEPENDS
    ITKMesh
  PRIVATE_DEPENDS
    ITKCommon
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
