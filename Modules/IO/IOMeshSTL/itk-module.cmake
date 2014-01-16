set(
  DOCUMENTATION
  "This module contains classes for reading and writing
QuadEdgeMeshes using the STL file format."
)
itk_module(
  IOSTL
  ENABLE_SHARED
  DEPENDS
    ITKCommon
    ITKIOMesh
  TEST_DEPENDS
    ITKTestKernel
    ITKQuadEdgeMesh
  DESCRIPTION "${DOCUMENTATION}"
)
