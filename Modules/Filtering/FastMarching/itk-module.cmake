set(
  DOCUMENTATION
  "This module contains implementations of generalized versions
of the Fast Marching filter. These implementations cover the use of Fast
Marching in both itk::Image and itk::QuadEdgeMesh objects."
)

itk_module(
  ITKFastMarching
  ENABLE_SHARED
  DEPENDS
    ITKCommon
    ITKMesh
    ITKQuadEdgeMesh
  COMPILE_DEPENDS
    ITKConnectedComponents
  TEST_DEPENDS
    ITKTestKernel
    ITKImageLabel
    ITKIOMesh
  DESCRIPTION "${DOCUMENTATION}"
)
