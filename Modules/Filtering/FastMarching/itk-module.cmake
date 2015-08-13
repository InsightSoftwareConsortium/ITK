set(DOCUMENTATION "This module contains implementations of generalized versions
of the Fast Marching filter. These implementations cover the use of Fast
Marching in both itk::Images and itk::QuadEdgeMeshes.")

itk_module(ITKFastMarching
  COMPILE_DEPENDS
    ITKQuadEdgeMesh
    ITKConnectedComponents
  TEST_DEPENDS
    ITKTestKernel
    ITKImageLabel
    ITKIOMesh
  DESCRIPTION
    "${DOCUMENTATION}"
)
