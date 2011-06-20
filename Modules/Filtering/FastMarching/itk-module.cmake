set(DOCUMENTATION "This module contains implementations of generalized versions
of the Fast Marching filter. These implementations cover the use of Fast
Marching in both itk::Images and itk::QuadEdgeMeshes.")

itk_module(ITK-FastMarching
  DEPENDS
    ITK-Common
    ITK-QuadEdgeMesh
    ITK-ConnectedComponents
  TEST_DEPENDS
    ITK-IO-NIFTI
    ITK-ImageLabel
    ITK-TestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
