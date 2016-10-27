set(DOCUMENTATION "This module contains classes to perturb
itk::Mesh and itk::QuadEdgeMesh classes with Gaussian noise.")

itk_module(ITKMeshNoise
  DEPENDS
    ITKCommon
    ITKMesh
    ITKQuadEdgeMesh
    ITKStatistics
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
