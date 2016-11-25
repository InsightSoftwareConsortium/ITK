set(DOCUMENTATION "This module contains classes to perturb
itk::Mesh and itk::QuadEdgeMesh objects with Gaussian noise.
This module is introduced in the Insight Journal article
http://hdl.handle.net/10380/3567")

itk_module(DVMeshNoise
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
