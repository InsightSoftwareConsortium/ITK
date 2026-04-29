set(
  DOCUMENTATION
  "Apply additive noise (Gaussian, Poisson, etc.) to ITK mesh point coordinates.
Useful for testing mesh-based registration and surface-reconstruction pipelines
with controlled perturbation."
)

itk_module(
  MeshNoise
  DEPENDS
    ITKCommon
    ITKMesh
    ITKQuadEdgeMesh
    ITKStatistics
  TEST_DEPENDS
    ITKTestKernel
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "${DOCUMENTATION}"
)
