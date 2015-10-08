set(DOCUMENTATION "This module contains classes that perform operations on the
itk::QuadEdgeMesh. This is a data structure designed to represent a
2-dimensional manifold embedded in a 3-dimensional space. That's typically the
case of an iso-surface extracted from a 3D image data set. The itk::QuadEdgeMesh
ensures the proper topological orientation of the surface. Filters in this
module include: smoothing, decimation, curvature computation (mean, gaussian,
min, max, principal), and normal computation, among others.")

itk_module(ITKQuadEdgeMeshFiltering
  DEPENDS
    ITKMesh
  COMPILE_DEPENDS
    ITKQuadEdgeMesh
  TEST_DEPENDS
    ITKTestKernel
    ITKIOMesh
  DESCRIPTION
    "${DOCUMENTATION}"
)
