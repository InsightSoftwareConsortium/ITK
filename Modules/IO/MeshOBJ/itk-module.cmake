set(DOCUMENTATION "This module contains classes for reading and writing
Meshes in the OBJ file format.")
itk_module(ITKIOMeshOBJ
  ENABLE_SHARED
  DEPENDS
    ITKCommon
    ITKIOMeshBase
  COMPILE_DEPENDS
    ITKMesh
  TEST_DEPENDS
    ITKTestKernel
    ITKQuadEdgeMesh
  FACTORY_NAMES
    MeshIO::OBJ
  DESCRIPTION
    "${DOCUMENTATION}"
)
