set(DOCUMENTATION "This module contains classes for reading and writing
Meshes in the OFF file format.")
itk_module(ITKIOMeshOFF
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
    MeshIO::OFF
  DESCRIPTION
    "${DOCUMENTATION}"
)
