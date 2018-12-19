set(DOCUMENTATION "This module contains classes for reading and writing
Meshes in the FreeSurfer file format.")
itk_module(ITKIOMeshFreeSurfer
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
    MeshIO::FreeSurferAscii
    MeshIO::FreeSurferBinary
  DESCRIPTION
    "${DOCUMENTATION}"
)
