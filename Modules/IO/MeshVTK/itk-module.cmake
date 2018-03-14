set(DOCUMENTATION "This module contains classes for reading and writing
Meshes in the VTK file format.")
itk_module(ITKIOMeshVTK
  ENABLE_SHARED
  DEPENDS
    ITKCommon
    ITKIOMeshBase
  PRIVATE_DEPENDS
    ITKDoubleConversion
  COMPILE_DEPENDS
    ITKMesh
  TEST_DEPENDS
    ITKTestKernel
    ITKQuadEdgeMesh
  FACTORY_NAMES
    MeshIO::VTKPolyData
  DESCRIPTION
    "${DOCUMENTATION}"
)
