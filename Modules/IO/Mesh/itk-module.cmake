set(DOCUMENTATION "This module contains classes for reading and writing
Meshes as opposed to general images.

This module is present for backwards compatibility and should not be used -
for support for a specific mesh file format, depend on the specific
module, instead.")

itk_module(ITKIOMesh
  DEPENDS
    ITKCommon
    ITKIOMeshBase
    ITKIOMeshVTK
    ITKIOMeshBYU
    ITKIOMeshFreeSurfer
    ITKIOMeshGifti
    ITKIOMeshOBJ
    ITKIOMeshOFF
  TEST_DEPENDS
    ITKTestKernel
    ITKQuadEdgeMesh
    ITKIOMeshVTK
    ITKIOMeshFreeSurfer
    ITKIOMeshGifti
    ITKIOMeshOBJ
    ITKIOMeshOFF
  FACTORY_NAMES
    MeshIO::BYU
    MeshIO::FreeSurferAscii
    MeshIO::FreeSurferBinary
    MeshIO::Gifti
    MeshIO::OBJ
    MeshIO::OFF
  DESCRIPTION
    "${DOCUMENTATION}"
)
