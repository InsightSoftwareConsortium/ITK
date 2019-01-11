set(DOCUMENTATION "This module contains classes for reading and writing
Meshes in the Gifti file format.")
itk_module(ITKIOMeshGifti
  ENABLE_SHARED
  DEPENDS
    ITKCommon
    ITKIOMeshBase
  COMPILE_DEPENDS
    ITKMesh
  PRIVATE_DEPENDS
    ITKGIFTI
  TEST_DEPENDS
    ITKTestKernel
    ITKQuadEdgeMesh
  FACTORY_NAMES
    MeshIO::Gifti
  DESCRIPTION
    "${DOCUMENTATION}"
)
