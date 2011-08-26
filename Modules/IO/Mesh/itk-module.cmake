set(DOCUMENTATION "This module contains classes for reading and writing
Meshes as opposed to general images.")
itk_module(ITKIOMesh
  DEPENDS
    ITKCommon
    ITKMesh
    ITKGIFTI
    ITKIOBase
  TEST_DEPENDS
    ITKTestKernel
    ITKQuadEdgeMesh
  DESCRIPTION
    "${DOCUMENTATION}"
)
