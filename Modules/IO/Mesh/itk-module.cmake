set(DOCUMENTATION "This module contains classes for reading and writing
Meshes as opposed to general images.")
itk_module(ITKIOMesh
  ENABLE_SHARED
  DEPENDS
    ITKCommon
    ITKMesh
    ITKGIFTI
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
    ITKQuadEdgeMesh
  DESCRIPTION
    "${DOCUMENTATION}"
)
