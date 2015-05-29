set(DOCUMENTATION "This module contains classes for reading and writing
Meshes as opposed to general images.")
itk_module(ITKIOMesh
  ENABLE_SHARED
  DEPENDS
    ITKCommon
    ITKGIFTI
    ITKIOImageBase
  COMPILE_DEPENDS
    ITKMesh
  TEST_DEPENDS
    ITKTestKernel
    ITKQuadEdgeMesh
  DESCRIPTION
    "${DOCUMENTATION}"
)
