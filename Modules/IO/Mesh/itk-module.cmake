set(DOCUMENTATION "This module contains classes for reading and writing
Meshes as opposed to general images.")
itk_module(ITKIOMesh
  ENABLE_SHARED
  PRIVATE_DEPENDS
    ITKIOImageBase
    ITKDoubleConversion
    ITKGIFTI
  COMPILE_DEPENDS
    ITKMesh
  TEST_DEPENDS
    ITKTestKernel
    ITKQuadEdgeMesh
  DESCRIPTION
    "${DOCUMENTATION}"
)
