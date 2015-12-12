set(DOCUMENTATION "This module contains classes for reading and writing
Meshes as opposed to general images.")
itk_module(ITKIOMesh
  ENABLE_SHARED
  DEPENDS
    ITKCommon
    ITKIOImageBase
    ITKQuadEdgeMesh
    ITKMesh
    ITKVoronoi
  PRIVATE_DEPENDS
    ITKDoubleConversion
    ITKGIFTI
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
