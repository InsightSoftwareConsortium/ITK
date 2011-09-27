set(DOCUMENTATION "This module contains implementations of generalized versions
of the Fast Marching filter. These implementations cover the use of Fast
Marching in both itk::Images and itk::QuadEdgeMeshes.")

itk_module(ITKFastMarching
  DEPENDS
    ITKCommon
    ITKQuadEdgeMesh
    ITKConnectedComponents
  TEST_DEPENDS
    ITKIONIFTI
    ITKImageLabel
    ITKTestKernel
    ITKIOMesh
  DESCRIPTION
    "${DOCUMENTATION}"
)
