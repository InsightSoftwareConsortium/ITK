set(DOCUMENTATION "This module contains classes to perform deformable image
registration with a structural mechanics, finite element method (FEM)
deformation model.  Local loads (forces) are determined by the image-to-image
metric, and the solid body is assumed to have uniform density and elasticity.")

itk_module(ITKFEMRegistration
  DEPENDS
    ITKFEM
    ITKImageStatistics
    ITKPDEDeformableRegistration
    ITKImageFeature
    ITKSpatialObjects
  TEST_DEPENDS
    ITKTestKernel
    ITKImageSources
  DESCRIPTION
    "${DOCUMENTATION}"
)
