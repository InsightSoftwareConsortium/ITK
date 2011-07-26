set(DOCUMENTATION "This module contains classes for deformable image
registration based on intensity differences by solving the PDE, optical flow
problem.  This includes Thirion's popular \"demons\" algorithm.")

itk_module(ITKPDEDeformableRegistration
  DEPENDS
    ITKRegistrationCommon
    ITKFiniteDifference
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
