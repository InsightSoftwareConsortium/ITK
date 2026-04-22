set(
  DOCUMENTATION
  "This module provides coherence-enhancing (CED) and edge-enhancing
(EED) anisotropic diffusion filters built on the
Lattice-Basis-Reduction (LBR) stencil scheme by Jean-Marie Mirebeau.
See the Doxygen on \\\\ref AnisotropicDiffusionLBRImageFilter and
\\\\ref CoherenceEnhancingDiffusionImageFilter for the algorithm and
citations, and the module README for in-tree vs archived-upstream
scope."
)

itk_module(
  AnisotropicDiffusionLBR
  DEPENDS
    ITKCommon
    ITKIOImageBase
    ITKImageGradient
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
  # Header only library, no ENABLE_SHARED
)
