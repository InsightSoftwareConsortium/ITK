set(
  DOCUMENTATION
  "This module provides the StructuralSimilarityImageFilter, which
computes the Structural Similarity Index (SSIM) between two images. SSIM
is a perceptual image-quality metric that compares local luminance,
contrast, and structure within a sliding Gaussian window. See Wang et
al., \"Image quality assessment: From error visibility to structural
similarity\" (IEEE TIP, 2004)."
)

itk_module(
  StructuralSimilarity
  DEPENDS
    ITKImageFilterBase
    ITKImageIntensity
    ITKSmoothing
  TEST_DEPENDS
    ITKTestKernel
    ITKGoogleTest
  DESCRIPTION "${DOCUMENTATION}"
)
