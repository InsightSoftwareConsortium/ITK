set(
  DOCUMENTATION
  "A fast approximation to the bilateral filter for ITK, edge-preserving
smoothing using a low-resolution intensity-spatial bilateral grid.
See the Insight Journal article: https://doi.org/10.54294/noo5vc."
)

# itk_module() defines the module dependencies in FastBilateral
# FastBilateral depends on ITKCommon
# The testing module in FastBilateral depends on ITKTestKernel
# and ITKMetaIO(besides FastBilateral and ITKCore)
# By convention those modules outside of ITK are not prefixed with
# ITK.

# define the dependencies of the include module and the tests
itk_module(
  FastBilateral
  DEPENDS
    ITKCommon
    ITKSmoothing
  COMPILE_DEPENDS
    ITKImageSources
  TEST_DEPENDS
    ITKTestKernel
    ITKMetaIO
  DESCRIPTION "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
