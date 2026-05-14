# the top-level README is used for describing this module, just
# re-used it for documentation here
# itk_module() defines the module dependencies in GrowCut
# GrowCut depends on ITKCommon
# The testing module in GrowCut depends on ITKTestKernel
# and ITKMetaIO(besides GrowCut and ITKCore)
# By convention those modules outside of ITK are not prefixed with
# ITK.

# define the dependencies of the include module and the tests
itk_module(
  GrowCut
  DEPENDS
    ITKCommon
    ITKImageGrid
  TEST_DEPENDS
    ITKTestKernel
    ITKSmoothing
    ITKMetaIO
  DESCRIPTION
    "Fast GrowCut segmentation: a Dijkstra-based region-growing
algorithm that propagates user-provided seed labels through an N-D image
using a Fibonacci-heap priority queue. Supports optional masking and
adaptive (incremental) re-segmentation when seeds change. Ingested from
the InsightSoftwareConsortium/ITKGrowCut remote module."
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
