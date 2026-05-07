# the top-level README is used for describing this module, just
# re-used it for documentation here
# itk_module() defines the module dependencies in SubdivisionQuadEdgeMeshFilter
# The testing module in SubdivisionQuadEdgeMeshFilter depends on ITKTestKernel
# By convention those modules outside of ITK are not prefixed with
# ITK

# define the dependencies of the include module and the tests
itk_module(
  SubdivisionQuadEdgeMeshFilter
  DEPENDS
    ITKQuadEdgeMesh
    ITKQuadEdgeMeshFiltering
  TEST_DEPENDS
    ITKTestKernel
    ITKIOMesh
  DESCRIPTION "Module ingested from upstream."
  EXCLUDE_FROM_DEFAULT
  # Not used for header only libraries ENABLE_SHARED
)
