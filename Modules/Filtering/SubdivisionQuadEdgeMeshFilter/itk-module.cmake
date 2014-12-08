set(
  DOCUMENTATION
  "Triangle Mesh Subdivision
http://www.insight-journal.org/browse/publication/831
"
)

itk_module(
  MinimalPathExtraction
  DEPENDS
    ITKQuadEdgeMesh
  TEST_DEPENDS
    ITKTestKernel
    ITKIOMesh
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "Module ingested from upstream."
)
