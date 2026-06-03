set(
  DOCUMENTATION
  "Approximate an image gradient from a BSpline fit to the intensities, and
related BSpline scattered-data gradient filters."
)

itk_module(
  BSplineGradient
  DEPENDS
    ITKCommon
    ITKMesh
  COMPILE_DEPENDS
    ITKImageGrid
    MeshToPolyData
  TEST_DEPENDS
    ITKTestKernel
    ITKMetaIO
    ITKIOMeshVTK
    ITKImageGradient
  DESCRIPTION "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
)
