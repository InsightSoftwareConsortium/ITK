# Public headers expose itk::Mesh types (ITKMesh), so it is a DEPENDS,
# not a COMPILE_DEPENDS, for correct wrapping/link propagation.
itk_module(
  MeshToPolyData
  DEPENDS
    ITKCommon
    ITKMesh
  TEST_DEPENDS
    ITKTestKernel
    ITKIOMeshVTK
  DESCRIPTION
    "Convert an ITK Mesh to a PolyData data structure compatible with vtkPolyData, and back, with supporting filters."
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
