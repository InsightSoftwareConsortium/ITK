itk_module(
  IOMeshSTL
  ENABLE_SHARED
  DEPENDS
    ITKCommon
    ITKIOMeshBase
  TEST_DEPENDS
    ITKTestKernel
    ITKQuadEdgeMesh
  FACTORY_NAMES
    MeshIO::STL
  DESCRIPTION "Read and write STL triangle mesh files."
  EXCLUDE_FROM_DEFAULT
)
