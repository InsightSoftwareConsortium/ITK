itk_module(
  IOMeshMZ3
  DEPENDS
    ITKCommon
    ITKIOMeshBase
  COMPILE_DEPENDS
    ITKMesh
  PRIVATE_DEPENDS
    ITKZLIB
  TEST_DEPENDS
    ITKTestKernel
    ITKMetaIO
  FACTORY_NAMES
    MeshIO::MZ3
  DESCRIPTION "Read and write MZ3 triangle mesh files."
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
