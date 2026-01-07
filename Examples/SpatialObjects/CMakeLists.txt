project(SpatialObjectsExamples)

add_executable(ArrowSpatialObject ArrowSpatialObject.cxx)
target_link_libraries(ArrowSpatialObject PRIVATE ITK::ITKSpatialObjectsModule)

add_executable(BlobSpatialObject BlobSpatialObject.cxx)
target_link_libraries(BlobSpatialObject PRIVATE ITK::ITKSpatialObjectsModule)

add_executable(EllipseSpatialObject EllipseSpatialObject.cxx)
target_link_libraries(EllipseSpatialObject PRIVATE ITK::ITKSpatialObjectsModule)

add_executable(GaussianSpatialObject GaussianSpatialObject.cxx)
target_link_libraries(
  GaussianSpatialObject
  PRIVATE
    ITK::ITKSpatialObjectsModule
)

add_executable(GroupSpatialObject GroupSpatialObject.cxx)
target_link_libraries(GroupSpatialObject PRIVATE ITK::ITKSpatialObjectsModule)

add_executable(ImageSpatialObject ImageSpatialObject.cxx)
target_link_libraries(
  ImageSpatialObject
  PRIVATE
    ITK::ITKCommonModule
    ITK::ITKSpatialObjectsModule
)

add_executable(ImageMaskSpatialObject ImageMaskSpatialObject.cxx)
target_link_libraries(
  ImageMaskSpatialObject
  PRIVATE
    ITK::ITKCommonModule
    ITK::ITKSpatialObjectsModule
    ITK::ITKTransformModule
)

add_executable(LandmarkSpatialObject LandmarkSpatialObject.cxx)
target_link_libraries(
  LandmarkSpatialObject
  PRIVATE
    ITK::ITKSpatialObjectsModule
)

add_executable(LineSpatialObject LineSpatialObject.cxx)
target_link_libraries(LineSpatialObject PRIVATE ITK::ITKSpatialObjectsModule)

add_executable(MeshSpatialObject MeshSpatialObject.cxx)
target_link_libraries(
  MeshSpatialObject
  PRIVATE
    ITK::ITKCommonModule
    ITK::ITKIOSpatialObjectsModule
    ITK::ITKSpatialObjectsModule
    ITK::ITKMeshModule
)

add_executable(ReadWriteSpatialObject ReadWriteSpatialObject.cxx)
target_link_libraries(
  ReadWriteSpatialObject
  PRIVATE
    ITK::ITKCommonModule
    ITK::ITKIOSpatialObjectsModule
    ITK::ITKSpatialObjectsModule
    ITK::ITKMeshModule
)

add_executable(SceneSpatialObject SceneSpatialObject.cxx)
target_link_libraries(SceneSpatialObject PRIVATE ITK::ITKSpatialObjectsModule)

add_executable(SpatialObjectHierarchy SpatialObjectHierarchy.cxx)
target_link_libraries(
  SpatialObjectHierarchy
  PRIVATE
    ITK::ITKSpatialObjectsModule
)

add_executable(SpatialObjectTransforms SpatialObjectTransforms.cxx)
target_link_libraries(
  SpatialObjectTransforms
  PRIVATE
    ITK::ITKSpatialObjectsModule
)

add_executable(
  SpatialObjectToImageStatisticsCalculator
  SpatialObjectToImageStatisticsCalculator.cxx
)
target_link_libraries(
  SpatialObjectToImageStatisticsCalculator
  PRIVATE
    ITK::ITKCommonModule
    ITK::ITKSpatialObjectsModule
    ITK::ITKTestKernelModule
)

add_executable(SurfaceSpatialObject SurfaceSpatialObject.cxx)
target_link_libraries(SurfaceSpatialObject PRIVATE ITK::ITKSpatialObjectsModule)

add_executable(TubeSpatialObject TubeSpatialObject.cxx)
target_link_libraries(TubeSpatialObject PRIVATE ITK::ITKSpatialObjectsModule)

add_executable(VesselTubeSpatialObject VesselTubeSpatialObject.cxx)
target_link_libraries(
  VesselTubeSpatialObject
  PRIVATE
    ITK::ITKSpatialObjectsModule
)

add_executable(DTITubeSpatialObject DTITubeSpatialObject.cxx)
target_link_libraries(DTITubeSpatialObject PRIVATE ITK::ITKSpatialObjectsModule)

add_executable(
  BoundingBoxFromImageMaskSpatialObject
  BoundingBoxFromImageMaskSpatialObject.cxx
)
target_link_libraries(
  BoundingBoxFromImageMaskSpatialObject
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKSpatialObjectsModule
)
