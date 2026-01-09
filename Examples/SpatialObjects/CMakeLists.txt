project(SpatialObjectsExamples)

add_executable(ArrowSpatialObject ArrowSpatialObject.cxx)
target_link_libraries(ArrowSpatialObject PRIVATE ITKSpatialObjectsModule)

add_executable(BlobSpatialObject BlobSpatialObject.cxx)
target_link_libraries(BlobSpatialObject PRIVATE ITKSpatialObjectsModule)

add_executable(EllipseSpatialObject EllipseSpatialObject.cxx)
target_link_libraries(EllipseSpatialObject PRIVATE ITKSpatialObjectsModule)

add_executable(GaussianSpatialObject GaussianSpatialObject.cxx)
target_link_libraries(GaussianSpatialObject PRIVATE ITKSpatialObjectsModule)

add_executable(GroupSpatialObject GroupSpatialObject.cxx)
target_link_libraries(GroupSpatialObject PRIVATE ITKSpatialObjectsModule)

add_executable(ImageSpatialObject ImageSpatialObject.cxx)
target_link_libraries(
  ImageSpatialObject
  PRIVATE
    ITKCommonModule
    ITKSpatialObjectsModule
)

add_executable(ImageMaskSpatialObject ImageMaskSpatialObject.cxx)
target_link_libraries(
  ImageMaskSpatialObject
  PRIVATE
    ITKCommonModule
    ITKSpatialObjectsModule
    ITKTransformModule
)

add_executable(LandmarkSpatialObject LandmarkSpatialObject.cxx)
target_link_libraries(LandmarkSpatialObject PRIVATE ITKSpatialObjectsModule)

add_executable(LineSpatialObject LineSpatialObject.cxx)
target_link_libraries(LineSpatialObject PRIVATE ITKSpatialObjectsModule)

add_executable(MeshSpatialObject MeshSpatialObject.cxx)
target_link_libraries(
  MeshSpatialObject
  PRIVATE
    ITKCommonModule
    ITKIOSpatialObjectsModule
    ITKSpatialObjectsModule
    ITKMeshModule
)

add_executable(ReadWriteSpatialObject ReadWriteSpatialObject.cxx)
target_link_libraries(
  ReadWriteSpatialObject
  PRIVATE
    ITKCommonModule
    ITKIOSpatialObjectsModule
    ITKSpatialObjectsModule
    ITKMeshModule
)

add_executable(SceneSpatialObject SceneSpatialObject.cxx)
target_link_libraries(SceneSpatialObject PRIVATE ITKSpatialObjectsModule)

add_executable(SpatialObjectHierarchy SpatialObjectHierarchy.cxx)
target_link_libraries(SpatialObjectHierarchy PRIVATE ITKSpatialObjectsModule)

add_executable(SpatialObjectTransforms SpatialObjectTransforms.cxx)
target_link_libraries(SpatialObjectTransforms PRIVATE ITKSpatialObjectsModule)

add_executable(
  SpatialObjectToImageStatisticsCalculator
  SpatialObjectToImageStatisticsCalculator.cxx
)
target_link_libraries(
  SpatialObjectToImageStatisticsCalculator
  PRIVATE
    ITKCommonModule
    ITKSpatialObjectsModule
    ITKTestKernelModule
)

add_executable(SurfaceSpatialObject SurfaceSpatialObject.cxx)
target_link_libraries(SurfaceSpatialObject PRIVATE ITKSpatialObjectsModule)

add_executable(TubeSpatialObject TubeSpatialObject.cxx)
target_link_libraries(TubeSpatialObject PRIVATE ITKSpatialObjectsModule)

add_executable(VesselTubeSpatialObject VesselTubeSpatialObject.cxx)
target_link_libraries(VesselTubeSpatialObject PRIVATE ITKSpatialObjectsModule)

add_executable(DTITubeSpatialObject DTITubeSpatialObject.cxx)
target_link_libraries(DTITubeSpatialObject PRIVATE ITKSpatialObjectsModule)

add_executable(
  BoundingBoxFromImageMaskSpatialObject
  BoundingBoxFromImageMaskSpatialObject.cxx
)
target_link_libraries(
  BoundingBoxFromImageMaskSpatialObject
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKSpatialObjectsModule
)
