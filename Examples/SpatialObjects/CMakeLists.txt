project(SpatialObjectsExamples)

add_executable(ArrowSpatialObject ArrowSpatialObject.cxx)
target_link_libraries(ArrowSpatialObject  ${ITK_LIBRARIES})

add_executable(BlobSpatialObject BlobSpatialObject.cxx)
target_link_libraries(BlobSpatialObject  ${ITK_LIBRARIES})

add_executable(CylinderSpatialObject CylinderSpatialObject.cxx)
target_link_libraries(CylinderSpatialObject  ${ITK_LIBRARIES})

add_executable(EllipseSpatialObject EllipseSpatialObject.cxx)
target_link_libraries(EllipseSpatialObject  ${ITK_LIBRARIES})

add_executable(GaussianSpatialObject GaussianSpatialObject.cxx)
target_link_libraries(GaussianSpatialObject  ${ITK_LIBRARIES})

add_executable(GroupSpatialObject GroupSpatialObject.cxx)
target_link_libraries(GroupSpatialObject  ${ITK_LIBRARIES})

add_executable(ImageSpatialObject ImageSpatialObject.cxx)
target_link_libraries(ImageSpatialObject  ${ITK_LIBRARIES})

add_executable(ImageMaskSpatialObject ImageMaskSpatialObject.cxx)
target_link_libraries(ImageMaskSpatialObject  ${ITK_LIBRARIES})

add_executable(LandmarkSpatialObject LandmarkSpatialObject.cxx)
target_link_libraries(LandmarkSpatialObject  ${ITK_LIBRARIES})

add_executable(LineSpatialObject LineSpatialObject.cxx)
target_link_libraries(LineSpatialObject  ${ITK_LIBRARIES})

add_executable(MeshSpatialObject MeshSpatialObject.cxx)
target_link_libraries(MeshSpatialObject  ${ITK_LIBRARIES})

add_executable(ReadWriteSpatialObject ReadWriteSpatialObject.cxx)
target_link_libraries(ReadWriteSpatialObject  ${ITK_LIBRARIES})

add_executable(SceneSpatialObject SceneSpatialObject.cxx)
target_link_libraries(SceneSpatialObject  ${ITK_LIBRARIES})

add_executable(SpatialObjectHierarchy SpatialObjectHierarchy.cxx)
target_link_libraries(SpatialObjectHierarchy  ${ITK_LIBRARIES})

add_executable(SpatialObjectTransforms SpatialObjectTransforms.cxx)
target_link_libraries(SpatialObjectTransforms  ${ITK_LIBRARIES})

add_executable(SpatialObjectTreeContainer SpatialObjectTreeContainer.cxx)
target_link_libraries(SpatialObjectTreeContainer  ${ITK_LIBRARIES})

add_executable(SpatialObjectToImageStatisticsCalculator SpatialObjectToImageStatisticsCalculator.cxx)
target_link_libraries(SpatialObjectToImageStatisticsCalculator  ${ITK_LIBRARIES})

add_executable(SurfaceSpatialObject SurfaceSpatialObject.cxx)
target_link_libraries(SurfaceSpatialObject  ${ITK_LIBRARIES})

add_executable(TubeSpatialObject TubeSpatialObject.cxx)
target_link_libraries(TubeSpatialObject  ${ITK_LIBRARIES})

add_executable(VesselTubeSpatialObject VesselTubeSpatialObject.cxx)
target_link_libraries(VesselTubeSpatialObject  ${ITK_LIBRARIES})

add_executable(DTITubeSpatialObject DTITubeSpatialObject.cxx)
target_link_libraries(DTITubeSpatialObject  ${ITK_LIBRARIES})

add_executable(BoundingBoxFromImageMaskSpatialObject BoundingBoxFromImageMaskSpatialObject.cxx)
target_link_libraries(BoundingBoxFromImageMaskSpatialObject  ${ITK_LIBRARIES})
