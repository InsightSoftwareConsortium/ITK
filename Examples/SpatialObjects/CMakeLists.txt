#
#  Examples on the use of SpatialObjects.
#
project(SpatialObjectsExamples)
include_regular_expression("^.*$")

add_executable(ArrowSpatialObject ArrowSpatialObject.cxx)
target_link_libraries(ArrowSpatialObject  ITKIO )

add_executable(BlobSpatialObject BlobSpatialObject.cxx)
target_link_libraries(BlobSpatialObject  ITKIO )

add_executable(CylinderSpatialObject CylinderSpatialObject.cxx)
target_link_libraries(CylinderSpatialObject  ITKIO ITKSpatialObject)

add_executable(EllipseSpatialObject EllipseSpatialObject.cxx)
target_link_libraries(EllipseSpatialObject  ITKIO )

add_executable(GaussianSpatialObject GaussianSpatialObject.cxx)
target_link_libraries(GaussianSpatialObject  ITKIO )

add_executable(GroupSpatialObject GroupSpatialObject.cxx)
target_link_libraries(GroupSpatialObject  ITKIO )

add_executable(ImageSpatialObject ImageSpatialObject.cxx)
target_link_libraries(ImageSpatialObject  ITKIO )

add_executable(ImageMaskSpatialObject ImageMaskSpatialObject.cxx)
target_link_libraries(ImageMaskSpatialObject  ITKIO )

add_executable(LandmarkSpatialObject LandmarkSpatialObject.cxx)
target_link_libraries(LandmarkSpatialObject  ITKIO )

add_executable(LineSpatialObject LineSpatialObject.cxx)
target_link_libraries(LineSpatialObject  ITKIO )

add_executable(MeshSpatialObject MeshSpatialObject.cxx)
target_link_libraries(MeshSpatialObject  ITKIO ITKAlgorithms ITKSpatialObject)

add_executable(ReadWriteSpatialObject ReadWriteSpatialObject.cxx)
target_link_libraries(ReadWriteSpatialObject  ITKIO ITKAlgorithms ITKSpatialObject)

add_executable(SceneSpatialObject SceneSpatialObject.cxx)
target_link_libraries(SceneSpatialObject  ITKIO )

add_executable(SpatialObjectHierarchy SpatialObjectHierarchy.cxx)
target_link_libraries(SpatialObjectHierarchy  ITKIO )

add_executable(SpatialObjectTransforms SpatialObjectTransforms.cxx)
target_link_libraries(SpatialObjectTransforms  ITKIO )

add_executable(SpatialObjectTreeContainer SpatialObjectTreeContainer.cxx)
target_link_libraries(SpatialObjectTreeContainer  ITKIO )

add_executable(SpatialObjectToImageStatisticsCalculator SpatialObjectToImageStatisticsCalculator.cxx)
target_link_libraries(SpatialObjectToImageStatisticsCalculator  ITKIO )

add_executable(SurfaceSpatialObject SurfaceSpatialObject.cxx)
target_link_libraries(SurfaceSpatialObject  ITKIO )

add_executable(TubeSpatialObject TubeSpatialObject.cxx)
target_link_libraries(TubeSpatialObject  ITKIO )

add_executable(VesselTubeSpatialObject VesselTubeSpatialObject.cxx)
target_link_libraries(VesselTubeSpatialObject  ITKIO )

add_executable(DTITubeSpatialObject DTITubeSpatialObject.cxx)
target_link_libraries(DTITubeSpatialObject  ITKIO )

add_executable(BoundingBoxFromImageMaskSpatialObject BoundingBoxFromImageMaskSpatialObject.cxx)
target_link_libraries(BoundingBoxFromImageMaskSpatialObject  ITKIO )
