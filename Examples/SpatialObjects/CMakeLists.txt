#
#  Examples on the use of SpatialObjects.
#
PROJECT(SpatialObjectsExamples)
INCLUDE_REGULAR_EXPRESSION("^.*$")

ADD_EXECUTABLE(ArrowSpatialObject ArrowSpatialObject.cxx)
TARGET_LINK_LIBRARIES(ArrowSpatialObject  ITKIO )

ADD_EXECUTABLE(BlobSpatialObject BlobSpatialObject.cxx)
TARGET_LINK_LIBRARIES(BlobSpatialObject  ITKIO )

ADD_EXECUTABLE(CylinderSpatialObject CylinderSpatialObject.cxx)
TARGET_LINK_LIBRARIES(CylinderSpatialObject  ITKIO ITKSpatialObject)

ADD_EXECUTABLE(EllipseSpatialObject EllipseSpatialObject.cxx)
TARGET_LINK_LIBRARIES(EllipseSpatialObject  ITKIO )

ADD_EXECUTABLE(GaussianSpatialObject GaussianSpatialObject.cxx)
TARGET_LINK_LIBRARIES(GaussianSpatialObject  ITKIO )

ADD_EXECUTABLE(GroupSpatialObject GroupSpatialObject.cxx)
TARGET_LINK_LIBRARIES(GroupSpatialObject  ITKIO )

ADD_EXECUTABLE(ImageSpatialObject ImageSpatialObject.cxx)
TARGET_LINK_LIBRARIES(ImageSpatialObject  ITKIO )

ADD_EXECUTABLE(ImageMaskSpatialObject ImageMaskSpatialObject.cxx)
TARGET_LINK_LIBRARIES(ImageMaskSpatialObject  ITKIO )

ADD_EXECUTABLE(LandmarkSpatialObject LandmarkSpatialObject.cxx)
TARGET_LINK_LIBRARIES(LandmarkSpatialObject  ITKIO )

ADD_EXECUTABLE(LineSpatialObject LineSpatialObject.cxx)
TARGET_LINK_LIBRARIES(LineSpatialObject  ITKIO )

ADD_EXECUTABLE(MeshSpatialObject MeshSpatialObject.cxx)
TARGET_LINK_LIBRARIES(MeshSpatialObject  ITKIO ITKAlgorithms ITKSpatialObject)

ADD_EXECUTABLE(ReadWriteSpatialObject ReadWriteSpatialObject.cxx)
TARGET_LINK_LIBRARIES(ReadWriteSpatialObject  ITKIO ITKAlgorithms ITKSpatialObject)

ADD_EXECUTABLE(SceneSpatialObject SceneSpatialObject.cxx)
TARGET_LINK_LIBRARIES(SceneSpatialObject  ITKIO )

ADD_EXECUTABLE(SpatialObjectHierarchy SpatialObjectHierarchy.cxx)
TARGET_LINK_LIBRARIES(SpatialObjectHierarchy  ITKIO )

ADD_EXECUTABLE(SpatialObjectTransforms SpatialObjectTransforms.cxx)
TARGET_LINK_LIBRARIES(SpatialObjectTransforms  ITKIO )

ADD_EXECUTABLE(SpatialObjectTreeContainer SpatialObjectTreeContainer.cxx)
TARGET_LINK_LIBRARIES(SpatialObjectTreeContainer  ITKIO )

ADD_EXECUTABLE(SpatialObjectToImageStatisticsCalculator SpatialObjectToImageStatisticsCalculator.cxx)
TARGET_LINK_LIBRARIES(SpatialObjectToImageStatisticsCalculator  ITKIO )

ADD_EXECUTABLE(SurfaceSpatialObject SurfaceSpatialObject.cxx)
TARGET_LINK_LIBRARIES(SurfaceSpatialObject  ITKIO )

ADD_EXECUTABLE(TubeSpatialObject TubeSpatialObject.cxx)
TARGET_LINK_LIBRARIES(TubeSpatialObject  ITKIO )

ADD_EXECUTABLE(VesselTubeSpatialObject VesselTubeSpatialObject.cxx)
TARGET_LINK_LIBRARIES(VesselTubeSpatialObject  ITKIO )

ADD_EXECUTABLE(DTITubeSpatialObject DTITubeSpatialObject.cxx)
TARGET_LINK_LIBRARIES(DTITubeSpatialObject  ITKIO )

ADD_EXECUTABLE(BoundingBoxFromImageMaskSpatialObject BoundingBoxFromImageMaskSpatialObject.cxx)
TARGET_LINK_LIBRARIES(BoundingBoxFromImageMaskSpatialObject  ITKIO )
