#
#  Examples on the use of SpatialObjects.
#
PROJECT(SpatialObjectsExamples)
INCLUDE_REGULAR_EXPRESSION("^.*$")

ADD_EXECUTABLE(BlobSpatialObject BlobSpatialObject.cxx)
TARGET_LINK_LIBRARIES(BlobSpatialObject ITKCommon ITKIO )

ADD_EXECUTABLE(EllipseSpatialObject EllipseSpatialObject.cxx)
TARGET_LINK_LIBRARIES(EllipseSpatialObject ITKCommon ITKIO )

ADD_EXECUTABLE(ImageSpatialObject ImageSpatialObject.cxx)
TARGET_LINK_LIBRARIES(ImageSpatialObject ITKCommon ITKIO )

ADD_EXECUTABLE(LandmarkSpatialObject LandmarkSpatialObject.cxx)
TARGET_LINK_LIBRARIES(LandmarkSpatialObject ITKCommon ITKIO )

ADD_EXECUTABLE(LineSpatialObject LineSpatialObject.cxx)
TARGET_LINK_LIBRARIES(LineSpatialObject ITKCommon ITKIO )

ADD_EXECUTABLE(ReadWriteSpatialObject ReadWriteSpatialObject.cxx)
TARGET_LINK_LIBRARIES(ReadWriteSpatialObject ITKCommon ITKIO )

ADD_EXECUTABLE(SceneSpatialObject SceneSpatialObject.cxx)
TARGET_LINK_LIBRARIES(SceneSpatialObject ITKCommon ITKIO )

ADD_EXECUTABLE(SpatialObjectHierarchy SpatialObjectHierarchy.cxx)
TARGET_LINK_LIBRARIES(SpatialObjectHierarchy ITKCommon ITKIO )

ADD_EXECUTABLE(SpatialObjectTransforms SpatialObjectTransforms.cxx)
TARGET_LINK_LIBRARIES(SpatialObjectTransforms ITKCommon ITKIO )

ADD_EXECUTABLE(SurfaceSpatialObject SurfaceSpatialObject.cxx)
TARGET_LINK_LIBRARIES(SurfaceSpatialObject ITKCommon ITKIO )

ADD_EXECUTABLE(TubeSpatialObject TubeSpatialObject.cxx)
TARGET_LINK_LIBRARIES(TubeSpatialObject ITKCommon ITKIO )

