#
#  Examples on the use of SpatialObjects.
#
PROJECT(SpatialObjectsExamples)
INCLUDE_REGULAR_EXPRESSION("^.*$")

ADD_EXECUTABLE(SpatialObject1 SpatialObject1.cxx)
TARGET_LINK_LIBRARIES(SpatialObject1 ITKCommon ITKIO ITKMetaIO itkzlib itkpng)

