WRAP_LIBRARY(ITKSpatialObjects)

set(WRAPPER_LIBRARY_GROUPS
  itkSpatialObjectProperty
  itkAffineGeometryFrame
  itkSpatialObjectBase
  itkSpatialObjectPoint
  itkContourSpatialObjectPoint
  itkDTITubeSpatialObjectPoint
  itkVesselTubeSpatialObjectPoint
  itkPointBasedSpatialObject
  itkImageSpatialObject
  itkBlobSpatialObject
  itkPolygonSpatialObject
)
AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
