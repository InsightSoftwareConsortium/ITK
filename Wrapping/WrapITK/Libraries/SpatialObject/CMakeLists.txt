WRAP_LIBRARY(SpatialObject)

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

set(WRAPPER_LIBRARY_DEPENDS VXLNumerics Base Transforms Calculators)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
