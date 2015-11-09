itk_wrap_module(ITKSpatialObjects)

set(WRAPPER_SUBMODULE_ORDER
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
itk_auto_load_submodules()
itk_end_wrap_module()
