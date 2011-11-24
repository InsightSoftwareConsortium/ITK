WRAP_LIBRARY(ITKLabelMap)

set(WRAPPER_LIBRARY_GROUPS
  itkLabelObjectLine
  itkLabelObject
  itkShapeLabelObject
  itkStatisticsLabelObject
  ITKLabelMapBase
  itkLabelMapFilter
  itkInPlaceLabelMapFilter
  itkChangeRegionLabelMapFilter
)
AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
