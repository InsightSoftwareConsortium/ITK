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

set(WRAPPER_LIBRARY_DEPENDS ITKImageLabel ITKStatistics ITKMathematicalMorphology)


set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
