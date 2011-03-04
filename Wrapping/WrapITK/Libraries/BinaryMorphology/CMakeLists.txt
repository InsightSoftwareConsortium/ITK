WRAP_LIBRARY(BinaryMorphology)

set(WRAPPER_LIBRARY_DEPENDS VXLNumerics Base Transforms Numerics)

set(WRAPPER_LIBRARY_GROUPS
  itkLabelObjectLine
  itkLabelObject
  itkShapeLabelObject
  itkStatisticsLabelObject
  itkLabelMap
  ITKLabelMapBase
  itkLabelMapFilter
  itkInPlaceLabelMapFilter
  itkChangeRegionLabelMapFilter
)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
