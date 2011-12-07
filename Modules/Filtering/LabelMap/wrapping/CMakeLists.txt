itk_wrap_module(ITKLabelMap)

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
itk_auto_load_submodules()
itk_end_wrap_module()
