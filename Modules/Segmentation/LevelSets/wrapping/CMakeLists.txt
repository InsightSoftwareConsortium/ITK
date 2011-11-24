WRAP_LIBRARY(ITKLevelSets)

set(WRAPPER_LIBRARY_GROUPS
  itkLevelSetFunction
  itkSparseFieldLevelSetImageFilter
  itkSparseFieldFourthOrderLevelSetImageFilter
  itkSegmentationLevelSetImageFilter
  itkShapePriorSegmentationLevelSetImageFilter
)
AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
