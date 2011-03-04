WRAP_LIBRARY(LevelSet)

set(WRAPPER_LIBRARY_GROUPS
  itkLevelSetFunction
  itkSparseFieldLevelSetImageFilter
  itkSparseFieldFourthOrderLevelSetImageFilter
  itkSegmentationLevelSetImageFilter
  itkShapePriorSegmentationLevelSetImageFilter
  itkFastMarchingImageFilter
)

set(WRAPPER_LIBRARY_DEPENDS VXLNumerics Base Numerics)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
