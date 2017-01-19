set(ITKKLMRegionGrowing_SRCS
  itkSegmentationRegion.cxx
  itkKLMSegmentationRegion.cxx
  itkSegmentationBorder.cxx
  itkKLMSegmentationBorder.cxx
  )

itk_module_add_library(ITKKLMRegionGrowing ${ITKKLMRegionGrowing_SRCS})
