set(
  ITKKLMRegionGrowing_SRCS
  itkKLMSegmentationBorder.cxx
  itkKLMSegmentationRegion.cxx
  itkSegmentationBorder.cxx
  itkSegmentationRegion.cxx
)

itk_module_add_library(ITKKLMRegionGrowing ${ITKKLMRegionGrowing_SRCS})
