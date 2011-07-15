set(ITKKLMRegionGrowing_SRC
itkSegmentationRegion.cxx
itkKLMSegmentationRegion.cxx
itkSegmentationBorder.cxx
itkKLMSegmentationBorder.cxx
)

add_library(ITKKLMRegionGrowing ${ITKKLMRegionGrowing_SRC})
target_link_libraries(ITKKLMRegionGrowing  ${ITKCommon_LIBRARIES})
itk_module_target(ITKKLMRegionGrowing)
