set(ITK-KLMRegionGrowing_SRC
itkSegmentationRegion.cxx
itkKLMSegmentationRegion.cxx
itkSegmentationBorder.cxx
itkKLMSegmentationBorder.cxx
)

add_library(ITK-KLMRegionGrowing ${ITK-KLMRegionGrowing_SRC})
target_link_libraries(ITK-KLMRegionGrowing  ${ITK-Common_LIBRARIES})
itk_module_target(ITK-KLMRegionGrowing)
