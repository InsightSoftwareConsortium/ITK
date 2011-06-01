set(ITK-Watersheds_SRC
itkWatershedMiniPipelineProgressCommand.cxx
itkOneWayEquivalencyTable.cxx
)

add_library(ITK-Watersheds ${ITK-Watersheds_SRC})
target_link_libraries(ITK-Watersheds  ${ITK-ImageIntensity_LIBRARIES} ${ITK-Thresholding_LIBRARIES} ${ITK-ImageGradient_LIBRARIES} ${ITK-Smoothing_LIBRARIES})
itk_module_target(ITK-Watersheds)
