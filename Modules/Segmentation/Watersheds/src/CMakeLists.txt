set(ITKWatersheds_SRC
itkWatershedMiniPipelineProgressCommand.cxx
itkOneWayEquivalencyTable.cxx
)

add_library(ITKWatersheds ${ITKWatersheds_SRC})
target_link_libraries(ITKWatersheds  ${ITKImageIntensity_LIBRARIES} ${ITKThresholding_LIBRARIES} ${ITKImageGradient_LIBRARIES} ${ITKSmoothing_LIBRARIES})
itk_module_target(ITKWatersheds)
