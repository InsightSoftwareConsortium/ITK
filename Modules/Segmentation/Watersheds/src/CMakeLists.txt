set(ITKWatersheds_SRC
itkWatershedMiniPipelineProgressCommand.cxx
itkOneWayEquivalencyTable.cxx
)

add_library(ITKWatersheds ${ITKWatersheds_SRC})
itk_module_link_dependencies()
itk_module_target(ITKWatersheds)
