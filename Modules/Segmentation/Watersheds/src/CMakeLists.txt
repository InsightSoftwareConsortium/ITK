set(
  ITKWatersheds_SRCS
  itkOneWayEquivalencyTable.cxx
  itkWatershedMiniPipelineProgressCommand.cxx
)

itk_module_add_library(ITKWatersheds ${ITKWatersheds_SRCS})
