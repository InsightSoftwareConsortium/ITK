set(ITKWatersheds_SRCS
  itkWatershedMiniPipelineProgressCommand.cxx
  itkOneWayEquivalencyTable.cxx
  )

itk_module_add_library(ITKWatersheds ${ITKWatersheds_SRCS})
