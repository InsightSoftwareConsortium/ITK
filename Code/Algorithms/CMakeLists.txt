ADD_LIBRARY(ITKAlgorithms
  itkWatershedMiniPipelineProgressCommand.cxx
  itkBioCellBase.cxx
  itkBioCellularAggregateBase.cxx
  itkBioGenome.cxx
  itkBioGene.cxx
  itkBioGeneNetwork.cxx
)
TARGET_LINK_LIBRARIES(ITKAlgorithms ITKNumerics)

INSTALL_TARGETS(/lib/InsightToolkit ITKAlgorithms)
INSTALL_FILES(/include/InsightToolkit/Algorithms "(\\.h|\\.txx)$")
