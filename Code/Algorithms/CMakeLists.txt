ADD_LIBRARY(ITKAlgorithms
  itkWatershedEquivalencyTable.cxx
  itkWatershedOneWayEquivalencyTable.cxx
  itkWatershedMiniPipelineProgressCommand.cxx
  itkBioGene.cxx
)
TARGET_LINK_LIBRARIES(ITKAlgorithms ITKNumerics)

INSTALL_TARGETS(/lib/InsightToolkit ITKAlgorithms)
INSTALL_FILES(/include/InsightToolkit/Algorithms "(\\.h|\\.txx)$")
