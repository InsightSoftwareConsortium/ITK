# Sources of non-templated classes.
SET(ITKAlgorithms_SRCS
itkWatershedEquivalencyTable.cxx
itkWatershedOneWayEquivalencyTable.cxx
itkWatershedMiniPipelineProgressCommand.cxx
)

ADD_LIBRARY(ITKAlgorithms ${ITKAlgorithms_SRCS})
TARGET_LINK_LIBRARIES(ITKAlgorithms ITKCommon ITKNumerics)

INSTALL_TARGETS(/lib/InsightToolkit ITKAlgorithms)
INSTALL_FILES(/include/InsightToolkit/Algorithms "(\\.h|\\.txx)$")
