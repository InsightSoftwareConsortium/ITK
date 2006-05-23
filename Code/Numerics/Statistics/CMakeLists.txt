# Sources for ITKStatistics library
ADD_LIBRARY(ITKStatistics 
  itkNormalVariateGenerator.cxx
  itkDenseFrequencyContainer.cxx
  itkSparseFrequencyContainer.cxx
 )

TARGET_LINK_LIBRARIES(ITKStatistics ITKCommon)

IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(ITKStatistics PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)
INSTALL_TARGETS(/lib/InsightToolkit ITKStatistics)
INSTALL_FILES(/include/InsightToolkit/Numerics/Statistics "(\\.h|\\.txx)$")
