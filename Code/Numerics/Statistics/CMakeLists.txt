# Sources for ITKStatistics library
ADD_LIBRARY(ITKStatistics 
  itkMersenneTwisterRandomVariateGenerator.cxx  
  itkNormalVariateGenerator.cxx
  itkHistogram.cxx
 )

TARGET_LINK_LIBRARIES(ITKStatistics ITKCommon)

INSTALL_TARGETS(/lib/InsightToolkit ITKStatistics)
INSTALL_FILES(/include/InsightToolkit/Numerics/Statistics "(\\.h|\\.txx)$")
