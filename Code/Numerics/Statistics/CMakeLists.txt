# Sources for ITKStatistics library
SET(ITKStatistics_SRCS    
itkNormalVariateGenerator.cxx
)   

ADD_LIBRARY(ITKStatistics ${ITKStatistics_SRCS})

TARGET_LINK_LIBRARIES(ITKStatistics ITKCommon)

INSTALL_TARGETS(/lib/InsightToolkit ITKStatistics)
INSTALL_FILES(/include/InsightToolkit/Numerics/Statistics "(\\.h|\\.txx)$")
