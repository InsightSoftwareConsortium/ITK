# Sources for ITKStatistics library
SET(ITKStatistics_SRCS    
itkNormalVariateGenerator.cxx
)   

ADD_LIBRARY(ITKStatistics ${ITKStatistics_SRCS})

INSTALL_TARGETS(/lib/InsightToolkit ITKStatistics)
INSTALL_FILES(/include/InsightToolkit/Numerics/Statistics "(\\.h|\\.txx)$")
