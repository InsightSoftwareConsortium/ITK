# Sources of non-templated classes.
SET(ITKCommon_SRCS
itkBloxItem.cxx
itkCommand.h
itkDataObject.cxx 
itkDecisionRuleBase.cxx 
itkDirectory.cxx  
itkDynamicLoader.cxx  
itkEventObject.cxx  
itkExceptionObject.cxx  
itkFastMutexLock.cxx  
itkFileOutputWindow.cxx 
itkGaussianKernelFunction.cxx 
itkHexahedronCellTopology.cxx
itkImageIOCommon.cxx 
itkImageIO.cxx 
itkIndent.cxx  
itkLightObject.cxx  
itkLightProcessObject.cxx 
itkKLMSegmentationBorder.cxx 
itkKLMSegmentationRegion.cxx 
itkMeshRegion.cxx 
itkMinimumDecisionRule.cxx 
itkMultiThreader.cxx 
itkMutexLock.cxx  
itkNumericTraits.cxx  
itkObject.cxx 
itkObjectFactoryBase.cxx  
itkOutputWindow.cxx  
itkProcessObject.cxx  
itkProgressReporter.cxx  
itkQuadraticTriangleCellTopology.cxx
itkQuadrilateralCellTopology.cxx
itkRegion.cxx 
itkSegmentationBorder.cxx 
itkSegmentationRegion.cxx  
itkSimpleFastMutexLock.cxx 
itkTetrahedronCellTopology.cxx
itkTimeStamp.cxx 
itkTimeProbe.cxx 
itkTimeProbesCollectorBase.cxx 
itkTriangleCellTopology.cxx
itkVersion.cxx 
itkVector.cxx 
itkXMLFileOutputWindow.cxx 
)

IF (WIN32)
   SET(ITKCommon_SRCS ${ITKCommon_SRCS};itkWin32OutputWindow.cxx)
ENDIF (WIN32)

ADD_LIBRARY(ITKCommon ${ITKCommon_SRCS})
TARGET_LINK_LIBRARIES (ITKCommon VXLNumerics)
IF(UNIX)
  TARGET_LINK_LIBRARIES(ITKCommon -lm)
ENDIF(UNIX)


INSTALL_TARGETS(/lib/InsightToolkit ITKCommon)
