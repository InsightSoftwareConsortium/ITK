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
  itkIndent.cxx  
  itkIterationReporter.cxx
  itkKLMSegmentationBorder.cxx 
  itkKLMSegmentationRegion.cxx 
  itkLightObject.cxx  
  itkLightProcessObject.cxx 
  itkMaximumDecisionRule.cxx 
  itkMaximumRatioDecisionRule.cxx 
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
  itkTextOutput.cxx
  itkTetrahedronCellTopology.cxx
  itkTimeProbe.cxx 
  itkTimeProbesCollectorBase.cxx 
  itkTimeStamp.cxx 
  itkTriangleCellTopology.cxx
  itkVector.cxx
  itkVersion.cxx
  itkXMLFileOutputWindow.cxx
  itkMetaDataObjectBase.cxx
  itkMetaDataDictionary.cxx
  itkOctreeNode.cxx
)

IF (WIN32)
   SET(ITKCommon_SRCS ${ITKCommon_SRCS};itkWin32OutputWindow.cxx;itkSmartPointerForwardReferenceProcessObject.cxx)
ENDIF (WIN32)

ADD_LIBRARY(ITKCommon ${ITK_COMMON_BUILD_TYPE} ${ITKCommon_SRCS})
TARGET_LINK_LIBRARIES (ITKCommon ${VXL_NUMERICS_LIBRARIES} itkkwsys)
IF(UNIX)
  TARGET_LINK_LIBRARIES(ITKCommon ${CMAKE_THREAD_LIBS} ${CMAKE_DL_LIBS} -lm)
ENDIF(UNIX)

INSTALL_TARGETS(/lib/InsightToolkit ITKCommon)
INSTALL_FILES(/include/InsightToolkit/Common "(\\.h|\\.txx)$")
