# Sources of non-templated classes.
SET(ITKCommon_SRCS
  itkBarrier.cxx
  itkBloxItem.cxx
  itkChainCodePath2D.cxx
  itkCommand.h itkCommand.cxx
  itkConditionVariable.cxx
  itkDataObject.cxx
  itkDecisionRuleBase.cxx
  itkDirectory.cxx
  itkDynamicLoader.cxx
  itkEquivalencyTable.cxx
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
  itkKernelFunction.cxx
  itkLightObject.cxx
  itkLightProcessObject.cxx
  itkMaximumDecisionRule.cxx
  itkMaximumRatioDecisionRule.cxx
  itkMeshRegion.cxx
  itkMinimumDecisionRule.cxx
  itkMultiThreader.cxx
  itkMutexLock.cxx
  itkNumericTraits.cxx
  itkNumericTraitsRGBPixel.cxx
  itkObject.cxx
  itkObjectFactoryBase.cxx
  itkOneWayEquivalencyTable.cxx
  itkOrthogonallyCorrected2DParametricPath.cxx
  itkOutputWindow.cxx
  itkParametricPath.cxx
  itkProcessObject.cxx
  itkProgressReporter.cxx
  itkQuadraticTriangleCellTopology.cxx
  itkQuadrilateralCellTopology.cxx
  itkRegion.cxx
  itkProgressAccumulator.cxx
  itkSemaphore.cxx
  itkSegmentationBorder.cxx
  itkSegmentationRegion.cxx
  itkSimpleFastMutexLock.cxx
  itkSimpleFilterWatcher.cxx
  itkSimplexMeshGeometry.cxx
  itkTextOutput.cxx
  itkTetrahedronCellTopology.cxx
  itkTimeProbe.cxx
  itkTimeProbesCollectorBase.cxx
  itkTimeStamp.cxx
  itkTransformFactory.cxx
  itkTriangleCellTopology.cxx
  itkVector.cxx
  itkVersion.cxx
  itkXMLFileOutputWindow.cxx
  itkMetaDataObjectBase.cxx
  itkMetaDataDictionary.cxx
  itkOctreeNode.cxx
  itkExplicitInstantiations.cxx
)

IF (WIN32)
   SET(ITKCommon_SRCS ${ITKCommon_SRCS};itkWin32OutputWindow.cxx;itkSmartPointerForwardReferenceProcessObject.cxx)
ENDIF (WIN32)

# Instantiations not provided by vnl but used by ITK.
ADD_LIBRARY(itkvnl_inst
  vnl_c_vector+ushort-.cxx
  vnl_matrix+ushort-.cxx
  vnl_matrix_fixed+double.3.12-.cxx
  vnl_matrix_fixed_ref+double.2.2-.cxx
  vnl_matrix_fixed_ref+double.2.3-.cxx
  vnl_matrix_fixed_ref+double.3.12-.cxx
  vnl_matrix_fixed_ref+double.4.3-.cxx
  vnl_matrix_fixed_ref+double.4.4-.cxx
  vnl_vector+ushort-.cxx
  )
TARGET_LINK_LIBRARIES (itkvnl_inst ${VXL_NUMERICS_LIBRARIES})

ADD_LIBRARY(ITKCommon ${ITK_COMMON_BUILD_TYPE} ${ITKCommon_SRCS})
TARGET_LINK_LIBRARIES (ITKCommon itkvnl_inst itksys)
IF(UNIX)
  TARGET_LINK_LIBRARIES(ITKCommon ${CMAKE_THREAD_LIBS} ${CMAKE_DL_LIBS} -lm)
ENDIF(UNIX)

INSTALL_TARGETS(/lib/InsightToolkit ITKCommon itkvnl_inst)
INSTALL_FILES(/include/InsightToolkit/Common "(\\.h|\\.txx)$")
