# Sources of non-templated classes.
SET(ITKCommon_SRCS
  itkBarrier.cxx
  itkBloxItem.cxx
  itkChainCodePath2D.cxx
  itkCommand.h itkCommand.cxx
  itkConditionVariable.cxx
  itkCovariantVector.cxx
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
  itkLoggerBase.cxx
  itkLogger.cxx
  itkLoggerManager.cxx
  itkLoggerOutput.cxx
  itkMaximumDecisionRule.cxx
  itkMaximumRatioDecisionRule.cxx
  itkMersenneTwisterRandomVariateGenerator.cxx
  itkMeshRegion.cxx
  itkMinimumDecisionRule.cxx
  itkMultiThreader.cxx
  itkMutexLock.cxx
  itkMultipleLogOutput.cxx
  itkNumericTraits.cxx
  itkNumericTraitsCovariantVectorPixel.cxx
  itkNumericTraitsRGBPixel.cxx
  itkNumericTraitsVectorPixel.cxx
  itkNumericTraitsVariableLengthVectorPixel.cxx
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
  itkRealTimeClock.cxx
  itkRegion.cxx
  itkProgressAccumulator.cxx
  itkSemaphore.cxx
  itkSegmentationBorder.cxx
  itkSegmentationRegion.cxx
  itkSimpleFastMutexLock.cxx
  itkSimpleFilterWatcher.cxx
  itkSimplexMeshGeometry.cxx
  itkStdStreamLogOutput.cxx
  itkTextOutput.cxx
  itkTetrahedronCellTopology.cxx
  itkThreadLogger.cxx
  itkTimeProbe.cxx
  itkTimeProbesCollectorBase.cxx
  itkTimeStamp.cxx
  itkTransformFactoryBase.cxx
  itkTriangleCellTopology.cxx
  itkVector.cxx
  itkVersion.cxx
  itkXMLFileOutputWindow.cxx
  itkMetaDataObjectBase.cxx
  itkMetaDataDictionary.cxx
  itkOctreeNode.cxx
)

IF(ITK_EXPLICIT_INSTANTIATION)
  SET(ITKCommon_SRCS ${ITKCommon_SRCS}
    Templates/itkFixedArrayD2.cxx
    Templates/itkFixedArrayD3.cxx
    Templates/itkFixedArrayF2.cxx
    Templates/itkFixedArrayF3.cxx
    Templates/itkImageBase2.cxx
    Templates/itkImageBase3.cxx
    Templates/itkImageD2.cxx
    Templates/itkImageD3.cxx
    Templates/itkImageF2.cxx
    Templates/itkImageF3.cxx
    Templates/itkImageI2.cxx
    Templates/itkImageI3.cxx
    Templates/itkImageL2.cxx
    Templates/itkImageL3.cxx
    Templates/itkImageRegion2.cxx
    Templates/itkImageRegion3.cxx
    Templates/itkImageS2.cxx
    Templates/itkImageS3.cxx
    Templates/itkImageSourceD2.cxx
    Templates/itkImageSourceD3.cxx
    Templates/itkImageSourceF2.cxx
    Templates/itkImageSourceF3.cxx
    Templates/itkImageSourceI2.cxx
    Templates/itkImageSourceI3.cxx
    Templates/itkImageSourceL2.cxx
    Templates/itkImageSourceL3.cxx
    Templates/itkImageSourceS2.cxx
    Templates/itkImageSourceS3.cxx
    Templates/itkImageSourceUC2.cxx
    Templates/itkImageSourceUC3.cxx
    Templates/itkImageSourceUI2.cxx
    Templates/itkImageSourceUI3.cxx
    Templates/itkImageSourceUL2.cxx
    Templates/itkImageSourceUL3.cxx
    Templates/itkImageSourceUS2.cxx
    Templates/itkImageSourceUS3.cxx
    Templates/itkImageUC2.cxx
    Templates/itkImageUC3.cxx
    Templates/itkImageUI2.cxx
    Templates/itkImageUI3.cxx
    Templates/itkImageUL2.cxx
    Templates/itkImageUL3.cxx
    Templates/itkImageUS2.cxx
    Templates/itkImageUS3.cxx
    Templates/itkImportImageContainerUL_D.cxx
    Templates/itkImportImageContainerUL_F.cxx
    Templates/itkImportImageContainerUL_I.cxx
    Templates/itkImportImageContainerUL_L.cxx
    Templates/itkImportImageContainerUL_S.cxx
    Templates/itkImportImageContainerUL_UC.cxx
    Templates/itkImportImageContainerUL_UI.cxx
    Templates/itkImportImageContainerUL_UL.cxx
    Templates/itkImportImageContainerUL_US.cxx
    Templates/itkPointD2.cxx
    Templates/itkPointD3.cxx
    Templates/itkPointF2.cxx
    Templates/itkPointF3.cxx
    Templates/itkVectorD2.cxx
    Templates/itkVectorD3.cxx
    Templates/itkVectorF2.cxx
    Templates/itkVectorF3.cxx
    )
ENDIF(ITK_EXPLICIT_INSTANTIATION)

IF (WIN32)
   SET(ITKCommon_SRCS ${ITKCommon_SRCS};itkWin32OutputWindow.cxx;itkSmartPointerForwardReferenceProcessObject.cxx)
ENDIF (WIN32)

# Instantiations not provided by vnl but used by ITK.
ADD_LIBRARY(itkvnl_inst
  vnl_c_vector+short-.cxx
  vnl_c_vector+ushort-.cxx
  vnl_c_vector+char-.cxx
  vnl_matrix+ushort-.cxx
  vnl_matrix+short-.cxx
  vnl_matrix+char-.cxx
  vnl_matrix_fixed+double.3.12-.cxx
  vnl_matrix_fixed_ref+double.2.2-.cxx
  vnl_matrix_fixed_ref+double.2.3-.cxx
  vnl_matrix_fixed_ref+double.3.12-.cxx
  vnl_matrix_fixed_ref+double.4.3-.cxx
  vnl_matrix_fixed_ref+double.4.4-.cxx
  vnl_vector+short-.cxx
  vnl_vector+ushort-.cxx
  vnl_vector+char-.cxx
  )
TARGET_LINK_LIBRARIES (itkvnl_inst ${VXL_NUMERICS_LIBRARIES} )

ADD_LIBRARY(ITKCommon ${ITK_COMMON_BUILD_TYPE} ${ITKCommon_SRCS})
TARGET_LINK_LIBRARIES (ITKCommon itkvnl_inst itksys)
IF(UNIX)
  TARGET_LINK_LIBRARIES(ITKCommon ${CMAKE_THREAD_LIBS} ${CMAKE_DL_LIBS} -lm)
ENDIF(UNIX)

INSTALL_TARGETS(/lib/InsightToolkit ITKCommon itkvnl_inst)
INSTALL_FILES(/include/InsightToolkit/Common "(\\.h|\\.txx)$")
