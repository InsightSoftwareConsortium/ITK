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
    Templates/itkFixedArray+double.2-.cxx
    Templates/itkFixedArray+double.3-.cxx
    Templates/itkFixedArray+float.2-.cxx
    Templates/itkFixedArray+float.3-.cxx
    Templates/itkImage+double.2-.cxx
    Templates/itkImage+double.3-.cxx
    Templates/itkImage+float.2-.cxx
    Templates/itkImage+float.3-.cxx
    Templates/itkImage+int.2-.cxx
    Templates/itkImage+int.3-.cxx
    Templates/itkImage+long.2-.cxx
    Templates/itkImage+long.3-.cxx
    Templates/itkImage+short.2-.cxx
    Templates/itkImage+short.3-.cxx
    Templates/itkImage+unsigned_char.2-.cxx
    Templates/itkImage+unsigned_char.3-.cxx
    Templates/itkImage+unsigned_int.2-.cxx
    Templates/itkImage+unsigned_int.3-.cxx
    Templates/itkImage+unsigned_long.2-.cxx
    Templates/itkImage+unsigned_long.3-.cxx
    Templates/itkImage+unsigned_short.2-.cxx
    Templates/itkImage+unsigned_short.3-.cxx
    Templates/itkImageBase+2-.cxx
    Templates/itkImageBase+3-.cxx
    Templates/itkImageRegion+2-.cxx
    Templates/itkImageRegion+3-.cxx
    Templates/itkImageSource+Image+double.2--.cxx
    Templates/itkImageSource+Image+double.3--.cxx
    Templates/itkImageSource+Image+float.2--.cxx
    Templates/itkImageSource+Image+float.3--.cxx
    Templates/itkImageSource+Image+int.2--.cxx
    Templates/itkImageSource+Image+int.3--.cxx
    Templates/itkImageSource+Image+long.2--.cxx
    Templates/itkImageSource+Image+long.3--.cxx
    Templates/itkImageSource+Image+short.2--.cxx
    Templates/itkImageSource+Image+short.3--.cxx
    Templates/itkImageSource+Image+unsigned_char.2--.cxx
    Templates/itkImageSource+Image+unsigned_char.3--.cxx
    Templates/itkImageSource+Image+unsigned_int.2--.cxx
    Templates/itkImageSource+Image+unsigned_int.3--.cxx
    Templates/itkImageSource+Image+unsigned_long.2--.cxx
    Templates/itkImageSource+Image+unsigned_long.3--.cxx
    Templates/itkImageSource+Image+unsigned_short.2--.cxx
    Templates/itkImageSource+Image+unsigned_short.3--.cxx
#     Templates/itkImageToImageFilter+Image+double.2-.Image+double.2--.cxx
#     Templates/itkImageToImageFilter+Image+double.3-.Image+double.3--.cxx
#     Templates/itkImageToImageFilter+Image+float.2-.Image+float.2--.cxx
#     Templates/itkImageToImageFilter+Image+float.3-.Image+float.3--.cxx
#     Templates/itkImageToImageFilter+Image+int.2-.Image+int.2--.cxx
#     Templates/itkImageToImageFilter+Image+int.3-.Image+int.3--.cxx
#     Templates/itkImageToImageFilter+Image+long.2-.Image+long.2--.cxx
#     Templates/itkImageToImageFilter+Image+long.3-.Image+long.3--.cxx
#     Templates/itkImageToImageFilter+Image+short.2-.Image+short.2--.cxx
#     Templates/itkImageToImageFilter+Image+short.3-.Image+short.3--.cxx
#     Templates/itkImageToImageFilter+Image+unsigned_char.2-.Image+unsigned_char.2--.cxx
#     Templates/itkImageToImageFilter+Image+unsigned_char.3-.Image+unsigned_char.3--.cxx
#     Templates/itkImageToImageFilter+Image+unsigned_int.2-.Image+unsigned_int.2--.cxx
#     Templates/itkImageToImageFilter+Image+unsigned_int.3-.Image+unsigned_int.3--.cxx
#     Templates/itkImageToImageFilter+Image+unsigned_long.2-.Image+unsigned_long.2--.cxx
#     Templates/itkImageToImageFilter+Image+unsigned_long.3-.Image+unsigned_long.3--.cxx
#     Templates/itkImageToImageFilter+Image+unsigned_short.2-.Image+unsigned_short.2--.cxx
#     Templates/itkImageToImageFilter+Image+unsigned_short.3-.Image+unsigned_short.3--.cxx
    Templates/itkImportImageContainer+unsigned_long.double-.cxx
    Templates/itkImportImageContainer+unsigned_long.float-.cxx
    Templates/itkImportImageContainer+unsigned_long.int-.cxx
    Templates/itkImportImageContainer+unsigned_long.long-.cxx
    Templates/itkImportImageContainer+unsigned_long.short-.cxx
    Templates/itkImportImageContainer+unsigned_long.unsigned_char-.cxx
    Templates/itkImportImageContainer+unsigned_long.unsigned_int-.cxx
    Templates/itkImportImageContainer+unsigned_long.unsigned_long-.cxx
    Templates/itkImportImageContainer+unsigned_long.unsigned_short-.cxx
    Templates/itkPoint+double.2-.cxx
    Templates/itkPoint+double.3-.cxx
    Templates/itkPoint+float.2-.cxx
    Templates/itkPoint+float.3-.cxx
    Templates/itkVector+double.2-.cxx
    Templates/itkVector+double.3-.cxx
    Templates/itkVector+float.2-.cxx
    Templates/itkVector+float.3-.cxx
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
TARGET_LINK_LIBRARIES (ITKCommon itkvnl_inst itkvcl itksys)
IF(UNIX)
  TARGET_LINK_LIBRARIES(ITKCommon ${CMAKE_THREAD_LIBS} ${CMAKE_DL_LIBS} -lm)
ENDIF(UNIX)

INSTALL_TARGETS(/lib/InsightToolkit ITKCommon itkvnl_inst)
INSTALL_FILES(/include/InsightToolkit/Common "(\\.h|\\.txx)$")
