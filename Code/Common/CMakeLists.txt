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
  itkSpatialOrientationAdapter.cxx
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

IF(NOT ITK_EXPLICIT_INSTANTIATION)
  SET(ITKCommon_SRCS
      ${ITKCommon_SRCS}
      itkParametricPath.cxx
     )
ENDIF(NOT ITK_EXPLICIT_INSTANTIATION)


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

IF(ITK_EXPLICIT_INSTANTIATION)
  INCLUDE(${ITK_SOURCE_DIR}/Wrapping/ExplicitITK/Modules/Common/ExplicitITKCommon.cmake)
ENDIF(ITK_EXPLICIT_INSTANTIATION)

ADD_LIBRARY(ITKCommon ${ITK_COMMON_BUILD_TYPE} ${ITKCommon_SRCS} ${WRAPPER_EXPLICIT_ITKCommon_SRCS})

TARGET_LINK_LIBRARIES (ITKCommon itkvnl_inst itksys)
IF(UNIX)
  TARGET_LINK_LIBRARIES(ITKCommon ${CMAKE_THREAD_LIBS} ${CMAKE_DL_LIBS} -lm)
ENDIF(UNIX)
IF(APPLE)
  TARGET_LINK_LIBRARIES(ITKCommon "-framework AppKit")
ENDIF(APPLE)

IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(ITKCommon PROPERTIES ${ITK_LIBRARY_PROPERTIES})
  SET_TARGET_PROPERTIES(itkvnl_inst PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)

IF(NOT ITK_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS ITKCommon itkvnl_inst
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)

IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  FILE(GLOB __files1 "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
  FILE(GLOB __files2 "${CMAKE_CURRENT_SOURCE_DIR}/*.txx")
  INSTALL(FILES ${__files1} ${__files2}
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24}/Common
    COMPONENT Development)

  IF(ITK_EXPLICIT_INSTANTIATION)
    FILE(GLOB __files3 "${CMAKE_CURRENT_BINARY_DIR}/Templates/*.h")
    INSTALL(FILES ${__files3}
      DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24}/Common/Templates
      COMPONENT Development)
  ENDIF(ITK_EXPLICIT_INSTANTIATION)
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)
