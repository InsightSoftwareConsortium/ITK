set(ITKCommon_SRCS
itkLoggerOutput.cxx
itkProgressAccumulator.cxx
itkNumericTraits.cxx
itkMutexLock.cxx
itkHexahedronCellTopology.cxx
itkIndent.cxx
itkEventObject.cxx
itkFileOutputWindow.cxx
itkSimpleFilterWatcher.cxx
itkKernelFunction.cxx
itkGaussianKernelFunction.cxx
itkNumericTraitsVectorPixel.cxx
itkNumericTraitsVariableLengthVectorPixel.cxx
itkObject.cxx
itkDynamicLoader.cxx
itkQuadrilateralCellTopology.cxx
itkIterationReporter.cxx
itkMemoryProbe.cxx
itkTextOutput.cxx
itkNumericTraitsTensorPixel2.cxx
itkNumericTraitsFixedArrayPixel2.cxx
itkConditionVariable.cxx
itkProcessObject.cxx
itkBarrier.cxx
itkSpatialOrientationAdapter.cxx
itkRealTimeInterval.cxx
itkOctreeNode.cxx
itkNumericTraitsFixedArrayPixel.cxx
itkMultiThreader.cxx
itkNumericTraitsArrayPixel.cxx
itkMetaDataDictionary.cxx
itkDataObject.cxx
itkThreadLogger.cxx
itkNumericTraitsTensorPixel.cxx
itkCommand.cxx
itkNumericTraitsPointPixel.cxx
itkLightObject.cxx
itkStdStreamLogOutput.cxx
itkLightProcessObject.cxx
itkRegion.cxx
itkFastMutexLock.cxx
itkVersion.cxx
itkNumericTraitsRGBAPixel.cxx
itkRealTimeClock.cxx
itkMetaDataObjectBase.cxx
itkCovariantVector.cxx
itkMemoryUsageObserver.cxx
itkMersenneTwisterRandomVariateGenerator.cxx
itkLoggerBase.cxx
itkNumericTraitsCovariantVectorPixel.cxx
itkProgressReporter.cxx
itkExceptionObject.cxx
itkMultipleLogOutput.cxx
itkQuadraticTriangleCellTopology.cxx
itkTimeProbesCollectorBase.cxx
itkSmapsFileParser.cxx
itkTriangleCellTopology.cxx
itkVector.cxx
itkRealTimeStamp.cxx
itkDirectory.cxx
itkLoggerManager.cxx
itkTimeProbe.cxx
itkNumericTraitsRGBPixel.cxx
itkTimeStamp.cxx
itkTetrahedronCellTopology.cxx
itkObjectFactoryBase.cxx
itkFloatingPointExceptions.cxx
itkOutputWindow.cxx
itkSimpleFastMutexLock.cxx
itkNumericTraitsDiffusionTensor3DPixel.cxx
itkEquivalencyTable.cxx
itkXMLFileOutputWindow.cxx
itkStoppingCriterionBase.cxx
)

if(WIN32)
   set(ITKCommon_SRCS ${ITKCommon_SRCS};itkWin32OutputWindow.cxx;itkSmartPointerForwardReferenceProcessObject.cxx)
endif(WIN32)

### generating libraries
add_library( ITKCommon ${ITK_LIBRARY_BUILD_TYPE} ${ITKCommon_SRCS})
if(NOT WIN32 AND CMAKE_COMPILER_IS_GNUCXX)
  set(ITKCommon_CXX_FLAGS "-fvisibility=hidden -fvisibility-inlines-hidden")
  CHECK_CXX_COMPILER_FLAG(${ITKCommon_CXX_FLAGS} ITKCommon_HAS_GCC_VISIBILITY_FLAGS)
  if(ITKCommon_HAS_GCC_VISIBILITY_FLAGS)
    set_target_properties(ITKCommon PROPERTIES
      COMPILE_FLAGS "${ITKCommon_CXX_FLAGS}"
    )
  endif(ITKCommon_HAS_GCC_VISIBILITY_FLAGS)
endif(NOT WIN32 AND CMAKE_COMPILER_IS_GNUCXX)

target_link_libraries(ITKCommon itksys ITKVNLInstantiation)

if(UNIX)
  target_link_libraries(ITKCommon ${CMAKE_THREAD_LIBS} ${CMAKE_DL_LIBS} -lm)
endif(UNIX)

itk_module_target(ITKCommon)
