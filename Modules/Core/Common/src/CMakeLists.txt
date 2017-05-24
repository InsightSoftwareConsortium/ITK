set(ITKCommon_SRCS
  itkMemoryProbesCollectorBase.cxx
  itkCreateObjectFunction.cxx
  itkLogger.cxx
  itkLogOutput.cxx
  itkLoggerOutput.cxx
  itkProgressAccumulator.cxx
  itkNumericTraits.cxx
  itkMutexLock.cxx
  itkHexahedronCellTopology.cxx
  itkIndent.cxx
  itkEventObject.cxx
  itkFileOutputWindow.cxx
  itkSimpleFilterWatcher.cxx
  itkNumericTraitsVectorPixel.cxx
  itkObject.cxx
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
  itkMetaDataObject.cxx
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
  itkImageIORegion.cxx
  itkImageSourceCommon.cxx
  itkImageToImageFilterCommon.cxx
  itkImageRegionSplitterBase.cxx
  itkImageRegionSplitterSlowDimension.cxx
  itkImageRegionSplitterDirection.cxx
  itkImageRegionSplitterMultidimensional.cxx
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
  itkThreadedIndexedContainerPartitioner.cxx
  itkObjectFactoryBase.cxx
  itkFloatingPointExceptions.cxx
  itkOutputWindow.cxx
  itkSimpleFastMutexLock.cxx
  itkNumericTraitsDiffusionTensor3DPixel.cxx
  itkEquivalencyTable.cxx
  itkXMLFileOutputWindow.cxx
  itkXMLFilterWatcher.cxx
  itkStoppingCriterionBase.cxx
  itkCompensatedSummation.cxx
  itkArrayOutputSpecialization.cxx
  itkNumberToString.cxx
  itkSmartPointerForwardReferenceProcessObject.cxx
  itkThreadPool.cxx
  itkRandomVariateGeneratorBase.cxx
  itkAtomicInt.cxx
  itkMath.cxx
  )

if(WIN32)
  list(APPEND ITKCommon_SRCS itkWin32OutputWindow.cxx)
endif()

if(ITK_USE_WIN32_THREADS)
  list(APPEND ITKCommon_SRCS itkThreadPoolWin32.cxx)
elseif(ITK_USE_PTHREADS)
  list(APPEND ITKCommon_SRCS itkThreadPoolPosix.cxx)
endif()

if(ITK_DYNAMIC_LOADING)
  list(APPEND ITKCommon_SRCS itkDynamicLoader.cxx)
endif()

# Prevent optimization from affecting the floating point calculation.
if( CMAKE_CXX_COMPILER MATCHES "^icc" )
  if( WIN32 )
    set_source_files_properties( itkCompensatedSummation.cxx PROPERTIES COMPILE_FLAGS /fp precise )
  else()
    set_source_files_properties( itkCompensatedSummation.cxx PROPERTIES COMPILE_FLAGS -fp-model precise )
  endif()
elseif( CMAKE_COMPILER_IS_GNUCXX )
  # -fno-associative-math appears to only be present in GCC 4.3 onwards.
  include( CheckCXXCompilerFlag )
  CHECK_CXX_COMPILER_FLAG( -fno-associative-math gcc_has_no_associative_math )
  if( gcc_has_no_associative_math )
    set_source_files_properties( itkCompensatedSummation.cxx PROPERTIES COMPILE_FLAGS -fno-associative-math )
  else()
    set_source_files_properties( itkCompensatedSummation.cxx PROPERTIES COMPILE_FLAGS -fno-unsafe-math-optimizations )
  endif()
elseif( MSVC )
  set_source_files_properties( itkCompensatedSummation.cxx PROPERTIES COMPILE_FLAGS -fp:precise )
endif()

if( CMAKE_CXX_COMPILER_ID MATCHES Clang AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS "4.0" )
  # this file contains explicitly instantiated template classes which are not correcly exported with the hidden visibility flag
  set_source_files_properties( itkSmartPointerForwardReferenceProcessObject.cxx PROPERTIES COMPILE_FLAGS -fvisibility=default )
endif()


### generating libraries
itk_module_add_library( ITKCommon ${ITKCommon_SRCS})

target_link_libraries(ITKCommon LINK_PUBLIC itksys ITKVNLInstantiation)

if(UNIX)
  target_link_libraries(ITKCommon LINK_PUBLIC ${CMAKE_THREAD_LIBS} -lm)
  if(ITK_DYNAMIC_LOADING)
    target_link_libraries(ITKCommon LINK_PUBLIC ${CMAKE_DL_LIBS})
  endif()
endif()
