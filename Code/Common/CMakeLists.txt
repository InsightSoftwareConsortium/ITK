# Sources of non-templated classes.
set(ITKCommon_SRCS
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
  itkLoggerManager.cxx
  itkLoggerOutput.cxx
  itkMaximumDecisionRule.cxx
  itkMaximumRatioDecisionRule.cxx
  itkMemoryProbe.cxx
  itkMemoryUsageObserver.cxx
  itkMersenneTwisterRandomVariateGenerator.cxx
  itkMeshRegion.cxx
  itkMinimumDecisionRule.cxx
  itkMultiThreader.cxx
  itkMutexLock.cxx
  itkMultipleLogOutput.cxx
  itkNumericTraits.cxx
  itkNumericTraitsCovariantVectorPixel.cxx
  itkNumericTraitsRGBPixel.cxx
  itkNumericTraitsRGBAPixel.cxx
  itkNumericTraitsVectorPixel.cxx
  itkNumericTraitsTensorPixel.cxx
  itkNumericTraitsTensorPixel2.cxx
  itkNumericTraitsDiffusionTensor3DPixel.cxx
  itkNumericTraitsFixedArrayPixel.cxx
  itkNumericTraitsFixedArrayPixel2.cxx
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
  itkSmapsFileParser.cxx
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

if(NOT ITK_EXPLICIT_INSTANTIATION)
  set(ITKCommon_SRCS
      ${ITKCommon_SRCS}
      itkParametricPath.cxx
     )
endif(NOT ITK_EXPLICIT_INSTANTIATION)

if(WIN32)
   set(ITKCommon_SRCS ${ITKCommon_SRCS};itkWin32OutputWindow.cxx;itkSmartPointerForwardReferenceProcessObject.cxx)
endif(WIN32)


# Instantiations not provided by vnl but used by ITK.
add_library(itkvnl_inst
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
  vnl_vector+size_t-.cxx
  vnl_fft_base+double.3-.cxx
  vnl_fft_base+float.3-.cxx
  )
target_link_libraries(itkvnl_inst ${VXL_NUMERICS_LIBRARIES} )

if(ITK_EXPLICIT_INSTANTIATION)
  include(${ITK_SOURCE_DIR}/Wrapping/ExplicitITK/Modules/Common/ExplicitITKCommon.cmake)

  set(WRAPPER_EXPLICIT_ITKCommon_SRCS
   ${WRAPPER_EXPLICIT_ITKCommon_SRCS}
   ${ITK_SOURCE_DIR}/Wrapping/ExplicitITK/itkMissingExplicitIntantiations.cxx
  )
endif(ITK_EXPLICIT_INSTANTIATION)

add_library(ITKCommon ${ITK_COMMON_BUILD_TYPE} ${ITKCommon_SRCS} ${WRAPPER_EXPLICIT_ITKCommon_SRCS})

target_link_libraries(ITKCommon itkvnl_inst itksys)
if(UNIX)
  target_link_libraries(ITKCommon ${CMAKE_THREAD_LIBS} ${CMAKE_DL_LIBS} -lm)
endif(UNIX)

include(CheckLibraryExists)

if(WIN32)
  CHECK_LIBRARY_EXISTS(Psapi EnumProcesses "" HAVE_PSAPI)
  if(HAVE_PSAPI)
    target_link_libraries(ITKCommon Psapi)
  endif(HAVE_PSAPI)
endif(WIN32)

CHECK_LIBRARY_EXISTS(malloc mallinfo "" HAVE_LIBMALLOC)
if(HAVE_LIBMALLOC)
  target_link_libraries(ITKCommon malloc)
endif(HAVE_LIBMALLOC)

if(ITK_LIBRARY_PROPERTIES)
  set_target_properties(ITKCommon PROPERTIES ${ITK_LIBRARY_PROPERTIES})
  set_target_properties(itkvnl_inst PROPERTIES ${ITK_LIBRARY_PROPERTIES})
endif(ITK_LIBRARY_PROPERTIES)

if(MINGW)
  get_target_property(MINGW_LINK_FLAGS ITKCommon LINK_FLAGS)
  if(NOT MINGW_LINK_FLAGS)
    set(MINGW_LINK_FLAGS "")
  endif(NOT MINGW_LINK_FLAGS)
  set_target_properties(ITKCommon PROPERTIES LINK_FLAGS "${MINGW_LINK_FLAGS} -Wl,--export-all-symbols")
endif(MINGW)

if(NOT ITK_INSTALL_NO_LIBRARIES)
  install(TARGETS ITKCommon itkvnl_inst
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
endif(NOT ITK_INSTALL_NO_LIBRARIES)

if(ITK_USE_FLAT_DIRECTORY_INSTALL)
  set( ITK_INSTALL_INCLUDE_DIR_CM24_COMMON ${ITK_INSTALL_INCLUDE_DIR_CM24})
  set( ITK_INSTALL_INCLUDE_DIR_CM24_COMMON_TEMPLATES ${ITK_INSTALL_INCLUDE_DIR_CM24})
else(ITK_USE_FLAT_DIRECTORY_INSTALL)
  set( ITK_INSTALL_INCLUDE_DIR_CM24_COMMON ${ITK_INSTALL_INCLUDE_DIR_CM24}/Common)
  set( ITK_INSTALL_INCLUDE_DIR_CM24_COMMON_TEMPLATES ${ITK_INSTALL_INCLUDE_DIR_CM24}/Common/Templates)
endif(ITK_USE_FLAT_DIRECTORY_INSTALL)


if(NOT ITK_INSTALL_NO_DEVELOPMENT)
  file(GLOB __files1 "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
  file(GLOB __files2 "${CMAKE_CURRENT_SOURCE_DIR}/*.txx")
  install(FILES ${__files1} ${__files2}
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24_COMMON}
    COMPONENT Development)

  if(ITK_EXPLICIT_INSTANTIATION)
    file(GLOB __files3 "${CMAKE_CURRENT_BINARY_DIR}/Templates/*.h")
    install(FILES ${__files3}
      DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24_COMMON_TEMPLATES}
      COMPONENT Development)
  endif(ITK_EXPLICIT_INSTANTIATION)
endif(NOT ITK_INSTALL_NO_DEVELOPMENT)

if( CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64" )
  set_target_properties(ITKCommon PROPERTIES COMPILE_FLAGS "-fPIC")
endif( CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64" )

