include(GetGitRevisionDescription)
get_git_head_revision(GIT_CONFIG_REFSPEC GIT_CONFIG_SHA1)
git_describe(GIT_VERSION_DESC)
git_head_date(GIT_CONFIG_DATE)
git_head_localmods(GIT_LOCAL_MODIFICATIONS)
if(GIT_LOCAL_MODIFICATIONS MATCHES ".*files changed.*")
  set(
    GIT_LOCAL_MODIFICATIONS
    " (with uncommitted code modifications ${GIT_LOCAL_MODIFICATIONS})"
  )
endif()

string(TIMESTAMP CONFIGURE_DATE "%Y-%m-%d %H:%M" UTC)

## MAKE_MAP_ENTRY is a macro to facilitate placing items in the itk::BuildInformation class
##                /--------------------------_---------------------------_-------------------------------------------------/
##                |    KEY                   |    VALUE                  | Description of value                            |
##                \--------------------------\---------------------------\-------------------------------------------------\
set(
  MAPPING_VALUES
  "
  MAKE_MAP_ENTRY( \"PROJECT_URL\",           \"https://www.itk.org\",      \"The URL of project.\"                     );
  MAKE_MAP_ENTRY( \"VERSION\",               \"${ITK_VERSION}\"    ,      \"The version at configuration time.\"      );
  MAKE_MAP_ENTRY( \"CONFIGURE_DATE\",        \"${CONFIGURE_DATE}\",       \"The date of configuration.\"              );

  MAKE_MAP_ENTRY( \"GIT_CONFIG_SHA1\",       \"${GIT_CONFIG_SHA1}${GIT_LOCAL_MODIFICATIONS}\",
                                                                               \"The git HEAD SHA1 has at configuration.\");
  MAKE_MAP_ENTRY( \"GIT_CONFIG_REFSPEC\",    \"${GIT_CONFIG_REFSPEC}\",   \"The git HEAD ref spec.\"                  );
  MAKE_MAP_ENTRY( \"GIT_CONFIG_DATE\",       \"${GIT_CONFIG_DATE}\",      \"The git HEAD date\"                       );
  MAKE_MAP_ENTRY( \"GIT_VERSION_DESC\",      \"${GIT_VERSION_DESC}\",     \"The git HEAD version tag-commits-hash\"   );

  MAKE_MAP_ENTRY( \"CMAKE_VERSION\",         \"${CMAKE_VERSION}\",        \"The version of cmake used to configure.\");
  MAKE_MAP_ENTRY( \"CMAKE_GENERATOR\",       \"${CMAKE_GENERATOR}\",      \"The generator used for building.\"        );
  MAKE_MAP_ENTRY( \"CMAKE_BUILD_TYPE\",      \"${CMAKE_BUILD_TYPE}\",     \"The build type requested.\"               );

  MAKE_MAP_ENTRY( \"CMAKE_C_COMPILER_ID\",   \"${CMAKE_C_COMPILER_ID}\",  \"The C compiler ID used for building.\"    );
  MAKE_MAP_ENTRY( \"CMAKE_C_COMPILER_VERSION\",   \"${CMAKE_C_COMPILER_VERSION}\",
                                                                          \"The C compiler version used for building.\"    );
  MAKE_MAP_ENTRY( \"CMAKE_C_COMPILER\",      \"${CMAKE_C_COMPILER}\",     \"The C compiler.\"                       );
  MAKE_MAP_ENTRY( \"CMAKE_C_FLAGS\",         \"${CMAKE_C_FLAGS} ${CMAKE_CXX_FLAGS_${CMAKE_BUILD_TYPE}}\",
                                                                          \"The C compiler flags.\"                 );

  MAKE_MAP_ENTRY( \"CMAKE_CXX_STANDARD\",    \"${CMAKE_CXX_STANDARD}\",   \"The CXX standard specified.\"             );
  MAKE_MAP_ENTRY( \"CMAKE_CXX_COMPILER_ID\", \"${CMAKE_CXX_COMPILER_ID}\",\"The CXX compiler ID used for building.\"  );
  MAKE_MAP_ENTRY( \"CMAKE_CXX_COMPILER_VERSION\",   \"${CMAKE_CXX_COMPILER_VERSION}\",
                                                                          \"The CXX compiler version used for building.\"  );
  MAKE_MAP_ENTRY( \"CMAKE_CXX_COMPILER\",    \"${CMAKE_CXX_COMPILER}\",   \"The CXX compiler.\"                       );
  MAKE_MAP_ENTRY( \"CMAKE_CXX_FLAGS\",       \"${CMAKE_CXX_FLAGS} ${CMAKE_CXX_FLAGS_${CMAKE_BUILD_TYPE}}\",
                                                                          \"The CXX compiler flags.\"                 );
"
)

configure_file(
  itkBuildInformation.cxx.in
  ${ITKCommon_BINARY_DIR}/itkBuildInformation.cxx
)
#[COPYONLY] [ESCAPE_QUOTES] [@ONLY]
#[NEWLINE_STYLE [UNIX|DOS|WIN32|LF|CRLF] ])

set(
  ITKCommon_SRCS
  ${ITKCommon_BINARY_DIR}/itkBuildInformation.cxx
  itkCommonEnums.cxx
  itkMemoryProbesCollectorBase.cxx
  itkCreateObjectFunction.cxx
  itkLogger.cxx
  itkLogOutput.cxx
  itkLoggerOutput.cxx
  itkProgressAccumulator.cxx
  itkTotalProgressReporter.cxx
  itkNumericTraits.cxx
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
  itkProcessObject.cxx
  itkStreamingProcessObject.cxx
  itkSpatialOrientationAdapter.cxx
  itkRealTimeInterval.cxx
  itkOctreeNode.cxx
  itkNumericTraitsFixedArrayPixel.cxx
  itkMultiThreaderBase.cxx
  itkPlatformMultiThreader.cxx
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
  itkSingleton.cxx
  itkTetrahedronCellTopology.cxx
  itkThreadedIndexedContainerPartitioner.cxx
  itkObjectFactoryBase.cxx
  itkFloatingPointExceptions.cxx
  itkOutputWindow.cxx
  itkNumericTraitsDiffusionTensor3DPixel.cxx
  itkEquivalencyTable.cxx
  itkXMLFileOutputWindow.cxx
  itkXMLFilterWatcher.cxx
  itkStoppingCriterionBase.cxx
  itkCompensatedSummation.cxx
  itkArrayOutputSpecialization.cxx
  itkNumberToString.cxx
  itkRandomVariateGeneratorBase.cxx
  itkMath.cxx
  itkProgressTransformer.cxx
  itkSymmetricEigenAnalysis.cxx
  itkExtractImageFilter.cxx
  itkLoggerThreadWrapper.cxx
  itkFrustumSpatialFunction.cxx
  itkObjectStore.cxx
  itkGaussianDerivativeOperator.cxx
  itkSpatialOrientation.cxx
  itkAnatomicalOrientation.cxx
)

if(ITK_WRAP_PYTHON)
  list(APPEND ITKCommon_SRCS itkPyCommand.cxx)
  set_source_files_properties(
    itkPyCommand.cxx
    PROPERTIES
      INCLUDE_DIRECTORIES
        "$<TARGET_PROPERTY:Python3::Module,INTERFACE_INCLUDE_DIRECTORIES>"
  )
endif()

if(WIN32)
  list(APPEND ITKCommon_SRCS itkWin32OutputWindow.cxx)
endif()
if(ITK_USE_WIN32_THREADS OR ITK_USE_PTHREADS)
  list(
    APPEND
    ITKCommon_SRCS
    itkPoolMultiThreader.cxx
    itkThreadPool.cxx
  )
endif()

if(ITK_DYNAMIC_LOADING)
  list(APPEND ITKCommon_SRCS itkDynamicLoader.cxx)
endif()

if(ITK_USE_TBB)
  list(APPEND ITKCommon_SRCS itkTBBMultiThreader.cxx)
endif()

# Prevent optimization from affecting the floating point calculation.
if(CMAKE_CXX_COMPILER MATCHES "^icc")
  if(WIN32)
    set_source_files_properties(
      itkCompensatedSummation.cxx
      PROPERTIES
        COMPILE_FLAGS
          /fp
        precise
    )
  else()
    set_source_files_properties(
      itkCompensatedSummation.cxx
      PROPERTIES
        COMPILE_FLAGS
          -fp-model
        precise
    )
  endif()
elseif(CMAKE_COMPILER_IS_GNUCXX)
  # -fno-associative-math appears to only be present in GCC 4.3 onwards.
  include(CheckCXXCompilerFlag)
  check_cxx_compiler_flag(
    -fno-associative-math
    gcc_has_no_associative_math
  )
  if(gcc_has_no_associative_math)
    set_source_files_properties(
      itkCompensatedSummation.cxx
      PROPERTIES
        COMPILE_FLAGS
          -fno-associative-math
    )
  else()
    set_source_files_properties(
      itkCompensatedSummation.cxx
      PROPERTIES
        COMPILE_FLAGS
          -fno-unsafe-math-optimizations
    )
  endif()
elseif(MSVC)
  set_source_files_properties(
    itkCompensatedSummation.cxx
    PROPERTIES
      COMPILE_FLAGS
        -fp:precise
  )
endif()

### generating libraries
itk_module_add_library(ITKCommon ${ITKCommon_SRCS})

target_link_libraries(ITKCommon LINK_PUBLIC itksys)

if(UNIX)
  target_link_libraries(
    ITKCommon
    LINK_PUBLIC
      ${CMAKE_THREAD_LIBS}
      -lm
  )
  if(ITK_DYNAMIC_LOADING)
    target_link_libraries(ITKCommon LINK_PUBLIC ${CMAKE_DL_LIBS})
  endif()
endif()
if(ITK_WRAP_PYTHON)
  target_link_libraries(ITKCommon PRIVATE Python3::Python)
endif()

if(ITK_USE_TBB)
  # https://software.intel.com/en-us/node/506139
  # The macro TBB_DEPRECATED controls deprecated features.
  #      Define it to 0 to remove deprecated Intel® Threading Building Blocks (Intel® TBB) 2.1 interfaces.
  target_compile_definitions(ITKCommon PUBLIC TBB_DEPRECATED=0)
  # The macro TBB_USE_CAPTURED_EXCEPTION controls rethrow of exceptions within the library.
  #      Define TBB_USE_CAPTURED_EXCEPTION=0 to request rethrow of the exact exception.
  target_compile_definitions(ITKCommon PUBLIC TBB_USE_CAPTURED_EXCEPTION=0)
endif()
