# create the ITKAlgorithmsTcl libraries
SET(WRAP_SOURCES
    wrap_itkCurvatureFlowImageFilter
    wrap_itkHistogramMatchingImageFilter
    wrap_itkImageRegistrationMethod
    wrap_itkImageToImageMetric
    wrap_itkMattesMutualInformationImageToImageMetric
    wrap_itkMeanSquaresImageToImageMetric
    wrap_itkMutualInformationImageToImageMetric
    wrap_itkNormalizedCorrelationImageToImageMetric
    wrap_itkOtsuThresholdImageCalculator
    wrap_itkPatternIntensityImageToImageMetric
    wrap_itkSimpleFuzzyConnectednessImageFilterBase
    wrap_itkSimpleFuzzyConnectednessScalarImageFilter
)

# loop over cable config files creating two lists:
# WRAP_TCL_SOURCES: list of generated files
FOREACH(Source ${WRAP_SOURCES})
   SET(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} ${Source}Tcl.cxx)
   SET(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} ${Source}Python.cxx)
   SET(ALL_IDX_FILES ${ALL_IDX_FILES} ${WrapITK_BINARY_DIR}/Algorithms/${Source}.idx )
   SET(INDEX_FILE_CONTENT "${INDEX_FILE_CONTENT}${WrapITK_BINARY_DIR}/Algorithms/${Source}.idx\n")
ENDFOREACH(Source)

SET(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} wrap_ITKAlgorithmsTclTcl.cxx)
SET(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} wrap_ITKAlgorithmsPythonPython.cxx)

# set the generated sources as generated
SET_SOURCE_FILES_PROPERTIES(${WRAP_TCL_SOURCES} ${WRAP_PYTHON_SOURCES} GENERATED )
IF(ITK_CSWIG_TCL)
  ADD_LIBRARY(ITKAlgorithmsTcl SHARED ${WRAP_TCL_SOURCES})
  TARGET_LINK_LIBRARIES(ITKAlgorithmsTcl ITKAlgorithms VXLNumericsTcl 
                        ITKBasicFiltersTcl ITKNumericsTcl ITKCommonTcl SwigRuntimeTcl ${TCL_LIBRARY})
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
  ADD_LIBRARY(_ITKAlgorithmsPython MODULE ${WRAP_PYTHON_SOURCES})
  IF(ITK_PYTHON_NEEDS_DEPENDS)
    ADD_DEPENDENCIES(_ITKAlgorithmsPython _ITKNumericsPython _ITKCommonPython _ITKBasicFiltersPython )
  ENDIF(ITK_PYTHON_NEEDS_DEPENDS)
  SET_TARGET_PROPERTIES( _ITKAlgorithmsPython PROPERTIES PREFIX "")
  TARGET_LINK_LIBRARIES(_ITKAlgorithmsPython ITKAlgorithms SwigRuntimePython ${PYTHON_LIBRARY})
ENDIF(ITK_CSWIG_PYTHON)

CONFIGURE_FILE(
${WrapITK_SOURCE_DIR}/Master.mdx.in
${WrapITK_BINARY_DIR}/Algorithms/ITKAlgorithms.mdx IMMEDIATE
)

SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumericsTcl.mdx"
                       "${WrapITK_BINARY_DIR}/Numerics/ITKNumericsTcl.mdx"
                       "${WrapITK_BINARY_DIR}/Common/ITKCommonTcl.mdx"
                       "${WrapITK_BINARY_DIR}/BasicFilters/ITKBasicFilters.mdx"
                       "${WrapITK_BINARY_DIR}/Algorithms/ITKAlgorithms.mdx"
)

FOREACH(Source ${WRAP_SOURCES})
  IF(ITK_CSWIG_TCL) 
     WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/Algorithms ${WrapITK_BINARY_DIR}/Algorithms
                     ${Source} ITKAlgorithmsTcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
  ENDIF(ITK_CSWIG_TCL)
  IF(ITK_CSWIG_PYTHON)
    WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/Algorithms ${WrapITK_BINARY_DIR}/Algorithms
                        ${Source} _ITKAlgorithmsPython "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
  ENDIF(ITK_CSWIG_PYTHON)
ENDFOREACH(Source)

IF(ITK_CSWIG_TCL)
  WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/Algorithms ${WrapITK_BINARY_DIR}/Algorithms
                   wrap_ITKAlgorithmsTcl ITKAlgorithmsTcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
ENDIF(ITK_CSWIG_TCL)
   
IF(ITK_CSWIG_PYTHON)
  WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/Algorithms ${WrapITK_BINARY_DIR}/Algorithms
                      wrap_ITKAlgorithmsPython _ITKAlgorithmsPython "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
ENDIF(ITK_CSWIG_PYTHON)