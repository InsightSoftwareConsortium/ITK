# create the ITKAlgorithmsTcl libraries
SET(WRAP_SOURCES
    wrap_ITKAlgorithms
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

# set the generated sources as generated
SET_SOURCE_FILES_PROPERTIES(${WRAP_TCL_SOURCES} ${WRAP_PYTHON_SOURCES} GENERATED )
IF(ITK_SWRAP_TCL)
  ADD_LIBRARY(ITKAlgorithmsTcl SHARED ${WRAP_TCL_SOURCES})
  TARGET_LINK_LIBRARIES(ITKAlgorithmsTcl ITKAlgorithms VXLNumericsTcl 
                        ITKNumericsTcl ITKCommonTcl SwigRuntimeTcl ${TCL_LIBRARY})
ENDIF(ITK_SWRAP_TCL)

IF(ITK_WRAP_PYTHON)
  ADD_LIBRARY(_ITKAlgorithmsTcl MODULE ${WRAP_PYTHON_SOURCES})
  SET_TARGET_PROPERTIES( _ITKAlgorithmsTcl PROPERTIES PREFIX "")
  TARGET_LINK_LIBRARIES(_ITKAlgorithmsTcl ITKAlgorithms SwigRuntimePython ${PYTHON_LIBRARY})
ENDIF(ITK_WRAP_PYTHON)

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
  IF(ITK_SWRAP_TCL)
# tcl
  GCCXML_CREATE_XML_FILE(ITKAlgorithmsTcl 
      ${ITK_TOP}/Wrapping/CSwig/Algorithms 
      ${WrapITK_BINARY_DIR}/Algorithms ${Source}.cxx ${Source}.xml 
      ITKAlgorithmsTcl)
  CINDEX_CREATE_IDX_FILE(${WrapITK_BINARY_DIR}/Algorithms ${Source}.xml ${Source}.idx ITKAlgorithmsTcl)
  CSWIG_CREATE_TCL_CXX_FILE(${WrapITK_BINARY_DIR}/Algorithms
                            "${MASTER_INDEX_FILES}" ${Source}.idx
                            ${Source}.xml ${Source}Tcl.cxx ITKAlgorithmsTcl
                            "${ALL_IDX_FILES}")
  ENDIF(ITK_SWRAP_TCL)
  IF(ITK_WRAP_PYTHON)
# python 
    GCCXML_CREATE_XML_FILE(ITKAlgorithmsPython 
           ${ITK_TOP}/Wrapping/CSwig/Algorithms 
           ${WrapITK_BINARY_DIR}/Algorithms ${Source}.cxx ${Source}.xml 
           _ITKAlgorithmsPython)
    CINDEX_CREATE_IDX_FILE(${WrapITK_BINARY_DIR}/Algorithms ${Source}.xml
                           ${Source}.idx _ITKAlgorithmsPython)
    CSWIG_CREATE_PYTHON_CXX_FILE(${WrapITK_BINARY_DIR}/Algorithms 
                            "${MASTER_INDEX_FILES}" ${Source}.idx
                            ${Source}.xml ${Source}Python.cxx 
                            _ITKAlgorithmsPython
                            "${ALL_IDX_FILES}")
  ENDIF(ITK_WRAP_PYTHON)
ENDFOREACH(Source)
