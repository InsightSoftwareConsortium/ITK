# create the ITKBasicFiltersTcl libraries
SET(WRAP_SOURCES
  wrap_itkAnisotropicDiffusionImageFilter
  wrap_itkBinaryThresholdImageFilter
  wrap_itkCannyEdgeDetectionImageFilter
  wrap_itkCastImageFilter
  wrap_itkConfidenceConnectedImageFilter
  wrap_itkConnectedThresholdImageFilter
  wrap_itkCurvatureAnisotropicDiffusionImageFilter
  wrap_itkExpImageFilter
  wrap_itkExpNegativeImageFilter
  wrap_itkExtractImageFilter
  wrap_itkGradientAnisotropicDiffusionImageFilter
  wrap_itkGradientRecursiveGaussianImageFilter
  wrap_itkGradientMagnitudeRecursiveGaussianImageFilter
  wrap_itkDanielssonDistanceMapImageFilter
  wrap_itkLaplacianImageFilter
  wrap_itkMeanImageFilter
  wrap_itkMedianImageFilter
  wrap_itkMinimumMaximumImageCalculator
  wrap_itkNaryAddImageFilter
  wrap_itkRandomImageSource
  wrap_itkRecursiveGaussianImageFilter
  wrap_itkRecursiveSeparableImageFilter
  wrap_itkResampleImageFilter
  wrap_itkRescaleIntensityImageFilter
  wrap_itkShiftScaleImageFilter
  wrap_itkSigmoidImageFilter
  wrap_itkStatisticsImageFilter
  wrap_itkSobelEdgeDetectionImageFilter
  wrap_itkSmoothingRecursiveGaussianImageFilter
  wrap_itkTernaryMagnitudeImageFilter
  wrap_itkThresholdImageFilter
)

# loop over cable config files creating two lists:
# WRAP_TCL_SOURCES: list of generated files
FOREACH(Source ${WRAP_SOURCES})
   SET(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} ${Source}Tcl.cxx)
   SET(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} ${Source}Python.cxx)
   SET(ALL_IDX_FILES ${ALL_IDX_FILES} ${WrapITK_BINARY_DIR}/BasicFilters/${Source}.idx )
   SET(INDEX_FILE_CONTENT "${INDEX_FILE_CONTENT}${WrapITK_BINARY_DIR}/BasicFilters/${Source}.idx\n")
ENDFOREACH(Source)

SET(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} wrap_ITKBasicFiltersTclTcl.cxx)
SET(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} wrap_ITKBasicFiltersPythonPython.cxx)

# set the generated sources as generated
SET_SOURCE_FILES_PROPERTIES(${WRAP_TCL_SOURCES} ${WRAP_PYTHON_SOURCES} GENERATED )

IF(ITK_CSWIG_TCL)
  ADD_LIBRARY(ITKBasicFiltersTcl SHARED ${WRAP_TCL_SOURCES})
  TARGET_LINK_LIBRARIES(ITKBasicFiltersTcl VXLNumericsTcl ITKNumericsTcl ITKCommonTcl SwigRuntimeTcl ${TCL_LIBRARY})
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
  ADD_LIBRARY(_ITKBasicFiltersPython MODULE ${WRAP_PYTHON_SOURCES})
  ADD_DEPENDENCIES(_ITKBasicFiltersPython _ITKNumericsPython _ITKCommonPython)
  SET_TARGET_PROPERTIES( _ITKBasicFiltersPython PROPERTIES PREFIX "")
  TARGET_LINK_LIBRARIES(_ITKBasicFiltersPython ITKBasicFilters SwigRuntimePython ${PYTHON_LIBRARY})
ENDIF(ITK_CSWIG_PYTHON)


CONFIGURE_FILE(
${WrapITK_SOURCE_DIR}/Master.mdx.in
${WrapITK_BINARY_DIR}/BasicFilters/ITKBasicFilters.mdx IMMEDIATE
)

SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumericsTcl.mdx"
                       "${WrapITK_BINARY_DIR}/Numerics/ITKNumericsTcl.mdx"
                       "${WrapITK_BINARY_DIR}/Common/ITKCommonTcl.mdx"
                       "${WrapITK_BINARY_DIR}/BasicFilters/ITKBasicFilters.mdx"
)

FOREACH(Source ${WRAP_SOURCES})
  IF(ITK_CSWIG_TCL)
    WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/BasicFilters ${WrapITK_BINARY_DIR}/BasicFilters
                     ${Source} ITKBasicFiltersTcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    ENDIF(ITK_CSWIG_TCL)

  IF(ITK_CSWIG_PYTHON)
    WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/BasicFilters ${WrapITK_BINARY_DIR}/BasicFilters
                        ${Source} _ITKBasicFiltersPython "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
  ENDIF(ITK_CSWIG_PYTHON)
ENDFOREACH(Source)

IF(ITK_CSWIG_TCL)
  WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/BasicFilters ${WrapITK_BINARY_DIR}/BasicFilters
                   wrap_ITKBasicFiltersTcl ITKBasicFiltersTcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
ENDIF(ITK_CSWIG_TCL)
   
IF(ITK_CSWIG_PYTHON)
  WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/BasicFilters ${WrapITK_BINARY_DIR}/BasicFilters
                      wrap_ITKBasicFiltersPython _ITKBasicFiltersPython "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
ENDIF(ITK_CSWIG_PYTHON)
