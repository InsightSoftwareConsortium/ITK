# create the ITKBasicFiltersTcl libraries
SET(WRAP_SOURCES
  wrap_ITKBasicFilters
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
  wrap_itkMinimumMaximumImageCalculator
  wrap_itkNaryAddImageFilter
  wrap_itkRandomImageSource
  wrap_itkRecursiveGaussianImageFilter
  wrap_itkRecursiveSeparableImageFilter
  wrap_itkResampleImageFilter
  wrap_itkRescaleIntensityImageFilter
  wrap_itkShiftScaleImageFilter
  wrap_itkStatisticsImageFilter
  wrap_itkSobelEdgeDetectionImageFilter
  wrap_itkTernaryMagnitudeImageFilter
)

# loop over cable config files creating two lists:
# WRAP_TCL_SOURCES: list of generated files
FOREACH(Source ${WRAP_SOURCES})
   SET(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} ${Source}Tcl.cxx)
   SET(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} ${Source}Python.cxx)
   SET(ALL_IDX_FILES ${ALL_IDX_FILES} ${WrapITK_BINARY_DIR}/BasicFilters/${Source}.idx )
   SET(INDEX_FILE_CONTENT "${INDEX_FILE_CONTENT}${WrapITK_BINARY_DIR}/BasicFilters/${Source}.idx\n")
ENDFOREACH(Source)

# set the generated sources as generated
SET_SOURCE_FILES_PROPERTIES(${WRAP_TCL_SOURCES} ${WRAP_PYTHON_SOURCES} GENERATED )

IF(ITK_SWRAP_TCL)
  ADD_LIBRARY(ITKBasicFiltersTcl SHARED ${WRAP_TCL_SOURCES})
  TARGET_LINK_LIBRARIES(ITKBasicFiltersTcl VXLNumericsTcl ITKNumericsTcl ITKCommonTcl SwigRuntimeTcl ${TCL_LIBRARY})
ENDIF(ITK_SWRAP_TCL)

IF(ITK_WRAP_PYTHON)
  ADD_LIBRARY(_ITKBasicFiltersPython MODULE ${WRAP_PYTHON_SOURCES})
  SET_TARGET_PROPERTIES( _ITKBasicFiltersPython PROPERTIES PREFIX "")
  TARGET_LINK_LIBRARIES(_ITKBasicFiltersPython ITKBasicFilters SwigRuntimePython ${PYTHON_LIBRARY})
ENDIF(ITK_WRAP_PYTHON)


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
  IF(ITK_SWRAP_TCL)
#tcl
    GCCXML_CREATE_XML_FILE(ITKBasicFiltersTcl ${ITK_TOP}/Wrapping/CSwig/BasicFilters 
                         ${WrapITK_BINARY_DIR}/BasicFilters ${Source}.cxx ${Source}.xml ITKBasicFiltersTcl)
    CINDEX_CREATE_IDX_FILE(${WrapITK_BINARY_DIR}/BasicFilters ${Source}.xml ${Source}.idx ITKBasicFiltersTcl)
    CSWIG_CREATE_TCL_CXX_FILE(${WrapITK_BINARY_DIR}/BasicFilters
                            "${MASTER_INDEX_FILES}" ${Source}.idx
                            ${Source}.xml ${Source}Tcl.cxx ITKBasicFiltersTcl
                            "${ALL_IDX_FILES}")
  ENDIF(ITK_SWRAP_TCL)

  IF(ITK_WRAP_PYTHON)
# python
    GCCXML_CREATE_XML_FILE(ITKBasicFiltersPython ${ITK_TOP}/Wrapping/CSwig/BasicFilters 
                         ${WrapITK_BINARY_DIR}/BasicFilters ${Source}.cxx ${Source}.xml _ITKBasicFiltersPython)
    CINDEX_CREATE_IDX_FILE(${WrapITK_BINARY_DIR}/BasicFilters ${Source}.xml ${Source}.idx _ITKBasicFiltersPython)
    CSWIG_CREATE_PYTHON_CXX_FILE(${WrapITK_BINARY_DIR}/BasicFilters 
                            "${MASTER_INDEX_FILES}" ${Source}.idx
                            ${Source}.xml ${Source}Python.cxx _ITKBasicFiltersPython
                            "${ALL_IDX_FILES}")
  ENDIF(ITK_WRAP_PYTHON)
ENDFOREACH(Source)
