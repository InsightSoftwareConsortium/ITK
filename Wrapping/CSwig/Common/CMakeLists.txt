# create a list of cable config files for wrapping
SET(WRAP_SOURCES
  wrap_ITKBase
  wrap_ITKCommon
  wrap_ITKFilterBase
  wrap_ITKInterpolators
  wrap_ITKRegions
  wrap_ITKTransforms
  wrap_itkArray
  wrap_itkContinuousIndex
  wrap_itkDenseFiniteDifferenceImageFilter
  wrap_itkDifferenceImageFilter
  wrap_itkEventObject
  wrap_itkFiniteDifferenceImageFilter
  wrap_itkFixedArray
  wrap_itkImage
  wrap_itkIndex
  wrap_itkPoint
  wrap_itkSize
  wrap_itkVector
)

SET(INDEX_FILE_CONTENT "")

# loop over cable config files creating two lists:
# WRAP_TCL_SOURCES: list of generated files
FOREACH(Source ${WRAP_SOURCES})
   SET(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} ${Source}Tcl.cxx)
   SET(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} ${Source}Python.cxx)
   SET(ALL_IDX_FILES ${ALL_IDX_FILES} ${WrapITK_BINARY_DIR}/Common/${Source}.idx )
   SET(INDEX_FILE_CONTENT "${INDEX_FILE_CONTENT}${WrapITK_BINARY_DIR}/Common/${Source}.idx\n")
ENDFOREACH(Source)

CONFIGURE_FILE(
${WrapITK_SOURCE_DIR}/Master.mdx.in
${WrapITK_BINARY_DIR}/Common/ITKCommonTcl.mdx
)

# set the generated sources as generated
SET_SOURCE_FILES_PROPERTIES(${WRAP_TCL_SOURCES} ${WRAP_PYTHON_SOURCES} GENERATED )

IF(ITK_SWRAP_TCL)
  # add the library
  ADD_LIBRARY(ITKCommonTcl SHARED ${WRAP_TCL_SOURCES})
  TARGET_LINK_LIBRARIES(ITKCommonTcl ${TCL_LIBRARY} ITKCommon VXLNumericsTcl SwigRuntimeTcl )
ENDIF(ITK_SWRAP_TCL)

IF(ITK_WRAP_PYTHON)
  ADD_LIBRARY(_ITKCommonPython MODULE ${WRAP_PYTHON_SOURCES})
  SET_TARGET_PROPERTIES( _ITKCommonPython PROPERTIES PREFIX "")
  # link to ITKCommon
  TARGET_LINK_LIBRARIES(_ITKCommonPython ${PYTHON_LIBRARY} ITKCommon SwigRuntimePython)
ENDIF(ITK_WRAP_PYTHON)

SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumericsTcl.mdx"
                       "${WrapITK_BINARY_DIR}/Common/ITKCommonTcl.mdx"
)

# wrap each of the files
FOREACH(Source ${WRAP_SOURCES})
  IF(ITK_SWRAP_TCL)
  # tcl
    GCCXML_CREATE_XML_FILE(ITKCommonTcl ${ITK_TOP}/Wrapping/CSwig/Common 
                         ${WrapITK_BINARY_DIR}/Common ${Source}.cxx ${Source}.xml ITKCommonTcl)
    CINDEX_CREATE_IDX_FILE(${WrapITK_BINARY_DIR}/Common ${Source}.xml ${Source}.idx ITKCommonTcl)
    CSWIG_CREATE_TCL_CXX_FILE(${WrapITK_BINARY_DIR}/Common 
                            "${MASTER_INDEX_FILES}" ${Source}.idx
                            ${Source}.xml ${Source}Tcl.cxx ITKCommonTcl
                            "${ALL_IDX_FILES}")
  ENDIF(ITK_SWRAP_TCL)
   
  IF(ITK_WRAP_PYTHON)
  # python
    GCCXML_CREATE_XML_FILE(ITKCommonPython ${ITK_TOP}/Wrapping/CSwig/Common 
                         ${WrapITK_BINARY_DIR}/Common ${Source}.cxx ${Source}.xml _ITKCommonPython)
    CINDEX_CREATE_IDX_FILE(${WrapITK_BINARY_DIR}/Common ${Source}.xml ${Source}.idx _ITKCommonPython)
    CSWIG_CREATE_PYTHON_CXX_FILE(${WrapITK_BINARY_DIR}/Common 
                            "${MASTER_INDEX_FILES}" ${Source}.idx
                            ${Source}.xml ${Source}Python.cxx _ITKCommonPython
                            "${ALL_IDX_FILES}")
  ENDIF(ITK_WRAP_PYTHON)
ENDFOREACH(Source)

