# create a list of cable config files for wrapping
SET(WRAP_SOURCES
  wrap_ITKBase
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
SET(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} wrap_ITKCommonTclTcl.cxx)
SET(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} wrap_ITKCommonPythonPython.cxx)

CONFIGURE_FILE(
${WrapITK_SOURCE_DIR}/Master.mdx.in
${WrapITK_BINARY_DIR}/Common/ITKCommonTcl.mdx
)

# set the generated sources as generated
SET_SOURCE_FILES_PROPERTIES(${WRAP_TCL_SOURCES} ${WRAP_PYTHON_SOURCES} GENERATED )

IF(ITK_CSWIG_TCL)
  # add the library
  ADD_LIBRARY(ITKCommonTcl SHARED ${WRAP_TCL_SOURCES})
  TARGET_LINK_LIBRARIES(ITKCommonTcl ${TCL_LIBRARY} ITKCommon VXLNumericsTcl SwigRuntimeTcl )
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
  ADD_LIBRARY(_ITKCommonPython MODULE ${WRAP_PYTHON_SOURCES})
  SET_TARGET_PROPERTIES( _ITKCommonPython PROPERTIES PREFIX "")
  # link to ITKCommon
  TARGET_LINK_LIBRARIES(_ITKCommonPython ${PYTHON_LIBRARY} ITKCommon SwigRuntimePython)
ENDIF(ITK_CSWIG_PYTHON)

SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumericsTcl.mdx"
                       "${WrapITK_BINARY_DIR}/Common/ITKCommonTcl.mdx"
)

# wrap each of the files
FOREACH(Source ${WRAP_SOURCES})
  IF(ITK_CSWIG_TCL)
  # tcl
    WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/Common ${WrapITK_BINARY_DIR}/Common
                     ${Source} ITKCommonTcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
  ENDIF(ITK_CSWIG_TCL)
   
  IF(ITK_CSWIG_PYTHON)
  # python
    WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/Common ${WrapITK_BINARY_DIR}/Common
                        ${Source} _ITKCommonPython "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
  ENDIF(ITK_CSWIG_PYTHON)
ENDFOREACH(Source)

IF(ITK_CSWIG_TCL)
# tcl
  WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/Common ${WrapITK_BINARY_DIR}/Common
                   wrap_ITKCommonTcl ITKCommonTcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
ENDIF(ITK_CSWIG_TCL)
   
IF(ITK_CSWIG_PYTHON)
# python
  WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/Common ${WrapITK_BINARY_DIR}/Common
                      wrap_ITKCommonPython _ITKCommonPython "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
ENDIF(ITK_CSWIG_PYTHON)