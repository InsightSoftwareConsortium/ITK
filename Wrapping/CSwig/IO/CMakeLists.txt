# create the ITKIOTcl libraries
SET(WRAP_SOURCES
    wrap_IOBase
    wrap_itkImageFileReader
    wrap_itkImageFileWriter
)

# loop over cable config files creating two lists:
# WRAP_TCL_SOURCES: list of generated files
FOREACH(Source ${WRAP_SOURCES})
   SET(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} ${Source}Tcl.cxx)
   SET(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} ${Source}Python.cxx)
   SET(ALL_IDX_FILES ${ALL_IDX_FILES} ${WrapITK_BINARY_DIR}/IO/${Source}.idx )
   SET(INDEX_FILE_CONTENT "${INDEX_FILE_CONTENT}${WrapITK_BINARY_DIR}/IO/${Source}.idx\n")
ENDFOREACH(Source)

SET(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} wrap_ITKIOTclTcl.cxx  wrap_itkTkImageViewer2DTcl.cxx)
SET(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} wrap_ITKIOPythonPython.cxx)

# set the generated sources as generated
SET_SOURCE_FILES_PROPERTIES(${WRAP_TCL_SOURCES} ${WRAP_PYTHON_SOURCES} GENERATED )

IF(ITK_CSWIG_TCL)
  SET(SWIG_INC ${SWIG_INC} -I${TCL_INCLUDE_PATH})
  ADD_LIBRARY(ITKIOTcl SHARED ${WRAP_TCL_SOURCES} itkTkImageViewer2D.cxx)
  TARGET_LINK_LIBRARIES(ITKIOTcl ITKIO VXLNumericsTcl ITKNumericsTcl ITKCommonTcl SwigRuntimeTcl ${TCL_LIBRARY} ${TK_LIBRARY})
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
  ADD_LIBRARY(_ITKIOPython MODULE ${WRAP_PYTHON_SOURCES})
  ADD_DEPENDENCIES(_ITKIOPython _VXLNumericsPython _ITKNumericsPython _ITKCommonPython)
  SET_TARGET_PROPERTIES( _ITKIOPython PROPERTIES PREFIX "")
  TARGET_LINK_LIBRARIES(_ITKIOPython ITKIO SwigRuntimePython ${PYTHON_LIBRARY})
ENDIF(ITK_CSWIG_PYTHON)

CONFIGURE_FILE(
${WrapITK_SOURCE_DIR}/Master.mdx.in
${WrapITK_BINARY_DIR}/IO/ITKIO.mdx IMMEDIATE
)

SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumericsTcl.mdx"
                       "${WrapITK_BINARY_DIR}/Numerics/ITKNumericsTcl.mdx"
                       "${WrapITK_BINARY_DIR}/Common/ITKCommonTcl.mdx"
                       "${WrapITK_BINARY_DIR}/BasicFilters/ITKBasicFilters.mdx"
                       "${WrapITK_BINARY_DIR}/IO/ITKIO.mdx"
)

FOREACH(Source ${WRAP_SOURCES})
  IF(ITK_CSWIG_TCL)
    WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/IO ${WrapITK_BINARY_DIR}/IO
                     ${Source} ITKIOTcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    ENDIF(ITK_CSWIG_TCL)

  IF(ITK_CSWIG_PYTHON)
    WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/IO ${WrapITK_BINARY_DIR}/IO
                        ${Source} _ITKIOPython "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
  ENDIF(ITK_CSWIG_PYTHON)
ENDFOREACH(Source)

# add tcl specific files
IF(ITK_CSWIG_TCL)
  WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/IO ${WrapITK_BINARY_DIR}/IO
                   wrap_ITKIOTcl ITKIOTcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
  WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/IO ${WrapITK_BINARY_DIR}/IO
                   wrap_itkTkImageViewer2D ITKIOTcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
ENDIF(ITK_CSWIG_TCL)
   
# add python specific files
IF(ITK_CSWIG_PYTHON)
  WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/IO ${WrapITK_BINARY_DIR}/IO
                      wrap_ITKIOPython _ITKIOPython "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
ENDIF(ITK_CSWIG_PYTHON)
