# create the ITKNumericsTcl libraries
SET(WRAP_SOURCES
  wrap_ITKCostFunctions
  wrap_ITKOptimizers
)

# loop over cable config files creating two lists:
# WRAP_TCL_SOURCES: list of generated files
FOREACH(Source ${WRAP_SOURCES})
   SET(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} ${Source}Tcl.cxx)
   SET(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} ${Source}Python.cxx)
   SET(ALL_IDX_FILES ${ALL_IDX_FILES} ${WrapITK_BINARY_DIR}/Numerics/${Source}.idx )
   SET(INDEX_FILE_CONTENT "${INDEX_FILE_CONTENT}${WrapITK_BINARY_DIR}/Numerics/${Source}.idx\n")
ENDFOREACH(Source)

SET(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} wrap_ITKNumericsTclTcl.cxx)
SET(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} wrap_ITKNumericsPythonPython.cxx)

# set the generated sources as generated
SET_SOURCE_FILES_PROPERTIES(${WRAP_TCL_SOURCES} ${WRAP_PYTHON_SOURCES} GENERATED )

IF(ITK_CSWIG_TCL)
  ADD_LIBRARY(ITKNumericsTcl SHARED ${WRAP_TCL_SOURCES})
  TARGET_LINK_LIBRARIES(ITKNumericsTcl ITKNumerics ITKCommonTcl SwigRuntimeTcl ${TCL_LIBRARY})
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
  ADD_LIBRARY(_ITKNumericsPython MODULE ${WRAP_PYTHON_SOURCES})
  IF(ITK_PYTHON_NEEDS_DEPENDS)
    ADD_DEPENDENCIES(_ITKNumericsPython _ITKCommonPython)
  ENDIF(ITK_PYTHON_NEEDS_DEPENDS)
  SET_TARGET_PROPERTIES( _ITKNumericsPython PROPERTIES PREFIX "")
  TARGET_LINK_LIBRARIES(_ITKNumericsPython ITKNumerics SwigRuntimePython ${PYTHON_LIBRARY})
ENDIF(ITK_CSWIG_PYTHON)

CONFIGURE_FILE(
${WrapITK_SOURCE_DIR}/Master.mdx.in
${WrapITK_BINARY_DIR}/Numerics/ITKNumericsTcl.mdx IMMEDIATE
)

SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumericsTcl.mdx"
                       "${WrapITK_BINARY_DIR}/Numerics/ITKNumericsTcl.mdx"
                       "${WrapITK_BINARY_DIR}/Common/ITKCommonTcl.mdx"
)

FOREACH(Source ${WRAP_SOURCES})
  IF(ITK_CSWIG_TCL)
  # tcl 
    WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/Numerics ${WrapITK_BINARY_DIR}/Numerics
                     ${Source} ITKNumericsTcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
  ENDIF(ITK_CSWIG_TCL)

  IF(ITK_CSWIG_PYTHON)
# python
    WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/Numerics ${WrapITK_BINARY_DIR}/Numerics
                        ${Source} _ITKNumericsPython "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
   ENDIF(ITK_CSWIG_PYTHON)
ENDFOREACH(Source)



IF(ITK_CSWIG_TCL)
  # tcl 
    WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/Numerics ${WrapITK_BINARY_DIR}/Numerics
                     wrap_ITKNumericsTcl ITKNumericsTcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
# python
    WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/Numerics ${WrapITK_BINARY_DIR}/Numerics
                        wrap_ITKNumericsPython _ITKNumericsPython "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
ENDIF(ITK_CSWIG_PYTHON)