# create the ITKNumericsTcl libraries
SET(WRAP_SOURCES
  wrap_ITKNumerics
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

# set the generated sources as generated
SET_SOURCE_FILES_PROPERTIES(${WRAP_TCL_SOURCES} ${WRAP_PYTHON_SOURCES} GENERATED )

IF(ITK_SWRAP_TCL)
  ADD_LIBRARY(ITKNumericsTcl SHARED ${WRAP_TCL_SOURCES})
  TARGET_LINK_LIBRARIES(ITKNumericsTcl ITKNumerics ITKCommonTcl SwigRuntimeTcl ${TCL_LIBRARY})
ENDIF(ITK_SWRAP_TCL)

IF(ITK_WRAP_PYTHON)
  ADD_LIBRARY(_ITKNumericsPython MODULE ${WRAP_PYTHON_SOURCES})
  SET_TARGET_PROPERTIES( _ITKNumericsPython PROPERTIES PREFIX "")
  TARGET_LINK_LIBRARIES(_ITKNumericsPython ITKNumerics SwigRuntimePython ${PYTHON_LIBRARY})
ENDIF(ITK_WRAP_PYTHON)

CONFIGURE_FILE(
${WrapITK_SOURCE_DIR}/Master.mdx.in
${WrapITK_BINARY_DIR}/Numerics/ITKNumericsTcl.mdx IMMEDIATE
)

SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumericsTcl.mdx"
                       "${WrapITK_BINARY_DIR}/Numerics/ITKNumericsTcl.mdx"
                       "${WrapITK_BINARY_DIR}/Common/ITKCommonTcl.mdx"
)

FOREACH(Source ${WRAP_SOURCES})
  IF(ITK_SWRAP_TCL)
  # tcl
    GCCXML_CREATE_XML_FILE(ITKNumericsTcl ${ITK_TOP}/Wrapping/CSwig/Numerics 
                         ${WrapITK_BINARY_DIR}/Numerics ${Source}.cxx ${Source}.xml ITKNumericsTcl)
    CINDEX_CREATE_IDX_FILE(${WrapITK_BINARY_DIR}/Numerics ${Source}.xml ${Source}.idx ITKNumericsTcl)
    CSWIG_CREATE_TCL_CXX_FILE(${WrapITK_BINARY_DIR}/Numerics
                            "${MASTER_INDEX_FILES}" ${Source}.idx
                            ${Source}.xml ${Source}Tcl.cxx ITKNumericsTcl
                            "${ALL_IDX_FILES}")
  ENDIF(ITK_SWRAP_TCL)

  IF(ITK_WRAP_PYTHON)
# python
    GCCXML_CREATE_XML_FILE(ITKNumericsPython ${ITK_TOP}/Wrapping/CSwig/Numerics 
                         ${WrapITK_BINARY_DIR}/Numerics ${Source}.cxx ${Source}.xml _ITKNumericsPython)
    CINDEX_CREATE_IDX_FILE(${WrapITK_BINARY_DIR}/Numerics ${Source}.xml ${Source}.idx _ITKNumericsPython)
    CSWIG_CREATE_PYTHON_CXX_FILE(${WrapITK_BINARY_DIR}/Numerics 
                            "${MASTER_INDEX_FILES}" ${Source}.idx
                            ${Source}.xml ${Source}Python.cxx _ITKNumericsPython
                            "${ALL_IDX_FILES}")
   ENDIF(ITK_WRAP_PYTHON)
ENDFOREACH(Source)
