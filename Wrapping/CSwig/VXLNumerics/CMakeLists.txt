# create the VXLNumerics libraries

SET(WRAP_SOURCES
  wrap_vnl_matrix
  wrap_vnl_vector
  wrap_vnl_c_vector
  wrap_vnl_diag_matrix
  wrap_vnl_file_matrix
  wrap_vnl_file_vector
  wrap_vnl_fortran_copy
  wrap_vnl_matrix_fixed
  wrap_vnl_matrix_fixed_ref
  wrap_vnl_matrix_ref
  wrap_vnl_vector_ref
)

# loop over cable config files creating two lists:
# WRAP_TCL_SOURCES: list of generated files
FOREACH(Source ${WRAP_SOURCES})
   SET(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} ${Source}Tcl.cxx)
   SET(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} ${Source}Python.cxx)
   SET(WRAP_JAVA_SOURCES ${WRAP_JAVA_SOURCES} ${Source}Java.cxx )
   SET(ALL_IDX_FILES ${ALL_IDX_FILES} ${WrapITK_BINARY_DIR}/VXLNumerics/${Source}.idx )
   SET(INDEX_FILE_CONTENT "${INDEX_FILE_CONTENT}${WrapITK_BINARY_DIR}/VXLNumerics/${Source}.idx\n")
ENDFOREACH(Source)
SET(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} wrap_VXLNumericsTclTcl.cxx)
SET(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} wrap_VXLNumericsPythonPython.cxx)
SET(WRAP_JAVA_SOURCES ${WRAP_JAVA_SOURCES} wrap_VXLNumericsJavaJava.cxx)

# set the generated sources as generated
SET_SOURCE_FILES_PROPERTIES(${WRAP_TCL_SOURCES} ${WRAP_PYTHON_SOURCES} ${WRAP_JAVA_SOURCES} GENERATED )
# add the tcl and python libraries

IF(ITK_CSWIG_TCL)
  ADD_LIBRARY(VXLNumericsTcl SHARED ${WRAP_TCL_SOURCES})
  TARGET_LINK_LIBRARIES(VXLNumericsTcl itkvnl itkvnl_algo SwigRuntimeTcl ${TCL_LIBRARY})
  SET_TARGET_PROPERTIES(VXLNumericsTcl PROPERTIES LINK_FLAGS "${CSWIG_EXTRA_LINKFLAGS}")
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
  ADD_LIBRARY(_VXLNumericsPython MODULE ${WRAP_PYTHON_SOURCES})
  SET_TARGET_PROPERTIES( _VXLNumericsPython PROPERTIES PREFIX "")
  TARGET_LINK_LIBRARIES(_VXLNumericsPython itkvnl itkvnl_algo  SwigRuntimePython ${PYTHON_LIBRARY})
ENDIF(ITK_CSWIG_PYTHON)

IF(ITK_CSWIG_JAVA)
  ADD_LIBRARY(VXLNumericsJava MODULE ${WRAP_JAVA_SOURCES})
  TARGET_LINK_LIBRARIES(VXLNumericsJava itkvnl itkvnl_algo  SwigRuntimeJava ${JAVA_LIBRARY})
ENDIF(ITK_CSWIG_JAVA)

CONFIGURE_FILE( 
${WrapITK_SOURCE_DIR}/Master.mdx.in
${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumericsTcl.mdx IMMEDIATE
)

SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumericsTcl.mdx")

# wrap each source in WRAP_SOURCES for tcl and python
FOREACH(Source ${WRAP_SOURCES})
  # tcl
  IF(ITK_CSWIG_TCL)
    WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/VXLNumerics ${WrapITK_BINARY_DIR}/VXLNumerics
                     ${Source} VXLNumericsTcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
  ENDIF(ITK_CSWIG_TCL)
  # python 
  IF(ITK_CSWIG_PYTHON)
    WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/VXLNumerics ${WrapITK_BINARY_DIR}/VXLNumerics
                     ${Source} _VXLNumericsPython "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
  ENDIF(ITK_CSWIG_PYTHON)

  # java 
  IF(ITK_CSWIG_JAVA)
    WRAP_JAVA_SOURCES(${ITK_TOP}/Wrapping/CSwig/VXLNumerics ${WrapITK_BINARY_DIR}/VXLNumerics
                     ${Source} VXLNumericsJava "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
  ENDIF(ITK_CSWIG_JAVA)
ENDFOREACH(Source)

# add package wrappers for tcl and python

IF(ITK_CSWIG_TCL)
   WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/VXLNumerics ${WrapITK_BINARY_DIR}/VXLNumerics
                    wrap_VXLNumericsTcl VXLNumericsTcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
   WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/VXLNumerics ${WrapITK_BINARY_DIR}/VXLNumerics
                     wrap_VXLNumericsPython _VXLNumericsPython "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
ENDIF(ITK_CSWIG_PYTHON)

IF(ITK_CSWIG_JAVA)
   WRAP_JAVA_SOURCES(${ITK_TOP}/Wrapping/CSwig/VXLNumerics ${WrapITK_BINARY_DIR}/VXLNumerics
                     wrap_VXLNumericsJava VXLNumericsJava "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
ENDIF(ITK_CSWIG_JAVA)
