# create the VXLNumerics libraries

SET(WRAP_SOURCES
  wrap_VXLNumerics
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
   SET(ALL_IDX_FILES ${ALL_IDX_FILES} ${WrapITK_BINARY_DIR}/VXLNumerics/${Source}.idx )
   SET(INDEX_FILE_CONTENT "${INDEX_FILE_CONTENT}${WrapITK_BINARY_DIR}/VXLNumerics/${Source}.idx\n")
ENDFOREACH(Source)

# set the generated sources as generated
SET_SOURCE_FILES_PROPERTIES(${WRAP_TCL_SOURCES} ${WRAP_PYTHON_SOURCES} GENERATED )
# add the tcl and python libraries

IF(ITK_SWRAP_TCL)
  ADD_LIBRARY(VXLNumericsTcl SHARED ${WRAP_TCL_SOURCES})
  TARGET_LINK_LIBRARIES(VXLNumericsTcl itkvnl itkvnl_algo SwigRuntimeTcl ${TCL_LIBRARY})
ENDIF(ITK_SWRAP_TCL)

IF(ITK_WRAP_PYTHON)
  ADD_LIBRARY(_VXLNumericsPython MODULE ${WRAP_PYTHON_SOURCES})
  SET_TARGET_PROPERTIES( _VXLNumericsPython PROPERTIES PREFIX "")
  TARGET_LINK_LIBRARIES(_VXLNumericsPython itkvnl itkvnl_algo  SwigRuntimePython ${PYTHON_LIBRARY})
ENDIF(ITK_WRAP_PYTHON)


CONFIGURE_FILE( 
${WrapITK_SOURCE_DIR}/Master.mdx.in
${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumericsTcl.mdx IMMEDIATE
)

SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumericsTcl.mdx")
FOREACH(Source ${WRAP_SOURCES})
# tcl
  IF(ITK_SWRAP_TCL)
    GCCXML_CREATE_XML_FILE(VXLNumericsTcl ${ITK_TOP}/Wrapping/CSwig/VXLNumerics 
                         ${WrapITK_BINARY_DIR}/VXLNumerics ${Source}.cxx ${Source}.xml VXLNumericsTcl)
    CINDEX_CREATE_IDX_FILE(${WrapITK_BINARY_DIR}/VXLNumerics ${Source}.xml ${Source}.idx VXLNumericsTcl)
    CSWIG_CREATE_TCL_CXX_FILE(${WrapITK_BINARY_DIR}/VXLNumerics
                            "${MASTER_INDEX_FILES}" ${Source}.idx
                            ${Source}.xml ${Source}Tcl.cxx VXLNumericsTcl
                            "${ALL_IDX_FILES}")
  ENDIF(ITK_SWRAP_TCL)
# python 
  IF(ITK_WRAP_PYTHON)
    GCCXML_CREATE_XML_FILE(VXLNumericsPython ${ITK_TOP}/Wrapping/CSwig/VXLNumerics 
                         ${WrapITK_BINARY_DIR}/VXLNumerics ${Source}.cxx ${Source}.xml _VXLNumericsPython)
    CINDEX_CREATE_IDX_FILE(${WrapITK_BINARY_DIR}/VXLNumerics ${Source}.xml ${Source}.idx _VXLNumericsPython)
    CSWIG_CREATE_PYTHON_CXX_FILE(${WrapITK_BINARY_DIR}/VXLNumerics 
                            "${MASTER_INDEX_FILES}" ${Source}.idx
                            ${Source}.xml ${Source}Python.cxx _VXLNumericsPython
                            "${ALL_IDX_FILES}")
  ENDIF(ITK_WRAP_PYTHON)
ENDFOREACH(Source)

