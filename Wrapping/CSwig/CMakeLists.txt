PROJECT(WrapITK)

OPTION(ITK_SWRAP_TCL "Wrap with tcl" ON)
OPTION(ITK_WRAP_PYTHON "Wrap with python" ON)

# find python and tcl
INCLUDE (${CMAKE_ROOT}/Modules/FindTCL.cmake)
INCLUDE (${CMAKE_ROOT}/Modules/FindPythonLibs.cmake)

IF(CSWIG)
  GET_FILENAME_COMPONENT(CSWIG_PATH ${CSWIG} PATH)
ENDIF(CSWIG)

IF(GCCXML)
  GET_FILENAME_COMPONENT(GCCXML_PATH ${GCCXML} PATH)
ENDIF(GCCXML)

IF(CABLE_INDEX)
  GET_FILENAME_COMPONENT(CABLE_INDEX_PATH ${CABLE_INDEX} PATH)
ENDIF(CABLE_INDEX)

# find cswig, gccxml, and cableidx programs
FIND_PROGRAM(CSWIG
  NAMES 
  cswig
  PATHS
  ${CSWIG_PATH} ${GCCXML_PATH} ${CABLE_INDEX_PATH}
  ${CYGWIN_INSTALL_PATH}/bin
  /bin
  /usr/bin 
  /usr/local/bin
)

FIND_PROGRAM(GCCXML
  NAMES 
  gccxml
  PATHS
  ${CSWIG_PATH} ${GCCXML_PATH} ${CABLE_INDEX_PATH}
  ${CYGWIN_INSTALL_PATH}/bin
  /bin
  /usr/bin 
  /usr/local/bin
)

FIND_PROGRAM(CABLE_INDEX
  NAMES 
  cableidx
  PATHS
  ${CSWIG_PATH} ${GCCXML_PATH} ${CABLE_INDEX_PATH}
  ${CYGWIN_INSTALL_PATH}/bin
  /bin
  /usr/bin 
  /usr/local/bin
)

SET(IGNORE_WARNINGS -w362 -w389 -w503 -w508 -w509)
# define macros for wrapping commands
MACRO(GCCXML_CREATE_XML_FILE Source Bin Input Output Library)
   ADD_CUSTOM_COMMAND(
   COMMENT "${Output} from "
   SOURCE ${Source}/${Input}
   COMMAND ${GCCXML}
   ARGS -fxml-start=_cable_  
        -DCSWIG -DCABLE_CONFIGURATION ${Source}/${Input} -fxml=${Bin}/${Output} ${SWIG_INC}
   TARGET ${Library}
   OUTPUTS ${Bin}/${Output} )
ENDMACRO(GCCXML_CREATE_XML_FILE)


MACRO(CINDEX_CREATE_IDX_FILE Bin Input Output Library)
   ADD_CUSTOM_COMMAND(
   COMMENT "${Output} from "
   SOURCE ${Bin}/${Input}
   COMMAND ${CABLE_INDEX}
   ARGS ${Bin}/${Input} ${Bin}/${Output}
   TARGET ${Library}
   OUTPUTS ${Bin}/${Output} 
   DEPENDS ${Bin}/${Input}
)
ENDMACRO(CINDEX_CREATE_IDX_FILE)

MACRO(CSWIG_CREATE_TCL_CXX_FILE Bin MasterIdx InputIdx InputXml OutputTclCxx Library LibraryIndexFiles)
   SET(CINDEX)
   FOREACH(MIDX ${MasterIdx})
     SET(CINDEX ${CINDEX} -Cindex "${MIDX}")
   ENDFOREACH(MIDX)
   ADD_CUSTOM_COMMAND(
     COMMENT "${OutputTclCxx} from "
     SOURCE ${Bin}/${InputIdx}
     COMMAND ${CSWIG}
     ARGS -c ${CINDEX} ${IGNORE_WARNINGS} 
          -o ${Bin}/${OutputTclCxx} -tcl -c++ ${Bin}/${InputXml}
     TARGET ${Library}
     OUTPUTS ${Bin}/${OutputTclCxx}
     DEPENDS ${LibraryIndexFiles} ${Bin}/${InputXml}
     )
ENDMACRO(CSWIG_CREATE_TCL_CXX_FILE)

MACRO(CSWIG_CREATE_PYTHON_CXX_FILE Bin MasterIdx InputIdx InputXml OutputTclCxx Library LibraryIndexFiles)
   SET(CINDEX)
   FOREACH(MIDX ${MasterIdx})
     SET(CINDEX ${CINDEX} -Cindex "${MIDX}")
   ENDFOREACH(MIDX)
   ADD_CUSTOM_COMMAND(
     COMMENT "${OutputTclCxx} from "
     SOURCE ${Bin}/${InputIdx}
     COMMAND ${CSWIG} 
     ARGS -c ${CINDEX} ${IGNORE_WARNINGS}
          -bindir "${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}" 
          -o ${Bin}/${OutputTclCxx} -python -c++ ${Bin}/${InputXml}
     TARGET ${Library}
     OUTPUTS ${Bin}/${OutputTclCxx}
     DEPENDS ${LibraryIndexFiles}  ${Bin}/${InputXml}
     )
ENDMACRO(CSWIG_CREATE_PYTHON_CXX_FILE)

# macro to create .xml, .idx and Tcl.cxx files
MACRO(WRAP_TCL_SOURCES Source Bin BaseName LibraryName MasterIdx LibraryIndexFiles)
   GCCXML_CREATE_XML_FILE(${Source} ${Bin} ${BaseName}.cxx ${BaseName}.xml ${LibraryName})
   CINDEX_CREATE_IDX_FILE(${Bin} ${BaseName}.xml  ${BaseName}.idx ${LibraryName})
   CSWIG_CREATE_TCL_CXX_FILE(${Bin} "${MasterIdx}" ${BaseName}.idx ${BaseName}.xml
                             ${BaseName}Tcl.cxx ${LibraryName} "${LibraryIndexFiles}")
ENDMACRO(WRAP_TCL_SOURCES)

# macro to create .xml, .idx and Python.cxx files
MACRO(WRAP_PYTHON_SOURCES Source Bin BaseName LibraryName MasterIdx LibraryIndexFiles)
   GCCXML_CREATE_XML_FILE(${Source} ${Bin} ${BaseName}.cxx ${BaseName}.xml ${LibraryName})
   CINDEX_CREATE_IDX_FILE(${Bin} ${BaseName}.xml  ${BaseName}.idx ${LibraryName})
   CSWIG_CREATE_PYTHON_CXX_FILE(${Bin} "${MasterIdx}" ${BaseName}.idx ${BaseName}.xml
                             ${BaseName}Python.cxx ${LibraryName} "${LibraryIndexFiles}")
ENDMACRO(WRAP_PYTHON_SOURCES)


# make sure required stuff is set
IF(NOT CSWIG)
   MESSAGE(SEND_ERROR "CSWIG Must be specified")
ENDIF(NOT CSWIG)
IF(NOT CABLE_INDEX)
   MESSAGE(SEND_ERROR "CABLE_INDEX Must be specified")
ENDIF(NOT CABLE_INDEX)
IF(NOT GCCXML)
   MESSAGE(SEND_ERROR "GCCXML Must be specified")
ENDIF(NOT GCCXML)


SET(ITK_TOP ${ITK_SOURCE_DIR})
INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH} ${TCL_INCLUDE_PATH})
FOREACH(INCDIR ${ITK_INCLUDE_DIRS_BUILD_TREE} ${ITK_INCLUDE_DIRS_BUILD_TREE_CXX} ${ITK_INCLUDE_DIRS_SYSTEM})
  SET(SWIG_INC ${SWIG_INC} -I${INCDIR})
ENDFOREACH(INCDIR)
SET(SWIG_INC ${SWIG_INC} -I${ITK_TOP}/Utilities/vxl/vnl)
SET(SWIG_INC ${SWIG_INC} -I${ITK_TOP}/Wrapping/CSwig)
SET(SWIG_INC ${SWIG_INC} -I${ITK_TOP}/Wrapping/CSwig/Common)
SET(SWIG_INC ${SWIG_INC} -I${ITK_TOP}/Wrapping/CSwig/VXLNumerics)
SET(SWIG_INC ${SWIG_INC} -I${ITK_TOP}/Wrapping/CSwig/Numerics)
SET(SWIG_INC ${SWIG_INC} -I${ITK_TOP}/Wrapping/CSwig/BasicFilters)
SET(SWIG_INC ${SWIG_INC} -I${ITK_TOP}/Wrapping/CSwig/IO)
SET(SWIG_INC ${SWIG_INC} -I${ITK_TOP}/Wrapping/CSwig/Algorithms)
ADD_DEFINITIONS(-DSWIG_GLOBAL)


SUBDIRS(SwigRuntime VXLNumerics Common Numerics BasicFilters IO Algorithms)
