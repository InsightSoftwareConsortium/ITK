PROJECT(WrapITK)

SET(ITK_PYTHON_NEEDS_DEPENDS 1)
IF(${CMAKE_MAKE_PROGRAM} MATCHES make)
  SET(ITK_PYTHON_NEEDS_DEPENDS 0)
ENDIF(${CMAKE_MAKE_PROGRAM} MATCHES make)

# find python and tcl
IF(ITK_CSWIG_PYTHON)
  INCLUDE (${CMAKE_ROOT}/Modules/FindPythonLibs.cmake)
ENDIF(ITK_CSWIG_PYTHON)

IF(ITK_CSWIG_JAVA)
  INCLUDE (${CMAKE_ROOT}/Modules/FindJNI.cmake)
ENDIF(ITK_CSWIG_JAVA)

#-----------------------------------------------------------------------------
# Configure Tcl support.
IF(ITK_CSWIG_TCL)
  INCLUDE(${CMAKE_ROOT}/Modules/FindTCL.cmake)

  # Hide useless settings provided by FindTCL.
  FOREACH(entry TCL_LIBRARY_DEBUG
                TK_LIBRARY_DEBUG
                TCL_STUB_LIBRARY
                TCL_STUB_LIBRARY_DEBUG
                TK_STUB_LIBRARY
                TK_STUB_LIBRARY_DEBUG
                TK_WISH)
    SET(${entry} "${${entry}}" CACHE INTERNAL "This value is not used by ITK.")
  ENDFOREACH(entry)
ENDIF(ITK_CSWIG_TCL)

FIND_PROGRAM(PYTHON_EXECUTABLE
  NAMES python2.3 python2.2 python2.1 python2.0 python1.6 python1.5 python
  PATHS
  [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.3\\InstallPath]
  [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.2\\InstallPath]
  [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.1\\InstallPath]
  [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.0\\InstallPath]
  [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\1.6\\InstallPath]
  [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\1.5\\InstallPath]
)
MARK_AS_ADVANCED(PYTHON_EXECUTABLE)

IF(CSWIG)
  GET_FILENAME_COMPONENT(CSWIG_PATH ${CSWIG} PATH)
ENDIF(CSWIG)

IF(GCCXML)
  GET_FILENAME_COMPONENT(GCCXML_PATH ${GCCXML} PATH)
ENDIF(GCCXML)

IF(CABLE_INDEX)
  GET_FILENAME_COMPONENT(CABLE_INDEX_PATH ${CABLE_INDEX} PATH)
ENDIF(CABLE_INDEX)


# if any one of the three are set, then look for the others
# in the same directory
# also look in the registry entries for cmake last build
# so if CableSwig was built on the same machine recently
# it can be found automatically
SET(CMAKE_SEARCH_PATHS
    ${CSWIG_PATH} ${GCCXML_PATH} ${CABLE_INDEX_PATH} 
    [HKEY_CURRENT_USER\\Software\\Kitware\\CMakeSetup\\Settings\\StartPath;WhereBuild1]/bin
    [HKEY_CURRENT_USER\\Software\\Kitware\\CMakeSetup\\Settings\\StartPath;WhereBuild2]/bin
    [HKEY_CURRENT_USER\\Software\\Kitware\\CMakeSetup\\Settings\\StartPath;WhereBuild3]/bin
    [HKEY_CURRENT_USER\\Software\\Kitware\\CMakeSetup\\Settings\\StartPath;WhereBuild4]/bin
    [HKEY_CURRENT_USER\\Software\\Kitware\\CMakeSetup\\Settings\\StartPath;WhereBuild5]/bin
    [HKEY_CURRENT_USER\\Software\\Kitware\\CMakeSetup\\Settings\\StartPath;WhereBuild6]/bin
    [HKEY_CURRENT_USER\\Software\\Kitware\\CMakeSetup\\Settings\\StartPath;WhereBuild7]/bin
    [HKEY_CURRENT_USER\\Software\\Kitware\\CMakeSetup\\Settings\\StartPath;WhereBuild8]/bin
    [HKEY_CURRENT_USER\\Software\\Kitware\\CMakeSetup\\Settings\\StartPath;WhereBuild9]/bin
    [HKEY_CURRENT_USER\\Software\\Kitware\\CMakeSetup\\Settings\\StartPath;WhereBuild10]/bin
)
FOREACH(DIR ${CMAKE_SEARCH_PATHS} )
   SET(CMAKE_SEARCH_PATHS ${CMAKE_SEARCH_PATHS} ${DIR}/Debug)
   SET(CMAKE_SEARCH_PATHS ${CMAKE_SEARCH_PATHS} ${DIR}/Release)
   SET(CMAKE_SEARCH_PATHS ${CMAKE_SEARCH_PATHS} ${DIR}/RelWithDebInfo)
   SET(CMAKE_SEARCH_PATHS ${CMAKE_SEARCH_PATHS} ${DIR}/MinSizeRel)
ENDFOREACH(DIR)


# find cswig, gccxml, and cableidx programs
FIND_PROGRAM(CSWIG
  NAMES 
  cswig
  PATHS
  ${CMAKE_SEARCH_PATHS}
  ${CYGWIN_INSTALL_PATH}/bin
  /bin
  /usr/bin 
  /usr/local/bin
)

FIND_PROGRAM(GCCXML
  NAMES 
  gccxml
  PATHS
  ${CMAKE_SEARCH_PATHS}
  ${CYGWIN_INSTALL_PATH}/bin
  /bin
  /usr/bin 
  /usr/local/bin
)

FIND_PROGRAM(CABLE_INDEX
  NAMES 
  cableidx
  PATHS
  ${CMAKE_SEARCH_PATHS}
  ${CYGWIN_INSTALL_PATH}/bin
  /bin
  /usr/bin 
  /usr/local/bin
)

SET(IGNORE_WARNINGS -w362 -w389 -w503 -w508 -w509)
# define macros for wrapping commands
MACRO(GCCXML_CREATE_XML_FILE Source Bin Input Output Library)
# if the make program is not an IDE then include
# the depend file in a way that will make cmake 
# re-run if it changes
   IF(${CMAKE_MAKE_PROGRAM} MATCHES "make")
     IF(EXISTS ${Bin}/${Output}.depend)
     ELSE(EXISTS ${Bin}/${Output}.depend)
        WRITE_FILE(${Bin}/${Output}.depend "")
     ENDIF(EXISTS ${Bin}/${Output}.depend)
     INCLUDE(${Bin}/${Output}.depend)
   ELSE(${CMAKE_MAKE_PROGRAM} MATCHES "make")
# for IDE generators like MS dev only include the depend files
# if they exist.   This is to prevent ecessive reloading of
# workspaces after each build.   This also means
# that the depends will not be correct until cmake
# is run once after the build has completed once.
# the depend files are created in the wrap tcl/python sections
# when the .xml file is parsed.
     INCLUDE(${Bin}/${Output}.depend OPTIONAL)
   ENDIF(${CMAKE_MAKE_PROGRAM} MATCHES "make")

   ADD_CUSTOM_COMMAND(
   COMMENT "${Output} from "
   SOURCE ${Source}/${Input}
   COMMAND ${GCCXML}
   ARGS -fxml-start=_cable_  
        -DCSWIG -DCABLE_CONFIGURATION ${Source}/${Input} -fxml=${Bin}/${Output} ${SWIG_INC}
   TARGET ${Library}
   OUTPUTS ${Bin}/${Output}
   DEPENDS ${GCCXML} ${CABLE_SWIG_DEPEND})
ENDMACRO(GCCXML_CREATE_XML_FILE)


MACRO(CINDEX_CREATE_IDX_FILE Bin Input Output Library)
   ADD_CUSTOM_COMMAND(
   COMMENT "${Output} from "
   SOURCE ${Bin}/${Input}
   COMMAND ${CABLE_INDEX}
   ARGS ${Bin}/${Input} ${Bin}/${Output}
   TARGET ${Library}
   OUTPUTS ${Bin}/${Output} 
   DEPENDS ${Bin}/${Input} ${CABLE_INDEX}
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
     ARGS -l${ITK_TOP}/Wrapping/CSwig/itk.swg
          -c ${CINDEX} ${IGNORE_WARNINGS} -depend ${Bin}/${InputXml}.depend
          -o ${Bin}/${OutputTclCxx} -tcl -pkgversion "${ITK_VERSION_STRING}" -c++ ${Bin}/${InputXml}
     TARGET ${Library}
     OUTPUTS ${Bin}/${OutputTclCxx}
     DEPENDS ${LibraryIndexFiles} ${Bin}/${InputXml} ${CSWIG} )
#     MESSAGE("depends are ${CABLE_SWIG_DEPEND}")
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
     ARGS -l${ITK_TOP}/Wrapping/CSwig/itk.swg
          -c ${CINDEX} ${IGNORE_WARNINGS} -depend ${Bin}/${InputXml}.depend
          -bindir "${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}" 
          -o ${Bin}/${OutputTclCxx} -python -c++ ${Bin}/${InputXml}
     TARGET ${Library}
     OUTPUTS ${Bin}/${OutputTclCxx}
     DEPENDS ${LibraryIndexFiles}  ${Bin}/${InputXml} ${CSWIG} )
ENDMACRO(CSWIG_CREATE_PYTHON_CXX_FILE)


MACRO(CSWIG_CREATE_JAVA_CXX_FILE Bin MasterIdx InputIdx InputXml OutputTclCxx Library LibraryIndexFiles)
   SET(CINDEX)
   FOREACH(MIDX ${MasterIdx})
     SET(CINDEX ${CINDEX} -Cindex "${MIDX}")
   ENDFOREACH(MIDX)
   ADD_CUSTOM_COMMAND(
     COMMENT "${OutputTclCxx} from "
     SOURCE ${Bin}/${InputIdx}
     COMMAND ${CSWIG} 
     ARGS -l${ITK_TOP}/Wrapping/CSwig/itk.swg
          -c ${CINDEX} ${IGNORE_WARNINGS} -depend ${Bin}/${InputXml}.depend
          -bindir "${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}" 
          -o ${Bin}/${OutputTclCxx} -java -c++ ${Bin}/${InputXml}
     TARGET ${Library}
     OUTPUTS ${Bin}/${OutputTclCxx}
     DEPENDS ${LibraryIndexFiles}  ${Bin}/${InputXml} ${CSWIG} )
ENDMACRO(CSWIG_CREATE_JAVA_CXX_FILE)


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

# macro to create .xml, .idx and Python.cxx files
MACRO(WRAP_JAVA_SOURCES Source Bin BaseName LibraryName MasterIdx LibraryIndexFiles)
   GCCXML_CREATE_XML_FILE(${Source} ${Bin} ${BaseName}.cxx ${BaseName}.xml ${LibraryName})
   CINDEX_CREATE_IDX_FILE(${Bin} ${BaseName}.xml  ${BaseName}.idx ${LibraryName})
   CSWIG_CREATE_JAVA_CXX_FILE(${Bin} "${MasterIdx}" ${BaseName}.idx ${BaseName}.xml
                             ${BaseName}Java.cxx ${LibraryName} "${LibraryIndexFiles}")
ENDMACRO(WRAP_JAVA_SOURCES)


# make sure required stuff is set
SET(CSWIG_MISSING_VALUES)
IF(NOT CSWIG)
   SET(CSWIG_MISSING_VALUES "${CSWIG_MISSING_VALUES} CSWIG ")
ENDIF(NOT CSWIG)
IF(NOT CABLE_INDEX)
   SET(CSWIG_MISSING_VALUES "${CSWIG_MISSING_VALUES} CABLE_INDEX ")
ENDIF(NOT CABLE_INDEX)
IF(NOT GCCXML)
   SET(CSWIG_MISSING_VALUES "${CSWIG_MISSING_VALUES} GCCXML ")
ENDIF(NOT GCCXML)
IF(CSWIG_MISSING_VALUES)
  MESSAGE(SEND_ERROR "To use cswig wrapping, CSWIG, CABLE_INDEX, and GCCXML executables must be specified.  If they are all in the same directory, only specifiy one of them, and then run cmake configure again and the others should be found.\nCurrently, you are missing the following:\n ${CSWIG_MISSING_VALUES}")
ENDIF(CSWIG_MISSING_VALUES)


SET(ITK_TOP ${ITK_SOURCE_DIR})
IF(ITK_CSWIG_PYTHON)
  INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH} )
ENDIF(ITK_CSWIG_PYTHON)
IF(ITK_CSWIG_TCL)
  INCLUDE_DIRECTORIES(${TCL_INCLUDE_PATH})
ENDIF(ITK_CSWIG_TCL)
IF(ITK_CSWIG_JAVA)
  INCLUDE_DIRECTORIES(${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${JAVA_AWT_INCLUDE_PATH})
ENDIF(ITK_CSWIG_JAVA)

FOREACH(INCDIR ${ITK_INCLUDE_DIRS_BUILD_TREE} ${ITK_INCLUDE_DIRS_BUILD_TREE_CXX} ${ITK_INCLUDE_DIRS_SYSTEM})
  SET(SWIG_INC ${SWIG_INC} -I${INCDIR})
ENDFOREACH(INCDIR)
SET(SWIG_INC ${SWIG_INC} -I${ITK_TOP}/Utilities/vxl/vnl)
SET(SWIG_INC ${SWIG_INC} -I${ITK_TOP}/Utilities/vxl/vcl)
SET(SWIG_INC ${SWIG_INC} -I${ITK_TOP}/Utilities/vxl/vcl/doc)
SET(SWIG_INC ${SWIG_INC} -I${ITK_TOP}/Wrapping/CSwig)
SET(SWIG_INC ${SWIG_INC} -I${ITK_TOP}/Wrapping/CSwig/Common)
SET(SWIG_INC ${SWIG_INC} -I${ITK_TOP}/Wrapping/CSwig/VXLNumerics)
SET(SWIG_INC ${SWIG_INC} -I${ITK_TOP}/Wrapping/CSwig/Numerics)
SET(SWIG_INC ${SWIG_INC} -I${ITK_TOP}/Wrapping/CSwig/BasicFilters)
SET(SWIG_INC ${SWIG_INC} -I${ITK_TOP}/Wrapping/CSwig/IO)
SET(SWIG_INC ${SWIG_INC} -I${ITK_TOP}/Wrapping/CSwig/Algorithms)
ADD_DEFINITIONS(-DSWIG_GLOBAL)


SUBDIRS(SwigRuntime VXLNumerics Common Numerics BasicFilters IO Algorithms Tests)

IF(UNIX)
  SET(ITK_CSWIG_LIBNAME_PREFIX "lib")
ELSE(UNIX)
  SET(ITK_CSWIG_LIBNAME_PREFIX "")
ENDIF(UNIX)

SET(ITK_CSWIG_PACKAGE_DIR_BUILD "${ITK_LIBRARY_PATH}")

#-----------------------------------------------------------------------------
# Configure pkgIndex.tcl for the build tree.
SET(ITK_CSWIG_CONFIGURATION_TYPES ${CMAKE_CONFIGURATION_TYPES})
SET(ITK_CSWIG_DATA_ROOT "${ITK_DATA_ROOT}")
SET(ITK_CSWIG_TEST_ROOT "${ITK_BINARY_DIR}/Testing")
SET(ITK_CSWIG_SCRIPT_DIR "${ITK_SOURCE_DIR}/Wrapping/CSwig/Tcl")

IF(ITK_CSWIG_CONFIGURATION_TYPES)
  FOREACH(config ${ITK_CSWIG_CONFIGURATION_TYPES})
    SET(ITK_CSWIG_PACKAGE_DIR "${ITK_CSWIG_PACKAGE_DIR_BUILD}/${config}")
    CONFIGURE_FILE("${ITK_SOURCE_DIR}/Wrapping/CSwig/Tcl/pkgIndex.tcl.in"
                   "${ITK_BINARY_DIR}/Wrapping/CSwig/Tcl/${config}/pkgIndex.tcl"
                   @ONLY IMMEDIATE)
  ENDFOREACH(config)
ELSE(ITK_CSWIG_CONFIGURATION_TYPES)
  SET(ITK_CSWIG_PACKAGE_DIR "${ITK_CSWIG_PACKAGE_DIR_BUILD}")
  CONFIGURE_FILE("${ITK_SOURCE_DIR}/Wrapping/CSwig/Tcl/pkgIndex.tcl.in"
                 "${ITK_BINARY_DIR}/Wrapping/CSwig/Tcl/pkgIndex.tcl"
                 @ONLY IMMEDIATE)
ENDIF(ITK_CSWIG_CONFIGURATION_TYPES)

#-----------------------------------------------------------------------------
# Configure pkgIndex.tcl for the install tree.

SET(ITK_CSWIG_SCRIPT_DIR "[file dirname [info script]]")
SET(ITK_CSWIG_PACKAGE_DIR "[file dirname [info script]]")
SET(ITK_CSWIG_DATA_ROOT "[file dirname [info script]]/Data")
SET(ITK_CSWIG_TEST_ROOT "<NO_DEFAULT>")
CONFIGURE_FILE("${ITK_SOURCE_DIR}/Wrapping/CSwig/Tcl/pkgIndex.tcl.in"
               "${ITK_BINARY_DIR}/Wrapping/CSwig/Tcl/InstallOnly/Hide/pkgIndex.tcl"
               IMMEDIATE @ONLY)
INSTALL_FILES(/lib/InsightToolkit FILES
              "${ITK_BINARY_DIR}/Wrapping/CSwig/Tcl/InstallOnly/Hide/pkgIndex.tcl")


SET(ITK_CSWIG_SCRIPT_DIR "'${ITK_BINARY_DIR}/Wrapping/CSwig/Python'")

IF(ITK_CSWIG_CONFIGURATION_TYPES)
  FOREACH(config ${ITK_CSWIG_CONFIGURATION_TYPES})
    SET(ITK_CSWIG_PACKAGE_DIR "'${ITK_CSWIG_PACKAGE_DIR_BUILD}/${config}'")
    CONFIGURE_FILE("${ITK_SOURCE_DIR}/Wrapping/CSwig/Python/itkbase.py.in"
                   "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/${config}/itkbase.py"
                   @ONLY IMMEDIATE)
  ENDFOREACH(config)
ELSE(ITK_CSWIG_CONFIGURATION_TYPES)
  SET(ITK_CSWIG_PACKAGE_DIR "'${ITK_CSWIG_PACKAGE_DIR_BUILD}'")
  CONFIGURE_FILE("${ITK_SOURCE_DIR}/Wrapping/CSwig/Python/itkbase.py.in"
                 "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/itkbase.py"
                 @ONLY IMMEDIATE)
ENDIF(ITK_CSWIG_CONFIGURATION_TYPES)

# Handle out-of-source builds correctly.
#
#  1. Create a list of Python files to be installed/copied.
#  2. Copy them to ITK_BINARY_DIR if it is different from ITK_SOURCE_DIR.
#  3. Use Python's compileall to compile the copied files.
#
# *** Step 1 has to be done carefully to avoid missing out files ***
IF(PYTHON_EXECUTABLE)
  ADD_CUSTOM_TARGET(itkpython_pyc ALL echo "...")

  # Make the necessary directories.
  MAKE_DIRECTORY(${ITK_BINARY_DIR}/Wrapping/CSwig/Python)

  # Now create a list of Python files.
  SET(ITK_PYTHON_FILES)

  # Wrapping/CSwig/Python/*.py
  SET(ITK_PYTHON_FILES 
    ${ITK_PYTHON_FILES}
    InsightToolkit
    itkalgorithms
    itkbasicfilters
    itkcommon
    itkio
    itknumerics
    vxlnumerics
    )
  # Done listing files.

  # Now copy these files if necessary.
  SET(ITK_PYTHON_SOURCE_FILES)
  SET(ITK_PYTHON_OUTPUT_FILES)
  IF(ITK_CSWIG_CONFIGURATION_TYPES)
    FOREACH(config ${ITK_CSWIG_CONFIGURATION_TYPES})
      FOREACH(file ${ITK_PYTHON_FILES})
        SET(src "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/${config}/${file}.py")
        SET(ITK_PYTHON_SOURCE_FILES ${ITK_PYTHON_SOURCE_FILES} ${src})
      ENDFOREACH(file)
    ENDFOREACH(config)
  ELSE(ITK_CSWIG_CONFIGURATION_TYPES)
    FOREACH(file ${ITK_PYTHON_FILES})
      SET(src "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/${file}.py")
      SET(ITK_PYTHON_SOURCE_FILES ${ITK_PYTHON_SOURCE_FILES} ${src})
    ENDFOREACH(file)
  ENDIF(ITK_CSWIG_CONFIGURATION_TYPES)

  IF ("${ITK_BINARY_DIR}" MATCHES "^${ITK_SOURCE_DIR}$")
    #MESSAGE("In source build -- no need to copy Python files.")
  ELSE ("${ITK_BINARY_DIR}" MATCHES "^${ITK_SOURCE_DIR}$")
    IF(ITK_CSWIG_CONFIGURATION_TYPES)
      FOREACH(file ${ITK_PYTHON_FILES})
        SET(src "${ITK_SOURCE_DIR}/Wrapping/CSwig/Python/${file}.py")
        SET(tgt "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/${CMAKE_CFG_INTDIR}/${file}.py")
        ADD_CUSTOM_COMMAND(SOURCE ${src}
                           COMMAND ${CMAKE_COMMAND}
                           ARGS -E copy ${src} ${tgt}
                           OUTPUTS ${tgt}
                           TARGET itkpython_pyc
                           COMMENT "source copy")
      ENDFOREACH(file)
    ELSE(ITK_CSWIG_CONFIGURATION_TYPES)
      FOREACH(file ${ITK_PYTHON_FILES})
        SET(src "${ITK_SOURCE_DIR}/Wrapping/CSwig/Python/${file}.py")
        SET(tgt "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/${file}.py")
        ADD_CUSTOM_COMMAND(SOURCE ${src}
 		         COMMAND ${CMAKE_COMMAND}
                          ARGS -E copy ${src} ${tgt}
                          OUTPUTS ${tgt}
 			 TARGET itkpython_pyc
                          COMMENT "source copy")
      ENDFOREACH(file)
    ENDIF(ITK_CSWIG_CONFIGURATION_TYPES)
  ENDIF ("${ITK_BINARY_DIR}" MATCHES "^${ITK_SOURCE_DIR}$")

  # Byte compile the Python files.
  WRITE_FILE(${ITK_BINARY_DIR}/Wrapping/CSwig/Python/compile_all_itk
        "import compileall\n"
        "compileall.compile_dir('${ITK_BINARY_DIR}/Wrapping/CSwig/Python')\n"
	"file = open('${ITK_BINARY_DIR}/Wrapping/CSwig/Python/itk_compile_complete', 'w')\n"
	"file.write('Done')\n")

  ADD_CUSTOM_COMMAND(
    SOURCE ${ITK_BINARY_DIR}/Wrapping/CSwig/Python/compile_all_itk
    COMMAND ${PYTHON_EXECUTABLE}
    ARGS ${ITK_BINARY_DIR}/Wrapping/CSwig/Python/compile_all_itk
    DEPENDS ${ITK_PYTHON_SOURCE_FILES}
    OUTPUTS "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/itk_compile_complete"
    TARGET itkpython_pyc
    )

  ADD_CUSTOM_COMMAND(
    SOURCE itkpython_pyc
    DEPENDS "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/itk_compile_complete"
    TARGET itkpython_pyc
    )

ENDIF(PYTHON_EXECUTABLE)

IF(ITK_CSWIG_TCL)
  SUBDIRS(Tcl)
ENDIF(ITK_CSWIG_TCL)
               
