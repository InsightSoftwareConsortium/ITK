PROJECT(WrapITK)

IF(ITK_CSWIG_TCL)
  SET(ITK_INCLUDE_DIRS_SYSTEM ${ITK_INCLUDE_DIRS_SYSTEM} ${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
  # Python include directory.
  SET(ITK_INCLUDE_DIRS_SYSTEM ${ITK_INCLUDE_DIRS_SYSTEM}
    ${PYTHON_INCLUDE_PATH})
ENDIF(ITK_CSWIG_PYTHON)

IF(ITK_CSWIG_JAVA)
  # Java include directories.
  SET(ITK_INCLUDE_DIRS_SYSTEM ${ITK_INCLUDE_DIRS_SYSTEM}
      ${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${JAVA_AWT_INCLUDE_PATH})
ENDIF(ITK_CSWIG_JAVA)

# Choose an install location for the Java wrapper libraries.  This
# must be next to the ITKCommon shared library.
IF(WIN32)
  SET(ITK_INSTALL_JAVA_LIBS_DIR /bin)
ELSE(WIN32)
  SET(ITK_INSTALL_JAVA_LIBS_DIR /lib/InsightToolkit)
ENDIF(WIN32)

# We have found CableSwig.  Use the settings.
SET(CABLE_INDEX ${CableSwig_cableidx_EXE})
SET(CSWIG ${CableSwig_cswig_EXE})
SET(GCCXML ${CableSwig_gccxml_EXE})

SET(ITK_WRAP_NEEDS_DEPEND 1)
IF(${CMAKE_MAKE_PROGRAM} MATCHES make)
  SET(ITK_WRAP_NEEDS_DEPEND 0)
ENDIF(${CMAKE_MAKE_PROGRAM} MATCHES make)
SET(ITK_TOP ${ITK_SOURCE_DIR})
SET(ITK_SWIG_DEFAULT_LIB ${ITK_TOP}/Utilities/CableSwig/Swig/Lib )

SET(CSWIG_EXTRA_LINKFLAGS )
IF(CMAKE_BUILD_TOOL MATCHES "(msdev|devenv|nmake)")
  SET(CSWIG_EXTRA_LINKFLAGS "/IGNORE:4049")
ENDIF(CMAKE_BUILD_TOOL MATCHES "(msdev|devenv|nmake)")

IF(CMAKE_SYSTEM MATCHES "IRIX.*")
  IF(CMAKE_CXX_COMPILER MATCHES "CC")
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -woff 1552")
  ENDIF(CMAKE_CXX_COMPILER MATCHES "CC")
ENDIF(CMAKE_SYSTEM MATCHES "IRIX.*")

IF(CMAKE_COMPILER_IS_GNUCXX)
  STRING(REGEX REPLACE "-Wcast-qual" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

SET(IGNORE_WARNINGS -w362 -w389 -w503 -w508 -w509 -w516)
# define macros for wrapping commands
MACRO(GCCXML_CREATE_XML_FILE Source Bin Input Output Library)
# if the make program is not an IDE then include
# the depend file in a way that will make cmake 
# re-run if it changes
   SET(CABLE_SWIG_DEPEND)
   SET(CABLE_SWIG_DEPEND_REGENERATE)
   IF(${CMAKE_MAKE_PROGRAM} MATCHES "make")
     IF(EXISTS ${Bin}/${Output}.depend)
     ELSE(EXISTS ${Bin}/${Output}.depend)
       CONFIGURE_FILE(
         ${ITK_SOURCE_DIR}/Wrapping/CSwig/empty.depend.in
         ${Bin}/${Output}.depend @ONLY IMMEDIATE)
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

   IF(CABLE_SWIG_DEPEND)
     # There are dependencies.  Make sure all the files are present.
     # If not, force the rule to re-run to update the dependencies.
     FOREACH(f ${CABLE_SWIG_DEPEND})
       IF(EXISTS ${f})
       ELSE(EXISTS ${f})
         SET(CABLE_SWIG_DEPEND_REGENERATE 1)
       ENDIF(EXISTS ${f})
     ENDFOREACH(f)
   ELSE(CABLE_SWIG_DEPEND)
     # No dependencies, make the output depend on the dependency file
     # itself, which should cause the rule to re-run.
     SET(CABLE_SWIG_DEPEND_REGENERATE 1)
   ENDIF(CABLE_SWIG_DEPEND)
   IF(CABLE_SWIG_DEPEND_REGENERATE)
     SET(CABLE_SWIG_DEPEND ${Bin}/${Output}.depend)
     CONFIGURE_FILE(
       ${ITK_SOURCE_DIR}/Wrapping/CSwig/empty.depend.in
       ${Bin}/${Output}.depend @ONLY IMMEDIATE)
   ENDIF(CABLE_SWIG_DEPEND_REGENERATE)

   ADD_CUSTOM_COMMAND(
   COMMENT "${Output} from "
   SOURCE ${Source}/${Input}
   COMMAND ${GCCXML}
   ARGS -fxml-start=_cable_
        -fxml=${Bin}/${Output} --gccxml-gcc-options ${SWIG_INC_FILE}
        -DCSWIG -DCABLE_CONFIGURATION ${Source}/${Input}
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

   # Need dependency on ${ITK_BINARY_DIR}/itkConfigure.h so
   # package is rebuilt when the ITK version changes.
   ADD_CUSTOM_COMMAND(
     COMMENT "${OutputTclCxx} from "
     SOURCE ${Bin}/${InputIdx}
     COMMAND ${CSWIG}
     ARGS -l${ITK_TOP}/Wrapping/CSwig/itk.swg
          -I${ITK_SWIG_DEFAULT_LIB}
          -I${ITK_SWIG_DEFAULT_LIB}/tcl
          -noruntime ${CINDEX} ${IGNORE_WARNINGS} -depend ${Bin}/${InputXml}.depend
          -o ${Bin}/${OutputTclCxx} -tcl -pkgversion "${ITK_VERSION_STRING}" -c++ ${Bin}/${InputXml}
     TARGET ${Library}
     OUTPUTS ${Bin}/${OutputTclCxx}
     DEPENDS ${LibraryIndexFiles} ${ITK_TOP}/Wrapping/CSwig/itk.swg ${Bin}/${InputXml} ${CSWIG} ${ITK_BINARY_DIR}/itkConfigure.h)
#     MESSAGE("depends are ${CABLE_SWIG_DEPEND}")
ENDMACRO(CSWIG_CREATE_TCL_CXX_FILE)

MACRO(CSWIG_CREATE_PERL_CXX_FILE Bin MasterIdx InputIdx InputXml OutputPerlCxx Library LibraryIndexFiles)
   SET(CINDEX)
   FOREACH(MIDX ${MasterIdx})
     SET(CINDEX ${CINDEX} -Cindex "${MIDX}")
   ENDFOREACH(MIDX)

   # Need dependency on ${ITK_BINARY_DIR}/itkConfigure.h so
   # package is rebuilt when the ITK version changes.
   ADD_CUSTOM_COMMAND(
     COMMENT "${OutputPerlCxx} from "
     SOURCE ${Bin}/${InputIdx}
     COMMAND ${CSWIG}
     ARGS -perl5 -l${ITK_TOP}/Wrapping/CSwig/itk.swg
          -I${ITK_SWIG_DEFAULT_LIB}
          -noruntime ${CINDEX} ${IGNORE_WARNINGS} -depend ${Bin}/${InputXml}.depend
          -o ${Bin}/${OutputPerlCxx} -c++ ${Bin}/${InputXml}
     TARGET ${Library}
     OUTPUTS ${Bin}/${OutputPerlCxx}
     DEPENDS ${LibraryIndexFiles} ${ITK_TOP}/Wrapping/CSwig/itk.swg ${Bin}/${InputXml} ${CSWIG} ${ITK_BINARY_DIR}/itkConfigure.h)
ENDMACRO(CSWIG_CREATE_PERL_CXX_FILE)

SET(ITK_CSWIG_PYTHON_NO_EXCEPTION_REGEX "(ContinuousIndex|Python)\\.xml$")
SET(ITK_CSWIG_JAVA_NO_EXCEPTION_REGEX "(Java)\\.xml$")

MACRO(CSWIG_CREATE_PYTHON_CXX_FILE Bin MasterIdx InputIdx InputXml OutputTclCxx Library LibraryIndexFiles)
   SET(CINDEX)
   FOREACH(MIDX ${MasterIdx})
     SET(CINDEX ${CINDEX} -Cindex "${MIDX}")
   ENDFOREACH(MIDX)
   IF("${InputXml}" MATCHES "${ITK_CSWIG_PYTHON_NO_EXCEPTION_REGEX}")
     SET(ITK_SWG_FILE "")
   ELSE("${InputXml}" MATCHES "${ITK_CSWIG_PYTHON_NO_EXCEPTION_REGEX}")
     SET(ITK_SWG_FILE "-l${ITK_TOP}/Wrapping/CSwig/itk.swg")
   ENDIF("${InputXml}" MATCHES "${ITK_CSWIG_PYTHON_NO_EXCEPTION_REGEX}")
   ADD_CUSTOM_COMMAND(
     COMMENT "${OutputTclCxx} from "
     SOURCE ${Bin}/${InputIdx}
     COMMAND ${CSWIG} 
     ARGS ${ITK_SWG_FILE}
          -I${ITK_SWIG_DEFAULT_LIB}
          -I${ITK_SWIG_DEFAULT_LIB}/python
          -noruntime ${CINDEX} ${IGNORE_WARNINGS} -depend ${Bin}/${InputXml}.depend
          -outdir "${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}" 
          -o ${Bin}/${OutputTclCxx} -python -c++ ${Bin}/${InputXml}
     TARGET ${Library}
     OUTPUTS ${Bin}/${OutputTclCxx}
     DEPENDS ${LibraryIndexFiles}  ${ITK_TOP}/Wrapping/CSwig/itk.swg ${ITK_TOP}/Wrapping/CSwig/itk.swg ${Bin}/${InputXml} ${CSWIG} )
ENDMACRO(CSWIG_CREATE_PYTHON_CXX_FILE)

MACRO(CSWIG_CREATE_JAVA_CXX_FILE Bin MasterIdx InputIdx InputXml OutputTclCxx Library LibraryIndexFiles)
   SET(CINDEX)
   FOREACH(MIDX ${MasterIdx})
     SET(CINDEX ${CINDEX} -Cindex "${MIDX}")
   ENDFOREACH(MIDX)
   IF("${InputXml}" MATCHES "${ITK_CSWIG_JAVA_NO_EXCEPTION_REGEX}")
     SET(ITK_SWG_FILE "")
   ELSE("${InputXml}" MATCHES "${ITK_CSWIG_JAVA_NO_EXCEPTION_REGEX}")
     SET(ITK_SWG_FILE "-l${ITK_TOP}/Wrapping/CSwig/itk.swg")
   ENDIF("${InputXml}" MATCHES "${ITK_CSWIG_JAVA_NO_EXCEPTION_REGEX}")
   ADD_CUSTOM_COMMAND(
     COMMENT "${OutputTclCxx} from "
     SOURCE ${Bin}/${InputIdx}
     COMMAND ${CSWIG} 
     ARGS -I${ITK_SWIG_DEFAULT_LIB}
          -I${ITK_SWIG_DEFAULT_LIB}/java
          ${ITK_SWG_FILE}
          -noruntime ${CINDEX} ${IGNORE_WARNINGS} -depend ${Bin}/${InputXml}.depend
          -outdir ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit
          -o ${Bin}/${OutputTclCxx} -package InsightToolkit -java -c++ ${Bin}/${InputXml}
     TARGET ${Library}
     OUTPUTS ${Bin}/${OutputTclCxx}
     DEPENDS ${LibraryIndexFiles} ${ITK_TOP}/Wrapping/CSwig/itk.swg  ${Bin}/${InputXml} ${CSWIG} )
ENDMACRO(CSWIG_CREATE_JAVA_CXX_FILE)


# macro to create .xml, .idx and Tcl.cxx files
MACRO(WRAP_TCL_SOURCES Source Bin BaseName LibraryName MasterIdx LibraryIndexFiles)
   GCCXML_CREATE_XML_FILE(${Source} ${Bin} ${BaseName}.cxx ${BaseName}.xml ${LibraryName})
   CINDEX_CREATE_IDX_FILE(${Bin} ${BaseName}.xml  ${BaseName}.idx ${LibraryName})
   CSWIG_CREATE_TCL_CXX_FILE(${Bin} "${MasterIdx}" ${BaseName}.idx ${BaseName}.xml
                             ${BaseName}Tcl.cxx ${LibraryName} "${LibraryIndexFiles}")
ENDMACRO(WRAP_TCL_SOURCES)

# macro to create .xml, .idx and Tcl.cxx files
MACRO(WRAP_PERL_SOURCES Source Bin BaseName LibraryName MasterIdx LibraryIndexFiles)
   GCCXML_CREATE_XML_FILE(${Source} ${Bin} ${BaseName}.cxx ${BaseName}.xml ${LibraryName})
   CINDEX_CREATE_IDX_FILE(${Bin} ${BaseName}.xml  ${BaseName}.idx ${LibraryName})
   CSWIG_CREATE_PERL_CXX_FILE(${Bin} "${MasterIdx}" ${BaseName}.idx ${BaseName}.xml
                             ${BaseName}Perl.cxx ${LibraryName} "${LibraryIndexFiles}")
ENDMACRO(WRAP_PERL_SOURCES)

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


IF(ITK_CSWIG_PYTHON)
  INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH} )
ENDIF(ITK_CSWIG_PYTHON)
IF(ITK_CSWIG_TCL)
  INCLUDE_DIRECTORIES(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})
ENDIF(ITK_CSWIG_TCL)
IF(ITK_CSWIG_PERL)
  INCLUDE_DIRECTORIES(${PERL_INCLUDE_PATH})
ENDIF(ITK_CSWIG_PERL)
IF(ITK_CSWIG_JAVA)
  INCLUDE_DIRECTORIES(${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${JAVA_AWT_INCLUDE_PATH})
ENDIF(ITK_CSWIG_JAVA)

SET(SWIG_INC
  ${TCL_INCLUDE_PATH}
  ${ITK_INCLUDE_DIRS_BUILD_TREE}
  ${ITK_INCLUDE_DIRS_BUILD_TREE_CXX}
  ${ITK_INCLUDE_DIRS_SYSTEM}
  ${ITK_TOP}/Wrapping/CSwig
  ${ITK_TOP}/Wrapping/CSwig/CommonA
  ${ITK_TOP}/Wrapping/CSwig/CommonB
  ${ITK_TOP}/Wrapping/CSwig/VXLNumerics
  ${ITK_TOP}/Wrapping/CSwig/Numerics
  ${ITK_TOP}/Wrapping/CSwig/BasicFiltersA
  ${ITK_TOP}/Wrapping/CSwig/BasicFiltersB
  ${ITK_TOP}/Wrapping/CSwig/IO
  ${ITK_TOP}/Wrapping/CSwig/Algorithms
  )
ADD_DEFINITIONS(-DSWIG_GLOBAL)

SET(ITK_KITS VXLNumerics CommonA CommonB Numerics
  BasicFiltersA BasicFiltersB IO Algorithms
  )
SUBDIRS(SwigRuntime ${ITK_KITS} Tests)
IF(ITK_CSWIG_JAVA) 
  SUBDIRS(Java)
ENDIF(ITK_CSWIG_JAVA) 


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
SET(ITK_CSWIG_PACKAGE_DIR "[file dirname [file dirname [info script]]]")
SET(ITK_CSWIG_DATA_ROOT "[file dirname [file dirname [info script]]]/Data")
SET(ITK_CSWIG_TEST_ROOT "<NO_DEFAULT>")
CONFIGURE_FILE("${ITK_SOURCE_DIR}/Wrapping/CSwig/Tcl/pkgIndex.tcl.in"
               "${ITK_BINARY_DIR}/Wrapping/CSwig/Tcl/InstallOnly/Hide/pkgIndex.tcl"
               IMMEDIATE @ONLY)
INSTALL_FILES(/lib/InsightToolkit/tcl FILES
              "${ITK_BINARY_DIR}/Wrapping/CSwig/Tcl/InstallOnly/Hide/pkgIndex.tcl")

#-----------------------------------------------------------------------------
# Configure python packages.
SET(ITK_CSWIG_DATA_ROOT "'${ITK_DATA_ROOT}'")
SET(ITK_CSWIG_TEST_ROOT "'${ITK_BINARY_DIR}/Testing'")
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
IF(PYTHON_EXECUTABLE AND ITK_CSWIG_PYTHON)

  # Deal with numarray package  Options
  SET(CMAKE_MODULE_PATH ${ITK_SOURCE_DIR}/CMake)
  OPTION(ITK_USE_PYTHON_NUMARRAY "Use the numarray python package" OFF)
  MARK_AS_ADVANCED(ITK_USE_PYTHON_NUMARRAY)
  IF( ITK_USE_PYTHON_NUMARRAY )
      FIND_PACKAGE( NUMARRAY )
  ENDIF( ITK_USE_PYTHON_NUMARRAY )

  IF(PYTHON_NUMARRAY_FOUND)
    INCLUDE_DIRECTORIES( ${PYTHON_NUMARRAY_INCLUDE_DIR} )
    SET(SWIG_INC ${SWIG_INC} ${PYTHON_NUMARRAY_INCLUDE_DIR})
  ENDIF(PYTHON_NUMARRAY_FOUND)


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
    itktesting
    itkdata
    itknumerics
    vxlnumerics
    )
  # Done listing files.

  # Now copy these files if necessary.
  SET(ITK_PYTHON_SOURCE_FILES)
  SET(ITK_PYTHON_OUTPUT_FILES)
  IF(ITK_CSWIG_CONFIGURATION_TYPES)
    FOREACH(file ${ITK_PYTHON_FILES})
      SET(src "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/${CMAKE_CFG_INTDIR}/${file}.py")
      SET(ITK_PYTHON_SOURCE_FILES ${ITK_PYTHON_SOURCE_FILES} ${src})
    ENDFOREACH(file)
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

ENDIF(PYTHON_EXECUTABLE AND ITK_CSWIG_PYTHON)

IF(ITK_CSWIG_TCL)
  SUBDIRS(Tcl)
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
  SET(ITK_OLD_CMAKE_INSTALL 0)
  IF(${CMAKE_MAJOR_VERSION} LESS 2)
    IF(${CMAKE_MINOR_VERSION} LESS 9)
      SET(ITK_OLD_CMAKE_INSTALL 1)
    ENDIF(${CMAKE_MINOR_VERSION} LESS 9)
  ENDIF(${CMAKE_MAJOR_VERSION} LESS 2)

  IF(ITK_OLD_CMAKE_INSTALL)
    IF(UNIX)
      # Ultra-hackery to install the generated python files.  This is not
      # officially supported by cmake and abuses the implementation of
      # make install.  Good luck figuring out how this works.
      CONFIGURE_FILE(${WrapITK_SOURCE_DIR}/pythonfiles.sh.in
        ${WrapITK_BINARY_DIR}/pythonfiles.sh @ONLY IMMEDIATE)
      EXEC_PROGRAM(chmod ${WrapITK_BINARY_DIR} ARGS 755 pythonfiles.sh
        OUTPUT_VARIABLE ITK_EXEC_PLEASE_BE_QUIET)
      INSTALL_FILES(/lib/InsightToolkit FILES "`${WrapITK_BINARY_DIR}/pythonfiles.sh`")
    ENDIF(UNIX)
  ELSE(ITK_OLD_CMAKE_INSTALL)
    CONFIGURE_FILE(
      "${WrapITK_SOURCE_DIR}/pythonfiles_install.cmake.in"
      "${WrapITK_BINARY_DIR}/pythonfiles_install.cmake"
      @ONLY IMMEDIATE)
    ADD_CUSTOM_TARGET(python_install)
    SET_TARGET_PROPERTIES(python_install PROPERTIES
      POST_INSTALL_SCRIPT "${WrapITK_BINARY_DIR}/pythonfiles_install.cmake")
  ENDIF(ITK_OLD_CMAKE_INSTALL)

  # Install the package python files.
  FOREACH(file ${ITK_PYTHON_FILES})
    INSTALL_FILES(/lib/InsightToolkit/python FILES
      "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/${file}.py")
  ENDFOREACH(file)
  
  SET(ITK_CSWIG_PACKAGE_DIR "os.path.dirname(selfpath)")
  SET(ITK_CSWIG_DATA_ROOT "os.path.join(os.path.dirname(selfpath),'Data')")
  SET(ITK_CSWIG_TEST_ROOT "'<NO_DEFAULT>'")
  CONFIGURE_FILE("${ITK_SOURCE_DIR}/Wrapping/CSwig/Python/itkbase.py.in"
                 "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/InstallOnly/itkbase.py"
                 @ONLY IMMEDIATE)
  INSTALL_FILES(/lib/InsightToolkit/python FILES
    "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/InstallOnly/itkbase.py")
ENDIF(ITK_CSWIG_PYTHON)


MACRO(ITK_WRAP_LIBRARY WRAP_SOURCES LIBRARY_NAME DIRECTORY DEPEND_LIBRARY EXTRA_SOURCES ITK_LINK_LIBRARIES)
  # loop over cable config files creating two lists:
  # WRAP_TCL_SOURCES: list of generated files
  SET(INDEX_FILE_CONTENT "%JavaLoader=InsightToolkit.itkbase.LoadLibrary(\"${LIBRARY_NAME}Java\")\n")
  FOREACH(Source ${WRAP_SOURCES})
    SET(WRAP_PERL_SOURCES ${WRAP_PERL_SOURCES} ${Source}Perl.cxx)
    SET(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} ${Source}Tcl.cxx)
    SET(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} ${Source}Python.cxx)
    SET(WRAP_JAVA_SOURCES ${WRAP_JAVA_SOURCES} ${Source}Java.cxx)
    STRING(REGEX REPLACE wrap_ "" JAVA_DEP ${Source})
    SET(${LIBRARY_NAME}_JAVA_DEPENDS_INIT ${${LIBRARY_NAME}_JAVA_DEPENDS_INIT} ${JAVA_DEP}.java)
    SET(ALL_IDX_FILES ${ALL_IDX_FILES} ${WrapITK_BINARY_DIR}/${DIRECTORY}/${Source}.idx )
    SET(INDEX_FILE_CONTENT "${INDEX_FILE_CONTENT}${WrapITK_BINARY_DIR}/${DIRECTORY}/${Source}.idx\n")
  ENDFOREACH(Source)
  SET(${LIBRARY_NAME}_JAVA_DEPENDS  "${${LIBRARY_NAME}_JAVA_DEPENDS_INIT}" CACHE INTERNAL "" FORCE)
  # add the package wrappers 
  SET(WRAP_PERL_SOURCES ${WRAP_PERL_SOURCES} wrap_${LIBRARY_NAME}PerlPerl.cxx)
  SET(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} wrap_${LIBRARY_NAME}TclTcl.cxx)
  SET(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} wrap_${LIBRARY_NAME}PythonPython.cxx)
  SET(WRAP_JAVA_SOURCES ${WRAP_JAVA_SOURCES} wrap_${LIBRARY_NAME}JavaJava.cxx)
  IF(ITK_EXTRA_TCL_WRAP)
    SET(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} ${ITK_EXTRA_TCL_WRAP}Tcl.cxx)
  ENDIF(ITK_EXTRA_TCL_WRAP)
  IF(ITK_EXTRA_PYTHON_WRAP)
    FOREACH( extraPython ${ITK_EXTRA_PYTHON_WRAP})
      SET(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} ${extraPython}Python.cxx)
    ENDFOREACH( extraPython )
  ENDIF(ITK_EXTRA_PYTHON_WRAP)
  IF(ITK_EXTRA_JAVA_WRAP)
    SET(WRAP_JAVA_SOURCES ${WRAP_JAVA_SOURCES} ${ITK_EXTRA_JAVA_WRAP}Java.cxx)
  ENDIF(ITK_EXTRA_JAVA_WRAP)
  IF(ITK_EXTRA_PERL_WRAP)
    SET(WRAP_PERL_SOURCES ${WRAP_PERL_SOURCES} ${ITK_EXTRA_PERL_WRAP}Java.cxx)
  ENDIF(ITK_EXTRA_PERL_WRAP)

  # set the generated sources as generated
  SET_SOURCE_FILES_PROPERTIES(
    ${WRAP_PERL_SOURCES} 
    ${WRAP_TCL_SOURCES} 
    ${WRAP_PYTHON_SOURCES} 
    ${WRAP_JAVA_SOURCES} GENERATED )
  SET(EXTRA_LIBS ${ITK_LINK_LIBRARIES})
  IF("${ITK_LINK_LIBRARIES}" MATCHES "^$")
    SET(EXTRA_LIBS ${LIBRARY_NAME})
  ENDIF("${ITK_LINK_LIBRARIES}" MATCHES "^$")
  IF(ITK_CSWIG_TCL)
    IF(ITK_SWIG_FILE)
      SET(SWIG_INC ${SWIG_INC} ${TCL_INCLUDE_PATH})
      SET_SOURCE_FILES_PROPERTIES(${ITK_SWIG_FILE_CXX}Tcl.cxx GENERATED)
      SET(WRAP_FILE ${ITK_SWIG_FILE_CXX}Tcl.cxx )
    ENDIF(ITK_SWIG_FILE)
      
    ADD_LIBRARY(${LIBRARY_NAME}Tcl SHARED 
      ${WRAP_TCL_SOURCES} 
      ${ITK_EXTRA_TCL_SOURCES} 
      ${WRAP_FILE}
      ${EXTRA_SOURCES})
    IF(ITK_WRAP_NEEDS_DEPEND)
      FOREACH(lib ${DEPEND_LIBRARY})
        ADD_DEPENDENCIES(${LIBRARY_NAME}Tcl ${lib}Tcl)
      ENDFOREACH(lib)
    ENDIF(ITK_WRAP_NEEDS_DEPEND)
    SET_TARGET_PROPERTIES(${LIBRARY_NAME}Tcl PROPERTIES LINK_FLAGS "${CSWIG_EXTRA_LINKFLAGS}")
    TARGET_LINK_LIBRARIES(${LIBRARY_NAME}Tcl ${EXTRA_LIBS} SwigRuntimeTcl ${TCL_LIBRARY})
    INSTALL_TARGETS(/lib/InsightToolkit ${LIBRARY_NAME}Tcl )
    IF(ITK_SWIG_FILE)
      ADD_CUSTOM_COMMAND(
        COMMENT "run native swig on ${ITK_SWIG_FILE}"
        SOURCE ${ITK_SWIG_FILE}
        COMMAND ${CSWIG} 
        ARGS -nocable -noruntime ${IGNORE_WARNINGS} -o ${WRAP_FILE}
        -c++ ${ITK_SWIG_FILE} 
        TARGET ${LIBRARY_NAME}Tcl
        OUTPUTS ${WRAP_FILE}
        DEPENDS ${ITK_SWIG_FILE} ${CSWIG})
    ENDIF(ITK_SWIG_FILE)
  ENDIF(ITK_CSWIG_TCL)


  IF(ITK_CSWIG_PERL)
    IF(ITK_SWIG_FILE)
      SET(SWIG_INC ${SWIG_INC} ${PERL_INCLUDE_PATH})
      SET_SOURCE_FILES_PROPERTIES(${ITK_SWIG_FILE_CXX}Perl.cxx GENERATED)
      SET(WRAP_FILE ${ITK_SWIG_FILE_CXX}Perl.cxx )
    ENDIF(ITK_SWIG_FILE)
      
    ADD_LIBRARY(${LIBRARY_NAME}Perl SHARED 
      ${WRAP_PERL_SOURCES} 
      ${ITK_EXTRA_PERL_SOURCES} 
      ${WRAP_FILE}
      ${EXTRA_SOURCES})
    SET_TARGET_PROPERTIES(${LIBRARY_NAME}PErl PROPERTIES LINK_FLAGS "${CSWIG_EXTRA_LINKFLAGS}")
    TARGET_LINK_LIBRARIES(${LIBRARY_NAME}Perl ${EXTRA_LIBS} SwigRuntimePerl ${PERL_LIBRARY})
    IF(ITK_WRAP_NEEDS_DEPEND)
      FOREACH(lib ${DEPEND_LIBRARY})
        ADD_DEPENDENCIES(${LIBRARY_NAME}Perl ${lib}Perl)
      ENDFOREACH(lib)
    ENDIF(ITK_WRAP_NEEDS_DEPEND)
    INSTALL_TARGETS(/lib/InsightToolkit ${LIBRARY_NAME}Perl )
    IF(ITK_SWIG_FILE)
      ADD_CUSTOM_COMMAND(
        COMMENT "run native swig on ${ITK_SWIG_FILE}"
        SOURCE ${ITK_SWIG_FILE}
        COMMAND ${CSWIG} 
        ARGS -nocable -noruntime ${IGNORE_WARNINGS} -o ${WRAP_FILE}
        -perl5 -c++ ${ITK_SWIG_FILE}
        TARGET ${LIBRARY_NAME}Perl
        OUTPUTS ${WRAP_FILE}
        DEPENDS ${ITK_SWIG_FILE} ${CSWIG})
    ENDIF(ITK_SWIG_FILE)
  ENDIF(ITK_CSWIG_PERL)


  IF(ITK_CSWIG_PYTHON)  
    IF(ITK_SWIG_FILE)
      SET(SWIG_INC ${SWIG_INC} ${PYTHON_INCLUDE_PATH})
      SET_SOURCE_FILES_PROPERTIES(${ITK_SWIG_FILE_CXX}Python.cxx GENERATED)
      SET(WRAP_FILE ${ITK_SWIG_FILE_CXX}Python.cxx )
    ENDIF(ITK_SWIG_FILE)
    
    ADD_LIBRARY(_${LIBRARY_NAME}Python MODULE
      ${WRAP_PYTHON_SOURCES}
      ${ITK_EXTRA_PYTHON_SOURCES}
      ${WRAP_FILE}
      ${EXTRA_SOURCES})
    IF(ITK_WRAP_NEEDS_DEPEND)
      FOREACH(lib ${DEPEND_LIBRARY})
        ADD_DEPENDENCIES(_${LIBRARY_NAME}Python _${lib}Python)
      ENDFOREACH(lib)
    ENDIF(ITK_WRAP_NEEDS_DEPEND)
    SET_TARGET_PROPERTIES( _${LIBRARY_NAME}Python PROPERTIES PREFIX "")
    TARGET_LINK_LIBRARIES(_${LIBRARY_NAME}Python ${EXTRA_LIBS} SwigRuntimePython ${PYTHON_LIBRARY})
    INSTALL_TARGETS(/lib/InsightToolkit _${LIBRARY_NAME}Python) 
    IF(ITK_SWIG_FILE)
      ADD_CUSTOM_COMMAND(
        COMMENT "run native swig on ${ITK_SWIG_FILE}"
        SOURCE ${ITK_SWIG_FILE}
        COMMAND ${CSWIG} 
        ARGS -nocable -noruntime ${IGNORE_WARNINGS} -o ${WRAP_FILE}
         -outdir "${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}"
         -python -c++ ${ITK_SWIG_FILE}
        TARGET _${LIBRARY_NAME}Python
        OUTPUTS ${WRAP_FILE}
        DEPENDS ${ITK_SWIG_FILE} ${CSWIG})
    ENDIF(ITK_SWIG_FILE)
  ENDIF(ITK_CSWIG_PYTHON)

  IF(ITK_CSWIG_JAVA)  
    IF(ITK_SWIG_FILE)
      SET(SWIG_INC ${SWIG_INC} ${JAVA_INCLUDE_PATH})
      SET_SOURCE_FILES_PROPERTIES(${ITK_SWIG_FILE_CXX}Java.cxx GENERATED)
      SET(WRAP_FILE ${ITK_SWIG_FILE_CXX}Java.cxx )
    ENDIF(ITK_SWIG_FILE)
    MAKE_DIRECTORY("${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit")
    ADD_LIBRARY(${LIBRARY_NAME}Java MODULE 
      ${WRAP_JAVA_SOURCES} 
      ${ITK_EXTRA_JAVA_SOURCES} 
      ${WRAP_FILE}
      ${EXTRA_SOURCES})
    TARGET_LINK_LIBRARIES(${LIBRARY_NAME}Java ${JAVA_LIBRARY} ${EXTRA_LIBS} )   
    IF(ITK_WRAP_NEEDS_DEPEND)
      FOREACH(lib ${DEPEND_LIBRARY})
        ADD_DEPENDENCIES(${LIBRARY_NAME}Java ${lib}Java)
      ENDFOREACH(lib)
    ENDIF(ITK_WRAP_NEEDS_DEPEND)
    INSTALL_TARGETS(${ITK_INSTALL_JAVA_LIBS_DIR} ${LIBRARY_NAME}Java)
    IF(ITK_SWIG_FILE)
      ADD_CUSTOM_COMMAND(
        COMMENT "run native swig on ${ITK_SWIG_FILE}"
        SOURCE ${ITK_SWIG_FILE}
        COMMAND ${CSWIG} 
        ARGS -nocable -noruntime ${IGNORE_WARNINGS} -o ${WRAP_FILE} 
        -I${ITK_TOP}/Code/Common -DITKCommon_EXPORT=
        -outdir ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit
        -package InsightToolkit -java -c++ ${ITK_SWIG_FILE}
        TARGET ${LIBRARY_NAME}Java
        OUTPUTS ${WRAP_FILE}
        DEPENDS ${ITK_SWIG_FILE} ${CSWIG})
    ENDIF(ITK_SWIG_FILE)
  ENDIF(ITK_CSWIG_JAVA)
  
  CONFIGURE_FILE(
    ${WrapITK_SOURCE_DIR}/Master.mdx.in
    ${WrapITK_BINARY_DIR}/${DIRECTORY}/${LIBRARY_NAME}.mdx IMMEDIATE
    )

  SET(SWIG_INC_FILE ${WrapITK_BINARY_DIR}/${DIRECTORY}/SwigInc.txt)
  SET(SWIG_INC_CONTENTS)
  FOREACH(dir ${SWIG_INC})
    SET(SWIG_INC_CONTENTS "${SWIG_INC_CONTENTS}-I${dir}\n")
  ENDFOREACH(dir)
  CONFIGURE_FILE(${WrapITK_SOURCE_DIR}/SwigInc.txt.in ${SWIG_INC_FILE}
    @ONLY IMMEDIATE)
  
  FOREACH(Source ${WRAP_SOURCES})
    IF(ITK_CSWIG_TCL)
      # tcl 
      WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
        ${Source} ${LIBRARY_NAME}Tcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    ENDIF(ITK_CSWIG_TCL)

    IF(ITK_CSWIG_PERL)
      # tcl 
      WRAP_PERL_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
        ${Source} ${LIBRARY_NAME}Perl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    ENDIF(ITK_CSWIG_PERL)
    
    IF(ITK_CSWIG_PYTHON)
      # python
      WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY} 
        ${Source} _${LIBRARY_NAME}Python "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    ENDIF(ITK_CSWIG_PYTHON)
    
    IF(ITK_CSWIG_JAVA)
      # java
      WRAP_JAVA_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
        ${Source} ${LIBRARY_NAME}Java "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    ENDIF(ITK_CSWIG_JAVA)
  ENDFOREACH(Source)
  
  
  # wrap the package files for tcl and python
  IF(ITK_CSWIG_TCL)
    # tcl 
    WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
      wrap_${LIBRARY_NAME}Tcl ${LIBRARY_NAME}Tcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    IF(ITK_EXTRA_TCL_WRAP)
      WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
        "${ITK_EXTRA_TCL_WRAP}" ${LIBRARY_NAME}Tcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    ENDIF(ITK_EXTRA_TCL_WRAP)
  ENDIF(ITK_CSWIG_TCL)

  IF(ITK_CSWIG_PERL)
    # perl
    WRAP_PERL_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
      wrap_${LIBRARY_NAME}Perl ${LIBRARY_NAME}Perl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    IF(ITK_EXTRA_PERL_WRAP)
      WRAP_PERL_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
        "${ITK_EXTRA_PERL_WRAP}" ${LIBRARY_NAME}Perl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    ENDIF(ITK_EXTRA_PERL_WRAP)
  ENDIF(ITK_CSWIG_PERL)
   
  IF(ITK_CSWIG_PYTHON)
    # python
    WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
      wrap_${LIBRARY_NAME}Python _${LIBRARY_NAME}Python "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    IF(ITK_EXTRA_PYTHON_WRAP)
      FOREACH( extraPython ${ITK_EXTRA_PYTHON_WRAP})
        WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
          ${extraPython} _${LIBRARY_NAME}Python "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
      ENDFOREACH( extraPython )
    ENDIF(ITK_EXTRA_PYTHON_WRAP)

  ENDIF(ITK_CSWIG_PYTHON)

  IF(ITK_CSWIG_JAVA)
    # python
    WRAP_JAVA_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
      wrap_${LIBRARY_NAME}Java ${LIBRARY_NAME}Java "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
  ENDIF(ITK_CSWIG_JAVA)

ENDMACRO(ITK_WRAP_LIBRARY)
