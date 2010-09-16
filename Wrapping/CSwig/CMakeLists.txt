project(WrapITK)

if(CMAKE_COMPILER_IS_GNUCXX)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w")
endif(CMAKE_COMPILER_IS_GNUCXX)

if(ITK_CSWIG_TCL)
  set(ITK_INCLUDE_DIRS_SYSTEM ${ITK_INCLUDE_DIRS_SYSTEM} ${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})
endif(ITK_CSWIG_TCL)

if(ITK_CSWIG_PYTHON)
  # Python include directory.
  set(ITK_INCLUDE_DIRS_SYSTEM ${ITK_INCLUDE_DIRS_SYSTEM}
    ${PYTHON_INCLUDE_PATH})
endif(ITK_CSWIG_PYTHON)

if(ITK_CSWIG_JAVA)
  # Java include directories.
  set(ITK_INCLUDE_DIRS_SYSTEM ${ITK_INCLUDE_DIRS_SYSTEM}
      ${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${JAVA_AWT_INCLUDE_PATH})
endif(ITK_CSWIG_JAVA)

# Choose an install location for the Java wrapper libraries.  This
# must be next to the ITKCommon shared library.
if(WIN32)
  set(ITK_INSTALL_JAVA_LIBS_DIR /bin)
else(WIN32)
  set(ITK_INSTALL_JAVA_LIBS_DIR ${ITK_INSTALL_LIB_DIR})
endif(WIN32)

# We have found CableSwig.  Use the settings.
set(CABLE_INDEX ${CableSwig_cableidx_EXE})
set(CSWIG ${CableSwig_cswig_EXE})
set(GCCXML ${CableSwig_gccxml_EXE})

set(ITK_WRAP_NEEDS_DEPEND 1)

set(ITK_TOP ${ITK_SOURCE_DIR})
set(ITK_SWIG_DEFAULT_LIB ${ITK_TOP}/Utilities/CableSwig/Swig/Lib )

set(CSWIG_EXTRA_LINKFLAGS )
if(CMAKE_BUILD_TOOL MATCHES "(msdev|devenv|nmake)")
  set(CSWIG_EXTRA_LINKFLAGS "/IGNORE:4049")
endif(CMAKE_BUILD_TOOL MATCHES "(msdev|devenv|nmake)")

if(CMAKE_COMPILER_IS_GNUCXX)
  string(REGEX REPLACE "-Wcast-qual" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
endif(CMAKE_COMPILER_IS_GNUCXX)

set(IGNORE_WARNINGS -w362 -w389 -w503 -w508 -w509 -w516)
# define macros for wrapping commands
macro(GCCXML_CREATE_XML_FILE Source Bin Input Output Library)
# if the make program is not an IDE then include
# the depend file in a way that will make cmake
# re-run if it changes
   set(CABLE_SWIG_DEPEND)
   set(CABLE_SWIG_DEPEND_REGENERATE)
   if(${CMAKE_MAKE_PROGRAM} MATCHES "make")
     if(EXISTS ${Bin}/${Output}.depend)
     else(EXISTS ${Bin}/${Output}.depend)
       configure_file(
         ${ITK_SOURCE_DIR}/Wrapping/CSwig/empty.depend.in
         ${Bin}/${Output}.depend @ONLY IMMEDIATE)
     endif(EXISTS ${Bin}/${Output}.depend)
     include(${Bin}/${Output}.depend)
   else(${CMAKE_MAKE_PROGRAM} MATCHES "make")
# for IDE generators like MS dev only include the depend files
# if they exist.   This is to prevent ecessive reloading of
# workspaces after each build.   This also means
# that the depends will not be correct until cmake
# is run once after the build has completed once.
# the depend files are created in the wrap tcl/python sections
# when the .xml file is parsed.
     include(${Bin}/${Output}.depend OPTIONAL)
   endif(${CMAKE_MAKE_PROGRAM} MATCHES "make")

   if(CABLE_SWIG_DEPEND)
     # There are dependencies.  Make sure all the files are present.
     # If not, force the rule to re-run to update the dependencies.
     foreach(f ${CABLE_SWIG_DEPEND})
       if(EXISTS ${f})
       else(EXISTS ${f})
         set(CABLE_SWIG_DEPEND_REGENERATE 1)
       endif(EXISTS ${f})
     endforeach(f)
   else(CABLE_SWIG_DEPEND)
     # No dependencies, make the output depend on the dependency file
     # itself, which should cause the rule to re-run.
     set(CABLE_SWIG_DEPEND_REGENERATE 1)
   endif(CABLE_SWIG_DEPEND)
   if(CABLE_SWIG_DEPEND_REGENERATE)
     set(CABLE_SWIG_DEPEND ${Bin}/${Output}.depend)
     configure_file(
       ${ITK_SOURCE_DIR}/Wrapping/CSwig/empty.depend.in
       ${Bin}/${Output}.depend @ONLY IMMEDIATE)
   endif(CABLE_SWIG_DEPEND_REGENERATE)

   add_custom_command(
   COMMENT "${Output} from "
   SOURCE ${Source}/${Input}
   COMMAND ${GCCXML}
   ARGS -fxml-start=_cable_
        -fxml=${Bin}/${Output} --gccxml-gcc-options ${SWIG_INC_FILE}
        -DCSWIG -DCABLE_CONFIGURATION ${Source}/${Input}
   TARGET ${Library}
   OUTPUTS ${Bin}/${Output}
   DEPENDS ${GCCXML} ${CABLE_SWIG_DEPEND})
endmacro(GCCXML_CREATE_XML_FILE)


macro(CINDEX_CREATE_IDX_FILE Bin Input Output Library)
   add_custom_command(
   COMMENT "${Output} from "
   SOURCE ${Bin}/${Input}
   COMMAND ${CABLE_INDEX}
   ARGS ${Bin}/${Input} ${Bin}/${Output}
   TARGET ${Library}
   OUTPUTS ${Bin}/${Output}
   DEPENDS ${Bin}/${Input} ${CABLE_INDEX}
)
endmacro(CINDEX_CREATE_IDX_FILE)

macro(CSWIG_CREATE_TCL_CXX_FILE Bin MasterIdx InputIdx InputXml OutputTclCxx Library LibraryIndexFiles)
   set(CINDEX)
   foreach(MIDX ${MasterIdx})
     set(CINDEX ${CINDEX} -Cindex "${MIDX}")
   endforeach(MIDX)

   # Need dependency on ${ITK_BINARY_DIR}/itkConfigure.h so
   # package is rebuilt when the ITK version changes.
   add_custom_command(
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
#     message("depends are ${CABLE_SWIG_DEPEND}")
endmacro(CSWIG_CREATE_TCL_CXX_FILE)

macro(CSWIG_CREATE_PERL_CXX_FILE Bin MasterIdx InputIdx InputXml OutputPerlCxx Library LibraryIndexFiles)
   set(CINDEX)
   foreach(MIDX ${MasterIdx})
     set(CINDEX ${CINDEX} -Cindex "${MIDX}")
   endforeach(MIDX)

   # Need dependency on ${ITK_BINARY_DIR}/itkConfigure.h so
   # package is rebuilt when the ITK version changes.
   add_custom_command(
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
endmacro(CSWIG_CREATE_PERL_CXX_FILE)

set(ITK_CSWIG_PYTHON_NO_EXCEPTION_REGEX "(ContinuousIndex|Python)\\.xml$")
set(ITK_CSWIG_JAVA_NO_EXCEPTION_REGEX "(Java)\\.xml$")

macro(CSWIG_CREATE_PYTHON_CXX_FILE Bin MasterIdx InputIdx InputXml OutputTclCxx Library LibraryIndexFiles)
   set(CINDEX)
   foreach(MIDX ${MasterIdx})
     set(CINDEX ${CINDEX} -Cindex "${MIDX}")
   endforeach(MIDX)
   if("${InputXml}" MATCHES "${ITK_CSWIG_PYTHON_NO_EXCEPTION_REGEX}")
     set(ITK_SWG_FILE "")
   else("${InputXml}" MATCHES "${ITK_CSWIG_PYTHON_NO_EXCEPTION_REGEX}")
     set(ITK_SWG_FILE "-l${ITK_TOP}/Wrapping/CSwig/itk.swg")
   endif("${InputXml}" MATCHES "${ITK_CSWIG_PYTHON_NO_EXCEPTION_REGEX}")
   add_custom_command(
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
endmacro(CSWIG_CREATE_PYTHON_CXX_FILE)

macro(CSWIG_CREATE_JAVA_CXX_FILE Bin MasterIdx InputIdx InputXml OutputTclCxx Library LibraryIndexFiles)
   set(CINDEX)
   foreach(MIDX ${MasterIdx})
     set(CINDEX ${CINDEX} -Cindex "${MIDX}")
   endforeach(MIDX)
   if("${InputXml}" MATCHES "${ITK_CSWIG_JAVA_NO_EXCEPTION_REGEX}")
     set(ITK_SWG_FILE "")
   else("${InputXml}" MATCHES "${ITK_CSWIG_JAVA_NO_EXCEPTION_REGEX}")
     set(ITK_SWG_FILE "-l${ITK_TOP}/Wrapping/CSwig/itk.swg")
   endif("${InputXml}" MATCHES "${ITK_CSWIG_JAVA_NO_EXCEPTION_REGEX}")
   add_custom_command(
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
endmacro(CSWIG_CREATE_JAVA_CXX_FILE)


# macro to create .xml, .idx and Tcl.cxx files
macro(WRAP_TCL_SOURCES Source Bin BaseName LibraryName MasterIdx LibraryIndexFiles)
   GCCXML_CREATE_XML_FILE(${Source} ${Bin} ${BaseName}.cxx ${BaseName}.xml ${LibraryName})
   CINDEX_CREATE_IDX_FILE(${Bin} ${BaseName}.xml  ${BaseName}.idx ${LibraryName})
   CSWIG_CREATE_TCL_CXX_FILE(${Bin} "${MasterIdx}" ${BaseName}.idx ${BaseName}.xml
                             ${BaseName}Tcl.cxx ${LibraryName} "${LibraryIndexFiles}")
endmacro(WRAP_TCL_SOURCES)

# macro to create .xml, .idx and Tcl.cxx files
macro(WRAP_PERL_SOURCES Source Bin BaseName LibraryName MasterIdx LibraryIndexFiles)
   GCCXML_CREATE_XML_FILE(${Source} ${Bin} ${BaseName}.cxx ${BaseName}.xml ${LibraryName})
   CINDEX_CREATE_IDX_FILE(${Bin} ${BaseName}.xml  ${BaseName}.idx ${LibraryName})
   CSWIG_CREATE_PERL_CXX_FILE(${Bin} "${MasterIdx}" ${BaseName}.idx ${BaseName}.xml
                             ${BaseName}Perl.cxx ${LibraryName} "${LibraryIndexFiles}")
endmacro(WRAP_PERL_SOURCES)

# macro to create .xml, .idx and Python.cxx files
macro(WRAP_PYTHON_SOURCES Source Bin BaseName LibraryName MasterIdx LibraryIndexFiles)
   GCCXML_CREATE_XML_FILE(${Source} ${Bin} ${BaseName}.cxx ${BaseName}.xml ${LibraryName})
   CINDEX_CREATE_IDX_FILE(${Bin} ${BaseName}.xml  ${BaseName}.idx ${LibraryName})
   CSWIG_CREATE_PYTHON_CXX_FILE(${Bin} "${MasterIdx}" ${BaseName}.idx ${BaseName}.xml
                             ${BaseName}Python.cxx ${LibraryName} "${LibraryIndexFiles}")
endmacro(WRAP_PYTHON_SOURCES)

# macro to create .xml, .idx and Python.cxx files
macro(WRAP_JAVA_SOURCES Source Bin BaseName LibraryName MasterIdx LibraryIndexFiles)
   GCCXML_CREATE_XML_FILE(${Source} ${Bin} ${BaseName}.cxx ${BaseName}.xml ${LibraryName})
   CINDEX_CREATE_IDX_FILE(${Bin} ${BaseName}.xml  ${BaseName}.idx ${LibraryName})
   CSWIG_CREATE_JAVA_CXX_FILE(${Bin} "${MasterIdx}" ${BaseName}.idx ${BaseName}.xml
                             ${BaseName}Java.cxx ${LibraryName} "${LibraryIndexFiles}")
endmacro(WRAP_JAVA_SOURCES)


# make sure required stuff is set
set(CSWIG_MISSING_VALUES)
if(NOT CSWIG)
   set(CSWIG_MISSING_VALUES "${CSWIG_MISSING_VALUES} CSWIG ")
endif(NOT CSWIG)
if(NOT CABLE_INDEX)
   set(CSWIG_MISSING_VALUES "${CSWIG_MISSING_VALUES} CABLE_INDEX ")
endif(NOT CABLE_INDEX)
if(NOT GCCXML)
   set(CSWIG_MISSING_VALUES "${CSWIG_MISSING_VALUES} GCCXML ")
endif(NOT GCCXML)
if(CSWIG_MISSING_VALUES)
  message(SEND_ERROR "To use cswig wrapping, CSWIG, CABLE_INDEX, and GCCXML executables must be specified.  If they are all in the same directory, only specifiy one of them, and then run cmake configure again and the others should be found.\nCurrently, you are missing the following:\n ${CSWIG_MISSING_VALUES}")
endif(CSWIG_MISSING_VALUES)


if(ITK_CSWIG_PYTHON)
  include_directories(${PYTHON_INCLUDE_PATH} )
endif(ITK_CSWIG_PYTHON)
if(ITK_CSWIG_TCL)
  include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})
endif(ITK_CSWIG_TCL)
if(ITK_CSWIG_PERL)
  include_directories(${PERL_INCLUDE_PATH})
endif(ITK_CSWIG_PERL)
if(ITK_CSWIG_JAVA)
  include_directories(${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2} ${JAVA_AWT_INCLUDE_PATH})
endif(ITK_CSWIG_JAVA)

set(SWIG_INC
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
if(ITK_USE_SYSTEM_VXL)
  set(SWIG_INC
    ${SWIG_INC}
    ${VXL_VCL_INCLUDE_DIR}
    ${VXL_CORE_INCLUDE_DIR}
    )
endif(ITK_USE_SYSTEM_VXL)
add_definitions(-DSWIG_GLOBAL)

set(ITK_KITS VXLNumerics CommonA CommonB Numerics
  BasicFiltersA BasicFiltersB IO Algorithms
  )
add_subdirectory(SwigRuntime ${ITK_KITS} Tests)
if(ITK_CSWIG_JAVA)
  add_subdirectory(Java)
endif(ITK_CSWIG_JAVA)


if(UNIX)
  set(ITK_CSWIG_LIBNAME_PREFIX "lib")
else(UNIX)
  set(ITK_CSWIG_LIBNAME_PREFIX "")
endif(UNIX)

set(ITK_CSWIG_PACKAGE_DIR_BUILD "${ITK_LIBRARY_PATH}")

#-----------------------------------------------------------------------------
# Configure pkgIndex.tcl for the build tree.
set(ITK_CSWIG_CONFIGURATION_TYPES ${CMAKE_CONFIGURATION_TYPES})
set(ITK_CSWIG_DATA_ROOT "${ITK_DATA_ROOT}")
set(ITK_CSWIG_TEST_ROOT "${ITK_BINARY_DIR}/Testing")
set(ITK_CSWIG_SCRIPT_DIR "${ITK_SOURCE_DIR}/Wrapping/CSwig/Tcl")

if(ITK_CSWIG_CONFIGURATION_TYPES)
  foreach(config ${ITK_CSWIG_CONFIGURATION_TYPES})
    set(ITK_CSWIG_PACKAGE_DIR "${ITK_CSWIG_PACKAGE_DIR_BUILD}/${config}")
    configure_file("${ITK_SOURCE_DIR}/Wrapping/CSwig/Tcl/pkgIndex.tcl.in"
                   "${ITK_BINARY_DIR}/Wrapping/CSwig/Tcl/${config}/pkgIndex.tcl"
                   @ONLY IMMEDIATE)
  endforeach(config)
else(ITK_CSWIG_CONFIGURATION_TYPES)
  set(ITK_CSWIG_PACKAGE_DIR "${ITK_CSWIG_PACKAGE_DIR_BUILD}")
  configure_file("${ITK_SOURCE_DIR}/Wrapping/CSwig/Tcl/pkgIndex.tcl.in"
                 "${ITK_BINARY_DIR}/Wrapping/CSwig/Tcl/pkgIndex.tcl"
                 @ONLY IMMEDIATE)
endif(ITK_CSWIG_CONFIGURATION_TYPES)

#-----------------------------------------------------------------------------
# Configure pkgIndex.tcl for the install tree.

set(ITK_CSWIG_SCRIPT_DIR "[file dirname [info script]]")
set(ITK_CSWIG_PACKAGE_DIR "[file dirname [file dirname [info script]]]")
set(ITK_CSWIG_DATA_ROOT "[file dirname [file dirname [info script]]]/Data")
set(ITK_CSWIG_TEST_ROOT "<NO_DEFAULT>")
configure_file("${ITK_SOURCE_DIR}/Wrapping/CSwig/Tcl/pkgIndex.tcl.in"
               "${ITK_BINARY_DIR}/Wrapping/CSwig/Tcl/InstallOnly/Hide/pkgIndex.tcl"
               IMMEDIATE @ONLY)
install(FILES "${ITK_BINARY_DIR}/Wrapping/CSwig/Tcl/InstallOnly/Hide/pkgIndex.tcl"
  DESTINATION ${ITK_INSTALL_LIB_DIR_CM24}/tcl
  COMPONENT RuntimeLibraries)

#-----------------------------------------------------------------------------
# Configure python packages.
set(ITK_CSWIG_DATA_ROOT "'${ITK_DATA_ROOT}'")
set(ITK_CSWIG_TEST_ROOT "'${ITK_BINARY_DIR}/Testing'")
if(ITK_CSWIG_CONFIGURATION_TYPES)
  foreach(config ${ITK_CSWIG_CONFIGURATION_TYPES})
    set(ITK_CSWIG_PACKAGE_DIR "'${ITK_CSWIG_PACKAGE_DIR_BUILD}/${config}'")
    configure_file("${ITK_SOURCE_DIR}/Wrapping/CSwig/Python/itkbase.py.in"
                   "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/${config}/itkbase.py"
                   @ONLY IMMEDIATE)
  endforeach(config)
else(ITK_CSWIG_CONFIGURATION_TYPES)
  set(ITK_CSWIG_PACKAGE_DIR "'${ITK_CSWIG_PACKAGE_DIR_BUILD}'")
  configure_file("${ITK_SOURCE_DIR}/Wrapping/CSwig/Python/itkbase.py.in"
                 "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/itkbase.py"
                 @ONLY IMMEDIATE)
endif(ITK_CSWIG_CONFIGURATION_TYPES)

# Handle out-of-source builds correctly.
#
#  1. Create a list of Python files to be installed/copied.
#  2. Copy them to ITK_BINARY_DIR if it is different from ITK_SOURCE_DIR.
#  3. Use Python's compileall to compile the copied files.
#
# *** Step 1 has to be done carefully to avoid missing out files ***
if(PYTHON_EXECUTABLE AND ITK_CSWIG_PYTHON)

  # Deal with numarray package  Options
  set(CMAKE_MODULE_PATH ${ITK_SOURCE_DIR}/CMake)
  option(ITK_USE_PYTHON_NUMARRAY "Use the numarray python package" OFF)
  mark_as_advanced(ITK_USE_PYTHON_NUMARRAY)
  if( ITK_USE_PYTHON_NUMARRAY )
      find_package( NUMARRAY )
  endif( ITK_USE_PYTHON_NUMARRAY )

  if(PYTHON_NUMARRAY_FOUND)
    include_directories( ${PYTHON_NUMARRAY_INCLUDE_DIR} )
    set(SWIG_INC ${SWIG_INC} ${PYTHON_NUMARRAY_INCLUDE_DIR})
  endif(PYTHON_NUMARRAY_FOUND)


  add_custom_target(itkpython_pyc ALL echo "...")

  # Make the necessary directories.
  make_directory(${ITK_BINARY_DIR}/Wrapping/CSwig/Python)

  # Now create a list of Python files.
  set(ITK_PYTHON_FILES)

  # Wrapping/CSwig/Python/*.py
  set(ITK_PYTHON_FILES
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
  set(ITK_PYTHON_SOURCE_FILES)
  set(ITK_PYTHON_OUTPUT_FILES)
  if(ITK_CSWIG_CONFIGURATION_TYPES)
    foreach(file ${ITK_PYTHON_FILES})
      set(src "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/${CMAKE_CFG_INTDIR}/${file}.py")
      set(ITK_PYTHON_SOURCE_FILES ${ITK_PYTHON_SOURCE_FILES} ${src})
    endforeach(file)
  else(ITK_CSWIG_CONFIGURATION_TYPES)
    foreach(file ${ITK_PYTHON_FILES})
      set(src "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/${file}.py")
      set(ITK_PYTHON_SOURCE_FILES ${ITK_PYTHON_SOURCE_FILES} ${src})
    endforeach(file)
  endif(ITK_CSWIG_CONFIGURATION_TYPES)
  if("${ITK_BINARY_DIR}" MATCHES "^${ITK_SOURCE_DIR}$")
    #message("In source build -- no need to copy Python files.")
  else("${ITK_BINARY_DIR}" MATCHES "^${ITK_SOURCE_DIR}$")
    if(ITK_CSWIG_CONFIGURATION_TYPES)
      foreach(file ${ITK_PYTHON_FILES})
        set(src "${ITK_SOURCE_DIR}/Wrapping/CSwig/Python/${file}.py")
        set(tgt "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/${CMAKE_CFG_INTDIR}/${file}.py")
        add_custom_command(SOURCE ${src}
                           COMMAND ${CMAKE_COMMAND}
                           ARGS -E copy ${src} ${tgt}
                           OUTPUTS ${tgt}
                           TARGET itkpython_pyc
                           COMMENT "source copy")
      endforeach(file)
    else(ITK_CSWIG_CONFIGURATION_TYPES)
      foreach(file ${ITK_PYTHON_FILES})
        set(src "${ITK_SOURCE_DIR}/Wrapping/CSwig/Python/${file}.py")
        set(tgt "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/${file}.py")
        add_custom_command(SOURCE ${src}
              COMMAND ${CMAKE_COMMAND}
                          ARGS -E copy ${src} ${tgt}
                          OUTPUTS ${tgt}
        TARGET itkpython_pyc
                          COMMENT "source copy")
      endforeach(file)
    endif(ITK_CSWIG_CONFIGURATION_TYPES)
  endif("${ITK_BINARY_DIR}" MATCHES "^${ITK_SOURCE_DIR}$")

  # Byte compile the Python files.
  write_file(${ITK_BINARY_DIR}/Wrapping/CSwig/Python/compile_all_itk
        "import compileall\n"
        "compileall.compile_dir('${ITK_BINARY_DIR}/Wrapping/CSwig/Python')\n"
  "file = open('${ITK_BINARY_DIR}/Wrapping/CSwig/Python/itk_compile_complete', 'w')\n"
  "file.write('Done')\n")

  add_custom_command(
    SOURCE ${ITK_BINARY_DIR}/Wrapping/CSwig/Python/compile_all_itk
    COMMAND ${PYTHON_EXECUTABLE}
    ARGS ${ITK_BINARY_DIR}/Wrapping/CSwig/Python/compile_all_itk
    DEPENDS ${ITK_PYTHON_SOURCE_FILES}
    OUTPUTS "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/itk_compile_complete"
    TARGET itkpython_pyc
    )

  add_custom_command(
    SOURCE itkpython_pyc
    DEPENDS "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/itk_compile_complete"
    TARGET itkpython_pyc
    )

endif(PYTHON_EXECUTABLE AND ITK_CSWIG_PYTHON)

if(ITK_CSWIG_TCL)
  add_subdirectory(Tcl)
endif(ITK_CSWIG_TCL)

if(ITK_CSWIG_PYTHON)
  # Install the python files created at build time.
  if("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" STREQUAL "2.4")
    # Old-style installation.
    configure_file(
      "${WrapITK_SOURCE_DIR}/pythonfiles_install.cmake.in"
      "${WrapITK_BINARY_DIR}/pythonfiles_install.cmake"
      @ONLY IMMEDIATE)
    add_custom_target(python_install)
    set_target_properties(python_install PROPERTIES
      POST_INSTALL_SCRIPT "${WrapITK_BINARY_DIR}/pythonfiles_install.cmake")
  else("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" STREQUAL "2.4")
    # Use CMake 2.6 FILES_MATCHING feature to install correctly.
    if(ITK_CSWIG_CONFIGURATION_TYPES)
      set(_FROMDIR ${EXECUTABLE_OUTPUT_PATH}/\${CMAKE_INSTALL_CONFIG_NAME})
    else(ITK_CSWIG_CONFIGURATION_TYPES)
      set(_FROMDIR ${EXECUTABLE_OUTPUT_PATH})
    endif(ITK_CSWIG_CONFIGURATION_TYPES)
    install(
      DIRECTORY ${_FROMDIR}/
      DESTINATION ${ITK_INSTALL_LIB_DIR_CM24}
      COMPONENT "RuntimeLibraries"
      FILES_MATCHING PATTERN "*.py"
      )
  endif("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" STREQUAL "2.4")

  # Install the package python files.
  foreach(file ${ITK_PYTHON_FILES})
    if(ITK_CSWIG_CONFIGURATION_TYPES)
      install(FILES "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/\${CMAKE_INSTALL_CONFIG_NAME}/${file}.py"
        DESTINATION ${ITK_INSTALL_LIB_DIR_CM24}/python
        COMPONENT RuntimeLibraries)
    else(ITK_CSWIG_CONFIGURATION_TYPES)
      install(FILES "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/${file}.py"
        DESTINATION ${ITK_INSTALL_LIB_DIR_CM24}/python
        COMPONENT RuntimeLibraries)
    endif(ITK_CSWIG_CONFIGURATION_TYPES)
  endforeach(file)

  set(ITK_CSWIG_PACKAGE_DIR "os.path.dirname(selfpath)")
  set(ITK_CSWIG_DATA_ROOT "os.path.join(os.path.dirname(selfpath),'Data')")
  set(ITK_CSWIG_TEST_ROOT "'<NO_DEFAULT>'")
  configure_file("${ITK_SOURCE_DIR}/Wrapping/CSwig/Python/itkbase.py.in"
                 "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/InstallOnly/itkbase.py"
                 @ONLY IMMEDIATE)
  install(FILES "${ITK_BINARY_DIR}/Wrapping/CSwig/Python/InstallOnly/itkbase.py"
    DESTINATION ${ITK_INSTALL_LIB_DIR_CM24}/python
    COMPONENT RuntimeLibraries)
endif(ITK_CSWIG_PYTHON)


macro(ITK_WRAP_LIBRARY WRAP_SOURCES LIBRARY_NAME DIRECTORY DEPEND_LIBRARY EXTRA_SOURCES ITK_LINK_LIBRARIES)
  # loop over cable config files creating two lists:
  # WRAP_TCL_SOURCES: list of generated files
  set(INDEX_FILE_CONTENT "%JavaLoader=InsightToolkit.itkbase.LoadLibrary(\"${LIBRARY_NAME}Java\")\n")
  foreach(Source ${WRAP_SOURCES})
    set(WRAP_PERL_SOURCES ${WRAP_PERL_SOURCES} ${Source}Perl.cxx)
    set(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} ${Source}Tcl.cxx)
    set(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} ${Source}Python.cxx)
    set(WRAP_JAVA_SOURCES ${WRAP_JAVA_SOURCES} ${Source}Java.cxx)
    string(REGEX REPLACE wrap_ "" JAVA_DEP ${Source})
    set(${LIBRARY_NAME}_JAVA_DEPENDS_INIT ${${LIBRARY_NAME}_JAVA_DEPENDS_INIT} ${JAVA_DEP}.java)
    set(ALL_IDX_FILES ${ALL_IDX_FILES} ${WrapITK_BINARY_DIR}/${DIRECTORY}/${Source}.idx )
    set(INDEX_FILE_CONTENT "${INDEX_FILE_CONTENT}${WrapITK_BINARY_DIR}/${DIRECTORY}/${Source}.idx\n")
  endforeach(Source)
  set(${LIBRARY_NAME}_JAVA_DEPENDS  "${${LIBRARY_NAME}_JAVA_DEPENDS_INIT}" CACHE INTERNAL "" FORCE)
  # add the package wrappers
  set(WRAP_PERL_SOURCES ${WRAP_PERL_SOURCES} wrap_${LIBRARY_NAME}PerlPerl.cxx)
  set(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} wrap_${LIBRARY_NAME}TclTcl.cxx)
  set(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} wrap_${LIBRARY_NAME}PythonPython.cxx)
  set(WRAP_JAVA_SOURCES ${WRAP_JAVA_SOURCES} wrap_${LIBRARY_NAME}JavaJava.cxx)
  if(ITK_EXTRA_TCL_WRAP)
    set(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} ${ITK_EXTRA_TCL_WRAP}Tcl.cxx)
  endif(ITK_EXTRA_TCL_WRAP)
  if(ITK_EXTRA_PYTHON_WRAP)
    foreach( extraPython ${ITK_EXTRA_PYTHON_WRAP})
      set(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} ${extraPython}Python.cxx)
    endforeach( extraPython )
  endif(ITK_EXTRA_PYTHON_WRAP)
  if(ITK_EXTRA_JAVA_WRAP)
    set(WRAP_JAVA_SOURCES ${WRAP_JAVA_SOURCES} ${ITK_EXTRA_JAVA_WRAP}Java.cxx)
  endif(ITK_EXTRA_JAVA_WRAP)
  if(ITK_EXTRA_PERL_WRAP)
    set(WRAP_PERL_SOURCES ${WRAP_PERL_SOURCES} ${ITK_EXTRA_PERL_WRAP}Java.cxx)
  endif(ITK_EXTRA_PERL_WRAP)

  # set the generated sources as generated
  set_source_files_properties(
    ${WRAP_PERL_SOURCES}
    ${WRAP_TCL_SOURCES}
    ${WRAP_PYTHON_SOURCES}
    ${WRAP_JAVA_SOURCES} GENERATED )
  set(EXTRA_LIBS ${ITK_LINK_LIBRARIES})
  if("${ITK_LINK_LIBRARIES}" MATCHES "^$")
    set(EXTRA_LIBS ${LIBRARY_NAME})
  endif("${ITK_LINK_LIBRARIES}" MATCHES "^$")
  if(ITK_CSWIG_TCL)
    if(ITK_SWIG_FILE)
      set(SWIG_INC ${SWIG_INC} ${TCL_INCLUDE_PATH})
      set_source_files_properties(${ITK_SWIG_FILE_CXX}Tcl.cxx GENERATED)
      set(WRAP_FILE ${ITK_SWIG_FILE_CXX}Tcl.cxx )
    endif(ITK_SWIG_FILE)

    add_library(${LIBRARY_NAME}Tcl SHARED
      ${WRAP_TCL_SOURCES}
      ${ITK_EXTRA_TCL_SOURCES}
      ${WRAP_FILE}
      ${EXTRA_SOURCES})
    if(ITK_WRAP_NEEDS_DEPEND)
      foreach(lib ${DEPEND_LIBRARY})
        add_dependencies(${LIBRARY_NAME}Tcl ${lib}Tcl)
      endforeach(lib)
    endif(ITK_WRAP_NEEDS_DEPEND)
    if(ITK_LIBRARY_PROPERTIES)
      set_target_properties(${LIBRARY_NAME}Tcl PROPERTIES LINK_FLAGS "${CSWIG_EXTRA_LINKFLAGS}" ${ITK_LIBRARY_PROPERTIES})
    else(ITK_LIBRARY_PROPERTIES)
    set_target_properties(${LIBRARY_NAME}Tcl PROPERTIES LINK_FLAGS "${CSWIG_EXTRA_LINKFLAGS}")
    endif(ITK_LIBRARY_PROPERTIES)
    target_link_libraries(${LIBRARY_NAME}Tcl ${EXTRA_LIBS} SwigRuntimeTcl ${TCL_LIBRARY})
    install(TARGETS ${LIBRARY_NAME}Tcl
      RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
      LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
      ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
    if(ITK_SWIG_FILE)
      add_custom_command(
        COMMENT "run native swig on ${ITK_SWIG_FILE}"
        SOURCE ${ITK_SWIG_FILE}
        COMMAND ${CSWIG}
        ARGS -nocable -noruntime ${IGNORE_WARNINGS} -o ${WRAP_FILE}
        -c++ ${ITK_SWIG_FILE}
        TARGET ${LIBRARY_NAME}Tcl
        OUTPUTS ${WRAP_FILE}
        DEPENDS ${ITK_SWIG_FILE} ${CSWIG})
    endif(ITK_SWIG_FILE)
  endif(ITK_CSWIG_TCL)


  if(ITK_CSWIG_PERL)
    if(ITK_SWIG_FILE)
      set(SWIG_INC ${SWIG_INC} ${PERL_INCLUDE_PATH})
      set_source_files_properties(${ITK_SWIG_FILE_CXX}Perl.cxx GENERATED)
      set(WRAP_FILE ${ITK_SWIG_FILE_CXX}Perl.cxx )
    endif(ITK_SWIG_FILE)

    add_library(${LIBRARY_NAME}Perl SHARED
      ${WRAP_PERL_SOURCES}
      ${ITK_EXTRA_PERL_SOURCES}
      ${WRAP_FILE}
      ${EXTRA_SOURCES})
    if(ITK_LIBRARY_PROPERTIES)
      set_target_properties(${LIBRARY_NAME}Perl PROPERTIES LINK_FLAGS "${CSWIG_EXTRA_LINKFLAGS}" ${ITK_LIBRARY_PROPERTIES})
    else(ITK_LIBRARY_PROPERTIES)
      set_target_properties(${LIBRARY_NAME}Perl PROPERTIES LINK_FLAGS "${CSWIG_EXTRA_LINKFLAGS}")
    endif(ITK_LIBRARY_PROPERTIES)
    target_link_libraries(${LIBRARY_NAME}Perl ${EXTRA_LIBS} SwigRuntimePerl ${PERL_LIBRARY})
    if(ITK_WRAP_NEEDS_DEPEND)
      foreach(lib ${DEPEND_LIBRARY})
        add_dependencies(${LIBRARY_NAME}Perl ${lib}Perl)
      endforeach(lib)
    endif(ITK_WRAP_NEEDS_DEPEND)
    install(TARGETS ${LIBRARY_NAME}Perl
      RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
      LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
      ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
    if(ITK_SWIG_FILE)
      add_custom_command(
        COMMENT "run native swig on ${ITK_SWIG_FILE}"
        SOURCE ${ITK_SWIG_FILE}
        COMMAND ${CSWIG}
        ARGS -nocable -noruntime ${IGNORE_WARNINGS} -o ${WRAP_FILE}
        -perl5 -c++ ${ITK_SWIG_FILE}
        TARGET ${LIBRARY_NAME}Perl
        OUTPUTS ${WRAP_FILE}
        DEPENDS ${ITK_SWIG_FILE} ${CSWIG})
    endif(ITK_SWIG_FILE)
  endif(ITK_CSWIG_PERL)


  if(ITK_CSWIG_PYTHON)
    if(ITK_SWIG_FILE)
      set(SWIG_INC ${SWIG_INC} ${PYTHON_INCLUDE_PATH})
      set_source_files_properties(${ITK_SWIG_FILE_CXX}Python.cxx GENERATED)
      set(WRAP_FILE ${ITK_SWIG_FILE_CXX}Python.cxx )
    endif(ITK_SWIG_FILE)

    add_library(_${LIBRARY_NAME}Python MODULE
      ${WRAP_PYTHON_SOURCES}
      ${ITK_EXTRA_PYTHON_SOURCES}
      ${WRAP_FILE}
      ${EXTRA_SOURCES})
    if(ITK_WRAP_NEEDS_DEPEND)
      foreach(lib ${DEPEND_LIBRARY})
        add_dependencies(_${LIBRARY_NAME}Python _${lib}Python)
      endforeach(lib)
    endif(ITK_WRAP_NEEDS_DEPEND)
    if(ITK_LIBRARY_PROPERTIES)
      set_target_properties( _${LIBRARY_NAME}Python PROPERTIES PREFIX "" ${ITK_LIBRARY_PROPERTIES})
    else(ITK_LIBRARY_PROPERTIES)
    set_target_properties( _${LIBRARY_NAME}Python PROPERTIES PREFIX "")
    endif(ITK_LIBRARY_PROPERTIES)

    # Python extension modules on Windows must have the extension ".pyd"
    # instead of ".dll" as of Python 2.5.  Older python versions do support
    # this suffix.
    if(WIN32 AND NOT CYGWIN)
      set_target_properties(_${LIBRARY_NAME}Python PROPERTIES SUFFIX ".pyd")
    endif(WIN32 AND NOT CYGWIN)

    target_link_libraries(_${LIBRARY_NAME}Python ${EXTRA_LIBS} SwigRuntimePython ${PYTHON_LIBRARY})
    install(TARGETS _${LIBRARY_NAME}Python
      RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
      LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
      ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
    if(ITK_SWIG_FILE)
      add_custom_command(
        COMMENT "run native swig on ${ITK_SWIG_FILE}"
        SOURCE ${ITK_SWIG_FILE}
        COMMAND ${CSWIG}
        ARGS -nocable -noruntime ${IGNORE_WARNINGS} -o ${WRAP_FILE}
         -outdir "${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}"
         -python -c++ ${ITK_SWIG_FILE}
        TARGET _${LIBRARY_NAME}Python
        OUTPUTS ${WRAP_FILE}
        DEPENDS ${ITK_SWIG_FILE} ${CSWIG})
    endif(ITK_SWIG_FILE)
  endif(ITK_CSWIG_PYTHON)

  if(ITK_CSWIG_JAVA)
    if(ITK_SWIG_FILE)
      set(SWIG_INC ${SWIG_INC} ${JAVA_INCLUDE_PATH})
      set_source_files_properties(${ITK_SWIG_FILE_CXX}Java.cxx GENERATED)
      set(WRAP_FILE ${ITK_SWIG_FILE_CXX}Java.cxx )
    endif(ITK_SWIG_FILE)
    make_directory("${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit")
    add_library(${LIBRARY_NAME}Java MODULE
      ${WRAP_JAVA_SOURCES}
      ${ITK_EXTRA_JAVA_SOURCES}
      ${WRAP_FILE}
      ${EXTRA_SOURCES})
    target_link_libraries(${LIBRARY_NAME}Java ${JAVA_LIBRARY} ${EXTRA_LIBS} )
    if(ITK_WRAP_NEEDS_DEPEND)
      foreach(lib ${DEPEND_LIBRARY})
        add_dependencies(${LIBRARY_NAME}Java ${lib}Java)
      endforeach(lib)
    endif(ITK_WRAP_NEEDS_DEPEND)
    if(ITK_LIBRARY_PROPERTIES)
      set_target_properties(${LIBRARY_NAME}Java PROPERTIES ${ITK_LIBRARY_PROPERTIES})
    endif(ITK_LIBRARY_PROPERTIES)
    install(TARGETS ${LIBRARY_NAME}Java
      RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
      LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
      ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
    if(ITK_SWIG_FILE)
      add_custom_command(
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
    endif(ITK_SWIG_FILE)
  endif(ITK_CSWIG_JAVA)

  configure_file(
    ${WrapITK_SOURCE_DIR}/Master.mdx.in
    ${WrapITK_BINARY_DIR}/${DIRECTORY}/${LIBRARY_NAME}.mdx IMMEDIATE
    )

  set(SWIG_INC_FILE ${WrapITK_BINARY_DIR}/${DIRECTORY}/SwigInc.txt)
  set(SWIG_INC_CONTENTS)
  foreach(dir ${SWIG_INC})
    set(SWIG_INC_CONTENTS "${SWIG_INC_CONTENTS}-I${dir}\n")
  endforeach(dir)
  configure_file(${WrapITK_SOURCE_DIR}/SwigInc.txt.in ${SWIG_INC_FILE}
    @ONLY IMMEDIATE)

  foreach(Source ${WRAP_SOURCES})
    if(ITK_CSWIG_TCL)
      # tcl
      WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
        ${Source} ${LIBRARY_NAME}Tcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    endif(ITK_CSWIG_TCL)

    if(ITK_CSWIG_PERL)
      # tcl
      WRAP_PERL_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
        ${Source} ${LIBRARY_NAME}Perl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    endif(ITK_CSWIG_PERL)

    if(ITK_CSWIG_PYTHON)
      # python
      WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
        ${Source} _${LIBRARY_NAME}Python "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    endif(ITK_CSWIG_PYTHON)

    if(ITK_CSWIG_JAVA)
      # java
      WRAP_JAVA_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
        ${Source} ${LIBRARY_NAME}Java "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    endif(ITK_CSWIG_JAVA)
  endforeach(Source)


  # wrap the package files for tcl and python
  if(ITK_CSWIG_TCL)
    # tcl
    WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
      wrap_${LIBRARY_NAME}Tcl ${LIBRARY_NAME}Tcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    if(ITK_EXTRA_TCL_WRAP)
      WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
        "${ITK_EXTRA_TCL_WRAP}" ${LIBRARY_NAME}Tcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    endif(ITK_EXTRA_TCL_WRAP)
  endif(ITK_CSWIG_TCL)

  if(ITK_CSWIG_PERL)
    # perl
    WRAP_PERL_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
      wrap_${LIBRARY_NAME}Perl ${LIBRARY_NAME}Perl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    if(ITK_EXTRA_PERL_WRAP)
      WRAP_PERL_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
        "${ITK_EXTRA_PERL_WRAP}" ${LIBRARY_NAME}Perl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    endif(ITK_EXTRA_PERL_WRAP)
  endif(ITK_CSWIG_PERL)

  if(ITK_CSWIG_PYTHON)
    # python
    WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
      wrap_${LIBRARY_NAME}Python _${LIBRARY_NAME}Python "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
    if(ITK_EXTRA_PYTHON_WRAP)
      foreach( extraPython ${ITK_EXTRA_PYTHON_WRAP})
        WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
          ${extraPython} _${LIBRARY_NAME}Python "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
      endforeach( extraPython )
    endif(ITK_EXTRA_PYTHON_WRAP)

  endif(ITK_CSWIG_PYTHON)

  if(ITK_CSWIG_JAVA)
    # python
    WRAP_JAVA_SOURCES(${ITK_TOP}/Wrapping/CSwig/${DIRECTORY} ${WrapITK_BINARY_DIR}/${DIRECTORY}
      wrap_${LIBRARY_NAME}Java ${LIBRARY_NAME}Java "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
  endif(ITK_CSWIG_JAVA)

endmacro(ITK_WRAP_LIBRARY)
