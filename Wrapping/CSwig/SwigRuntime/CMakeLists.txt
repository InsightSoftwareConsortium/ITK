#goes ignored. The solution for me was to add a .i with just the
#following:

#%module swigruntime

#And compile it without -c, while compiling the others with -c. Then
#using -DSWIG_GLOBAL in my CFLAGS for everything. I expected I might
#get some linker clashes from this but it went smoothly. I'll have to
#dig around the automake docs to see if i can set it for just the
#swigruntime.cc.

SET_SOURCE_FILES_PROPERTIES(${WrapITK_BINARY_DIR}/SwigRuntime/swigrunTcl.cxx GENERATED)
SET_SOURCE_FILES_PROPERTIES(${WrapITK_BINARY_DIR}/SwigRuntime/swigrunPython.cxx GENERATED)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

IF(ITK_CSWIG_TCL)
  ADD_LIBRARY(SwigRuntimeTcl SHARED ${WrapITK_BINARY_DIR}/SwigRuntime/swigrunTcl.cxx)
  IF(ITK_LIBRARY_PROPERTIES)
    SET_TARGET_PROPERTIES(SwigRuntimeTcl PROPERTIES LINK_FLAGS "${CSWIG_EXTRA_LINKFLAGS}" ${ITK_LIBRARY_PROPERTIES})
  ELSE(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(SwigRuntimeTcl PROPERTIES LINK_FLAGS "${CSWIG_EXTRA_LINKFLAGS}")
  ENDIF(ITK_LIBRARY_PROPERTIES)
  TARGET_LINK_LIBRARIES(SwigRuntimeTcl ${TCL_LIBRARY})
  INSTALL_TARGETS(${ITK_INSTALL_LIB_DIR} SwigRuntimeTcl)
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
  ADD_LIBRARY(SwigRuntimePython SHARED ${WrapITK_BINARY_DIR}/SwigRuntime/swigrunPython.cxx)
  TARGET_LINK_LIBRARIES(SwigRuntimePython ${PYTHON_LIBRARY})
  IF(ITK_LIBRARY_PROPERTIES)
    SET_TARGET_PROPERTIES(SwigRuntimePython PROPERTIES ${ITK_LIBRARY_PROPERTIES})
  ENDIF(ITK_LIBRARY_PROPERTIES)
  INSTALL_TARGETS(${ITK_INSTALL_LIB_DIR} SwigRuntimePython)
ENDIF(ITK_CSWIG_PYTHON)

IF(ITK_CSWIG_PERL)
  ADD_LIBRARY(SwigRuntimePerl SHARED ${WrapITK_BINARY_DIR}/SwigRuntime/swigrunPerl.cxx)
  TARGET_LINK_LIBRARIES(SwigRuntimePerl "${PERL_LIBRARY}")
  IF(ITK_LIBRARY_PROPERTIES)
    SET_TARGET_PROPERTIES(SwigRuntimePerl PROPERTIES ${ITK_LIBRARY_PROPERTIES})
  ENDIF(ITK_LIBRARY_PROPERTIES)
  INSTALL_TARGETS(${ITK_INSTALL_LIB_DIR} SwigRuntimePerl)
ENDIF(ITK_CSWIG_PERL)

MACRO(CREATE_SWIG_RUNTIME LangOption LibName LangPostFix)
     ADD_CUSTOM_COMMAND(
       SOURCE ${WrapITK_SOURCE_DIR}/SwigRuntime/swigrun.h
       COMMAND ${GCCXML}
       ARGS -fxml-start=_cable_ -DCABLE_CONFIGURATION ${WrapITK_SOURCE_DIR}/SwigRuntime/swigrun.h
          -fxml=${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml 
       TARGET ${LibName}
       OUTPUTS ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml
       DEPENDS ${GCCXML})
     SET(XML_COMMAND_ADDED 1)
   ADD_CUSTOM_COMMAND(
    SOURCE ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml
    COMMAND ${CSWIG}
    ARGS -o ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun${LangPostFix}.cxx ${LangOption}
         -c++ ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml
    TARGET ${LibName}
    OUTPUTS ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun${LangPostFix}.cxx
    DEPENDS ${CSWIG})
ENDMACRO(CREATE_SWIG_RUNTIME)

IF(ITK_CSWIG_TCL)
   CREATE_SWIG_RUNTIME(-tcl SwigRuntimeTcl Tcl)
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
   CREATE_SWIG_RUNTIME(-python SwigRuntimePython Python)
ENDIF(ITK_CSWIG_PYTHON)

IF(ITK_CSWIG_PERL)
   CREATE_SWIG_RUNTIME(-perl5 SwigRuntimePerl Perl)
ENDIF(ITK_CSWIG_PERL)

IF(ITK_CSWIG_JAVA)
  SET_SOURCE_FILES_PROPERTIES(${WrapITK_BINARY_DIR}/SwigRuntime/JavaCWDJava.cxx GENERATED)
  ADD_LIBRARY(SwigRuntimeJava MODULE ${WrapITK_BINARY_DIR}/SwigRuntime/JavaCWDJava.cxx JavaCWD.cxx)
  IF(ITK_LIBRARY_PROPERTIES)
    SET_TARGET_PROPERTIES(SwigRuntimeJava PROPERTIES ${ITK_LIBRARY_PROPERTIES})
  ENDIF(ITK_LIBRARY_PROPERTIES)
  INSTALL_TARGETS(${ITK_INSTALL_JAVA_LIBS_DIR} SwigRuntimeJava)
  MAKE_DIRECTORY(${WrapITK_BINARY_DIR}/Java/InsightToolkit)
  ADD_CUSTOM_COMMAND(
        COMMENT "run native swig on SwigExtras.i"
        SOURCE ${WrapITK_SOURCE_DIR}/SwigRuntime/JavaCWD.i
        COMMAND ${CSWIG} 
        ARGS -nocable -noruntime ${IGNORE_WARNINGS} -o ${WrapITK_BINARY_DIR}/SwigRuntime/JavaCWDJava.cxx
        -outdir ${WrapITK_BINARY_DIR}/Java/InsightToolkit
        -package InsightToolkit
        -java -c++ ${WrapITK_SOURCE_DIR}/SwigRuntime/JavaCWD.i
        TARGET ${LIBRARY_NAME}Java
        OUTPUTS ${WrapITK_BINARY_DIR}/SwigRuntime/JavaCWDJava.cxx
        DEPENDS ${WrapITK_SOURCE_DIR}/SwigRuntime/JavaCWD.i ${CSWIG})
ENDIF(ITK_CSWIG_JAVA)

