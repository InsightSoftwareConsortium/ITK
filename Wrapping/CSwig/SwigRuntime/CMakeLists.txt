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
SET_SOURCE_FILES_PROPERTIES(${WrapITK_BINARY_DIR}/SwigRuntime/swigrunJava.cxx GENERATED)

IF(ITK_CSWIG_TCL)
  ADD_LIBRARY(SwigRuntimeTcl SHARED ${WrapITK_BINARY_DIR}/SwigRuntime/swigrunTcl.cxx)
  SET_TARGET_PROPERTIES(SwigRuntimeTcl PROPERTIES LINK_FLAGS "${CSWIG_EXTRA_LINKFLAGS}")
  TARGET_LINK_LIBRARIES(SwigRuntimeTcl ${TCL_LIBRARY})
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
  ADD_LIBRARY(SwigRuntimePython SHARED ${WrapITK_BINARY_DIR}/SwigRuntime/swigrunPython.cxx)
  TARGET_LINK_LIBRARIES(SwigRuntimePython ${PYTHON_LIBRARY})
ENDIF(ITK_CSWIG_PYTHON)

IF(ITK_CSWIG_JAVA)
  ADD_LIBRARY(SwigRuntimeJava SHARED ${WrapITK_BINARY_DIR}/SwigRuntime/swigrunJava.cxx)
  TARGET_LINK_LIBRARIES(SwigRuntimeJava ${JAVA_AWT_LIBRARY})
ENDIF(ITK_CSWIG_JAVA)


MACRO(CREATE_SWIG_RUNTIME LangOption LibName LangPostFix)
     ADD_CUSTOM_COMMAND(
       SOURCE ${WrapITK_SOURCE_DIR}/SwigRuntime/swigrun.h
       COMMAND ${GCCXML}
       ARGS -fxml-start=_cable_ -DCABLE_CONFIGURATION ${WrapITK_SOURCE_DIR}/SwigRuntime/swigrun.h
          -fxml=${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml 
       TARGET ${LibName}
       OUTPUTS ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml )
     SET(XML_COMMAND_ADDED 1)
   ADD_CUSTOM_COMMAND(
    SOURCE ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml
    COMMAND ${CSWIG}
    ARGS -o ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun${LangPostFix}.cxx ${LangOption}
         -c++ ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml
    TARGET ${LibName}
    OUTPUTS ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun${LangPostFix}.cxx)
ENDMACRO(CREATE_SWIG_RUNTIME)

IF(ITK_CSWIG_TCL)
   CREATE_SWIG_RUNTIME(-tcl SwigRuntimeTcl Tcl)
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
   CREATE_SWIG_RUNTIME(-python SwigRuntimePython Python)
ENDIF(ITK_CSWIG_PYTHON)

IF(ITK_CSWIG_JAVA)
   CREATE_SWIG_RUNTIME(-java SwigRuntimeJava Java)
ENDIF(ITK_CSWIG_JAVA)
