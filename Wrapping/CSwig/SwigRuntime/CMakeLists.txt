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

IF(ITK_CSWIG_TCL)
  ADD_LIBRARY(SwigRuntimeTcl SHARED ${WrapITK_BINARY_DIR}/swigrunTcl.cxx)
  TARGET_LINK_LIBRARIES(SwigRuntimeTcl ${TCL_LIBRARY})
ENDIF(ITK_CSWIG_TCL)
IF(ITK_CSWIG_PYTHON)
  ADD_LIBRARY(SwigRuntimePython SHARED ${WrapITK_BINARY_DIR}/swigrunPython.cxx)
  TARGET_LINK_LIBRARIES(SwigRuntimePython ${PYTHON_LIBRARY})
ENDIF(ITK_CSWIG_PYTHON)


IF(ITK_CSWIG_TCL)
# create the xml file from the .h file for the TCL wrap
  ADD_CUSTOM_COMMAND(
   SOURCE ${WrapITK_SOURCE_DIR}/SwigRuntime/swigrun.h
   COMMAND ${GCCXML}
   ARGS -fxml-start=_cable_ -DCABLE_CONFIGURATION ${WrapITK_SOURCE_DIR}/SwigRuntime/swigrun.h
        -fxml=${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml 
   TARGET SwigRuntimeTcl
   OUTPUTS ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml )

# create the wrap .cxx file from the .xml file for TCL
  ADD_CUSTOM_COMMAND(
   SOURCE ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml
   COMMAND ${CSWIG}
   ARGS -o ${WrapITK_BINARY_DIR}/SwigRuntime/swigrunTcl.cxx -tcl -c++ ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml
   TARGET SwigRuntimeTcl
   OUTPUTS ${WrapITK_BINARY_DIR}/SwigRuntime/swigrunTcl.cxx)
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
# create the xml file from the .h file for Python Wrap
  ADD_CUSTOM_COMMAND(
   SOURCE ${WrapITK_SOURCE_DIR}/SwigRuntime/swigrun.h
   COMMAND ${GCCXML}
   ARGS -fxml-start=_cable_ -DCABLE_CONFIGURATION ${WrapITK_SOURCE_DIR}/SwigRuntime/swigrun.h
        -fxml=${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml 
   TARGET SwigRuntimePython
   OUTPUTS ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml )

# create the wrap .cxx file from the .xml for Python
  ADD_CUSTOM_COMMAND(
   SOURCE ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml
   COMMAND ${CSWIG}
   ARGS -o ${WrapITK_BINARY_DIR}/SwigRuntime/swigrunPython.cxx -python -c++ ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml
   TARGET SwigRuntimePython
   OUTPUTS ${WrapITK_BINARY_DIR}/SwigRuntime/swigrunPython.cxx)
ENDIF(ITK_CSWIG_PYTHON)
