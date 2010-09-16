#goes ignored. The solution for me was to add a .i with just the
#following:

#%module swigruntime

#And compile it without -c, while compiling the others with -c. Then
#using -DSWIG_GLOBAL in my CFLAGS for everything. I expected I might
#get some linker clashes from this but it went smoothly. I'll have to
#dig around the automake docs to see if i can set it for just the
#swigruntime.cc.

set_source_files_properties(${WrapITK_BINARY_DIR}/SwigRuntime/swigrunTcl.cxx GENERATED)
set_source_files_properties(${WrapITK_BINARY_DIR}/SwigRuntime/swigrunPython.cxx GENERATED)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

if(ITK_CSWIG_TCL)
  add_library(SwigRuntimeTcl SHARED ${WrapITK_BINARY_DIR}/SwigRuntime/swigrunTcl.cxx)
  if(ITK_LIBRARY_PROPERTIES)
    set_target_properties(SwigRuntimeTcl PROPERTIES LINK_FLAGS "${CSWIG_EXTRA_LINKFLAGS}" ${ITK_LIBRARY_PROPERTIES})
  else(ITK_LIBRARY_PROPERTIES)
  set_target_properties(SwigRuntimeTcl PROPERTIES LINK_FLAGS "${CSWIG_EXTRA_LINKFLAGS}")
  endif(ITK_LIBRARY_PROPERTIES)
  target_link_libraries(SwigRuntimeTcl ${TCL_LIBRARY})
  install(TARGETS SwigRuntimeTcl
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
endif(ITK_CSWIG_TCL)

if(ITK_CSWIG_PYTHON)
  add_library(SwigRuntimePython SHARED ${WrapITK_BINARY_DIR}/SwigRuntime/swigrunPython.cxx)
  target_link_libraries(SwigRuntimePython ${PYTHON_LIBRARY})
  if(ITK_LIBRARY_PROPERTIES)
    set_target_properties(SwigRuntimePython PROPERTIES ${ITK_LIBRARY_PROPERTIES})
  endif(ITK_LIBRARY_PROPERTIES)
  install(TARGETS SwigRuntimePython
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
endif(ITK_CSWIG_PYTHON)

if(ITK_CSWIG_PERL)
  add_library(SwigRuntimePerl SHARED ${WrapITK_BINARY_DIR}/SwigRuntime/swigrunPerl.cxx)
  target_link_libraries(SwigRuntimePerl "${PERL_LIBRARY}")
  if(ITK_LIBRARY_PROPERTIES)
    set_target_properties(SwigRuntimePerl PROPERTIES ${ITK_LIBRARY_PROPERTIES})
  endif(ITK_LIBRARY_PROPERTIES)
  install(TARGETS SwigRuntimePerl
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
endif(ITK_CSWIG_PERL)

macro(CREATE_SWIG_RUNTIME LangOption LibName LangPostFix)
     add_custom_command(
       SOURCE ${WrapITK_SOURCE_DIR}/SwigRuntime/swigrun.h
       COMMAND ${GCCXML}
       ARGS -fxml-start=_cable_ -DCABLE_CONFIGURATION ${WrapITK_SOURCE_DIR}/SwigRuntime/swigrun.h
          -fxml=${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml
       TARGET ${LibName}
       OUTPUTS ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml
       DEPENDS ${GCCXML})
     set(XML_COMMAND_ADDED 1)
   add_custom_command(
    SOURCE ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml
    COMMAND ${CSWIG}
    ARGS -o ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun${LangPostFix}.cxx ${LangOption}
         -c++ ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun.xml
    TARGET ${LibName}
    OUTPUTS ${WrapITK_BINARY_DIR}/SwigRuntime/swigrun${LangPostFix}.cxx
    DEPENDS ${CSWIG})
endmacro(CREATE_SWIG_RUNTIME)

if(ITK_CSWIG_TCL)
   CREATE_SWIG_RUNTIME(-tcl SwigRuntimeTcl Tcl)
endif(ITK_CSWIG_TCL)

if(ITK_CSWIG_PYTHON)
   CREATE_SWIG_RUNTIME(-python SwigRuntimePython Python)
endif(ITK_CSWIG_PYTHON)

if(ITK_CSWIG_PERL)
   CREATE_SWIG_RUNTIME(-perl5 SwigRuntimePerl Perl)
endif(ITK_CSWIG_PERL)

if(ITK_CSWIG_JAVA)
  set_source_files_properties(${WrapITK_BINARY_DIR}/SwigRuntime/JavaCWDJava.cxx GENERATED)
  add_library(SwigRuntimeJava MODULE ${WrapITK_BINARY_DIR}/SwigRuntime/JavaCWDJava.cxx JavaCWD.cxx)
  if(ITK_LIBRARY_PROPERTIES)
    set_target_properties(SwigRuntimeJava PROPERTIES ${ITK_LIBRARY_PROPERTIES})
  endif(ITK_LIBRARY_PROPERTIES)
  install(TARGETS SwigRuntimeJava
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
  make_directory(${WrapITK_BINARY_DIR}/Java/InsightToolkit)
  add_custom_command(
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
endif(ITK_CSWIG_JAVA)

