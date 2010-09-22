if(WRAP_ITK_JAVA)
  set_source_files_properties(${WrapITK_BINARY_DIR}/SwigRuntime/JavaCWDJava.cxx GENERATED)
  add_library(SwigRuntimeJava SHARED ${WrapITK_BINARY_DIR}/SwigRuntime/JavaCWDJava.cxx JavaCWD.cxx)
  if(APPLE)
    set_target_properties(SwigRuntimeJava PROPERTIES SUFFIX .jnilib)
  endif(APPLE)

  # fix missing stdc++ linkage on solaris
  if(CMAKE_SYSTEM MATCHES "SunOS-." AND CMAKE_COMPILER_IS_GNUCXX AND CMAKE_COMPILER_IS_GNUCC)
    target_link_libraries(SwigRuntimeJava stdc++)
  endif(CMAKE_SYSTEM MATCHES "SunOS-." AND CMAKE_COMPILER_IS_GNUCXX AND CMAKE_COMPILER_IS_GNUCC)

  install_targets("${WRAP_ITK_INSTALL_PREFIX}/lib" SwigRuntimeJava)
  include_directories(${CMAKE_CURRENT_SOURCE_DIR})
  add_custom_command(
        COMMENT "run native swig on SwigExtras.i"
        SOURCE ${CMAKE_CURRENT_SOURCE_DIR}/JavaCWD.i
        COMMAND ${CSWIG}
        ARGS -nocable -noruntime ${IGNORE_WARNINGS} -o ${WrapITK_BINARY_DIR}/SwigRuntime/JavaCWDJava.cxx
        -outdir "${WRAP_ITK_JAVA_DIR}/InsightToolkit"
        -package InsightToolkit
        -java -c++ ${CMAKE_CURRENT_SOURCE_DIR}/JavaCWD.i
        TARGET ${LIBRARY_NAME}Java
        OUTPUTS ${WrapITK_BINARY_DIR}/SwigRuntime/JavaCWDJava.cxx ${WrapITK_BINARY_DIR}/${WRAP_ITK_JAVA_DIR}/InsightToolkit/JavaCWD.java
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/JavaCWD.i ${CSWIG})
endif(WRAP_ITK_JAVA)


