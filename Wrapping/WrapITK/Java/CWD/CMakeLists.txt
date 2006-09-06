IF(WRAP_ITK_JAVA)
  SET_SOURCE_FILES_PROPERTIES(${WrapITK_BINARY_DIR}/SwigRuntime/JavaCWDJava.cxx GENERATED)
  ADD_LIBRARY(SwigRuntimeJava SHARED ${WrapITK_BINARY_DIR}/SwigRuntime/JavaCWDJava.cxx JavaCWD.cxx)
  INSTALL_TARGETS("${WRAP_ITK_INSTALL_PREFIX}/lib" SwigRuntimeJava)
  INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})
  ADD_CUSTOM_COMMAND(
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
ENDIF(WRAP_ITK_JAVA)


