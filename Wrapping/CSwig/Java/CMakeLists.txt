SET(ITK_JAVA_CLASSPATH ${ITK_BINARY_DIR}/Wrapping/CSwig/Java)
SET(ITK_JAVA_OUTPATH ${ITK_BINARY_DIR}/Wrapping/CSwig/Java)
IF(UNIX)
  SET(ITK_JAVA_INSTALL_DIR ${CMAKE_INSTALL_PREFIX}/lib/InsightToolkit)
ENDIF(UNIX)
IF(CMAKE_CONFIGURATION_TYPES)
  SET(ITK_BASE_JAVA_FILE ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${CMAKE_CFG_INTDIR}/InsightToolkit/itkbase.java)
  FOREACH(config ${CMAKE_CONFIGURATION_TYPES})
    SET(ITK_JAVA_BUILD_DIR ${LIBRARY_OUTPUT_PATH}/${config})
    CONFIGURE_FILE(
      ${ITK_SOURCE_DIR}/Wrapping/CSwig/Java/itkbase.java.in
      ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${config}/InsightToolkit/itkbase.java
      @ONLY IMMEDIATE
      )
    FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/${config}/itk_build_tree.mark
      "File next to InsightToolkit.jar to mark the build tree.\n"
    )
  ENDFOREACH(config)
  SET(ITK_JAVA_CLASSPATH
    "${ITK_JAVA_CLASSPATH}\;${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${CMAKE_CFG_INTDIR}")
  SET(ITK_JAVA_OUTPATH "${ITK_JAVA_OUTPATH}/${CMAKE_CFG_INTDIR}")
ELSE(CMAKE_CONFIGURATION_TYPES)
  SET(ITK_JAVA_BUILD_DIR ${LIBRARY_OUTPUT_PATH})
  SET(ITK_BASE_JAVA_FILE ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit/itkbase.java)
  CONFIGURE_FILE(
    ${ITK_SOURCE_DIR}/Wrapping/CSwig/Java/itkbase.java.in
    ${ITK_BASE_JAVA_FILE}
    @ONLY IMMEDIATE
    )
  FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/itk_build_tree.mark
    "File next to InsightToolkit.jar to mark the build tree.\n"
    )
  INSTALL_FILES(/lib/InsightToolkit/java FILES
    ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit.jar
    )
ENDIF(CMAKE_CONFIGURATION_TYPES)

SET(ORDERING_DEP)
FOREACH(Kit ${ITK_KITS})
  IF(${Kit} MATCHES VXLNumerics)
    SET(DEP_FILES ${VXLNumerics_JAVA_DEPENDS})
    SET(KIT_JAVA_NAME ${Kit})
  ELSE(${Kit} MATCHES VXLNumerics)
    SET(KIT_JAVA_NAME ITK${Kit})
    SET(DEP_FILES ${ITK${Kit}_JAVA_DEPENDS})
  ENDIF(${Kit} MATCHES VXLNumerics)
  FOREACH(File ${DEP_FILES})
    SET(FULL_DEP_FILES ${FULL_DEP_FILES} ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit/${File})
  ENDFOREACH(File)

  SET(ITK_JAVA_KIT_LIBS ${ITK_JAVA_KIT_LIBS} ${KIT_JAVA_NAME}Java)
  SET(KIT_FILE_NAME ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit/${KIT_JAVA_NAME}Java)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${KIT_FILE_NAME}.class
    DEPENDS ${KIT_FILE_NAME}.java ${FULL_DEP_FILES} ${ITK_BASE_JAVA_FILE} ${ORDERING_DEP}
    COMMAND ${JAVA_COMPILE}
    ARGS -classpath "${ITK_JAVA_CLASSPATH}" -d "${ITK_JAVA_OUTPATH}"
    ${KIT_FILE_NAME}.java
    COMMENT "Java Class")
  SET(ITK_JAVA_KITS_FILES ${ITK_JAVA_KITS_FILES} ${KIT_FILE_NAME}.class)
  SET(ORDERING_DEP ${KIT_FILE_NAME}.class)
ENDFOREACH(Kit)

IF(CMAKE_CONFIGURATION_TYPES)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${CMAKE_CFG_INTDIR}/InsightToolkit.jar
    DEPENDS ${ITK_JAVA_KITS_FILES}
    COMMAND ${JAVA_ARCHIVE}
    ARGS -cf ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${CMAKE_CFG_INTDIR}/InsightToolkit.jar
    -C ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${CMAKE_CFG_INTDIR} InsightToolkit
    COMMENT "Java Archive"
    )
  ADD_CUSTOM_TARGET(ITKJavaJar ALL DEPENDS ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${CMAKE_CFG_INTDIR}/InsightToolkit.jar)
ELSE(CMAKE_CONFIGURATION_TYPES)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit.jar
    DEPENDS ${ITK_JAVA_KITS_FILES}
    COMMAND ${JAVA_ARCHIVE}
    ARGS -cf ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit.jar
    -C ${ITK_BINARY_DIR}/Wrapping/CSwig/Java InsightToolkit
    COMMENT "Java Archive"
    )
  ADD_CUSTOM_TARGET(ITKJavaJar ALL DEPENDS ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit.jar)
ENDIF(CMAKE_CONFIGURATION_TYPES)
ADD_DEPENDENCIES(ITKJavaJar ${ITK_JAVA_KIT_LIBS} SwigRuntimeJava)
FOREACH(Kit ${ITK_KITS})
  IF(${Kit} MATCHES VXLNumerics)
    SET(KIT_JAVA_NAME ${Kit})
  ELSE(${Kit} MATCHES VXLNumerics)
    SET(KIT_JAVA_NAME ITK${Kit})
  ENDIF(${Kit} MATCHES VXLNumerics)
  ADD_DEPENDENCIES(ITKJavaJar "${KIT_JAVA_NAME}Java")
ENDFOREACH(Kit)
ADD_LIBRARY(ITKJavaJarDummyLibrary ITKJavaJarDummyLibrary.c)
ADD_DEPENDENCIES(ITKJavaJarDummyLibrary ITKJavaJar)
