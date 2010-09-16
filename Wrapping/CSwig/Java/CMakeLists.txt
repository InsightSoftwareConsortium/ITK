set(ITK_JAVA_CLASSPATH ${ITK_BINARY_DIR}/Wrapping/CSwig/Java)
set(ITK_JAVA_OUTPATH ${ITK_BINARY_DIR}/Wrapping/CSwig/Java)

# Relative path from InsightToolkit.jar installation to dlls.
if(WIN32)
  set(ITK_JAVA_INSTALL_DIR "/../../bin")
else(WIN32)
  set(ITK_JAVA_INSTALL_DIR "")
endif(WIN32)

if(CMAKE_CONFIGURATION_TYPES)
  set(ITK_BASE_JAVA_FILE ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${CMAKE_CFG_INTDIR}/InsightToolkit/itkbase.java)
  foreach(config ${CMAKE_CONFIGURATION_TYPES})
    set(ITK_JAVA_BUILD_DIR ${LIBRARY_OUTPUT_PATH}/${config})
    configure_file(
      ${ITK_SOURCE_DIR}/Wrapping/CSwig/Java/itkbase.java.in
      ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${config}/InsightToolkit/itkbase.java
      @ONLY IMMEDIATE
      )
    file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/${config}/itk_build_tree.mark
      "File next to InsightToolkit.jar to mark the build tree.\n"
    )
  endforeach(config)
  set(ITK_JAVA_CLASSPATH
    "${ITK_JAVA_CLASSPATH}\;${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${CMAKE_CFG_INTDIR}")
  set(ITK_JAVA_OUTPATH "${ITK_JAVA_OUTPATH}/${CMAKE_CFG_INTDIR}")
else(CMAKE_CONFIGURATION_TYPES)
  set(ITK_JAVA_BUILD_DIR ${LIBRARY_OUTPUT_PATH})
  set(ITK_BASE_JAVA_FILE ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit/itkbase.java)
  configure_file(
    ${ITK_SOURCE_DIR}/Wrapping/CSwig/Java/itkbase.java.in
    ${ITK_BASE_JAVA_FILE}
    @ONLY IMMEDIATE
    )
  file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/itk_build_tree.mark
    "File next to InsightToolkit.jar to mark the build tree.\n"
    )
endif(CMAKE_CONFIGURATION_TYPES)

set(ORDERING_DEP)
foreach(Kit ${ITK_KITS})
  if(${Kit} MATCHES VXLNumerics)
    set(DEP_FILES ${VXLNumerics_JAVA_DEPENDS})
    set(KIT_JAVA_NAME ${Kit})
  else(${Kit} MATCHES VXLNumerics)
    set(KIT_JAVA_NAME ITK${Kit})
    set(DEP_FILES ${ITK${Kit}_JAVA_DEPENDS})
  endif(${Kit} MATCHES VXLNumerics)
  foreach(File ${DEP_FILES})
    set(FULL_DEP_FILES ${FULL_DEP_FILES} ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit/${File})
  endforeach(File)

  set(ITK_JAVA_KIT_LIBS ${ITK_JAVA_KIT_LIBS} ${KIT_JAVA_NAME}Java)
  set(KIT_FILE_NAME ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit/${KIT_JAVA_NAME}Java)
  add_custom_command(
    OUTPUT ${KIT_FILE_NAME}.class
    DEPENDS ${KIT_FILE_NAME}.java ${FULL_DEP_FILES} ${ITK_BASE_JAVA_FILE} ${ORDERING_DEP}
    COMMAND ${JAVA_COMPILE}
    ARGS -classpath "${ITK_JAVA_CLASSPATH}" -d "${ITK_JAVA_OUTPATH}"
    ${KIT_FILE_NAME}.java
    COMMENT "Java Class")
  set(ITK_JAVA_KITS_FILES ${ITK_JAVA_KITS_FILES} ${KIT_FILE_NAME}.class)
  set(ORDERING_DEP ${KIT_FILE_NAME}.class)
endforeach(Kit)

if(CMAKE_CONFIGURATION_TYPES)
  add_custom_command(
    OUTPUT ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${CMAKE_CFG_INTDIR}/InsightToolkit.jar
    DEPENDS ${ITK_JAVA_KITS_FILES}
    COMMAND ${JAVA_ARCHIVE}
    ARGS -cf ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${CMAKE_CFG_INTDIR}/InsightToolkit.jar
    -C ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${CMAKE_CFG_INTDIR} InsightToolkit
    COMMENT "Java Archive"
    )
  add_custom_target(ITKJavaJar ALL DEPENDS ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${CMAKE_CFG_INTDIR}/InsightToolkit.jar)
  set(DOLLAR "$")
  install(FILES
    ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${DOLLAR}{BUILD_TYPE}/InsightToolkit.jar
    DESTINATION ${ITK_INSTALL_LIB_DIR_CM24}
    COMPONENT RuntimeLibraries)
else(CMAKE_CONFIGURATION_TYPES)
  add_custom_command(
    OUTPUT ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit.jar
    DEPENDS ${ITK_JAVA_KITS_FILES}
    COMMAND ${JAVA_ARCHIVE}
    ARGS -cf ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit.jar
    -C ${ITK_BINARY_DIR}/Wrapping/CSwig/Java InsightToolkit
    COMMENT "Java Archive"
    )
  add_custom_target(ITKJavaJar ALL DEPENDS ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit.jar)
  install(FILES
    ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit.jar
    DESTINATION ${ITK_INSTALL_LIB_DIR_CM24}
    COMPONENT RuntimeLibraries)
endif(CMAKE_CONFIGURATION_TYPES)
add_dependencies(ITKJavaJar ${ITK_JAVA_KIT_LIBS} SwigRuntimeJava)
foreach(Kit ${ITK_KITS})
  if(${Kit} MATCHES VXLNumerics)
    set(KIT_JAVA_NAME ${Kit})
  else(${Kit} MATCHES VXLNumerics)
    set(KIT_JAVA_NAME ITK${Kit})
  endif(${Kit} MATCHES VXLNumerics)
  add_dependencies(ITKJavaJar "${KIT_JAVA_NAME}Java")
endforeach(Kit)
add_library(ITKJavaJarDummyLibrary ITKJavaJarDummyLibrary.c)
add_dependencies(ITKJavaJarDummyLibrary ITKJavaJar)
