SUBDIRS(Tests)

MAKE_DIRECTORY(${WRAP_ITK_JAVA_DIR})
MAKE_DIRECTORY(${WRAP_ITK_JAVA_DIR}/InsightToolkit)
SET(ITK_JAVA_CLASSPATH ${WRAP_ITK_JAVA_DIR})
SET(ITK_JAVA_OUTPATH ${WRAP_ITK_JAVA_DIR})

# Relative path from InsightToolkit.jar installation to dlls.
SET(ITK_JAVA_INSTALL_DIR "/../Java-SWIG")

# a macro to create sym links on apple
MACRO(APPLE_SYMLINK Kit)
  IF(APPLE)
    # on mac os x, java files must have the name .jnilib
    # just link them with std lib file, and install the link
    # STRING(REGEX REPLACE ".dylib\$" ".jnilib" tgt "${clean_library_location}")
    SET(src ${LIBRARY_OUTPUT_PATH}/lib${Kit}Java.dylib)
    SET(tgt ${LIBRARY_OUTPUT_PATH}/lib${Kit}Java.jnilib)
    ADD_CUSTOM_COMMAND(
                      OUTPUT ${tgt}
                      DEPENDS ${Kit}Java
                      COMMAND ln
                      ARGS -sf ${src} ${tgt}
                      COMMENT "Creating symbolic link for ${Kit}Java")
    # installing the symlink will copy the regular file, not a symlink
    # so in the install tree, we don't need to have the .dylib
    WRAP_ITK_INSTALL("/lib" ${tgt})
    SET(ITK_JAVA_KITS_FILES ${ITK_JAVA_KITS_FILES} ${tgt})
  ENDIF(APPLE)
ENDMACRO(APPLE_SYMLINK)


IF(CMAKE_CONFIGURATION_TYPES)
  SET(ITK_BASE_JAVA_FILE ${WRAP_ITK_JAVA_DIR}/${CMAKE_CFG_INTDIR}/InsightToolkit/itkbase.java)
  FOREACH(config ${CMAKE_CONFIGURATION_TYPES})
    SET(ITK_JAVA_BUILD_DIR ${LIBRARY_OUTPUT_PATH}/${config})
    CONFIGURE_FILE(
      ${WrapITK_SOURCE_DIR}/Java/itkbase.java.in
      ${WrapITK_BINARY_DIR}/Java/${config}/InsightToolkit/itkbase.java
      @ONLY IMMEDIATE
      )
    FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/${config}/itk_build_tree.mark
      "File next to InsightToolkit.jar to mark the build tree.\n"
    )
  ENDFOREACH(config)
  SET(ITK_JAVA_CLASSPATH
    "${ITK_JAVA_CLASSPATH}\;${WRAP_ITK_JAVA_DIR}/${CMAKE_CFG_INTDIR}")
  SET(ITK_JAVA_OUTPATH "${ITK_JAVA_OUTPATH}/${CMAKE_CFG_INTDIR}")
ELSE(CMAKE_CONFIGURATION_TYPES)
  SET(ITK_JAVA_BUILD_DIR ${LIBRARY_OUTPUT_PATH})
  SET(ITK_BASE_JAVA_FILE ${WRAP_ITK_JAVA_DIR}/InsightToolkit/itkbase.java)
  CONFIGURE_FILE(
    ${WrapITK_SOURCE_DIR}/Java/itkbase.java.in
    ${ITK_BASE_JAVA_FILE}
    @ONLY IMMEDIATE
    )
  FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/itk_build_tree.mark
    "File next to InsightToolkit.jar to mark the build tree.\n"
    )
  WRAP_ITK_INSTALL(/Java ${WRAP_ITK_JAVA_DIR}/InsightToolkit.jar )
ENDIF(CMAKE_CONFIGURATION_TYPES)

SET(ORDERING_DEP)
# iterators are not supported by java
REMOVE(java_itk_modules "${WRAP_ITK_MODULES}" Iterators)
FOREACH(Kit ${java_itk_modules})
  SET(KIT_JAVACLASS_DEP_FILES)
  SET(DEP_FILES ${${Kit}_JAVA_DEPENDS})
  SET(KIT_JAVA_NAME ${Kit})
  FOREACH(File ${DEP_FILES})
    SET(FULL_JAVA_DEP_FILES ${FULL_JAVA_DEP_FILES} ${WRAP_ITK_JAVA_DIR}/InsightToolkit/${File}.java)
    SET(KIT_JAVACLASS_DEP_FILES ${KIT_JAVACLASS_DEP_FILES} ${WRAP_ITK_JAVA_DIR}/InsightToolkit/${File}.class)
    ADD_CUSTOM_COMMAND(
      OUTPUT ${WRAP_ITK_JAVA_DIR}/InsightToolkit/${File}.class
      DEPENDS ${WRAP_ITK_JAVA_DIR}/InsightToolkit/${File}.java ${WRAP_ITK_JAVA_DIR}/InsightToolkit/JavaCWD.java ${ITK_BASE_JAVA_FILE} ${ORDERING_DEP}
      COMMAND ${JAVA_COMPILE}
      ARGS -classpath "${ITK_JAVA_CLASSPATH}" -d "${ITK_JAVA_OUTPATH}"
      ${WRAPPER_LIBRARY_OUTPUT_DIR}/${WRAP_ITK_JAVA_DIR}/InsightToolkit/${File}.java
      COMMENT "Compile ${File}.java")
  ENDFOREACH(File)
  SET(FULL_JAVACLASS_DEP_FILES ${FULL_JAVACLASS_DEP_FILES} ${KIT_JAVACLASS_DEP_FILES})

  SET(ITK_JAVA_KIT_LIBS ${ITK_JAVA_KIT_LIBS} ${KIT_JAVA_NAME}Java)
  SET(KIT_FILE_NAME ${WRAP_ITK_JAVA_DIR}/InsightToolkit/${KIT_JAVA_NAME}Java)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${KIT_FILE_NAME}.class
    DEPENDS ${KIT_FILE_NAME}.java ${FULL_JAVA_DEP_FILES} ${ITK_BASE_JAVA_FILE} ${WRAPPER_LIBRARY_OUTPUT_DIR}/${WRAP_ITK_JAVA_DIR}/InsightToolkit/JavaCWD.java ${ORDERING_DEP}
    COMMAND ${JAVA_COMPILE}
    ARGS -classpath "${ITK_JAVA_CLASSPATH}" -d "${ITK_JAVA_OUTPATH}"
    ${KIT_FILE_NAME}.java
    COMMENT "Building ${KIT_FILE_NAME}.class")
  SET(ITK_JAVA_KITS_FILES ${ITK_JAVA_KITS_FILES} ${KIT_FILE_NAME}.class)
  SET(ORDERING_DEP ${KIT_FILE_NAME}.class)
  
  APPLE_SYMLINK("${Kit}")
  
ENDFOREACH(Kit)

# symlink SwigRuntime
APPLE_SYMLINK(SwigRuntime)


IF(CMAKE_CONFIGURATION_TYPES)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${WRAP_ITK_JAVA_DIR}/${CMAKE_CFG_INTDIR}/InsightToolkit.jar
    DEPENDS ${ITK_JAVA_KITS_FILES}
    COMMAND ${JAVA_ARCHIVE}
    ARGS -cf ${WRAP_ITK_JAVA_DIR}/${CMAKE_CFG_INTDIR}/InsightToolkit.jar
    -C ${CMAKE_CFG_INTDIR}
    InsightToolkit
    COMMENT "Java Archive"
    )
  ADD_CUSTOM_TARGET(ITKJavaJar ALL DEPENDS ${WRAP_ITK_JAVA_DIR}/${CMAKE_CFG_INTDIR}/InsightToolkit.jar)
  SET(DOLLAR "$")
  WRAP_ITK_INSTALL(/Java ${WRAP_ITK_JAVA_DIR}/${DOLLAR}{BUILD_TYPE}/InsightToolkit.jar)
ELSE(CMAKE_CONFIGURATION_TYPES)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${WRAP_ITK_JAVA_DIR}/InsightToolkit.jar
    DEPENDS ${ITK_JAVA_KITS_FILES} ${FULL_JAVACLASS_DEP_FILES} ${FULL_JAVA_DEP_FILES} ${WRAP_ITK_JNILIB}
    COMMAND ${JAVA_ARCHIVE}
    ARGS -cf ${WRAP_ITK_JAVA_DIR}/InsightToolkit.jar
    InsightToolkit
    COMMENT "Java Archive"
    )
  ADD_CUSTOM_TARGET(ITKJavaJar ALL DEPENDS ${WRAP_ITK_JAVA_DIR}/InsightToolkit.jar)
  WRAP_ITK_INSTALL(/Java ${WRAP_ITK_JAVA_DIR}/InsightToolkit.jar)
ENDIF(CMAKE_CONFIGURATION_TYPES)

ADD_DEPENDENCIES(ITKJavaJar ${ITK_JAVA_KIT_LIBS}  SwigRuntimeJava)
FOREACH(Kit ${WRAP_ITK_MODULES})
  ADD_DEPENDENCIES(ITKJavaJar "${Kit}Java")
ENDFOREACH(Kit)
ADD_LIBRARY(ITKJavaJarDummyLibrary ITKJavaJarDummyLibrary.c)
ADD_DEPENDENCIES(ITKJavaJarDummyLibrary ITKJavaJar)


SUBDIRS(CWD)


