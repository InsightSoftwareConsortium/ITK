add_subdirectory(Tests)

make_directory(${WRAP_ITK_JAVA_DIR})
make_directory(${WRAP_ITK_JAVA_DIR}/InsightToolkit)
set(ITK_JAVA_CLASSPATH ${WRAP_ITK_JAVA_DIR})
set(ITK_JAVA_OUTPATH ${WRAP_ITK_JAVA_DIR})

# Relative path from InsightToolkit.jar installation to dlls.
set(ITK_JAVA_INSTALL_DIR "/../Java-SWIG")

# a macro to create sym links on apple
macro(APPLE_SYMLINK Kit)
  if(APPLE)
    # on mac os x, java files must have the name .jnilib
    # just link them with std lib file, and install the link
    # string(REGEX REPLACE ".dylib\$" ".jnilib" tgt "${clean_library_location}")
    set(src ${LIBRARY_OUTPUT_PATH}/lib${Kit}Java.dylib)
    set(tgt ${LIBRARY_OUTPUT_PATH}/lib${Kit}Java.jnilib)
    add_custom_command(
                      OUTPUT ${tgt}
                      DEPENDS ${Kit}Java
                      COMMAND ln
                      ARGS -sf ${src} ${tgt}
                      COMMENT "Creating symbolic link for ${Kit}Java")
    # installing the symlink will copy the regular file, not a symlink
    # so in the install tree, we don't need to have the .dylib
    WRAP_ITK_INSTALL("/lib" ${tgt})
    set(ITK_JAVA_KITS_FILES ${ITK_JAVA_KITS_FILES} ${tgt})
  endif(APPLE)
endmacro(APPLE_SYMLINK)


if(CMAKE_CONFIGURATION_TYPES)
  set(ITK_BASE_JAVA_FILE ${WRAP_ITK_JAVA_DIR}/${CMAKE_CFG_INTDIR}/InsightToolkit/itkbase.java)
  foreach(config ${CMAKE_CONFIGURATION_TYPES})
    set(ITK_JAVA_BUILD_DIR ${LIBRARY_OUTPUT_PATH}/${config})
    configure_file(
      ${WrapITK_SOURCE_DIR}/Java/itkbase.java.in
      ${WrapITK_BINARY_DIR}/Java/${config}/InsightToolkit/itkbase.java
      @ONLY IMMEDIATE
      )
    file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/${config}/itk_build_tree.mark
      "File next to InsightToolkit.jar to mark the build tree.\n"
    )
  endforeach(config)
  set(ITK_JAVA_CLASSPATH
    "${ITK_JAVA_CLASSPATH}\;${WRAP_ITK_JAVA_DIR}/${CMAKE_CFG_INTDIR}")
  set(ITK_JAVA_OUTPATH "${ITK_JAVA_OUTPATH}/${CMAKE_CFG_INTDIR}")
else(CMAKE_CONFIGURATION_TYPES)
  set(ITK_JAVA_BUILD_DIR ${LIBRARY_OUTPUT_PATH})
  set(ITK_BASE_JAVA_FILE ${WRAP_ITK_JAVA_DIR}/InsightToolkit/itkbase.java)
  configure_file(
    ${WrapITK_SOURCE_DIR}/Java/itkbase.java.in
    ${ITK_BASE_JAVA_FILE}
    @ONLY IMMEDIATE
    )
  file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/itk_build_tree.mark
    "File next to InsightToolkit.jar to mark the build tree.\n"
    )
  WRAP_ITK_INSTALL(/Java ${WRAP_ITK_JAVA_DIR}/InsightToolkit.jar )
endif(CMAKE_CONFIGURATION_TYPES)

set(ORDERING_DEP)
# iterators are not supported by java
remove(java_itk_modules "${WRAP_ITK_MODULES}" Iterators)
foreach(Kit ${java_itk_modules})
  set(KIT_JAVACLASS_DEP_FILES)
  set(DEP_FILES ${${Kit}_JAVA_DEPENDS})
  set(KIT_JAVA_NAME ${Kit})
  foreach(File ${DEP_FILES})
    set(FULL_JAVA_DEP_FILES ${FULL_JAVA_DEP_FILES} ${WRAP_ITK_JAVA_DIR}/InsightToolkit/${File}.java)
    set(KIT_JAVACLASS_DEP_FILES ${KIT_JAVACLASS_DEP_FILES} ${WRAP_ITK_JAVA_DIR}/InsightToolkit/${File}.class)
    add_custom_command(
      OUTPUT ${WRAP_ITK_JAVA_DIR}/InsightToolkit/${File}.class
      DEPENDS ${WRAP_ITK_JAVA_DIR}/InsightToolkit/${File}.java ${WRAP_ITK_JAVA_DIR}/InsightToolkit/JavaCWD.java ${ITK_BASE_JAVA_FILE} ${ORDERING_DEP}
      COMMAND ${JAVA_COMPILE}
      ARGS -classpath "${ITK_JAVA_CLASSPATH}" -d "${ITK_JAVA_OUTPATH}"
      ${WRAPPER_LIBRARY_OUTPUT_DIR}/${WRAP_ITK_JAVA_DIR}/InsightToolkit/${File}.java
      COMMENT "Compile ${File}.java")
  endforeach(File)
  set(FULL_JAVACLASS_DEP_FILES ${FULL_JAVACLASS_DEP_FILES} ${KIT_JAVACLASS_DEP_FILES})

  set(ITK_JAVA_KIT_LIBS ${ITK_JAVA_KIT_LIBS} ${KIT_JAVA_NAME}Java)
  set(KIT_FILE_NAME ${WRAP_ITK_JAVA_DIR}/InsightToolkit/${KIT_JAVA_NAME}Java)
  add_custom_command(
    OUTPUT ${KIT_FILE_NAME}.class
    DEPENDS ${KIT_FILE_NAME}.java ${FULL_JAVA_DEP_FILES} ${ITK_BASE_JAVA_FILE} ${WRAPPER_LIBRARY_OUTPUT_DIR}/${WRAP_ITK_JAVA_DIR}/InsightToolkit/JavaCWD.java ${ORDERING_DEP}
    COMMAND ${JAVA_COMPILE}
    ARGS -classpath "${ITK_JAVA_CLASSPATH}" -d "${ITK_JAVA_OUTPATH}"
    ${KIT_FILE_NAME}.java
    COMMENT "Building ${KIT_FILE_NAME}.class")
  set(ITK_JAVA_KITS_FILES ${ITK_JAVA_KITS_FILES} ${KIT_FILE_NAME}.class)
  set(ORDERING_DEP ${KIT_FILE_NAME}.class)

  APPLE_SYMLINK("${Kit}")

endforeach(Kit)

# symlink SwigRuntime
APPLE_SYMLINK(SwigRuntime)


if(CMAKE_CONFIGURATION_TYPES)
  add_custom_command(
    OUTPUT ${WRAP_ITK_JAVA_DIR}/${CMAKE_CFG_INTDIR}/InsightToolkit.jar
    DEPENDS ${ITK_JAVA_KITS_FILES}
    COMMAND ${JAVA_ARCHIVE}
    ARGS -cf ${WRAP_ITK_JAVA_DIR}/${CMAKE_CFG_INTDIR}/InsightToolkit.jar
    -C ${CMAKE_CFG_INTDIR}
    InsightToolkit
    COMMENT "Java Archive"
    )
  add_custom_target(ITKJavaJar ALL DEPENDS ${WRAP_ITK_JAVA_DIR}/${CMAKE_CFG_INTDIR}/InsightToolkit.jar)
  set(DOLLAR "$")
  WRAP_ITK_INSTALL(/Java ${WRAP_ITK_JAVA_DIR}/${DOLLAR}{BUILD_TYPE}/InsightToolkit.jar)
else(CMAKE_CONFIGURATION_TYPES)
  add_custom_command(
    OUTPUT ${WRAP_ITK_JAVA_DIR}/InsightToolkit.jar
    DEPENDS ${ITK_JAVA_KITS_FILES} ${FULL_JAVACLASS_DEP_FILES} ${FULL_JAVA_DEP_FILES} ${WRAP_ITK_JNILIB}
    COMMAND ${JAVA_ARCHIVE}
    ARGS -cf ${WRAP_ITK_JAVA_DIR}/InsightToolkit.jar
    InsightToolkit
    COMMENT "Java Archive"
    )
  add_custom_target(ITKJavaJar ALL DEPENDS ${WRAP_ITK_JAVA_DIR}/InsightToolkit.jar)
  WRAP_ITK_INSTALL(/Java ${WRAP_ITK_JAVA_DIR}/InsightToolkit.jar)
endif(CMAKE_CONFIGURATION_TYPES)

add_dependencies(ITKJavaJar ${ITK_JAVA_KIT_LIBS}  SwigRuntimeJava)
foreach(Kit ${WRAP_ITK_MODULES})
  add_dependencies(ITKJavaJar "${Kit}Java")
endforeach(Kit)
add_library(ITKJavaJarDummyLibrary ITKJavaJarDummyLibrary.c)
add_dependencies(ITKJavaJarDummyLibrary ITKJavaJar)


add_subdirectory(CWD)


