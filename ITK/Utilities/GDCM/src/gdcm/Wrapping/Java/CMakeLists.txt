# TODO:
# SWIG is really a pain in the neck to use, a better alternative is Py++ which is using
# gccxml for the C++ parser and allow a full ANSI C++ support
FIND_PACKAGE(SWIG REQUIRED)
INCLUDE(${SWIG_USE_FILE})

INCLUDE_DIRECTORIES(
  "${GDCM_BINARY_DIR}/Source/Common"
  "${GDCM_SOURCE_DIR}/Source/Common"
  "${GDCM_SOURCE_DIR}/Source/DataStructureAndEncodingDefinition"
  "${GDCM_SOURCE_DIR}/Source/InformationObjectDefinition"
  "${GDCM_SOURCE_DIR}/Source/MediaStorageAndFileFormat"
  "${GDCM_SOURCE_DIR}/Source/DataDictionary"
)

# $ export JAVA_HOME=/usr/lib/j2sdk1.6-sun/
# $ export JAVA_HOME=/usr/lib/jvm/java-1.5.0-sun/
FIND_PACKAGE(Java 1.5 REQUIRED) # javac, jar
FIND_PACKAGE(JNI REQUIRED)
INCLUDE_DIRECTORIES(
  ${JNI_INCLUDE_DIRS}
  #${JAVA_INCLUDE_PATH}
  #${JAVA_INCLUDE_PATH2}
  #${JAVA_AWT_INCLUDE_PATH}
  )
SET_SOURCE_FILES_PROPERTIES(gdcm.i PROPERTIES CPLUSPLUS ON)
#INCLUDE(${GDCM_SOURCE_DIR}/CMake/UseSWIGExtra.cmake)

SET(GDCM_JAVA_HOME ${CMAKE_CURRENT_BINARY_DIR}/java/gdcm)
MAKE_DIRECTORY(${GDCM_JAVA_HOME})
SET(CMAKE_SWIG_OUTDIR "${GDCM_JAVA_HOME}")

SET(CMAKE_SWIG_FLAGS "-package gdcm")
SEPARATE_ARGUMENTS(CMAKE_SWIG_FLAGS)
# http://www.debian.org/doc/packaging-manuals/java-policy/x105.html
SWIG_ADD_MODULE(gdcmjni java gdcm.i)
SWIG_LINK_LIBRARIES(gdcmjni gdcmMSFF
  #${JNI_LIBRARIES}
  ${JAVA_AWT_LIB_PATH}
  ${JAVA_JVM_LIB_PATH}
)
# Stupid cmake-swig module is doing that for us, when not needed
IF(UNIX)
  SET_TARGET_PROPERTIES(${SWIG_MODULE_gdcmjni_REAL_NAME} PROPERTIES PREFIX "lib")
ENDIF(UNIX)
SET_TARGET_PROPERTIES(${SWIG_MODULE_gdcmjni_REAL_NAME} PROPERTIES LINK_INTERFACE_LIBRARIES "")

# swig-java dummy run:
# the following execute_process is a dummy run and generate all *.java files in a directory
# we can then glob all *.java to build rules in case the java wrapper becomes broken
#EXECUTE_PROCESS(COMMAND ${SWIG_EXECUTABLE}
#  -java
#  -I${GDCM_BINARY_DIR}/Source/Common
#  -I${GDCM_SOURCE_DIR}/Source/Common
#  -I${GDCM_SOURCE_DIR}/Source/DataStructureAndEncodingDefinition
#  -I${GDCM_SOURCE_DIR}/Source/MediaStorageAndFileFormat
#  -I${GDCM_SOURCE_DIR}/Source/DataDictionary
#  -c++
#  -o dummy.o
#  ${CMAKE_CURRENT_SOURCE_DIR}/gdcm.i
#  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
#  RESULT_VARIABLE res
#)
#MESSAGE("res:${res}")
#FILE(GLOB javafiles "${CMAKE_CURRENT_BINARY_DIR}/*.java")
#MESSAGE("javafile:${javafiles}")

ADD_CUSTOM_COMMAND(
  OUTPUT ${LIBRARY_OUTPUT_PATH}/gdcm.jar
# 1. run the custom command only when the gdcmJAVA_wrap.cxx has been generated (which means all *.java should be there)
# Some compiler are picky about Java version. For instance covariant return type was only added to Java 5, so to
# please compiler such as ecj (Eclipse Java Compiler 0.894_R34x) explicitly state we want 1.5 version to compile
# gdcm::ImageToImageFilter::GetOutput() const as covariant return type
  COMMAND ${JAVA_COMPILE} ARGS -source 1.5 "gdcm/*.java" #${javafiles}
# 2. now that the *.class have been generated construct the jar file. We can only rely on the gdcm.java / gdcm.class
# to build dependencie, I am pretty sure it will break parallel builds... oh well
  COMMAND ${JAVA_ARCHIVE} ARGS cvf ${LIBRARY_OUTPUT_PATH}/gdcm.jar gdcm/*.class
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/java
  DEPENDS "${swig_generated_file_fullname}"
  COMMENT "javac *.java; jar cvf -> gdcm.jar"
)

# 3. ok now add the target
ADD_CUSTOM_TARGET(GDCMJavaJar ALL
  DEPENDS ${LIBRARY_OUTPUT_PATH}/gdcm.jar
  COMMENT "building gdcm.jar"
)

# 4. gdcm.jar has been generated, let's put it right next to libgdcm.so:
#ADD_CUSTOM_COMMAND(
#  TARGET   GDCMJavaJar
#  POST_BUILD
#  COMMAND   ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/gdcm.jar ${LIBRARY_OUTPUT_PATH}
#  DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/gdcm.jar
#  COMMENT   "Copy gdcm.jar into ${LIBRARY_OUTPUT_PATH}"
#)

# Module are always place in the library destination
# but for poor win32 user I decided to place them
# right next to the other dlls
IF(NOT GDCM_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS ${SWIG_MODULE_gdcmjni_REAL_NAME}
    RUNTIME DESTINATION ${GDCM_INSTALL_BIN_DIR} COMPONENT JavaModule
    LIBRARY DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT JavaModule
    ARCHIVE DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT JavaModule
  )
  # because gdcm.jar is constructed with custom commands, it need the INSTALL(FILES signature:
  INSTALL(FILES ${LIBRARY_OUTPUT_PATH}/gdcm.jar
    DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT JavaModule
  )
ENDIF(NOT GDCM_INSTALL_NO_LIBRARIES)
