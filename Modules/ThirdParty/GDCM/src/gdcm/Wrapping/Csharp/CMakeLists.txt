# TODO:
# http://www.disconnectd.com/?p=21

# C# Cmd line options:
# http://msdn.microsoft.com/en-us/library/ms379563(VS.80).aspx
# http://msdn.microsoft.com/en-us/library/aa288436(VS.71).aspx
# http://msdn.microsoft.com/en-us/library/aa288481(VS.71).aspx

FIND_PACKAGE(SWIG REQUIRED)
MARK_AS_ADVANCED(SWIG_DIR SWIG_EXECUTABLE SWIG_VERSION)
INCLUDE(${SWIG_USE_FILE})

INCLUDE_DIRECTORIES(
  "${GDCM_BINARY_DIR}/Source/Common"
  "${GDCM_SOURCE_DIR}/Source/Common"
  "${GDCM_SOURCE_DIR}/Source/DataStructureAndEncodingDefinition"
  "${GDCM_SOURCE_DIR}/Source/InformationObjectDefinition"
  "${GDCM_SOURCE_DIR}/Source/MediaStorageAndFileFormat"
  "${GDCM_SOURCE_DIR}/Source/DataDictionary"
  #${CMAKE_CURRENT_SOURCE_DIR}
)

# $ export JAVA_HOME=/usr/lib/j2sdk1.6-sun/
# $ export JAVA_HOME=/usr/lib/jvm/java-1.5.0-sun/
#FIND_PACKAGE(JNI REQUIRED)
#INCLUDE_DIRECTORIES(
#  #${JNI_INCLUDE_PATH}
#  ${JAVA_INCLUDE_PATH}
#  ${JAVA_INCLUDE_PATH2}
#  ${JAVA_AWT_INCLUDE_PATH}
#  )
SET_SOURCE_FILES_PROPERTIES(gdcm.i PROPERTIES CPLUSPLUS ON)

# Some old swig 1.3 did not support this option:
#SET(CMAKE_SWIG_OUTDIR "${CMAKE_CURRENT_BINARY_DIR}")

# See sf.net bug #2895067
SET(CMAKE_SWIG_FLAGS "-namespace gdcm -dllimport gdcmsharpglue")
SEPARATE_ARGUMENTS(CMAKE_SWIG_FLAGS)
# libgdksharpglue.so
SWIG_ADD_MODULE(gdcmsharpglue csharp gdcm.i)
SWIG_LINK_LIBRARIES(gdcmsharpglue gdcmMSFF
  #${JNI_LIBRARIES}
#  ${JAVA_AWT_LIB_PATH}
#  ${JAVA_JVM_LIB_PATH}
)
# Stupid cmake-swig module is doing that for us, when not needed
IF(UNIX)
  SET_TARGET_PROPERTIES(${SWIG_MODULE_gdcmsharpglue_REAL_NAME} PROPERTIES PREFIX "lib")
ENDIF(UNIX)
SET_TARGET_PROPERTIES(${SWIG_MODULE_gdcmsharpglue_REAL_NAME} PROPERTIES LINK_INTERFACE_LIBRARIES "")


#Module are always place in the library destination
#but for poor win32 user I decided to place them
# right next to the other dlls
IF(NOT GDCM_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS ${SWIG_MODULE_gdcmsharpglue_REAL_NAME}
    RUNTIME DESTINATION ${GDCM_INSTALL_BIN_DIR} COMPONENT Applications
    LIBRARY DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT Libraries
    ARCHIVE DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT Development
  ${CPACK_NAMELINK_TYPE}
  )

ENDIF(NOT GDCM_INSTALL_NO_LIBRARIES)



##IF(UNIX)
##  FIND_PACKAGE(MONO REQUIRED)
#FIND_PACKAGE(CSharp REQUIRED)
#INCLUDE(${CSharp_USE_FILE})
#
#  CONFIGURE_FILE(
#    ${CMAKE_CURRENT_SOURCE_DIR}/AssemblyInfo.cs.in
#    ${CMAKE_CURRENT_BINARY_DIR}/AssemblyInfo.cs
#    @ONLY)
##  ADD_CUSTOM_COMMAND(
##    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/gdcm-sharp.dll
##    # $(CSC) /t:library /out:mapscript_csharp.dll *.cs config/AssemblyInfo.cs
##    COMMAND ${MCS_EXECUTABLE} ARGS "/t:library" "/out:gdcm-sharp.dll" "*.cs" ${CMAKE_CURRENT_BINARY_DIR}/AssemblyInfo.cs
##    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
##    DEPENDS "${swig_generated_file_fullname}"
##      ${CMAKE_CURRENT_BINARY_DIR}/AssemblyInfo.cs
##    COMMENT "mcs *.cs"
##  )
#
## building HelloWorld:
##$(CSC) /r:mapscript_csharp.dll /out:shpdump.exe examples/shpdump.cs
## Cannot use glob since files are not generated yet...
##FILE(GLOB bla "${CMAKE_CURRENT_BINARY_DIR}/*.cs")
#CSHARP_ADD_LIBRARY(gdcm-sharp ${CMAKE_CURRENT_BINARY_DIR}/AssemblyInfo.cs \"*.cs\")
#ADD_CUSTOM_TARGET(CSharp_gdcm_sharp ALL
#    DEPENDS ${SWIG_MODULE_gdcmsharpglue_REAL_NAME}
#  )
#
## For some reason csc v1.1.4322 does not like full path, so I need to locally copy it to the binary dir
#IF(WIN32)
#  CONFIGURE_FILE(
#    ${CMAKE_CURRENT_SOURCE_DIR}/HelloWorld.cs
#    ${CMAKE_CURRENT_BINARY_DIR}/HelloWorld.cs
#  )
#ENDIF(WIN32)
##CSHARP_ADD_EXECUTABLE(HelloWorld HelloWorld.cs)
##CSHARP_LINK_LIBRARIES(HelloWorld gdcm-sharp)
##ADD_CUSTOM_TARGET(CSHARP_EXECUTABLE_HelloWorld ALL
##    DEPENDS
##      ${CMAKE_CURRENT_BINARY_DIR}/gdcm-sharp.dll
##    # CSharp_gdcm_csharp
##  )
#
#
#
##  $ export MONO_LOG_LEVEL="debug"; export MONO_LOG_MASK="dll"
## $ export LD_LIBRARY_PATH=/home/mmalaterre/Projects/gdcm/debug-gcc43/Wrapping/Csharp/:/home/mmalaterre/Projects/gdcm/debug-gcc43/bin
##ADD_CUSTOM_TARGET(GDCMCSharp ALL
##  DEPENDS
##    ${CMAKE_CURRENT_BINARY_DIR}/gdcm-sharp.dll
##    ${CMAKE_CURRENT_BINARY_DIR}/HelloWorld.exe
##  COMMENT "building gdcm-sharp.dll"
##)
#
#IF(BUILD_TESTING)
#  ADD_TEST(HelloWorldCSharp ${CMAKE_CSHARP_INTERPRETER} ${CMAKE_CURRENT_BINARY_DIR}/HelloWorld.exe ${GDCM_DATA_ROOT}/test.acr)
#ENDIF(BUILD_TESTING)
#
##ENDIF(UNIX)

SET(OLDSTYLECSHARP 1)

IF(OLDSTYLECSHARP)
  CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/AssemblyInfo.cs.in
    ${CMAKE_CURRENT_BINARY_DIR}/AssemblyInfo.cs
    @ONLY)

# From Mirco Bauer on debian-cli
# Hm? You wrote the C# binding for gdcm? Using C# in the lib name is
# very wrong in all cases. The CIL bytecode can be used by any language
# that targets the CLI. To give some examples: VB.NET, Boo, Nemerle (yes,
# those are all in debian already :)).
#
# There is a common prefix of nFOO (like nunit or nant) and for
# bindings even more common is the suffix of FOO-sharp (not -csharp)
# like gtk-sharp, gnome-sharp, evolution-sharp.
#
# /usr/lib/mono/gtk-sharp/gtk-sharp.dll
# /usr/lib/mono/gtk-sharp/gdk-sharp.dll
# /usr/lib/mono/gtk-sharp/atk-sharp.dll

  ADD_CUSTOM_COMMAND(
    #OUTPUT ${GDCM_LIBRARY_DIR}/gdcm-sharp.dll
    OUTPUT ${GDCM_LIBRARY_DIR}/gdcm-sharp.dll
    COMMAND ${CMAKE_CSHARP_COMPILER} ARGS "/t:library" "/doc:${GDCM_LIBRARY_DIR}/gdcm-sharp.dll.xml" "/out:${GDCM_LIBRARY_DIR}/gdcm-sharp.dll" "*.cs"
#${CMAKE_CURRENT_BINARY_DIR}/AssemblyInfo.cs
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS "${swig_generated_file_fullname}"
      ${CMAKE_CURRENT_BINARY_DIR}/AssemblyInfo.cs
    COMMENT "csc *.cs"
  )

IF(BUILD_EXAMPLES)
# building HelloCSharpWorld:
  ADD_CUSTOM_COMMAND(
    OUTPUT ${GDCM_EXECUTABLE_DIR}/HelloCsharpWorld.exe
    COMMAND ${CMAKE_CSHARP_COMPILER} ARGS "/r:${GDCM_LIBRARY_DIR}/gdcm-sharp.dll" "/out:${GDCM_EXECUTABLE_DIR}/HelloCsharpWorld.exe" ${CMAKE_CURRENT_SOURCE_DIR}/HelloCsharpWorld.cs
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS ${GDCM_LIBRARY_DIR}/gdcm-sharp.dll
            ${CMAKE_CURRENT_SOURCE_DIR}/HelloCsharpWorld.cs
    COMMENT "Create HelloCsharpWorld.exe"
  )
ENDIF(BUILD_EXAMPLES)

# This config file is only a mono thingy anyway
#  ADD_CUSTOM_COMMAND(
#    OUTPUT  ${GDCM_LIBRARY_DIR}/gdcm-sharp.dll.config
#    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/gdcm-sharp.dll.config ${GDCM_LIBRARY_DIR}
#    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/gdcm-sharp.dll.config
#    COMMENT "Copying gdcm-sharp.dll.config"
#  )


#IF(WIN32)
#  CONFIGURE_FILE(
#    ${CMAKE_CURRENT_SOURCE_DIR}/HelloWorld.cs
#    ${CMAKE_CURRENT_BINARY_DIR}/HelloWorld.cs
#  )
#ENDIF(WIN32)

#  $ export MONO_LOG_LEVEL="debug"; export MONO_LOG_MASK="dll"
# $ export LD_LIBRARY_PATH=/home/mmalaterre/Projects/gdcm/debug-gcc43/Wrapping/Csharp/:/home/mmalaterre/Projects/gdcm/debug-gcc43/bin
ADD_CUSTOM_TARGET(GDCMCSharp ALL
  DEPENDS
    ${GDCM_LIBRARY_DIR}/gdcm-sharp.dll
    #${GDCM_LIBRARY_DIR}/gdcm-sharp.dll.config
  COMMENT "building gdcm-sharp.dll"
)

IF(BUILD_EXAMPLES)
ADD_CUSTOM_TARGET(GDCMCSharpExample ALL
  DEPENDS
    ${GDCM_EXECUTABLE_DIR}/HelloCsharpWorld.exe
  COMMENT "building HelloCsharpWorld.exe"
)
#add_dependencies(GDCMCSharpExample
# GDCMCSharp)

ENDIF(BUILD_EXAMPLES)



ELSE(OLDSTYLECSHARP)
ENDIF(OLDSTYLECSHARP)

# Cannot do test here as CTest has not yet been included. Simply construct example and hope someone else
# will call the test for us...
#IF(BUILD_TESTING)
#  ADD_TEST(HelloWorldCSharp ${CMAKE_CSHARP_INTERPRETER} ${GDCM_EXECUTABLE_DIR}/HelloWorld.exe ${GDCM_DATA_ROOT}/012345.002.050.dcm)
#ENDIF(BUILD_TESTING)

IF(NOT GDCM_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS ${SWIG_MODULE_gdcmsharpglue_REAL_NAME}
    RUNTIME DESTINATION ${GDCM_INSTALL_BIN_DIR} COMPONENT CSharpModule
    LIBRARY DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT CSharpModule
    ARCHIVE DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT CSharpModule
  ${CPACK_NAMELINK_TYPE}
  )
SET(GDCM_LIBRARY_DIR2 ${LIBRARY_OUTPUT_PATH}/\${BUILD_TYPE})
  # because gdcm-sharp.dll is constructed with custom commands, it need the INSTALL(FILES signature:
    INSTALL(FILES
      ${GDCM_LIBRARY_DIR2}/gdcm-sharp.dll
  #    ${GDCM_LIBRARY_DIR2}/gdcm-sharp.dll.config
      DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT CSharpModule
    )
ENDIF(NOT GDCM_INSTALL_NO_LIBRARIES)
