###############################################################################

if(APPLE)
  set( WRAP_ITK_JAVAC_ARGS "-J-mx1024m" CACHE STRING "Extra arguments for javac")
else(APPLE)
  set( WRAP_ITK_JAVAC_ARGS "" CACHE STRING "Extra arguments for javac")
endif(APPLE)
mark_as_advanced(WRAP_ITK_JAVAC_ARGS)


set(WRAP_ITK_JAVA_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}" CACHE INTERNAL "java source dir")
set(WRAP_ITK_JAVA_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}" CACHE INTERNAL "java binary dir")

# java stuff
find_package(Java)
find_package(JNI)
include_directories(${JAVA_INCLUDE_PATH} ${JAVA_INCLUDE_PATH2})

# create the java directory in the classindex dir
file(MAKE_DIRECTORY ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/java)

# some glorbal variables
set(WRAP_ITK_JAVA_OUT_DIR ${PROJECT_BINARY_DIR}/Languages/Java/Proxies CACHE INTERNAL "Java proxies directory")
set(WRAP_ITK_JAVA_SRC_DIR ${WRAP_ITK_JAVA_OUT_DIR}/src CACHE INTERNAL "Java proxies source directory")

#set(WRAP_ITK_JAVA_BIN_DIR ${WRAP_ITK_JAVA_OUT_DIR}/bin CACHE INTERNAL "Java proxies binary directory")

set(WRAP_ITK_JAVA_BIN_DIR ${WRAP_ITK_JAVA_SRC_DIR} CACHE INTERNAL "Java proxies binary directory")
set(WRAP_ITK_JAVA_JAR_DIR ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} CACHE INTERNAL "Java jar files directory")
set(WRAP_ITK_JAVA_CLASS_NAMES "" CACHE INTERNAL "")
set(WRAP_ITK_JAVA_JAR_DEPS "" CACHE INTERNAL "")
set(WRAP_ITK_JAVA_USER_CLASS_PATH "" CACHE INTERNAL "additional user java classpath")

# java proxy package
set(WRAP_ITK_JAVA_CORE_PACKAGE_NAME "org.itk" CACHE INTERNAL "")
set(WRAP_ITK_JAVA_CORE_PACKAGE_PATH "org/itk" CACHE INTERNAL "") #TODO: this should be automated

# TODO: iterators are not supported by java
remove(JAVA_ITK_MODULES "${WRAP_ITK_MODULES}" Iterators)

###############################################################################
# Install java stuff for external projects
if(NOT EXTERNAL_WRAP_ITK_PROJECT)
  WRAP_ITK_INSTALL(/Configuration/Languages/Java
    CMakeLists.txt
    java.i
    module_ext.i.in
    manifest.in
  )
endif(NOT EXTERNAL_WRAP_ITK_PROJECT)


###############################################################################
macro(CREATE_JAVA_DIRECTORY_STRUCTURE)

  message(STATUS "Creating wrapping Java wrapping directories.")

  file(MAKE_DIRECTORY ${WRAP_ITK_JAVA_OUT_DIR})
  file(MAKE_DIRECTORY ${WRAP_ITK_JAVA_SRC_DIR})
  file(MAKE_DIRECTORY ${WRAP_ITK_JAVA_BIN_DIR})
  file(MAKE_DIRECTORY ${WRAP_ITK_JAVA_JAR_DIR})

  # Packages output directory
  file(MAKE_DIRECTORY ${WRAP_ITK_JAVA_SRC_DIR}/${WRAP_ITK_JAVA_CORE_PACKAGE_PATH})

endmacro(CREATE_JAVA_DIRECTORY_STRUCTURE)


#-----------------------------------------------------------------------------#
# call the initialisation macros
CREATE_JAVA_DIRECTORY_STRUCTURE()
#-----------------------------------------------------------------------------#


###############################################################################
macro(WRAP_LIBRARY_JAVA library_name)

#  set(WRAP_ITK_JAVA_CONFIGURATION_TEMPLATES "")
#  set(WRAP_ITK_JAVA_LIBRARY_MODULE "")
#  set(WRAP_ITK_JAVA_LIBRARY_DECLS )
#  set(WRAP_ITK_JAVA_LIBRARY_CALLS )

  set(WRAP_ITK_JAVA_LIBRARY_DEPS )
  set(WRAP_ITK_JAVA_CXX_FILES )
  set(WRAP_ITK_JAVA_EXTENDED_JAVAIMPORTS )

  string(TOLOWER ${WRAPPER_LIBRARY_NAME} _java_package_name)

  set(java_package_path ${WRAP_ITK_JAVA_SRC_DIR}/${WRAP_ITK_JAVA_CORE_PACKAGE_PATH}/${_java_package_name})

  # clean java proxies. It's difficult to know which files are produced by swig,
  # so all the java are compiled at once in the Proxies subdirectories. A file
  # from a previous build can make the build fail.
  set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "${java_package_path}")

  # create the directory during the build so it can be recreated after a make clean.
  # dummy is there to make the command not depend on ${java_package_path} directly
  # to avoid rerunning the command when a file is added or modified in that dir.
  add_custom_command(COMMAND ${CMAKE_COMMAND}
    ARGS -E make_directory ${java_package_path}/dummy
    OUTPUT ${java_package_path}/dummy
  )
endmacro(WRAP_LIBRARY_JAVA library_name)


###############################################################################
macro(END_WRAP_LIBRARY_JAVA)

  # Loop over the extra swig input files and add them to the generated files
  # lists. Guess that the generated cxx output will have the same name as
  # the .i input file.
  foreach(source ${WRAPPER_LIBRARY_SWIG_INPUTS})
    get_filename_component(base_name ${source} NAME_WE)
    WRAP_MODULE_JAVA("${base_name}")
    END_WRAP_MODULE_JAVA("${base_name}")
  endforeach(source)

  # set the package name and path
  string(TOLOWER ${WRAPPER_LIBRARY_NAME} _java_package_name)
  set(java_package_path ${WRAP_ITK_JAVA_SRC_DIR}/${WRAP_ITK_JAVA_CORE_PACKAGE_PATH}/${_java_package_name})
  set(java_package_bin_path ${WRAP_ITK_JAVA_BIN_DIR}/${WRAP_ITK_JAVA_CORE_PACKAGE_PATH}/${_java_package_name})
  set(java_package_name ${WRAP_ITK_JAVA_CORE_PACKAGE_NAME}.${_java_package_name})
  set(java_jar_file_name ${java_package_name}.jar)
  set(java_jar_file_path ${WRAP_ITK_JAVA_JAR_DIR}/${java_jar_file_name})

  # set the java source and bin files
  set(java_file )
  set(java_bin_file )
  foreach(class_name ${WRAP_ITK_JAVA_CLASS_NAMES})
    set(java_file ${java_file} ${java_package_path}/${class_name}.java)
    set(java_bin_file ${java_bin_file} ${java_package_bin_path}/${class_name}.class)
  endforeach(class_name ${WRAP_ITK_JAVA_CLASS_NAMES})

  configure_file("${WRAP_ITK_JAVA_SOURCE_DIR}/module_ext.i.in"
    "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/java/${WRAPPER_LIBRARY_NAME}_ext.i"
    @ONLY IMMEDIATE)

  WRAP_ITK_INSTALL(/Configuration/Typedefs/java
    "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/java/${WRAPPER_LIBRARY_NAME}_ext.i")

  set(interface_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}Module.i")

  # create java wrapping binaries
  set(lib ${WRAPPER_LIBRARY_NAME}Java)
  include_directories(${WRAPPER_LIBRARY_INCLUDE_DIRECTORIES})
  add_library(${lib} MODULE ${WRAP_ITK_JAVA_CXX_FILES} ${WRAPPER_LIBRARY_CXX_SOURCES})

  if(APPLE)
    set_target_properties(${lib} PROPERTIES SUFFIX .jnilib)
  endif(APPLE)

  # work around linkage problem on some solaris systems
  if(CMAKE_SYSTEM MATCHES "SunOS-." AND CMAKE_COMPILER_IS_GNUCXX AND CMAKE_COMPILER_IS_GNUCC)
    target_link_libraries(${lib} stdc++)
  endif(CMAKE_SYSTEM MATCHES "SunOS-." AND CMAKE_COMPILER_IS_GNUCXX AND CMAKE_COMPILER_IS_GNUCC)

  # strict-aliasing breaks produced code with gcc, according to http://www.swig.org/Doc1.3/Java.html#compiling_dynamic
  if(CMAKE_COMPILER_IS_GNUCC)
    set_target_properties(${lib} PROPERTIES COMPILE_FLAGS -fno-strict-aliasing)
  endif(CMAKE_COMPILER_IS_GNUCC)

  target_link_libraries(${lib} ${WRAPPER_LIBRARY_LINK_LIBRARIES})

  if(WRAP_ITK_EXPLICIT AND NOT ${WRAPPER_LIBRARY_NAME} STREQUAL ITKJavaBase)
    target_link_libraries(${lib} ${WRAPPER_LIBRARY_NAME}Explicit)
  endif(WRAP_ITK_EXPLICIT AND NOT ${WRAPPER_LIBRARY_NAME} STREQUAL ITKJavaBase)

  add_dependencies(${lib} ${WRAPPER_LIBRARY_NAME}Swig)
  install(TARGETS "${lib}" DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}/Java")

  if(EXTERNAL_WRAP_ITK_PROJECT)
    # don't depends on the targets from wrapitk in external projects
    foreach(dep ${WRAPPER_LIBRARY_DEPENDS} ITKJavaBase)
      add_dependencies(${lib} ${dep}Swig)
    endforeach(dep)
  endif(EXTERNAL_WRAP_ITK_PROJECT)

  if(WIN32)
    set(sep "\;")
  else(WIN32)
    set(sep ":")
  endif(WIN32)

  set(class_path )
  # Current module source path
  set(class_path "${class_path}${java_package_path}${sep}")
  # Dependency modules bin path
  #set(class_path "${class_path}${WRAP_ITK_JAVA_BIN_DIR}${sep}")
  # Dependency modules jar files
  # manifest content
  set(WRAP_ITK_JAVA_MANIFEST )
  foreach(dep ${WRAPPER_LIBRARY_DEPENDS} ITKJavaBase)
    # create the import package commands
    string(TOLOWER ${dep} ldep)
    set(dep_jar_file ${WRAP_ITK_LIB_DIRECTORY}/${WRAP_ITK_JAVA_CORE_PACKAGE_NAME}.${ldep}.jar)
    set(class_path "${class_path}${dep_jar_file}${sep}")
    set(WRAP_ITK_JAVA_MANIFEST "${WRAP_ITK_JAVA_MANIFEST} \n ${WRAP_ITK_JAVA_CORE_PACKAGE_NAME}.${ldep}.jar ")
  endforeach(dep)
  foreach(ucp ${WRAP_ITK_JAVA_USER_CLASS_PATH})
    set(class_path "${class_path}${ucp}${sep}")
  endforeach(ucp)

  # create the manifest file
  if(WRAP_ITK_JAVA_MANIFEST)
    set(WRAP_ITK_JAVA_MANIFEST "Class-Path: ${WRAP_ITK_JAVA_MANIFEST}")
  endif(WRAP_ITK_JAVA_MANIFEST)
  configure_file("${WRAP_ITK_JAVA_SOURCE_DIR}/manifest.in"
                "${CMAKE_CURRENT_BINARY_DIR}/manifest"
                @ONLY IMMEDIATE)

  # Add java proxy compilation stuff
  # Only add this if there is any classes to be built
  if(NOT "${WRAP_ITK_JAVA_CLASS_NAMES}" STREQUAL "")
    # compile java proxies
    set(lib_jar ${lib}Jar)
    add_custom_command(
      OUTPUT ${java_bin_file}
      DEPENDS ${java_file}
      COMMAND ${Java_JAVAC_EXECUTABLE}
      ARGS
      ${WRAP_ITK_JAVAC_ARGS}
      -classpath "${class_path}"
      -d "${WRAP_ITK_JAVA_BIN_DIR}"
#      -verbose
      "${java_package_path}/*.java"
      COMMENT "Compiling java proxies: ${java_package_name}"
      )

    add_custom_command(
      OUTPUT ${java_jar_file_path}
      DEPENDS ${java_bin_file} ${CMAKE_CURRENT_BINARY_DIR}/manifest
      COMMAND ${Java_JAR_EXECUTABLE}
      ARGS
      -cmf "${CMAKE_CURRENT_BINARY_DIR}/manifest" "${java_jar_file_path}"
      "${WRAP_ITK_JAVA_CORE_PACKAGE_PATH}/${_java_package_name}/"
      WORKING_DIRECTORY ${WRAP_ITK_JAVA_BIN_DIR}
      COMMENT "Creating java archive: ${java_jar_file_name}"
      )
    add_custom_target(${lib_jar} ALL DEPENDS ${java_bin_file} ${java_jar_file_path})
    WRAP_ITK_BINDINGS_INSTALL(/Java ${java_jar_file_path})

    add_dependencies(${lib_jar} ${lib})
    if(NOT EXTERNAL_WRAP_ITK_PROJECT)
      # don't depends on the targets from wrapitk in external projects
      foreach(dep ${WRAPPER_LIBRARY_DEPENDS} ITKJavaBase)
        add_dependencies(${lib_jar} ${dep}Java)
        add_dependencies(${lib_jar} ${dep}JavaJar)
      endforeach(dep)
    endif(NOT EXTERNAL_WRAP_ITK_PROJECT)

    set(WRAP_ITK_JAVA_JAR_DEPS ${WRAP_ITK_JAVA_JAR_DEPS} ${lib_jar})
  else(NOT "${WRAP_ITK_JAVA_CLASS_NAMES}" STREQUAL "")
    message("Warning: Unable to create java proxies for ${WRAPPER_LIBRARY_NAME}, nothing to get wrapped in this module.")
  endif(NOT "${WRAP_ITK_JAVA_CLASS_NAMES}" STREQUAL "")

  # store the module dependencies to be able to reuse them easily - without recurssion
  set(deps )
  foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
    set(deps ${deps} ${WRAP_ITK_JAVA_${dep}_DEPS} ${dep})
  endforeach()
  if(deps)
    list(REMOVE_DUPLICATES deps)
  endif()
  set(WRAP_ITK_JAVA_${WRAPPER_LIBRARY_NAME}_DEPS ${deps} CACHE INTERNALL "java module dependencies" FORCE)
endmacro(END_WRAP_LIBRARY_JAVA)

###############################################################################
macro(END_WRAP_MODULE_JAVA group_name)
  set(WRAP_ITK_JAVA_CLASS_NAMES ${WRAP_ITK_JAVA_CLASS_NAMES} ${group_name}Java)

  # set the package name and path
  string(TOLOWER ${WRAPPER_LIBRARY_NAME} java_package_name)
  set(java_package_path ${WRAP_ITK_JAVA_SRC_DIR}/${WRAP_ITK_JAVA_CORE_PACKAGE_PATH}/${java_package_name})
  set(java_package_name ${WRAP_ITK_JAVA_CORE_PACKAGE_NAME}.${java_package_name})

  # set the java source files
  set(java_file )
  foreach(class_name ${WRAP_ITK_JAVA_CLASS_NAMES})
    set(java_file ${java_file} ${java_package_path}/${class_name}.java)
  endforeach(class_name ${WRAP_ITK_JAVA_CLASS_NAMES})

  set(base_name ${group_name})

  # create the swig interface for all the groups in the module
  set(interface_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${base_name}.i" )
  set(lib ${group_name}Java)
  set(cpp_file "${CMAKE_CURRENT_BINARY_DIR}/${base_name}Java.cpp")

  set(WRAP_ITK_JAVA_CXX_FILES ${WRAP_ITK_JAVA_CXX_FILES} ${cpp_file})

  # prepare dependencies
  set(DEPS )
  set(WRAP_ITK_JAVA_TYPEMAP_JAVAIMPORTS ${WRAP_ITK_JAVA_EXTENDED_JAVAIMPORTS})
  foreach(dep ${WRAPPER_LIBRARY_DEPENDS} ITKJavaBase)
    # library dependencies
    set(DEPS ${DEPS} ${${dep}SwigFiles})

    # create the import package commands
    string(TOLOWER ${dep} java_import_package_name)
    set(java_import_package_name ${WRAP_ITK_JAVA_CORE_PACKAGE_NAME}.${java_import_package_name})
    set(WRAP_ITK_JAVA_TYPEMAP_JAVAIMPORTS ${WRAP_ITK_JAVA_TYPEMAP_JAVAIMPORTS} "import ${java_import_package_name}.*;")
    foreach(ddep ${WRAP_ITK_JAVA_${dep}_DEPS})
      string(TOLOWER ${ddep} java_import_package_name)
      set(java_import_package_name ${WRAP_ITK_JAVA_CORE_PACKAGE_NAME}.${java_import_package_name})
      set(WRAP_ITK_JAVA_TYPEMAP_JAVAIMPORTS ${WRAP_ITK_JAVA_TYPEMAP_JAVAIMPORTS} "import ${java_import_package_name}.*;")
    endforeach()
  endforeach(dep)

  set(swig_command ${SWIG_EXECUTABLE})
  if(WRAP_ITK_USE_CCACHE)
    set(swig_command ${CCACHE_EXECUTABLE} ${swig_command})
  endif(WRAP_ITK_USE_CCACHE)

  # if this is for an external library, let the user add extra swig args
  if(EXTERNAL_WRAP_ITK_PROJECT)
    set(WRAP_ITK_SWIG_ARGS_JAVA "" CACHE STRING "Extra user-defined swig arguments to be to the swig executable.")
  endif(EXTERNAL_WRAP_ITK_PROJECT)

  add_custom_command(
    OUTPUT ${cpp_file} ${java_file}
    COMMAND ${swig_command} -c++ -java -O -features autodoc=1
#    -Werror
    -package ${java_package_name}
#       -fcompact
    -w508 -w312 -w314 -w509 -w302 -w362
    -w389 # operator[], to be suppressed later...
    -w384 -w383 # operator++ ane operator--
    -w361 # operator!
    -w467 # overloaded functions (with typemaps)
    -w401 -w503 -w516 #!
    -w303 # extend itklightobject
    -w350 -w394 -w395  # operator new/new[]/delete[] ignored
    -o ${cpp_file}
    -I${LANGUAGES_SRC_DIR}
    -I${WRAP_ITK_JAVA_SOURCE_DIR}
    -I${WRAP_ITK_TYPEDEFS_DIRECTORY}/java
    -I${WRAP_ITK_TYPEDEFS_DIRECTORY}
    ${WRAP_ITK_SWIG_ARGS_JAVA}
    -outdir ${java_package_path}
    ${interface_file}
    WORKING_DIRECTORY ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/java
    DEPENDS ${java_package_path}/dummy ${DEPS} ${interface_file} ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/java/${base_name}_ext.i ${WRAP_ITK_JAVA_SOURCE_DIR}/java.i ${SWIG_EXECUTABLE}
    COMMENT "Generating Java wrap files for class ${java_package_name}.${group_name}"
  )
  # ${WRAP_ITK_JAVA_LIBRARY_DEPS}

  configure_file("${WRAP_ITK_JAVA_SOURCE_DIR}/module_ext.i.in"
    "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/java/${group_name}_ext.i"
    @ONLY IMMEDIATE)

  WRAP_ITK_INSTALL("/Configuration/Typedefs/java"
   "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/java/${group_name}_ext.i")

  set(WRAP_ITK_JAVA_LIBRARY_DEPS ${WRAP_ITK_JAVA_LIBRARY_DEPS} "${java_file}" "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/java/${base_name}_ext.i")

endmacro(END_WRAP_MODULE_JAVA group_name)


###############################################################################
macro(WRAP_MODULE_JAVA module)

  set(text "\n")

  set(text "${text}%include java.i\n\n")

  set(WRAP_ITK_JAVA_SWIG_EXT "${text}")

endmacro(WRAP_MODULE_JAVA)


###############################################################################
macro(ADD_SIMPLE_TYPEDEF_JAVA wrap_class swig_name)

  if("${swig_name}" MATCHES "_Pointer$")
    string(REGEX REPLACE "_Pointer$" "" smart_pointed "${swig_name}")
    ADD_JAVA_POINTER_TYPEMAP("${smart_pointed}")
  endif()

endmacro(ADD_SIMPLE_TYPEDEF_JAVA wrap_class swig_name)


###############################################################################
macro(ADD_JAVA_POINTER_TYPEMAP template_params)

  set(text "DECLARE_REF_COUNT_CLASS_JAVA(${template_params})\n")
  set(WRAP_ITK_JAVA_SWIG_EXT "${WRAP_ITK_JAVA_SWIG_EXT}${text}")

endmacro(ADD_JAVA_POINTER_TYPEMAP template_params)


###############################################################################
macro(ADD_ONE_TYPEDEF_JAVA wrap_method wrap_class swig_name template_params)

  # store the wrapped final class names
  set(WRAP_ITK_JAVA_CLASS_NAMES ${WRAP_ITK_JAVA_CLASS_NAMES} ${swig_name})

endmacro(ADD_ONE_TYPEDEF_JAVA wrap_method wrap_class swig_name template_params)


###############################################################################
# Create the JavaUtils library

if(NOT EXTERNAL_WRAP_ITK_PROJECT)
  macro(END_WRAP_LIBRARIES_JAVA)
    add_subdirectory(${WRAP_ITK_JAVA_SOURCE_DIR}/Tests)
#    add_subdirectory(${WRAP_ITK_JAVA_SOURCE_DIR}/JavaUtils)
  endmacro(END_WRAP_LIBRARIES_JAVA)

  macro(WRAP_LIBRARIES_JAVA)
    add_subdirectory(${WRAP_ITK_JAVA_SOURCE_DIR}/JavaBase)
  endmacro(WRAP_LIBRARIES_JAVA)

else(NOT EXTERNAL_WRAP_ITK_PROJECT)
  macro(END_WRAP_LIBRARIES_JAVA)
    # just do nothing
  endmacro(END_WRAP_LIBRARIES_JAVA)

  macro(WRAP_LIBRARIES_JAVA)
    # just do nothing
  endmacro(WRAP_LIBRARIES_JAVA)

endif(NOT EXTERNAL_WRAP_ITK_PROJECT)
