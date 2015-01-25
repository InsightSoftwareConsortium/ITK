###############################################################################
# Find python
find_package(PythonInterp REQUIRED)

###############################################################################
# Build swig

option(ITK_USE_SYSTEM_SWIG "Use system swig. If OFF, swig is built as an external project." OFF)

# Minimal swig version
set(swig_version_min 3.0.0)

set(ITK_SWIG_VERSION 3.0.5)
set(swig_md5     dcb9638324461b9baba8e044fe59031d)
set(swigwin_md5  fd2e050f29e2a00b2348f5f7d3476490)

if(WIN32)
  set(swig_ep "${CMAKE_CURRENT_BINARY_DIR}/swigwin-${ITK_SWIG_VERSION}/swig.exe")
else()
    # follow the standard EP_PREFIX locations
    set ( swig_binary_dir ${CMAKE_CURRENT_BINARY_DIR}/swig-prefix/src/swig-build )
    set ( swig_source_dir ${CMAKE_CURRENT_BINARY_DIR}/swig-prefix/src/swig )
    set ( swig_install_dir ${CMAKE_CURRENT_BINARY_DIR}/swig )

  set(swig_ep "${swig_install_dir}/bin/swig")
endif()

if(ITK_USE_SYSTEM_SWIG)

  # the path set for the EP build prevents find_package to do its job
  if("${SWIG_EXECUTABLE}" STREQUAL "${swig_ep}")
    unset(SWIG_DIR CACHE)
    unset(SWIG_EXECUTABLE CACHE)
    unset(SWIG_VERSION CACHE)
  endif()

  # Look for system swig
  find_package(SWIG REQUIRED)

  # Check for the swig version
  if(${SWIG_VERSION} VERSION_LESS ${swig_version_min})
    message(WARNING "Swig version less than ${swig_version_min}: \"${SWIG_VERSION}\".")
  endif()

else()

  # Install swig ourselves
  set(SWIG_VERSION ${ITK_SWIG_VERSION})

  include(ExternalProject)
  if(WIN32)
    # If we are building ITK
    if(ITK_BINARY_DIR)
      itk_download_attempt_check(SWIG)
    endif()
    ExternalProject_Add(swig
      URL http://prdownloads.sourceforge.net/swig/swigwin-${SWIG_VERSION}.zip
      URL_MD5 ${swigwin_md5}
      SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/swigwin-${SWIG_VERSION}
      CONFIGURE_COMMAND ""
      BUILD_COMMAND ""
      INSTALL_COMMAND ""
      )
    set(SWIG_DIR ${CMAKE_CURRENT_BINARY_DIR}/swigwin-${SWIG_VERSION})
  else()
    # From PCRE configure
    # Some influential environment variables:
    #  CC          C compiler command
    #  CFLAGS      C compiler flags
    #  LDFLAGS     linker flags, e.g. -L<lib dir> if you have libraries in a
    #              nonstandard directory <lib dir>
    #  LIBS        libraries to pass to the linker, e.g. -l<library>
    #  CPPFLAGS    (Objective) C/C++ preprocessor flags, e.g. -I<include dir> if
    #              you have headers in a nonstandard directory <include dir>
    #  CXX         C++ compiler command
    #  CXXFLAGS    C++ compiler flags
    #  CPP         C preprocessor
    #  CXXCPP      C++ preprocessor

    # build swig as an external project

    # If we are building ITK
    if(ITK_BINARY_DIR)
      itk_download_attempt_check(PCRE)
    endif()
    ExternalProject_Add(PCRE
    URL http://downloads.sourceforge.net/project/pcre/pcre/8.36/pcre-8.36.tar.gz
    URL_MD5 ff7b4bb14e355f04885cf18ff4125c98
    CONFIGURE_COMMAND
      env
        "CC=${CMAKE_C_COMPILER} ${CMAKE_C_COMPILER_ARG1}"
        "CFLAGS=${CMAKE_C_FLAGS} ${CMAKE_C_FLAGS_RELEASE}"
        "LDFLAGS=$ENV{LDFLAGS}"
        "LIBS=$ENV{LIBS}"
        "CPPFLAGS=$ENV{CPPFLAGS}"
        "CXX=${CMAKE_CXX_COMPILER} ${CMAKE_CXX_COMPILER_ARG1}"
        "CXXFLAGS=${CMAKE_CXX_FLAGS} ${CMAKE_CXX_FLAGS_RELEASE}"
        "CPP=$ENV{CPP}"
        "CXXPP=$ENV{CXXPP}"
      ../PCRE/configure
      --prefix=${CMAKE_CURRENT_BINARY_DIR}/PCRE
      --enable-shared=no
    )

    # swig uses bison find it by cmake and pass it down
    find_package ( BISON )
    set ( BISON_FLAGS "" CACHE STRING "Flags used by bison" )
    mark_as_advanced ( BISON_FLAGS )


    # From SWIG configure
    # Some influential environment variables:
    #  CC          C compiler command
    #  CFLAGS      C compiler flags
    #  LDFLAGS     linker flags, e.g. -L<lib dir> if you have libraries in a
    #              nonstandard directory <lib dir>
    #  LIBS        libraries to pass to the linker, e.g. -l<library>
    #  CPPFLAGS    (Objective) C/C++ preprocessor flags, e.g. -I<include dir> if
    #              you have headers in a nonstandard directory <include dir>
    #  CXX         C++ compiler command
    #  CXXFLAGS    C++ compiler flags
    #  CPP         C preprocessor
    #  PCRE_CONFIG config script used for pcre
    #  PCRE_CFLAGS CFLAGS used for pcre
    #  PCRE_LIBS   LIBS used for pcre
    #  YACC        The `Yet Another C Compiler' implementation to use. Defaults to
    #              the first program found out of: `bison -y', `byacc', `yacc'.
    #  YFLAGS      The list of arguments that will be passed by default to $YACC.
    #              This script will default YFLAGS to the empty string to avoid a
    #              default value of `-d' given by some make applications.

    # If we are building ITK
    if(ITK_BINARY_DIR)
      itk_download_attempt_check(SWIG)
    endif()
    ExternalProject_Add(swig
    URL http://prdownloads.sourceforge.net/swig/swig-${SWIG_VERSION}.tar.gz
    URL_MD5 ${swig_md5}
    CONFIGURE_COMMAND
      env
        "CC=${CMAKE_C_COMPILER} ${CMAKE_C_COMPILER_ARG1}"
        "CFLAGS=${CMAKE_C_FLAGS} ${CMAKE_C_FLAGS_RELEASE}"
        "LDFLAGS=$ENV{LDFLAGS}"
        "LIBS=$ENV{LIBS}"
        "CPPFLAGS=$ENV{CPPFLAGS}"
        "CXX=${CMAKE_CXX_COMPILER} ${CMAKE_CXX_COMPILER_ARG1}"
        "CXXFLAGS=${CMAKE_CXX_FLAGS} ${CMAKE_CXX_FLAGS_RELEASE}"
        "CPP=$ENV{CPP}"
        "YACC=${BISON_EXECUTABLE}"
        "YFLAGS=${BISON_FLAGS}"
      ../swig/configure
        --prefix=${CMAKE_CURRENT_BINARY_DIR}/swig
        --with-pcre-prefix=${CMAKE_CURRENT_BINARY_DIR}/PCRE
    DEPENDS PCRE

    )
    set(SWIG_DIR ${CMAKE_CURRENT_BINARY_DIR}/swig/share/swig/${SWIG_VERSION} CACHE FILEPATH "swig directory." FORCE)
  endif()
  set(SWIG_EXECUTABLE ${swig_ep} CACHE FILEPATH "swig executable." FORCE)
endif()

###############################################################################

set(IDX_GENERATOR ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/idx.py CACHE INTERNAL "idx generator path")
set(PYGCCXML_DIR ${CMAKE_SOURCE_DIR}/Modules/ThirdParty/pygccxml/src CACHE INTERNAL "pygccxml path")

###############################################################################
# install the files requires for swiginterface
if(NOT EXTERNAL_WRAP_ITK_PROJECT)
  WRAP_ITK_INSTALL(/Configuration/Generators/SwigInterface CMakeLists.txt)
  WRAP_ITK_INSTALL(/Configuration/Generators/SwigInterface Master.mdx.in)
  WRAP_ITK_INSTALL(/Configuration/Generators/SwigInterface empty.in)
  WRAP_ITK_INSTALL(/Configuration/Generators/SwigInterface module.i.in)
  WRAP_ITK_INSTALL(/Configuration/Generators/SwigInterface module.includes.in)

  install(DIRECTORY ${PYGCCXML_DIR}
    DESTINATION "${WRAP_ITK_INSTALL_PREFIX}/Configuration/Generators/SwigInterface"
    PATTERN ".git" EXCLUDE
  )

  install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/idx.py"
                "${CMAKE_CURRENT_SOURCE_DIR}/igenerator.py"
    DESTINATION "${WRAP_ITK_INSTALL_PREFIX}/Configuration/Generators/SwigInterface"
  )

endif()


###############################################################################
# store the current dir, so it can be reused later
set(ITK_WRAP_SWIGINTERFACE_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}" CACHE INTERNAL "swig interface source dir")
set(ITK_WRAP_SWIGINTERFACE_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}" CACHE INTERNAL "swig interface binary dir")

set(WRAPPER_MASTER_INDEX_OUTPUT_DIR "${PROJECT_BINARY_DIR}/Typedefs" CACHE INTERNAL "typedefs dir")
set(IGENERATOR  "${CMAKE_CURRENT_SOURCE_DIR}/igenerator.py" CACHE INTERNAL "igenerator.py path" FORCE)

macro(itk_wrap_module_swig_interface library_name)
  # store the content of the mdx file
  set(SWIG_INTERFACE_MDX_CONTENT )
  # store the content of the .i file for the module - a set of import of all the .i files generated for the module
  set(SWIG_INTERFACE_MODULE_CONTENT )
  # the list of .idx files generated for the module
  set(SWIG_INTERFACE_IDX_FILES )
  # build a list of modules to create the igenerator custom command in
  # itk_end_wrap_module_swig_interface
  set(SWIG_INTERFACE_MODULES )
endmacro()

macro(itk_end_wrap_module_swig_interface)

  # Loop over the extra swig input files and copy them to the Typedefs directory
  foreach(source ${WRAPPER_LIBRARY_SWIG_INPUTS})
    file(COPY "${source}"
         DESTINATION "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}")
    get_filename_component(basename ${source} NAME)
    set(dest "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${basename}")
    WRAP_ITK_INSTALL("/Configuration/Typedefs" "${dest}")
#    set(SWIG_INTERFACE_MODULE_CONTENT "${SWIG_INTERFACE_MODULE_CONTENT}%import ${basename}\n")
  endforeach()

  # the list of .i files generated for the module
  set(SWIG_INTERFACE_FILES )

  # prepare dependencies
  set(DEPS )
  foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
    set(DEPS ${DEPS} ${${dep}IdxFiles} ${${dep}SwigFiles})
    set(SWIG_INTERFACE_MDX_CONTENT "${dep}.mdx\n${SWIG_INTERFACE_MDX_CONTENT}")
  endforeach()

  # add some libs required by this module
  set(swig_libs )
  foreach(swig_lib ${WRAPPER_SWIG_LIBRARY_FILES})
    get_filename_component(basename ${swig_lib} NAME)
    set(swig_libs ${swig_libs} --swig-include ${basename})
    file(COPY "${swig_lib}"
      DESTINATION "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}")
    set(dest "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${basename}")
    WRAP_ITK_INSTALL("/Configuration/Typedefs" "${dest}")
  endforeach()

  # accumulate the idx files already generated before this module to generate usable depenencies
  set(idx_files )
  foreach(module ${SWIG_INTERFACE_MODULES})
    set(idx_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${module}.idx")
    list(APPEND idx_files ${idx_file})
  endforeach()
  foreach(module ${SWIG_INTERFACE_MODULES})
    # create the swig interface
    set(interface_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${module}.i")
    set(xml_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module}.xml")
#    set(includes_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.includes")
    set(module_interface_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.i")
    set(typedef_in_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module}SwigInterface.h.in")
    set(typedef_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${module}SwigInterface.h")
    # prepare the options
    set(opts )
    foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
      set(opts ${opts} --mdx "${WRAP_ITK_TYPEDEFS_DIRECTORY}/${dep}.mdx")
#      set(opts ${opts} --include "${dep}.includes")
#      set(opts ${opts} --import "${dep}.i")
    endforeach()
    # import the interface files previously defined instead of importing all the files defined
    # in the module to avoid many warnings when running swig
#    foreach(i ${SWIG_INTERFACE_FILES})
#      get_filename_component(bi "${i}" NAME)
#      set(opts ${opts} --import "${bi}")
#    endforeach()

    if(ITK_WRAP_EXPLICIT)
      set(opts ${opts} --include "${WRAPPER_LIBRARY_NAME}Explicit.h")
    endif()

    add_custom_command(
      OUTPUT ${interface_file} ${typedef_file}
      COMMAND ${PYTHON_EXECUTABLE} ${IGENERATOR}
        ${opts}
        --swig-include itk.i
        ${swig_libs}
        --mdx ${mdx_file}
#        --include ${WRAPPER_LIBRARY_NAME}.includes
#        --import ${module_interface_file}
        --swig-include ${module}_ext.i
        -w1 -w3 -w51 -w52 -w53 -w54
        -A protected -A private
        -p ${PYGCCXML_DIR}
        -g ${CASTXML}
        --typedef-input ${typedef_in_file}
        --typedef-output ${typedef_file}
        --include ${module}SwigInterface.h
        ${xml_file}
        ${interface_file}
      DEPENDS ${DEPS} ${idx_files} ${IGENERATOR} # ${SWIG_INTERFACE_IDX_FILES} ${SWIG_INTERFACE_FILES} ${typedef_in_file} # ${includes_file}
      VERBATIM
    )

    set(SWIG_INTERFACE_FILES ${SWIG_INTERFACE_FILES} ${interface_file})
    WRAP_ITK_INSTALL("/Configuration/Typedefs" "${interface_file}")
  endforeach()

  # the mdx file
  set(mdx_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.mdx")
  set(CONFIG_INDEX_FILE_CONTENT "${SWIG_INTERFACE_MDX_CONTENT}")
  configure_file("${ITK_WRAP_SWIGINTERFACE_SOURCE_DIR}/Master.mdx.in" "${mdx_file}"
     @ONLY)
  WRAP_ITK_INSTALL("/Configuration/Typedefs" "${mdx_file}")

  set(module_interface_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.i")
  set(module_interface_ext_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}_ext.i")
  set(deps_imports )
  set(deps_includes )
  foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
    set(deps_imports "${deps_imports}%import ${dep}.i\n")
#    set(deps_includes "${deps_includes}#include \"${dep}.includes\"\n")
  endforeach()

#  set(SWIG_INTERFACE_INCLUDES "${deps_includes}#include \"${WRAPPER_LIBRARY_NAME}.includes\"")
  set(CONFIG_MODULE_INTERFACE_CONTENT ) #"${deps_imports}${SWIG_INTERFACE_MODULE_CONTENT}")
  configure_file("${ITK_WRAP_SWIGINTERFACE_SOURCE_DIR}/module.i.in" "${module_interface_file}"
    @ONLY)
  WRAP_ITK_INSTALL("/Configuration/Typedefs/" "${module_interface_file}")

#  set(WRAP_ITK_FILE_CONTENT )
#  configure_file("${WRAP_ITK_CONFIG_DIR}/empty.in" "${module_interface_ext_file}"
#    @ONLY)
#  install(FILES "${module_interface_ext_file}"
#    DESTINATION "${WRAP_ITK_INSTALL_PREFIX}/Configuration/Typedefs/"
#  )

  add_custom_target(${WRAPPER_LIBRARY_NAME}Idx DEPENDS ${SWIG_INTERFACE_IDX_FILES})
  add_dependencies(${WRAPPER_LIBRARY_NAME}Idx ${WRAPPER_LIBRARY_NAME}GccXML)
  set(${WRAPPER_LIBRARY_NAME}IdxFiles ${SWIG_INTERFACE_IDX_FILES} CACHE INTERNAL "Internal ${WRAPPER_LIBRARY_NAME}Idx file list.")

  add_custom_target(${WRAPPER_LIBRARY_NAME}Swig DEPENDS ${SWIG_INTERFACE_FILES})
  set(${WRAPPER_LIBRARY_NAME}SwigFiles ${SWIG_INTERFACE_FILES} CACHE INTERNAL "Internal ${WRAPPER_LIBRARY_NAME}Swig file list.")
  add_dependencies(${WRAPPER_LIBRARY_NAME}Swig ${WRAPPER_LIBRARY_NAME}Idx)
  if(NOT EXTERNAL_WRAP_ITK_PROJECT)
    # don't depends on the targets from wrapitk in external projects
    foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
      add_dependencies(${WRAPPER_LIBRARY_NAME}Swig ${dep}Swig ${dep}Idx)
    endforeach()
  endif()

endmacro()


macro(itk_wrap_include_swig_interface include_file)
  list(APPEND SWIG_INTERFACE_INCLUDES ${include_file})
endmacro()


macro(itk_wrap_submodule_swig_interface module)
  # store the content of the SwigInterface.h files - a set of #includes for that module
  set(SWIG_INTERFACE_INCLUDES )
  # typedefs for swig
  set(SWIG_INTERFACE_TYPEDEFS )
endmacro()

macro(itk_end_wrap_submodule_swig_interface module)
  # variables used:
  # WRAPPER_LIBRARY_NAME
  # WRAPPER_LIBRARY_OUTPUT_DIR
  # WRAPPER_LIBRARY_DEPENDS
  # WRAPPER_MASTER_INDEX_OUTPUT_DIR
  # MODULE_INCLUDES


  set(SWIG_INTERFACE_INCLUDES_CONTENT )
#  foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
#    set(SWIG_INTERFACE_INCLUDES_CONTENT "${SWIG_INTERFACE_INCLUDES_CONTENT}#include \"${dep}.includes\"\n")
#  endforeach()
  if(SWIG_INTERFACE_INCLUDES)
    list(REMOVE_DUPLICATES SWIG_INTERFACE_INCLUDES)
    foreach(include_file ${SWIG_INTERFACE_INCLUDES})
      if("${include_file}" MATCHES "<.*>")
        set(SWIG_INTERFACE_INCLUDES_CONTENT "${SWIG_INTERFACE_INCLUDES_CONTENT}#include ${include_file}\n")
      else()
        set(SWIG_INTERFACE_INCLUDES_CONTENT "${SWIG_INTERFACE_INCLUDES_CONTENT}#include \"${include_file}\"\n")
      endif()
    endforeach()
  endif()
  # create the file which store all the includes
  set(includes_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module}SwigInterface.h.in")
  configure_file("${ITK_WRAP_SWIGINTERFACE_SOURCE_DIR}/module.includes.in"
     ${includes_file}
     @ONLY)
  WRAP_ITK_INSTALL("/Configuration/Typedefs/" "${includes_file}")

  # the master idx file
  set(mdx_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.mdx")

  # generate the idx file for all the groups of the module
  set(idx_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${module}.idx")
  add_custom_command(
    OUTPUT ${idx_file}
    COMMAND ${IDX_GENERATOR}
      ${PYGCCXML_DIR} ${CASTXML} ${xml_file} ${idx_file}
    DEPENDS ${IDX_GENERATOR} ${xml_file}
    VERBATIM
  )
  WRAP_ITK_INSTALL("/Configuration/Typedefs/" "${idx_file}")
#   add_custom_target(${module}Idx DEPENDS ${idx_file})

  # store the path of the idx file to store it in the mdx file
  set(SWIG_INTERFACE_MDX_CONTENT "${SWIG_INTERFACE_MDX_CONTENT}${module}.idx\n")
  set(SWIG_INTERFACE_IDX_FILES ${SWIG_INTERFACE_IDX_FILES} ${idx_file})

  set(SWIG_INTERFACE_MODULE_CONTENT "${SWIG_INTERFACE_MODULE_CONTENT}%import ${module}.i\n")

  set(SWIG_INTERFACE_MODULES ${SWIG_INTERFACE_MODULES} ${module})

#  set(interface_ext_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${module}_ext.i")
#  set(WRAP_ITK_FILE_CONTENT )
#  configure_file("${WRAP_ITK_CONFIG_DIR}/empty.in" "${interface_ext_file}"
#    @ONLY)
#  install(FILES "${interface_ext_file}"
#    DESTINATION "${WRAP_ITK_INSTALL_PREFIX}/Configuration/Typedefs/"
#  )

endmacro()


macro(itk_wrap_one_type_swig_interface wrap_method wrap_class swig_name)
  # Add one  typedef to WRAPPER_TYPEDEFS
  # 'wrap_method' is the one of the valid WRAPPER_WRAP_METHODS from itk_wrap_class,
  # 'wrap_class' is the fully-qualified C++ name of the class
  # 'swig_name' is what the swigged class should be called
  # The optional last argument is the template parameters that should go between
  # the < > brackets in the C++ template definition.
  # Only pass 3 parameters to wrap a non-templated class
  #
  # Global vars used: none
  # Global vars modified: WRAPPER_TYPEDEFS

  # get the base C++ class name (no namespaces) from wrap_class:
  string(REGEX REPLACE "(.*::)" "" base_name "${wrap_class}")

  set(wrap_pointer 0)
  set(template_parameters "${ARGV3}")
  if(template_parameters)
    set(full_class_name "${wrap_class}< ${template_parameters} >")
  else()
    set(full_class_name "${wrap_class}")
  endif()

  # itk_wrap_one_type_all_generators("${wrap_method}" "${wrap_class}" "${swig_name}" "${ARGV3}")

  # Add a typedef for the class. We have this funny looking full_name::base_name
  # thing (it expands to, for example "typedef itk::Foo<baz, 2>::Foo") used
  # for gccxml typedefs

  if("${wrap_method}" MATCHES "2_SUPERCLASSES")
    itk_wrap_simple_type_swig_interface("${full_class_name}::Superclass::Superclass" "${swig_name}_Superclass_Superclass")
    itk_wrap_simple_type_swig_interface("${full_class_name}::Superclass::Superclass::Pointer" "${swig_name}_Superclass_Superclass_Pointer")
  endif()

  if("${wrap_method}" MATCHES "SUPERCLASS")
    itk_wrap_simple_type_swig_interface("${full_class_name}::Superclass" "${swig_name}_Superclass")
    itk_wrap_simple_type_swig_interface("${full_class_name}::Superclass::Pointer" "${swig_name}_Superclass_Pointer")
  endif()

  if("${wrap_method}" MATCHES "CONST_POINTER")
    # add a const pointer typedef if we are so asked
    itk_wrap_simple_type_swig_interface("${full_class_name}::ConstPointer" "${swig_name}_ConstPointer")
  endif()

  # the same output with or without FORCE_INSTANTIATE
  itk_wrap_simple_type_swig_interface("${full_class_name}" "${swig_name}")

  if("${wrap_method}" MATCHES "POINTER")
    if("${wrap_method}" STREQUAL "AUTOPOINTER")
      # add a pointer typedef if we are so asked
      itk_wrap_simple_type_swig_interface("${full_class_name}::SelfAutoPointer" "${swig_name}_AutoPointer")
    else()
      # add a pointer typedef if we are so asked
      itk_wrap_simple_type_swig_interface("${full_class_name}::Pointer" "${swig_name}_Pointer")
    endif()
  endif()

endmacro()


macro(itk_wrap_simple_type_swig_interface wrap_class swig_name)
  set(SWIG_INTERFACE_TYPEDEFS "${SWIG_INTERFACE_TYPEDEFS}typedef ${wrap_class} ${swig_name};\n")
endmacro()

include_directories("${WRAPPER_MASTER_INDEX_OUTPUT_DIR}")
