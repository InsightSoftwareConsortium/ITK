###############################################################################
# find swig
find_package(SWIG REQUIRED)
include(${SWIG_USE_FILE})

# find python
find_package(PythonInterp REQUIRED)

set(CABLE_INDEX ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/idx.py CACHE INTERNAL "cableidx path")

if("${CMAKE_VERSION}" STRLESS "2.6")
    # cmake 2.4 doesn't provied VERSION
    if(SWIG_DIR)
      set(SWIG_FOUND 1)
      set(SWIG_USE_FILE ${CMAKE_ROOT}/Modules/UseSWIG.cmake)
      execute_process(COMMAND ${SWIG_EXECUTABLE} -version
        OUTPUT_VARIABLE SWIG_version_output
        ERROR_VARIABLE SWIG_version_output
        RESULT_VARIABLE SWIG_version_result)
      if(SWIG_version_result)
        message(SEND_ERROR "Command \"${SWIG_EXECUTABLE} -version\" failed with output:\n${SWIG_version_output}")
      else(SWIG_version_result)
        string(REGEX REPLACE ".*SWIG Version[^0-9.]*\([0-9.]+\).*" "\\1"
          SWIG_version_output "${SWIG_version_output}")
        set(SWIG_VERSION ${SWIG_version_output} CACHE STRING "Swig version" FORCE)
      endif(SWIG_version_result)
    endif(SWIG_DIR)
endif("${CMAKE_VERSION}" STRLESS "2.6")

###############################################################################
# install the files requires for swiginterface
if(NOT EXTERNAL_WRAP_ITK_PROJECT)
  WRAP_ITK_INSTALL(/Configuration/Languages/SwigInterface CMakeLists.txt)
  WRAP_ITK_INSTALL(/Configuration/Languages/SwigInterface Master.mdx.in)
  WRAP_ITK_INSTALL(/Configuration/Languages/SwigInterface empty.in)
  WRAP_ITK_INSTALL(/Configuration/Languages/SwigInterface module.i.in)
  WRAP_ITK_INSTALL(/Configuration/Languages/SwigInterface module.includes.in)

  install(DIRECTORY pygccxml-1.0.0
    DESTINATION "${WRAP_ITK_INSTALL_PREFIX}/Configuration/Languages/SwigInterface"
    PATTERN ".svn" EXCLUDE
    PATTERN "CVS" EXCLUDE
    PATTERN "docs" EXCLUDE
    PATTERN "unittests" EXCLUDE
  )

  install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/idx.py"
                "${CMAKE_CURRENT_SOURCE_DIR}/igenerator.py"
    DESTINATION "${WRAP_ITK_INSTALL_PREFIX}/Configuration/Languages/SwigInterface"
  )

endif(NOT EXTERNAL_WRAP_ITK_PROJECT)


###############################################################################
# store the current dir, so it can be reused later
set(WRAP_ITK_SWIGINTERFACE_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}" CACHE INTERNAL "swig interface source dir")
set(WRAP_ITK_SWIGINTERFACE_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}" CACHE INTERNAL "swig interface binary dir")

set(WRAPPER_MASTER_INDEX_OUTPUT_DIR "${PROJECT_BINARY_DIR}/Typedefs" CACHE INTERNAL "typedefs dir")
set(IGENERATOR  "${CMAKE_CURRENT_SOURCE_DIR}/igenerator.py" CACHE INTERNAL "igenerator.py path" FORCE)

macro(WRAP_LIBRARY_SWIG_INTERFACE library_name)
  # store the content of the mdx file
  set(SWIG_INTERFACE_MDX_CONTENT )
  # store the content of the .i file for the module - a set of import of all the .i files generated for the module
  set(SWIG_INTERFACE_MODULE_CONTENT )
  # store the content of the .includes files - a set of #includes for that module
  set(SWIG_INTERFACE_INCLUDES )
  # the list of .idx files generated for the module
  set(SWIG_INTERFACE_IDX_FILES )
  # build a list of modules to create the ignerator custom command in
  # END_WRAP_LIBRARY_SWIG_INTERFACE
  set(SWIG_INTERFACE_MODULES )
  # typedefs for swig
  set(SWIG_INTERFACE_TYPEDEFS )
endmacro(WRAP_LIBRARY_SWIG_INTERFACE)

macro(END_WRAP_LIBRARY_SWIG_INTERFACE)

  # Loop over the extra swig input files and copy them to the Typedefs directory
  foreach(source ${WRAPPER_LIBRARY_SWIG_INPUTS})
    get_filename_component(basename ${source} NAME)
    set(dest "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${basename}")
    exec_program(${CMAKE_COMMAND} ARGS -E copy_if_different "\"${source}\"" "\"${dest}\"")
    WRAP_ITK_INSTALL("/Configuration/Typedefs" "${dest}")
#    set(SWIG_INTERFACE_MODULE_CONTENT "${SWIG_INTERFACE_MODULE_CONTENT}%import ${basename}\n")
  endforeach(source)


  set(SWIG_INTERFACE_INCLUDES_CONTENT )
  foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
    set(SWIG_INTERFACE_INCLUDES_CONTENT "${SWIG_INTERFACE_INCLUDES_CONTENT}#include \"${dep}.includes\"\n")
  endforeach(dep)
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

  # the list of .i files generated for the module
  set(SWIG_INTERFACE_FILES )

  # prepare dependencies
  set(DEPS )
  foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
    set(DEPS ${DEPS} ${${dep}IdxFiles} ${${dep}SwigFiles})
  endforeach(dep)

  # add some libs required by this module
  set(swig_libs )
  foreach(swig_lib ${WRAPPER_SWIG_LIBRARY_FILES})
    get_filename_component(basename ${swig_lib} NAME)
    set(swig_libs ${swig_libs} --swig-include ${basename})
    set(dest "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${basename}")
    exec_program(${CMAKE_COMMAND} ARGS -E copy_if_different "\"${swig_lib}\"" "\"${dest}\"")
    WRAP_ITK_INSTALL("/Configuration/Typedefs" "${dest}")
  endforeach(swig_lib WRAPPER_SWIG_LIBRARY_FILES)

  # accumulate the idx files already generated before this module to generate usable depenencies
  set(idx_files )
  foreach(module ${SWIG_INTERFACE_MODULES})
    # create the swig interface
    set(interface_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${module}.i")
    set(xml_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module}.xml")
    set(idx_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${module}.idx")
    set(idx_files ${idx_files} ${idx_file})
    set(includes_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.includes")
    set(module_interface_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.i")
    # prepare the options
    set(opts )
    foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
      set(opts ${opts} --mdx "${WRAP_ITK_TYPEDEFS_DIRECTORY}/${dep}.mdx")
      set(opts ${opts} --include "${dep}.includes")
#      set(opts ${opts} --import "${dep}.i")
    endforeach(dep)
    # import the interface files previously defined instead of importing all the files defined
    # in the module to avoid many warnings when running swig
#    foreach(i ${SWIG_INTERFACE_FILES})
#      get_filename_component(bi "${i}" NAME)
#      set(opts ${opts} --import "${bi}")
#    endforeach(i)

  # message("${opts}")

    # configure the test driver, to set the python path to the pygccxml dir
  #   find_program(ITK_TEST_DRIVER itkTestDriver)
  #   set(PYTHON_PYGCCXML_DRIVER "${ITK_TEST_DRIVER}"
  #     --add-before-env PYTHONPATH "${WRAP_ITK_CMAKE_DIR}/pygccxml-1.0.0/"
  #     "${PYTHON_EXECUTABLE}"
  #   )

    if(WRAP_ITK_EXPLICIT)
      set(opts ${opts} --include "${WRAPPER_LIBRARY_NAME}Explicit.h")
    endif(WRAP_ITK_EXPLICIT)

    add_custom_command(
      OUTPUT ${interface_file}
      COMMAND ${PYTHON_EXECUTABLE} ${IGENERATOR}
        ${opts}
        --swig-include itk.i
        ${swig_libs}
        --mdx ${mdx_file}
        --include ${WRAPPER_LIBRARY_NAME}.includes
#        --import ${module_interface_file}
        --swig-include ${module}_ext.i
        -w1 -w3 -w51 -w52 -w53 -w54 #--warning-error
        -A protected -A private
        # --verbose
        ${xml_file}
        ${interface_file}
      DEPENDS ${DEPS} ${idx_files} ${includes_file} ${IGENERATOR} # ${SWIG_INTERFACE_IDX_FILES} ${SWIG_INTERFACE_FILES}
    )
  #   add_custom_target(${module}Swig DEPENDS ${interface_file})
  #   add_dependencies(${module}Swig ${WRAPPER_LIBRARY_NAME}Idx)
  #   add_dependencies(${WRAPPER_LIBRARY_NAME}Swig ${module}Swig)

    set(SWIG_INTERFACE_FILES ${SWIG_INTERFACE_FILES} ${interface_file})

    WRAP_ITK_INSTALL("/Configuration/Typedefs" "${interface_file}")

  endforeach(module)


  # the mdx file
  set(mdx_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.mdx")
  set(CONFIG_INDEX_FILE_CONTENT "${SWIG_INTERFACE_MDX_CONTENT}")
  configure_file("${WRAP_ITK_SWIGINTERFACE_SOURCE_DIR}/Master.mdx.in" "${mdx_file}"
     @ONLY IMMEDIATE )
  WRAP_ITK_INSTALL("/Configuration/Typedefs" "${mdx_file}")

  set(module_interface_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.i")
  set(module_interface_ext_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}_ext.i")
  set(deps_imports )
  set(deps_includes )
  foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
    set(deps_imports "${deps_imports}%import ${dep}.i\n")
    set(deps_includes "${deps_includes}#include \"${dep}.includes\"\n")
  endforeach(dep)

  set(SWIG_INTERFACE_INCLUDES "${deps_includes}#include \"${WRAPPER_LIBRARY_NAME}.includes\"")
  set(CONFIG_MODULE_INTERFACE_CONTENT ) #"${deps_imports}${SWIG_INTERFACE_MODULE_CONTENT}")
  configure_file("${WRAP_ITK_SWIGINTERFACE_SOURCE_DIR}/module.i.in" "${module_interface_file}"
    @ONLY IMMEDIATE )
  WRAP_ITK_INSTALL("/Configuration/Typedefs/" "${module_interface_file}")

#  set(WRAP_ITK_FILE_CONTENT )
#  configure_file("${WRAP_ITK_CONFIG_DIR}/empty.in" "${module_interface_ext_file}"
#    @ONLY IMMEDIATE )
#  install(FILES "${module_interface_ext_file}"
#    DESTINATION "${WRAP_ITK_INSTALL_PREFIX}/Configuration/Typedefs/"
#  )

  # create the file which store all the includes
  set(includes_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.includes")
  configure_file("${WRAP_ITK_SWIGINTERFACE_SOURCE_DIR}/module.includes.in"
     ${includes_file}
     @ONLY IMMEDIATE )
  WRAP_ITK_INSTALL("/Configuration/Typedefs/" "${includes_file}")

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
    endforeach(dep)
  endif(NOT EXTERNAL_WRAP_ITK_PROJECT)

endmacro(END_WRAP_LIBRARY_SWIG_INTERFACE)


macro(WRAP_INCLUDE_SWIG_INTERFACE include_file)
  list(APPEND SWIG_INTERFACE_INCLUDES ${include_file})
endmacro(WRAP_INCLUDE_SWIG_INTERFACE)


macro(WRAP_MODULE_SWIG_INTERFACE module)
endmacro(WRAP_MODULE_SWIG_INTERFACE)

macro(END_WRAP_MODULE_SWIG_INTERFACE module)
  # variables used:
  # WRAPPER_LIBRARY_NAME
  # WRAPPER_LIBRARY_OUTPUT_DIR
  # WRAPPER_LIBRARY_CABLESWIG_INPUTS
  # WRAPPER_LIBRARY_DEPENDS
  # WRAPPER_MASTER_INDEX_OUTPUT_DIR
  # MODULE_INCLUDES


  # the master idx file
  set(mdx_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.mdx")

  # generate the idx file for all the groups of the module
  set(idx_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${module}.idx")
  add_custom_command(
    OUTPUT ${idx_file}
    COMMAND ${CABLE_INDEX}
          ${xml_file} ${idx_file}
    DEPENDS ${CABLE_INDEX} ${xml_file}
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
#    @ONLY IMMEDIATE )
#  install(FILES "${interface_ext_file}"
#    DESTINATION "${WRAP_ITK_INSTALL_PREFIX}/Configuration/Typedefs/"
#  )

endmacro(END_WRAP_MODULE_SWIG_INTERFACE)


macro(ADD_ONE_TYPEDEF_SWIG_INTERFACE wrap_method wrap_class swig_name)
  # Add one  typedef to WRAPPER_TYPEDEFS
  # 'wrap_method' is the one of the valid WRAPPER_WRAP_METHODS from WRAP_CLASS,
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
  else(template_parameters)
    set(full_class_name "${wrap_class}")
  endif(template_parameters)

  # ADD_ONE_TYPEDEF_ALL_LANGUAGES("${wrap_method}" "${wrap_class}" "${swig_name}" "${ARGV3}")

  # Add a typedef for the class. We have this funny looking full_name::base_name
  # thing (it expands to, for example "typedef itk::Foo<baz, 2>::Foo") used
  # for gccxml typedefs

  if("${wrap_method}" MATCHES "2_SUPERCLASSES")
    ADD_SIMPLE_TYPEDEF_SWIG_INTERFACE("${full_class_name}::Superclass::Superclass" "${swig_name}_Superclass_Superclass")
    ADD_SIMPLE_TYPEDEF_SWIG_INTERFACE("${full_class_name}::Superclass::Superclass::Pointer" "${swig_name}_Superclass_Superclass_Pointer")
  endif("${wrap_method}" MATCHES "2_SUPERCLASSES")

  if("${wrap_method}" MATCHES "SUPERCLASS")
    ADD_SIMPLE_TYPEDEF_SWIG_INTERFACE("${full_class_name}::Superclass" "${swig_name}_Superclass")
    ADD_SIMPLE_TYPEDEF_SWIG_INTERFACE("${full_class_name}::Superclass::Pointer" "${swig_name}_Superclass_Pointer")
  endif("${wrap_method}" MATCHES "SUPERCLASS")

  # the same output with or without FORCE_INSTANTIATE
  ADD_SIMPLE_TYPEDEF_SWIG_INTERFACE("${full_class_name}" "${swig_name}")

  if("${wrap_method}" MATCHES "POINTER")
    if("${wrap_method}" STREQUAL "AUTOPOINTER")
      # add a pointer typedef if we are so asked
      ADD_SIMPLE_TYPEDEF_SWIG_INTERFACE("${full_class_name}::SelfAutoPointer" "${swig_name}_AutoPointer")
    else("${wrap_method}" STREQUAL "AUTOPOINTER")
      # add a pointer typedef if we are so asked
      ADD_SIMPLE_TYPEDEF_SWIG_INTERFACE("${full_class_name}::Pointer" "${swig_name}_Pointer")
    endif("${wrap_method}" STREQUAL "AUTOPOINTER")
  endif("${wrap_method}" MATCHES "POINTER")

endmacro(ADD_ONE_TYPEDEF_SWIG_INTERFACE)


macro(ADD_SIMPLE_TYPEDEF_SWIG_INTERFACE wrap_class swig_name)
  set(SWIG_INTERFACE_TYPEDEFS "${SWIG_INTERFACE_TYPEDEFS}typedef ${wrap_class} ${swig_name};\n")
endmacro(ADD_SIMPLE_TYPEDEF_SWIG_INTERFACE)

include_directories("${WRAPPER_MASTER_INDEX_OUTPUT_DIR}")
