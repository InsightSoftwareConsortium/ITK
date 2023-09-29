function(generate_castxml_commandline_flags)
  # this function sets up all the command line arguments needed
  # to successfully run castxml to generate the xml.

  # Before this function is run, the variables @CASTXML_INCLUDES@, @WRAPPER_MODULE_NAME@, @CASTXML_TYPEDEFS@, @CASTXML_FORCE_INSTANTIATE@
  # need to have been set so that the they can be expanded into the cxx file that is used
  # for generating the xml AST from the cxx_file.

  ## = Setup standard compiler flags===========================
  if(NOT ITK_USE_SYSTEM_CASTXML)
    # ExternalProject target for CastXML.
    set(_castxml_depends castxml)
  endif()

  unset(_ccache_cmd)
  if(ITK_USE_CCACHE)
    set(_ccache_cmd ${CCACHE_EXECUTABLE})
  endif()

  # Avoid missing omp.h include
  set(_castxml_cc_flags ${CMAKE_CXX_FLAGS})
  if(CMAKE_CXX_EXTENSIONS)
    set(_castxml_cc_flags "${_castxml_cc_flags} ${CMAKE_CXX17_EXTENSION_COMPILE_OPTION}")
  else()
    set(_castxml_cc_flags "${_castxml_cc_flags} ${CMAKE_CXX17_STANDARD_COMPILE_OPTION}")
  endif()

  # Aggressive optimization flags cause cast_xml to give invalid error conditions
  set(INVALID_OPTIMIZATION_FLAGS "-fopenmp;-march=[a-zA-Z0-9\-]*;-mtune=[a-zA-Z0-9\-]*;-mfma")
  foreach(rmmatch ${INVALID_OPTIMIZATION_FLAGS})
    string(
      REGEX
      REPLACE ${rmmatch}
              ""
              _castxml_cc_flags
              "${_castxml_cc_flags}")
  endforeach()
  unset(INVALID_OPTIMIZATION_FLAGS)

  # Configure the internal Clang preprocessor and target platform to match that of the given compiler command.
  separate_arguments(_castxml_cc_flags)
  unset(_castxml_cc)
  if(MSVC)
    set(_castxml_cc --castxml-cc-msvc ("${CMAKE_CXX_COMPILER}" ${_castxml_cc_flags}) -fexceptions)
    if(MSVC90)
      # needed for VS2008 64 bit
      set(_castxml_cc ${_castxml_cc} "-D_HAS_TR1=0")
    endif()
  else()
    set(_castxml_cc --castxml-cc-gnu ("${CMAKE_CXX_COMPILER}" ${_castxml_cc_flags}))
  endif()

  # Override castxml target platform when cross compiling
  unset(_target)
  if(CMAKE_CROSSCOMPILING)
    if(NOT CMAKE_CXX_COMPILER_TARGET)
      message(FATAL_ERROR "Set the target triple in CMAKE_CXX_COMPILER_TARGET "
                          " as described in https://clang.llvm.org/docs/CrossCompilation.html")
    endif()
    set(_target "--target=${CMAKE_CXX_COMPILER_TARGET}")
  endif()

  unset(_build_env)
  if(APPLE)
    # If building on OS X, make sure that CastXML's calls to the compiler have the
    # settings that the output files will be compiled with.  This prevents headers
    # from one version of OS X from being used when building for another version.
    list(
      APPEND
      _build_env
      env
      "SDKROOT=${CMAKE_OSX_SYSROOT}"
      "MACOSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}")
  endif()
  ## ============================

  # create the files used to pass the file to include to castxml
  get_directory_property(include_dir_list INCLUDE_DIRECTORIES)
  list(REMOVE_DUPLICATES include_dir_list)

  # CONFIG_CASTXML_INC_CONTENTS - variable used for building contents to write with configure_file()
  unset(CONFIG_CASTXML_INC_CONTENTS)
  foreach(dir ${include_dir_list})
    set(CONFIG_CASTXML_INC_CONTENTS "${CONFIG_CASTXML_INC_CONTENTS}\"-I${dir}\"\n")
  endforeach()
  unset(include_dir_list)

  set(CONFIG_CASTXML_INC_CONTENTS "${CONFIG_CASTXML_INC_CONTENTS}-Qunused-arguments\n")
  set(CONFIG_CASTXML_INC_CONTENTS "${CONFIG_CASTXML_INC_CONTENTS}-DITK_WRAPPING_PARSER\n")
  set(CONFIG_CASTXML_INC_CONTENTS "${CONFIG_CASTXML_INC_CONTENTS}-DITK_MANUAL_INSTANTIATION\n")

  # Get the compile_definitions of the module added with add_compile_definitions
  # From the wrapping folder (current)
  get_directory_property(compile_definition_list COMPILE_DEFINITIONS)
  # And from the top module folder
  set(module_folder "${WRAPPER_LIBRARY_SOURCE_DIR}/..")
  get_directory_property(compile_definition_list_at_module DIRECTORY "${module_folder}" COMPILE_DEFINITIONS)
  unset(module_folder)
  # Merge and remove duplicates
  list(APPEND compile_definition_list ${compile_definition_list_at_module})
  unset(compile_definition_list_at_module)
  list(REMOVE_DUPLICATES compile_definition_list)

  foreach(def ${compile_definition_list})
    set(CONFIG_CASTXML_INC_CONTENTS "${CONFIG_CASTXML_INC_CONTENTS}\"-D${def}\"\n")
  endforeach()
  unset(compile_definition_list)
  foreach(include_file ${WRAPPER_INCLUDE_FILES})
    if("${include_file}" MATCHES "<.*>")
      string(APPEND CASTXML_INCLUDES "#include ${include_file}\n")
    else()
      string(APPEND CASTXML_INCLUDES "#include \"${include_file}\"\n")
    endif()
  endforeach()

  #Write compile definitions and include paths to file.  @CONFIG_CASTXML_INC_CONTENTS@ expanded in configure_file
  set(castxml_inc_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/castxml_inputs/${library_name}.castxml.inc")
  configure_file("${ITK_WRAP_CASTXML_SOURCE_DIR}/cast_xml.inc.in" "${castxml_inc_file}" @ONLY)
  unset(CONFIG_CASTXML_INC_CONTENTS)

  # write the wrap_*.cxx file
  # Create the cxx file which will be given to castxml.
  set(cxx_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/castxml_inputs/${_each_submodule_this_module}.cxx")

  # The wrap_.cxx.in file expands the following variables:
  # @CASTXML_INCLUDES@, @WRAPPER_MODULE_NAME@, @CASTXML_TYPEDEFS@, @CASTXML_FORCE_INSTANTIATE@
  configure_file("${ITK_WRAP_CASTXML_SOURCE_DIR}/wrap_.cxx.in" "${cxx_file}" @ONLY)
  unset(CASTXML_INCLUDES)

  # ====== Get list of include files that may trigger needing a re-write of castxml files
  set(_include ${${WRAPPER_LIBRARY_NAME}_SOURCE_DIR}/include)
  if(EXISTS ${_include})
    file(GLOB_RECURSE glob_hdrs ${_include}/*.h)
  endif()
  foreach(header IN LISTS glob_hdrs)
    get_filename_component(header_name ${header} NAME)
    if(${header_name} IN_LIST WRAPPER_INCLUDE_FILES)
      list(APPEND _hdrs ${header})
    endif()
  endforeach()

  # ===== Run the castxml command
  add_custom_command(
    OUTPUT ${xml_file}
    COMMAND
      ${_build_env} ${_ccache_cmd} ${CASTXML_EXECUTABLE} -o ${xml_file} --castxml-gccxml ${_target} --castxml-start
      _wrapping_ ${_castxml_cc} -w -c # needed for ccache to think we are not calling for link
      @${castxml_inc_file} ${cxx_file}
    VERBATIM
    DEPENDS ${_castxml_depends}
            ${cxx_file}
            ${castxml_inc_file}
            ${_hdrs})
  unset(cxx_file)
  unset(castxml_inc_file)
  unset(_build_env)
  unset(_target)
  unset(_castxml_cc)
  unset(_castxml_cc_flags)
  unset(_ccache_cmd)
  unset(_castxml_depends)
endfunction()

function(test_lib_module_names_different)
  # We run into some trouble if there's a module with the same name as the
  # wrapper library. Fix this.
  string(TOUPPER "${_each_submodule_this_module}" upper_module)
  string(TOUPPER "${WRAPPER_LIBRARY_NAME}" upper_lib)
  if("${upper_module}" STREQUAL "${upper_lib}")
    message(
      FATAL_ERROR
        "The module ${_each_submodule_this_module} can't have the same name as its library. Note that the names are not case sensitive."
    )
  endif()
  unset(upper_lib)
  unset(upper_module)
  ## RETURN VALUES TO PARENT SCOPE
  set(WRAPPER_INCLUDE_FILES
      "${WRAPPER_INCLUDE_FILES}"
      PARENT_SCOPE)
endfunction()

function(generate_swig_interface_in_file)
  # Uses global variables WRAPPER_INCLUDE_FILES, SWIG_INTERFACE_TYPEDEFS
  # store the content of the SwigInterface.h files - a set of #includes for that module
  foreach(include_file ${WRAPPER_INCLUDE_FILES})
    list(APPEND _SWIG_INTERFACE_INCLUDES ${include_file})
  endforeach()

  if(_SWIG_INTERFACE_INCLUDES)
    list(REMOVE_DUPLICATES _SWIG_INTERFACE_INCLUDES)
    foreach(include_file ${_SWIG_INTERFACE_INCLUDES})
      if("${include_file}" MATCHES "<.*>")
        string(APPEND SWIG_INTERFACE_INCLUDES_CONTENT "#include ${include_file}\n")
      else()
        string(APPEND SWIG_INTERFACE_INCLUDES_CONTENT "#include \"${include_file}\"\n")
      endif()
    endforeach()
  endif()

  # create the file which stores all of the includes
  set(_includes_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/castxml_inputs/${_each_submodule_this_module}SwigInterface.h.in")
  # module.includes.in expands cmake variables: @SWIG_INTERFACE_INCLUDES_CONTENT@  #@SWIG_INTERFACE_TYPEDEFS@
  configure_file("${ITK_WRAP_SWIGINTERFACE_SOURCE_DIR}/module.includes.in" ${_includes_file} @ONLY)
endfunction()

function(generate_wrap_doc)
  set(_doxy2swig_config_file ${CMAKE_CURRENT_BINARY_DIR}/Doc/${_each_submodule_this_module}.conf)
  configure_file("${ITK_WRAP_DOC_SOURCE_DIR}/itk_doxy2swig.conf.in" "${_doxy2swig_config_file}" @ONLY)
  # run itk_doxy2swig
  set(_itk_doxy2swig_py "${ITK_WRAP_DOC_SOURCE_DIR}/itk_doxy2swig.py")
  add_custom_command(
    OUTPUT ${swig_doc_interface_file}
    COMMAND ${Python3_EXECUTABLE} ${_itk_doxy2swig_py} ${_doxy2swig_config_file} ${swig_doc_interface_file}
    #DEPENDS ${ITK_WRAP_DOC_DOXYGEN_XML_FILES} ${_doxy2swig_config_file} ${_itk_doxy2swig_py}
    DEPENDS ${WRAPPER_LIBRARY_NAME}Doxygen ${_doxy2swig_config_file} ${_itk_doxy2swig_py}
    #    COMMENT "-- Wrapping library ${_each_submodule_this_module}: Generating swig interface for inline documentation."
  )
endfunction()

macro(itk_auto_load_submodules)
  # Global vars used: WRAPPER_LIBRARY_NAME WRAPPER_DEFAULT_INCLUDE
  #                   WRAPPER_LIBRARY_SOURCE_DIR WRAPPER_LIBRARY_OUTPUT_DIR
  #                   WRAPPER_SUBMODULE_ORDER -- order specified for this submodule, unset after use in this module
  # Global vars modified: WRAPPER_TYPEDEFS
  #                       WRAPPER_INCLUDE_FILES --
  #                       WRAPPER_AUTO_INCLUDE_HEADERS
  #                       SWIG_INTERFACE_MDX_CONTENT --

  # Include the *.wrap files in WRAPPER_LIBRARY_SOURCE_DIR. This causes
  # corresponding wrap_*.cxx files to be generated WRAPPER_LIBRARY_OUTPUT_DIR,
  # and added to the WRAPPER_LIBRARY_SWIG_INPUTS list.
  # In addition, this causes the other required wrap_*.cxx files for the entire
  # library and each wrapper language to be created.
  # This macro causes the language support files for the templates and
  # library here defined to be created.

  # Now search for other *.wrap files to include
  file(GLOB _wrap_cmake_files "${WRAPPER_LIBRARY_SOURCE_DIR}/*.wrap")
  # sort the list of files so we are sure to always get the same order on all system
  # and for all builds. That's important for several reasons:
  # - the order is important for the order of creation of python template
  # - the typemaps files are always the same, and the rebuild can be avoided
  list(SORT _wrap_cmake_files)
  set(THIS_MODULE_SUBMODULE_ORDER ${WRAPPER_SUBMODULE_ORDER})
  unset(WRAPPER_SUBMODULE_ORDER)

  foreach(_file ${_wrap_cmake_files})
    # get the module name from module.wrap
    get_filename_component(_module_from_file "${_file}" NAME_WE)
    # append found implied modules to end of proscribed ordering
    list(APPEND THIS_MODULE_SUBMODULE_ORDER "${_module_from_file}")
  endforeach()
  unset(_wrap_cmake_files)
  unset(_file)
  list(REMOVE_DUPLICATES THIS_MODULE_SUBMODULE_ORDER)
  foreach(_each_submodule_this_module ${THIS_MODULE_SUBMODULE_ORDER})
    # include a cmake module file and generate the associated wrap_*.cxx file.
    # This basically sets the global vars that will be added to or modified
    # by the commands in the included *.wrap module.
    message(STATUS "${WRAPPER_LIBRARY_NAME}: Creating ${_each_submodule_this_module} submodule.")

    test_lib_module_names_different()

    # call generator specific logic to set several associated global variables
    # clear the typedefs and the includes
    unset(CASTXML_TYPEDEFS)
    unset(CASTXML_FORCE_INSTANTIATE)

    # typedefs for swig
    unset(SWIG_INTERFACE_TYPEDEFS)

    unset(ITK_WRAP_DOC_DOXY2SWIG_INPUT) # the c++ name - swig names definitions

    # WRAPPER_INCLUDE_FILES: contains a list of all files to include in the final cxx file
    unset(WRAPPER_INCLUDE_FILES)

    # Add WRAPPER_DEFAULT_INCLUDE to the list of files in WRAPPER_INCLUDE_FILES
    # to be #included in the final cxx file
    foreach(inc ${WRAPPER_DEFAULT_INCLUDE})
      # MODIFIES WRAPPER_INCLUDE_FILES
      itk_wrap_include("${inc}")
    endforeach()

    #MODIFIES ITK_WRAP_PYTHON_SWIG_EXT ITK_WRAP_PYTHON_LIBRARY_IMPORTS
    itk_wrap_submodule_python("${_each_submodule_this_module}" "${WRAPPER_LIBRARY_NAME}")

    dump_cmake_variables("prelogger_${_each_submodule_this_module}" "${_each_submodule_this_module}")
    # Indicates that the appropriate itk header for this class will be automatically included
    # in later stages of the wrapping process
    set(WRAPPER_AUTO_INCLUDE_HEADERS ON)

    # Now include the .wrap file, and read manually requested types needed
    # to build lists of items to be wrapped for both CASTXML and SWIG
    if(EXISTS "${WRAPPER_LIBRARY_SOURCE_DIR}/${_each_submodule_this_module}.wrap")
      include("${WRAPPER_LIBRARY_SOURCE_DIR}/${_each_submodule_this_module}.wrap")
    else()
      # for backward compatibility, to be removed in ITKv6
      if(EXISTS "${WRAPPER_LIBRARY_SOURCE_DIR}/wrap_${_each_submodule_this_module}.cmake")
        message(
          FATAL_ERROR
            "INCORRECT FILE NAME PATTERN: ${WRAPPER_LIBRARY_SOURCE_DIR}/wrap_${_each_submodule_this_module}.cmake should be named ${WRAPPER_LIBRARY_SOURCE_DIR}/${_each_submodule_this_module}.cmake"
        )
      endif()
      message(SEND_ERROR "Module ${WRAPPER_LIBRARY_SOURCE_DIR}/${_each_submodule_this_module}.wrap not found.")
    endif()

    dump_cmake_variables("postlogger_${_each_submodule_this_module}" "${_each_submodule_this_module}")
    write_changed_cmake_variables_to_file(
      "${WRAPPER_LIBRARY_OUTPUT_DIR}/diff_${_each_submodule_this_module}.diff"
      "${prelogger_${_each_submodule_this_module}}"
      "${postlogger_${_each_submodule_this_module}}"
      "${_each_submodule_this_module}")

    # ======== RUN COMMANDS FOR GENERATING XML AST information
    # write the module.xml file using castxml the xml file to be generated
    set(xml_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/castxml_inputs/${_each_submodule_this_module}.xml")
    ## @CASTXML_INCLUDES@, @WRAPPER_MODULE_NAME@, @CASTXML_TYPEDEFS@, @CASTXML_FORCE_INSTANTIATE@
    generate_castxml_commandline_flags()
    list(APPEND CastXML_OUTPUT_FILES ${xml_file})
    unset(xml_file)

    # store the path of the idx file to store it in the mdx file
    string(APPEND SWIG_INTERFACE_MDX_CONTENT "${_each_submodule_this_module}.idx\n")
    string(APPEND SWIG_INTERFACE_MODULE_CONTENT "%import ${_each_submodule_this_module}.i\n")
    generate_swig_interface_in_file()

    itk_end_wrap_submodule_python("${_each_submodule_this_module}")
    if(${module_prefix}_WRAP_DOC)
      set(swig_doc_interface_file ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${_each_submodule_this_module}_doc.i)
      generate_wrap_doc()
      list(APPEND ITK_WRAP_DOC_DOCSTRING_FILES ${swig_doc_interface_file})
      unset(swig_doc_interface_file)
    endif()

  endforeach()
  unset(_each_submodule_this_module)
endmacro()
