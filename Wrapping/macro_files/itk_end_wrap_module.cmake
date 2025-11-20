macro(itk_end_wrap_module)
  ######## The code that follows was previously encapsulated in the itk_end_wrap_module
  # INPUTS:
  # ${WRAPPER_LIBRARY_NAME}
  # ${module_prefix}
  #  ${WRAPPER_LIBRARY_NAME}CastXML
  #  ${CastXML_OUTPUT_FILES}
  #  ${WRAPPER_LIBRARY_SWIG_INPUTS}
  #  ${WRAPPER_MASTER_INDEX_OUTPUT_DIR}
  #  ${WRAPPER_SWIG_LIBRARY_FILES}
  #  ${module_prefix}_WRAP_PYTHON
  #  ${THIS_MODULE_SUBMODULE_ORDER}

  if(NOT TARGET ${WRAPPER_LIBRARY_NAME}CastXML)
    add_custom_target(
      ${WRAPPER_LIBRARY_NAME}CastXML
      DEPENDS
        ${CastXML_OUTPUT_FILES}
    )
    set(
      ${WRAPPER_LIBRARY_NAME}XmlFiles
      ${CastXML_OUTPUT_FILES}
      CACHE INTERNAL
      "Internal ${WRAPPER_LIBRARY_NAME}Xml file list."
    )
  endif()

  # Loop over the extra swig input files and copy them to the Typedefs directory
  foreach(source ${WRAPPER_LIBRARY_SWIG_INPUTS})
    file(COPY "${source}" DESTINATION "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}")
  endforeach()
  unset(basename)

  # prepare dependencies
  foreach(_dep ${WRAPPER_LIBRARY_DEPENDS})
    string(PREPEND SWIG_INTERFACE_MDX_CONTENT "${_dep}.mdx\n")
  endforeach()
  unset(_dep)

  # add some libs required by this module
  foreach(swig_lib ${WRAPPER_SWIG_LIBRARY_FILES})
    get_filename_component(basename ${swig_lib} NAME)
    list(
      APPEND
      swig_libs
      --swig-include
      ${basename}
    )
    file(COPY "${swig_lib}" DESTINATION "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}")
  endforeach()
  unset(basename)
  unset(swig_lib)

  # the list of files generated for the module
  unset(i_files)

  unset(idx_files)
  unset(typedef_in_files)
  unset(typedef_files)
  set(mdx_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.mdx")
  set(
    module_interface_file
    "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.i"
  )

  # ITK_PYI_INDEX_FILES: A list of the index files generated for a specific submodule
  #   Used to generate a complete list of index files generated which should be used as a
  #   dependency in the final pyi_generator step.
  unset(ITK_PYI_INDEX_FILES)
  unset(${WRAPPER_LIBRARY_NAME}PyiIdxFiles CACHE)

  foreach(_module ${THIS_MODULE_SUBMODULE_ORDER})
    # create the swig interface
    list(
      APPEND
      typedef_in_files
      "${WRAPPER_LIBRARY_OUTPUT_DIR}/castxml_inputs/${_module}SwigInterface.h.in"
    )
    list(APPEND i_files "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${_module}.i")
    list(APPEND idx_files "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${_module}.idx")
    list(
      APPEND
      typedef_files
      "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${_module}SwigInterface.h"
    )

    # ITK_PYI_INDEX_FILES: A list of the index files generated for a specific submodule
    #   Used to generate a complete list of index files generated which should be used as a
    #   dependency in the final pyi_generator step. (duplicate comment from above)
    if(_module STREQUAL "stdcomplex" OR _module STREQUAL "stdnumeric_limits")
      # Skip classes that require specialized wrapping behavior, see igenerator.py comments
      # for ["stdcomplex", "stdnumeric_limits"]
    else()
      set(THIS_MODULE_PYI_INDEX_FILE "${ITK_PKL_DIR}/${_module}.index.txt")
      list(APPEND ITK_PYI_INDEX_FILES "${THIS_MODULE_PYI_INDEX_FILE}")
      unset(THIS_MODULE_PYI_INDEX_FILE)
    endif()
  endforeach()

  # the master idx file (mdx file)
  unset(mdx_files)
  unset(mdx_opts)
  unset(deps_imports)

  list(
    APPEND
    mdx_files
    "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.mdx"
  )
  foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
    list(APPEND mdx_files "${WRAP_ITK_TYPEDEFS_DIRECTORY}/${dep}.mdx")
    list(APPEND deps_imports "%import ${dep}.i\n")
  endforeach()

  foreach(mdx_file ${mdx_files})
    list(
      APPEND
      mdx_opts
      --mdx
      ${mdx_file}
    )
  endforeach()

  set(CONFIG_INDEX_FILE_CONTENT "${SWIG_INTERFACE_MDX_CONTENT}")
  # ONLY 1 variable @CONFIG_INDEX_FILE_CONTENT@"
  configure_file(
    "${ITK_WRAP_SWIGINTERFACE_SOURCE_DIR}/Master.mdx.in"
    "${mdx_file}"
    @ONLY
  )
  unset(CONFIG_INDEX_FILE_CONTENT)

  unset(CONFIG_MODULE_INTERFACE_CONTENT) #"${deps_imports}${SWIG_INTERFACE_MODULE_CONTENT}")
  #@WRAPPER_LIBRARY_NAME@ @CONFIG_MODULE_INTERFACE_INCLUDES@ CONFIG_MODULE_INTERFACE_INCLUDES@
  configure_file(
    "${ITK_WRAP_SWIGINTERFACE_SOURCE_DIR}/module.i.in"
    "${module_interface_file}"
    @ONLY
  )
  unset(deps_imports)
  unset(module_interface_file)

  set(WRAPPING_CONFIG_WORKING_DIR "${ITK_DIR}/Wrapping/WorkingDirectory")
  list(LENGTH i_files number_interface_files)
  if(number_interface_files GREATER 0)
    file(MAKE_DIRECTORY "${WRAPPING_CONFIG_WORKING_DIR}")

    # NOTE:  snake_case_config_file is both an input and an output to this command.
    #        the ${IGENERATOR} script appends to this file.
    # NOTE: The Configuration files should be placed in the itk package directory.
    set(
      ITK_WRAP_PYTHON_SNAKE_CONFIG_DIR
      "${WRAPPER_LIBRARY_OUTPUT_DIR}/Generators/Python/itk/Configuration"
    )
    set(
      snake_case_config_file
      "${ITK_WRAP_PYTHON_SNAKE_CONFIG_DIR}/${WRAPPER_LIBRARY_NAME}_snake_case.py"
    )
    unset(ITK_WRAP_PYTHON_SNAKE_CONFIG_DIR)

    # Set up outputs and byproducts for custom command
    set(igenerator_outputs "")
    set(igenerator_byproducts "")

    list(APPEND igenerator_outputs "${i_files}") # Typedefs/<class>.i
    list(APPEND igenerator_outputs "${typedef_files}") # Typedefs/<class>SwigInterface.h
    list(APPEND igenerator_outputs "${idx_files}") # Typedefs/<class>.idx
    list(APPEND igenerator_outputs "${snake_case_config_file}") # Generators/Python/itk/Configuration/<module>_snake_case.py
    if(CMAKE_GENERATOR STREQUAL "Ninja")
      # Ninja generator requires byproduct for correct dependency handling
      # See https://cmake.org/cmake/help/latest/policy/CMP0058.html
      list(APPEND igenerator_byproducts "${ITK_PYI_INDEX_FILES}") # Generators/Python/itk-pkl/<class>.index.txt
    else()
      list(APPEND igenerator_outputs "${ITK_PYI_INDEX_FILES}") # Generators/Python/itk-pkl/<class>.index.txt
    endif()

    # Generate custom wrapping files via igenerator.py
    add_custom_command(
      OUTPUT
        ${igenerator_outputs}
      BYPRODUCTS
        ${igenerator_byproducts}
      COMMAND
        ${Python3_EXECUTABLE} ${IGENERATOR} ${mdx_opts} ${swig_libs} -w1 -w3
        -w51 -w52 -w53 -w54 -A protected -A private -p ${PYGCCXML_DIR} -g
        ${CASTXML_EXECUTABLE} --snake-case-file "${snake_case_config_file}"
        --interface-output-dir "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}"
        --library-output-dir "${WRAPPER_LIBRARY_OUTPUT_DIR}" --submodule-order
        "${THIS_MODULE_SUBMODULE_ORDER}" --pyi_index_list
        "${ITK_PYI_INDEX_FILES}" --pyi_dir "${ITK_STUB_DIR}" --pkl_dir
        "${ITK_PKL_DIR}"
      DEPENDS
        ${IGENERATOR}
        ${ITK_WRAP_DOC_DOCSTRING_FILES}
        ${CastXML_OUTPUT_FILES}
        ${typedef_in_files}
        ${mdx_files}
        ${WRAPPER_SWIG_LIBRARY_FILES}
      WORKING_DIRECTORY
        "${WRAPPING_CONFIG_WORKING_DIR}" # Arguments to WORKING_DIRECTORY may use generator expressions
      COMMENT "Run igenerator.py for ${WRAPPER_LIBRARY_NAME}"
      VERBATIM
    )

    unset(snake_case_config_file)
  else()
    #message(FATAL_ERROR "Number of interface files is 0 :${WRAPPER_LIBRARY_NAME}:")
    message(STATUS "Number of interface files is 0 :${WRAPPER_LIBRARY_NAME}:")
  endif()
  unset(number_interface_files)
  unset(WRAPPING_CONFIG_WORKING_DIR)
  unset(typedef_in_files)
  unset(swig_libs)
  unset(mdx_opts)
  unset(igenerator_byproducts)
  unset(igenerator_outputs)

  # the ${WRAPPER_LIBRARY_NAME}Swig target will run igenerator.py if files do not already exist
  if(NOT TARGET ${WRAPPER_LIBRARY_NAME}Swig)
    add_custom_target(
      ${WRAPPER_LIBRARY_NAME}Swig
      DEPENDS
        ${mdx_file}
        ${i_files}
        ${typedef_files}
        ${idx_files}
    )
    add_dependencies(${WRAPPER_LIBRARY_NAME}Swig ${WRAPPER_LIBRARY_NAME}CastXML)
  endif()
  unset(typedef_files)
  unset(mdx_file)
  unset(i_files)

  if(NOT EXTERNAL_WRAP_ITK_PROJECT)
    # don't depend on the targets from wrapitk in external projects
    foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
      add_dependencies(${WRAPPER_LIBRARY_NAME}Swig ${dep}Swig)
    endforeach()
  endif()
  unset(ITK_STUB_PYI_FILES)

  list(APPEND GLOBAL_IdxFilesList ${ITK_PYI_INDEX_FILES})
  list(REMOVE_DUPLICATES GLOBAL_IdxFilesList)

  set(
    GLOBAL_IdxFilesList
    ${GLOBAL_IdxFilesList}
    CACHE INTERNAL
    "Master list of all idx files"
  )
  set(
    ${WRAPPER_LIBRARY_NAME}PyiIdxFiles
    ${ITK_PYI_INDEX_FILES}
    CACHE INTERNAL
    "Internal ${WRAPPER_LIBRARY_NAME} .index.txt file list"
  )
  set(
    ${WRAPPER_LIBRARY_NAME}IdxFiles
    ${idx_files}
    CACHE INTERNAL
    "Internal ${WRAPPER_LIBRARY_NAME}Idx file list."
  )
  set(
    ${WRAPPER_LIBRARY_NAME}SwigFiles
    ${i_files}
    CACHE INTERNAL
    "Internal ${WRAPPER_LIBRARY_NAME}Swig file list."
  )

  unset(idx_files)
  unset(ITK_PYI_INDEX_FILES)

  # Loop over the extra swig input files and add them to the generated files
  # lists. Guess that the generated cxx output will have the same name as
  # the .i input file.
  set(ITK_WRAP_PYTHON_PROCESS_SWIG_INPUTS ON)
  foreach(source ${WRAPPER_LIBRARY_SWIG_INPUTS})
    get_filename_component(base_name ${source} NAME_WE)
    itk_wrap_submodule_python("${base_name}" "${WRAPPER_LIBRARY_NAME}")
    itk_end_wrap_submodule_python("${base_name}")
  endforeach()
  set(ITK_WRAP_PYTHON_PROCESS_SWIG_INPUTS OFF)

  # create the python config file
  # this file store all the name - type association and a dependencies list for the modules
  #
  # first build the dependency list
  set(ITK_WRAP_PYTHON_CONFIGURATION_DEPENDS "")

  foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
    string(PREPEND ITK_WRAP_PYTHON_CONFIGURATION_DEPENDS "'${dep}', ")
    string(PREPEND ITK_WRAP_PYTHON_LIBRARY_IMPORTS "import itk.${dep}Python\n")
  endforeach()

  # ITKPyBase is always included, excepted ITKPyBase itself
  if(NOT "${WRAPPER_LIBRARY_NAME}" STREQUAL "ITKPyBase")
    string(PREPEND ITK_WRAP_PYTHON_CONFIGURATION_DEPENDS "'ITKPyBase', ")
    string(
      PREPEND
      ITK_WRAP_PYTHON_LIBRARY_IMPORTS
      "import itk.ITKPyBasePython\n"
    )
    set(
      ITK_WRAP_PYTHON_SNAKE_CASE
      "${ITK_WRAP_PYTHON_ROOT_BINARY_DIR}/itk/Configuration/${WRAPPER_LIBRARY_NAME}_snake_case.py"
    )
  else()
    unset(ITK_WRAP_PYTHON_SNAKE_CASE)
  endif()
  set(
    ITK_WRAP_PYTHON_LIBRARY_CONFIG_FILE
    "${ITK_WRAP_PYTHON_ROOT_BINARY_DIR}/itk/Configuration/${WRAPPER_LIBRARY_NAME}Config.py"
  )

  # Pass module factory names into module configuration
  set(ITK_WRAP_PYTHON_CONFIGURATION_FACTORIES)
  foreach(factory IN LISTS ITK_MODULE_${WRAPPER_LIBRARY_NAME}_FACTORY_NAMES)
    string(REPLACE "::" "\",\"" factory_list ${factory})
    string(
      APPEND
      ITK_WRAP_PYTHON_CONFIGURATION_FACTORIES
      "(\"${factory_list}\"),"
    )
  endforeach()

  # and create the file, with the var ITK_WRAP_PYTHON_CONFIGURATION_TEMPLATES and
  # ITK_WRAP_PYTHON_CONFIGURATION_DEPENDS created earlier
  configure_file(
    "${ITK_WRAP_PYTHON_SOURCE_DIR}/itk/support/ModuleConfig.py.in"
    "${ITK_WRAP_PYTHON_LIBRARY_CONFIG_FILE}"
    @ONLY
  )
  unset(ITK_WRAP_PYTHON_CONFIGURATION_DEPENDS)
  unset(ITK_WRAP_PYTHON_CONFIGURATION_TEMPLATES)

  wrap_itk_python_bindings_install(
      /itk/Configuration
      "${WRAPPER_LIBRARY_NAME}"
      "${ITK_WRAP_PYTHON_LIBRARY_CONFIG_FILE}"
      "${ITK_WRAP_PYTHON_SNAKE_CASE}"
  )
  unset(ITK_WRAP_PYTHON_LIBRARY_CONFIG_FILE)
  unset(ITK_WRAP_PYTHON_SNAKE_CASE)

  unset(ITK_WRAP_PYTHON_GLOBAL_TIMESTAMP_DECLS)
  unset(ITK_WRAP_PYTHON_GLOBAL_TIMESTAMP_CALLS)
  if(NOT BUILD_SHARED_LIBS)
    if(WRAPPER_LIBRARY_NAME STREQUAL "ITKCommon")
      if(WIN32)
        set(DO_NOT_WAIT_FOR_THREADS_DECLS "#include \"itkThreadPool.h\"")
        set(
          DO_NOT_WAIT_FOR_THREADS_CALLS
          "itk::ThreadPool::SetDoNotWaitForThreads(true);"
        )
      endif()

      set(
        ITK_WRAP_PYTHON_GLOBAL_TIMESTAMP_DECLS
        "
#define _ITKCommonPython_MODULE
#include \"itkPyITKCommonCAPI.h\"
${DO_NOT_WAIT_FOR_THREADS_DECLS}

static
_ITKCommonPython_GetGlobalSingletonIndex_RETURN
_ITKCommonPython_GetGlobalSingletonIndex
_ITKCommonPython_GetGlobalSingletonIndex_PROTO
{
itk::ObjectFactoryBase::Initialize();
return itk::SingletonIndex::GetInstance();
}

"
      )

      set(
        ITK_WRAP_PYTHON_GLOBAL_TIMESTAMP_CALLS
        "
static void * _ITKCommonPython_API[_ITKCommonPython_API_pointers];

/* Initialize the C API pointer array */
_ITKCommonPython_API[_ITKCommonPython_GetGlobalSingletonIndex_NUM] = (void *)_ITKCommonPython_GetGlobalSingletonIndex;

/* Create a Capsule containing the API pointer array's address */
PyObject * cAPIObject = PyCapsule_New((void *)_ITKCommonPython_API,
\"itk._ITKCommonPython._C_API\", NULL);

if(cAPIObject != NULL)
{
PyModule_AddObject(m, \"_C_API\", cAPIObject);
}
${DO_NOT_WAIT_FOR_THREADS_CALLS}
"
      )
    elseif("ITKCommon" IN_LIST WRAPPER_LIBRARY_LINK_LIBRARIES)
      set(
        ITK_WRAP_PYTHON_GLOBAL_TIMESTAMP_DECLS
        "
#include \"itkPyITKCommonCAPI.h\"
${DO_NOT_WAIT_FOR_THREADS_DECLS}
"
      )
      set(
        ITK_WRAP_PYTHON_GLOBAL_TIMESTAMP_CALLS
        "
if(import__ITKCommonPython() < 0)
{
#if PY_VERSION_HEX >= 0x03000000
return NULL;
#else
return;
#endif
}
itk::SingletonIndex::SetInstance(_ITKCommonPython_GetGlobalSingletonIndex());
itk::ObjectFactoryBase::Initialize();
${DO_NOT_WAIT_FOR_THREADS_CALLS}
"
      )
    endif()
  endif()

  # Create the Python customization stuff in the main module
  # It allows to group the python submodules in a single shared lib (.so),
  # by loading the init functions of the module.
  # The objects from the submodules are also loaded in the main module.
  #
  # It uses:
  # ITK_WRAP_PYTHON_LIBRARY_DECLS, ITK_WRAP_PYTHON_LIBRARY_CALLS,
  # ITK_WRAP_PYTHON_LIBRARY_IMPORTS,
  # ITK_WRAP_PYTHON_GLOBAL_TIMESTAMP_CALLS, ITK_WRAP_PYTHON_GLOBAL_TIMESTAMP_DECLS
  configure_file(
    "${ITK_WRAP_PYTHON_SOURCE_DIR}/main_module_ext.i.in"
    "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python/${WRAPPER_LIBRARY_NAME}_ext.i"
    @ONLY
  )

  unset(ITK_WRAP_PYTHON_LIBRARY_DECLS)
  unset(ITK_WRAP_PYTHON_LIBRARY_CALLS)
  unset(ITK_WRAP_PYTHON_LIBRARY_IMPORTS)
  unset(ITK_WRAP_PYTHON_GLOBAL_TIMESTAMP_CALLS)
  unset(ITK_WRAP_PYTHON_GLOBAL_TIMESTAMP_DECLS)
  unset(DO_NOT_WAIT_FOR_THREADS_CALLS)
  unset(DO_NOT_WAIT_FOR_THREADS_DECLS)

  # set some var reused later
  set(
    interface_file
    "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.i"
  )
  set(_swig_python_suffix "Python")
  set(lib ${WRAPPER_LIBRARY_NAME}${_swig_python_suffix})
  set(
    python_file
    "${ITK_PYTHON_PACKAGE_DIR}/${WRAPPER_LIBRARY_NAME}${_swig_python_suffix}.py"
  )
  set(
    cpp_file
    "${CMAKE_CURRENT_BINARY_DIR}/${WRAPPER_LIBRARY_NAME}${_swig_python_suffix}.cpp"
  )
  unset(_swig_python_suffix)

  # if this is for an external library, let the user add extra swig args
  if(EXTERNAL_WRAP_ITK_PROJECT)
    set(
      WRAP_ITK_SWIG_ARGS_PYTHON
      ""
      CACHE STRING
      "Extra user-defined swig arguments to be to the swig executable."
    )
    mark_as_advanced(WRAP_ITK_SWIG_ARGS_PYTHON)
  endif()

  # Run swig to produce the *Python.cpp and the *Python.py file
  itk_setup_swig_python(
      "Module"
      ${base_name}
      ${interface_file}
      ${python_file}
      ${cpp_file}
      ""
  )

  # build all the c++ files from this module in a common lib
  if(NOT TARGET ${lib})
    # -- START PYTHON MODULE CREATION
    if(ITK_USE_PYTHON_LIMITED_API)
      set(
        _Python3_ABI_SETTINGS
        USE_SABI
        ${Python3_VERSION_MAJOR}.${Python3_VERSION_MINOR}
        WITH_SOABI
      )
    else()
      unset(_Python3_ABI_SETTINGS)
    endif()

    #Python3_add_library sets PREFIX "" and the correct extension suffix automatically.
    # No manual SUFFIX editing is needed.
    #WITH_SOABI can be added to Python3_add_library(...) if you want the SOABI tag
    #in the filename for non-SABI builds.
    #Use Development.Module for normal CPython extensions. Use Development.SABIModule plus Py_LIMITED_API for abi3-compatible builds.
    python3_add_library(
      ${lib}
      MODULE
      ${_Python3_ABI_SETTINGS}
      ${cpp_file}
      ${ITK_WRAP_PYTHON_CXX_FILES}
      ${WRAPPER_LIBRARY_CXX_SOURCES}
    )
    unset(_Python3_ABI_SETTINGS)
    # Override the default naming convensions for libraries
    # and add an ITK python prefix of "_" for the generated names
    set_target_properties(
      ${lib}
      PROPERTIES
        PREFIX
          "_"
    )

    # gcc 4.4 complains a lot without this flag when building in release mode
    if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
      set_target_properties(
        ${lib}
        PROPERTIES
          COMPILE_FLAGS
            "-fno-strict-aliasing -w"
      )
    else()
      if(MSVC)
        # Disables 'conversion from 'type1' to 'type2', possible loss of data warnings
        set_target_properties(
          ${lib}
          PROPERTIES
            COMPILE_FLAGS
              "/wd4244"
        )
      endif()
    endif()

    # Link the modules together
    target_link_libraries(${lib} LINK_PUBLIC ${WRAPPER_LIBRARY_LINK_LIBRARIES})

    # Set IPO if it is supported
    if(NOT MSVC)
      include(CheckIPOSupported)
      check_ipo_supported(RESULT ipo_is_supported)
      if(ipo_is_supported)
        set_property(
          TARGET
            ${lib}
          PROPERTY
            INTERPROCEDURAL_OPTIMIZATION_RELEASE
              TRUE
        )
      endif()
      unset(ipo_is_supported)
    endif()

    if(USE_COMPILER_HIDDEN_VISIBILITY)
      # Prefer to use target properties supported by newer cmake
      set_target_properties(
        ${lib}
        PROPERTIES
          CXX_VISIBILITY_PRESET
            hidden
      )
      set_target_properties(
        ${lib}
        PROPERTIES
          C_VISIBILITY_PRESET
            hidden
      )
      set_target_properties(
        ${lib}
        PROPERTIES
          VISIBILITY_INLINES_HIDDEN
            1
      )
    endif()
    ## END MODULE CONFIGURATION

    add_dependencies(${lib} ${WRAPPER_LIBRARY_NAME}Swig)
    if(${module_prefix}_WRAP_DOC)
      add_dependencies(${lib} ${WRAPPER_LIBRARY_NAME}Doxygen)
    endif()
    set(_component_module "")
    if(WRAP_ITK_INSTALL_COMPONENT_PER_MODULE)
      if("${WRAPPER_LIBRARY_NAME}" MATCHES "^ITKPyBase$")
        set(_component_module "ITKCommon")
      else()
        set(_component_module "${WRAPPER_LIBRARY_NAME}")
      endif()
    endif()
    install(
      TARGETS
        "${lib}"
      DESTINATION "${PY_SITE_PACKAGES_PATH}/itk"
      COMPONENT
        ${_component_module}${WRAP_ITK_INSTALL_COMPONENT_IDENTIFIER}RuntimeLibraries
    )
    unset(_component_module)

    if(NOT EXTERNAL_WRAP_ITK_PROJECT)
      # don't depends on the targets from wrapitk in external projects
      foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
        add_dependencies(${lib} ${dep}Swig)
        if(${module_prefix}_WRAP_DOC)
          add_dependencies(${lib} ${dep}Doxygen)
        endif()
      endforeach()
    endif()
  endif()

  if(${module_prefix}_WRAP_DOC)
    # be sure to not include a header several times
    if(NOT "${ITK_WRAP_DOC_DOXYGEN_HEADERS}" STREQUAL "")
      list(REMOVE_DUPLICATES ITK_WRAP_DOC_DOXYGEN_HEADERS)
    endif()

    # which files are produced?
    set(outputs ${ITK_WRAP_DOC_DOXYGEN_XML_FILES})

    # run doxygen

    # create the target doc dir
    set(ITK_WRAP_DOC_LIBRARY_DIR "${CMAKE_CURRENT_BINARY_DIR}/Doc") # Library documentation interface files building directory
    # TODO: direct name of the library dir?
    file(MAKE_DIRECTORY ${ITK_WRAP_DOC_LIBRARY_DIR})

    # The DoxygenConfig.cmake is a replacement for configuring a doxygen.config.in file
    # https://cmake.org/cmake/help/v3.16/module/FindDoxygen.html
    set(ITK_DOXYGEN_OUTPUT_DIR "${ITK_WRAP_DOC_LIBRARY_DIR}")
    include(${ITK_SOURCE_DIR}/Utilities/Doxygen/DoxygenConfig.cmake)
    doxygen_add_docs(
      ${WRAPPER_LIBRARY_NAME}Doxygen
      ${ITK_WRAP_DOC_DOXYGEN_HEADERS}
      ALL
      WORKING_DIRECTORY ${ITK_WRAP_DOC_LIBRARY_DIR}
      USE_STAMP_FILE
      COMMENT
        "-- Wrapping library ${WRAPPER_LIBRARY_NAME}: Constructing documentation xml structure."
    )
    add_dependencies(${lib} ${WRAPPER_LIBRARY_NAME}Doxygen)
    unset(ITK_DOXYGEN_OUTPUT_DIR)
    unset(ITK_WRAP_DOC_LIBRARY_DIR)
  endif()

  # Add testing
  set(wrapping_test_directory ${CMAKE_CURRENT_SOURCE_DIR}/test)
  if(BUILD_TESTING AND EXISTS ${wrapping_test_directory}/CMakeLists.txt)
    add_subdirectory(${wrapping_test_directory})
  endif()
  unset(wrapping_test_directory)
  unset(cpp_file)
  unset(python_file)
  unset(interface_file)
  unset(lib)

  ###
  # These variable are only used inside this function

  unset(CASTXML_FORCE_INSTANTIATE)
  unset(CastXML_OUTPUT_FILES)
  unset(CASTXML_TYPEDEFS)
  unset(ITK_WRAP_DOC_DOXY2SWIG_INPUT) # the c++ name - swig names definitions
  unset(ITK_WRAP_PYTHON_CONFIGURATION_TEMPLATES)
  unset(ITK_WRAP_PYTHON_CURRENT_CLASS)
  unset(ITK_WRAP_PYTHON_CURRENT_SWIG_NAME)
  unset(ITK_WRAP_PYTHON_CXX_FILES)
  unset(ITK_WRAP_PYTHON_FILES)
  unset(ITK_WRAP_PYTHON_LIBRARY_DECLS)
  unset(ITK_WRAP_PYTHON_LIBRARY_DEPS)
  unset(ITK_WRAP_PYTHON_LIBRARY_IMPORTS)
  unset(ITK_WRAP_PYTHON_SWIG_EXT)
  unset(PixelType)
  unset(SWIG_INTERFACE_MDX_CONTENT)
  unset(SWIG_INTERFACE_MODULE_CONTENT)
  unset(SWIG_INTERFACE_TYPEDEFS)
  unset(WRAPPER_CLASS)
  unset(WRAPPER_INCLUDE_FILES)
endmacro() # itk_end_wrap_module
