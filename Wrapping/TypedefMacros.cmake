################################################################################
# Macro definitions for creating proper Swig input files from *.wrap
# files.
# This file includes definitions for the macros to call from a CMakeList file
# to cause *.wrap files to be turned into CXX files, and definitions for
# the macros to use in the *.wrap files themselves to declare that certain
# classes and template instantiations be wrapped.
# Note on convention: variable names in ALL_CAPS are global, and shared between
# macros or between CMake and files that are configured. Variable names in
# lower_case are local to a given macro.
################################################################################


set(WRAPPER_LIBRARY_itk_wrap_modules_STATUS "NOT_EXECUTED" CACHE INTERNAL "status var used to avoid the use of itk_wrap_modules in simple contributions.")

macro(itk_wrap_modules)
  set(WRAPPER_LIBRARY_itk_wrap_modules_STATUS "EXECUTED" CACHE INTERNAL "status var used to avoid the use of itk_wrap_modules in simple contributions.")

  if(${module_prefix}_WRAP_PYTHON)
    # Wrap PyBase
    if(NOT EXTERNAL_WRAP_ITK_PROJECT)
      add_subdirectory(${ITK_WRAP_PYTHON_SOURCE_DIR}/PyBase)
    endif()
  endif()

endmacro()

macro(itk_end_wrap_modules_all_generators)
  if(${module_prefix}_WRAP_PYTHON)
    # Wrap PyUtils
    if(NOT EXTERNAL_WRAP_ITK_PROJECT)
      add_subdirectory(${ITK_WRAP_PYTHON_SOURCE_DIR}/PyUtils)
    endif()
  endif()
endmacro()


# Support for additional include directories of each module
# WARNING: Each module must set this variable BEFORE calling itk_wrap_module
# TODO: is this the place place for this?
set(WRAPPER_LIBRARY_INCLUDE_DIRECTORIES "" CACHE INTERNAL "additional include directories for each module")

###############################################################################
# Define fundamental wrapping macro which sets up the global variables used
# across all of the wrapping macros included at the end of this file.
# All variables set here are optional and have sensible default values.
# Also define some other global defaults like WRAPPER_MASTER_INDEX_OUTPUT_DIR.
###############################################################################
macro(itk_wrap_module library_name)
  set(WRAPPER_LIBRARY_NAME "${library_name}")

  message(STATUS "${WRAPPER_LIBRARY_NAME}: Creating module.")

  # Mark the current source dir for inclusion because it may contain header files.
  include_directories(BEFORE "${CMAKE_CURRENT_SOURCE_DIR}")
  include_directories(BEFORE ${WRAPPER_LIBRARY_INCLUDE_DIRECTORIES})

  # WRAPPER_LIBRARY_INCLUDE_DIRECTORIES. List of other include directories that
  # contain the desired header files.
  #set(WRAPPER_LIBRARY_INCLUDE_DIRECTORIES )

  # WRAPPER_LIBRARY_SOURCE_DIR. Directory to be scanned for *.wrap files.
  set(WRAPPER_LIBRARY_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

  # WRAPPER_LIBRARY_DEPENDS. List of names of other wrapper libraries that
  # define symbols used by this wrapper library.
  INTERSECTION(WRAPPER_LIBRARY_DEPENDS "${ITK_MODULE_${library_name}_DEPENDS}" "${WRAP_ITK_MODULES}")
  # set(WRAPPER_LIBRARY_DEPENDS ${ITK_MODULE_${library_name}_DEPENDS})

  # WRAPPER_LIBRARY_LINK_LIBRARIES. List of other libraries that should
  # be linked to the wrapper library.
  set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES} ${${itk-module}_LIBRARIES})

  # WRAPPER_SUBMODULE_ORDER. List of *.wrap submodules in the source dir
  # that should be included/wrapped before the rest in the given order.
  # Just the submodule group name is needed, not the full path or file name.
  set(WRAPPER_SUBMODULE_ORDER )

  # WRAPPER_LIBRARY_SWIG_INPUTS. List of C++ source files to be used
  # as input for Swig. This list is then appended to by
  # WRAPPER_LIBRARY_AUTO_INCLUDE_WRAP_FILES. A full path to each input is required.
  set(WRAPPER_LIBRARY_SWIG_INPUTS )

  # WRAPPER_SWIG_LIBRARY_FILES. List of swig .swg files to pass to cswig to control
  # type handling and so forth. A full path to each include is required.
  # The itk.swg file and the library file for the current library are implicitly added.
  set(WRAPPER_SWIG_LIBRARY_FILES )

  # WRAPPER_LIBRARY_CXX_SOURCES. C++ sources to be compiled and linked in
  # to the wrapper library (with no prior processing by swig, etc.)
  # A full path to each input is required.
  set(WRAPPER_LIBRARY_CXX_SOURCES )

  if("${ARGC}" EQUAL 2)
    foreach(lang ${WRAP_ITK_GENERATORS})
      string(TOUPPER ${lang} LANG)
      set(WRAPPER_LIBRARY_${LANG} OFF)
    endforeach()
    foreach(lang ${ARGV1})
      string(TOUPPER ${lang} LANG)
      set(WRAPPER_LIBRARY_${LANG} ON)
    endforeach()
  else()
    foreach(lang ${WRAP_ITK_GENERATORS})
      string(TOUPPER ${lang} LANG)
      set(WRAPPER_LIBRARY_${LANG} ON)
    endforeach()
  endif()
  unset(LANG)

  if("${WRAPPER_LIBRARY_itk_wrap_modules_STATUS}" STREQUAL "NOT_EXECUTED")
    itk_wrap_modules()
    # change the status of WRAPPER_LIBRARY_itk_wrap_modules_STATUS, so we can call itk_end_wrap_modules_all_generators when
    # itk_end_wrap_module will be called
    set(WRAPPER_LIBRARY_itk_wrap_modules_STATUS "EXECUTED_IN_itk_wrap_module" CACHE INTERNAL "status var used to avoid the use of itk_wrap_modules in simple contributions.")
  endif()

  # Call the language support initialization function
  if(${module_prefix}_WRAP_CASTXML)
    itk_wrap_module_castxml("${library_name}")
  endif()

  if(${module_prefix}_WRAP_SWIGINTERFACE)
    # store the content of the mdx file
    set(SWIG_INTERFACE_MDX_CONTENT )
    # store the content of the .i file for the module - a set of import of all the .i files generated for the module
    set(SWIG_INTERFACE_MODULE_CONTENT )
    # build a list of modules to create the igenerator custom command
    set(SWIG_INTERFACE_MODULES )
  endif()

  if(${module_prefix}_WRAP_DOC)
    set(ITK_WRAP_DOC_DOXYGEN_HEADERS )  # doxygen headers to process in this lib
    set(ITK_WRAP_DOC_DOXYGEN_XML_FILES )  # xml files produced by doxygen in this lib
    set(ITK_WRAP_DOC_DOCSTRING_FILES )  # swig docstring files produced by doxygen in this lib
  endif()

  if(${module_prefix}_WRAP_PYTHON AND WRAPPER_LIBRARY_PYTHON)
    itk_wrap_module_python("${library_name}")
  endif()
endmacro()


macro(itk_end_wrap_module)
  if("${WRAPPER_LIBRARY_itk_wrap_modules_STATUS}" STREQUAL "EXECUTED_IN_itk_wrap_module")
    itk_end_wrap_modules_all_generators()
  endif()

  if(${module_prefix}_WRAP_CASTXML)
    if(NOT TARGET ${WRAPPER_LIBRARY_NAME}CastXML)
      add_custom_target(${WRAPPER_LIBRARY_NAME}CastXML DEPENDS ${CastXML_OUTPUT_FILES})
      set(${WRAPPER_LIBRARY_NAME}XmlFiles ${CastXML_OUTPUT_FILES} CACHE INTERNAL "Internal ${WRAPPER_LIBRARY_NAME}Xml file list.")
    endif()
  endif()

  if(${module_prefix}_WRAP_SWIGINTERFACE)
    # Loop over the extra swig input files and copy them to the Typedefs directory
    foreach(source ${WRAPPER_LIBRARY_SWIG_INPUTS})
      file(COPY "${source}"
              DESTINATION "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}")
      get_filename_component(basename ${source} NAME)
      set(dest "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${basename}")
    endforeach()
    unset(basename)

    # prepare dependencies
    set(DEPS )
    foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
      set(SWIG_INTERFACE_MDX_CONTENT "${dep}.mdx\n${SWIG_INTERFACE_MDX_CONTENT}")
    endforeach()

    # add some libs required by this module
    set(swig_libs )
    foreach(swig_lib ${WRAPPER_SWIG_LIBRARY_FILES})
      get_filename_component(basename ${swig_lib} NAME)
      list(APPEND swig_libs --swig-include ${basename})
      file(COPY "${swig_lib}"
              DESTINATION "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}")
      set(dest "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${basename}")
    endforeach()
    unset(basename)

    # the list of files generated for the module
    set(i_files )
    set(xml_files )
    set(idx_files )
    set(typedef_in_files )
    set(typedef_files )
    set(mdx_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.mdx")
    set(module_interface_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.i")
    set(module_interface_ext_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}_ext.i")

    if(${module_prefix}_WRAP_PYTHON)
      set(ITK_STUB_DIR "${ITK_DIR}/Wrapping/Generators/Python/itk-stubs")
      set(ITK_STUB_PYI_FILES)
    else()
      unset(ITK_STUB_DIR)
      unset(ITK_STUB_PYI_FILES)
    endif()

    foreach(_module ${SWIG_INTERFACE_MODULES})
      # create the swig interface
      list(APPEND i_files "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${_module}.i")
      list(APPEND xml_files "${WRAPPER_LIBRARY_OUTPUT_DIR}/${_module}.xml")
      list(APPEND idx_files "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${_module}.idx")
      list(APPEND typedef_in_files "${WRAPPER_LIBRARY_OUTPUT_DIR}/${_module}SwigInterface.h.in")
      list(APPEND typedef_files "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${_module}SwigInterface.h")
      if(${module_prefix}_WRAP_PYTHON)
        list(APPEND ITK_STUB_PYI_FILES "${ITK_STUB_DIR}/${_module}Template.pyi")
        list(APPEND ITK_STUB_PYI_FILES "${ITK_STUB_DIR}/${_module}Proxy.pyi")
        set(ENV{ITK_STUB_TEMPLATE_IMPORTS} "$ENV{ITK_STUB_TEMPLATE_IMPORTS}from .${_module}Template import *;")
        set(ENV{ITK_STUB_PROXY_IMPORTS} "$ENV{ITK_STUB_PROXY_IMPORTS}from .${_module}Proxy import *;")
      endif()
    endforeach()

    # the master idx file (mdx file)
    set(mdx_opts )
    set(deps_imports )

    list(APPEND mdx_opts --mdx "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.mdx")

    foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
      list(APPEND mdx_opts --mdx "${WRAP_ITK_TYPEDEFS_DIRECTORY}/${dep}.mdx")
      list(APPEND deps_imports "%import ${dep}.i\n")
    endforeach()

    set(CONFIG_INDEX_FILE_CONTENT "${SWIG_INTERFACE_MDX_CONTENT}")
    configure_file("${ITK_WRAP_SWIGINTERFACE_SOURCE_DIR}/Master.mdx.in" "${mdx_file}"
            @ONLY)
    unset(CONFIG_INDEX_FILE_CONTENT)

    set(CONFIG_MODULE_INTERFACE_CONTENT ) #"${deps_imports}${SWIG_INTERFACE_MODULE_CONTENT}")
    configure_file("${ITK_WRAP_SWIGINTERFACE_SOURCE_DIR}/module.i.in" "${module_interface_file}"
            @ONLY)

    set(WRAPPING_CONFIG_WORKING_DIR "${ITK_DIR}/Wrapping/WorkingDirectory")
    list(LENGTH i_files number_interface_files)
    if(number_interface_files GREATER 0)

      FILE(MAKE_DIRECTORY "${WRAPPING_CONFIG_WORKING_DIR}")
      if(${module_prefix}_WRAP_PYTHON)
        set(ITK_STUB_DIR "${ITK_DIR}/Wrapping/Generators/Python/itk-stubs")
        # NOTE:  snake_case_config_file is both an input and an output to this command.
        #        the ${IGENERATOR} script appends to this file.
        # NOTE: The Configuration files should be placed in the itk package directory.
        set(ITK_WRAP_PYTHON_SNAKE_CONFIG_DIR
                "${WRAPPER_LIBRARY_OUTPUT_DIR}/Generators/Python/itk/Configuration"
                )
        set(snake_case_config_file
                "${ITK_WRAP_PYTHON_SNAKE_CONFIG_DIR}/${WRAPPER_LIBRARY_NAME}_snake_case.py")
        unset(ITK_WRAP_PYTHON_SNAKE_CONFIG_DIR)
        add_custom_command(
                OUTPUT ${i_files} ${typedef_files} ${idx_files} ${snake_case_config_file} ${ITK_STUB_PYI_FILES}
                COMMAND ${Python3_EXECUTABLE} ${IGENERATOR}
                ${mdx_opts}
                ${swig_libs}
                -w1 -w3 -w51 -w52 -w53 -w54
                -A protected -A private
                -p ${PYGCCXML_DIR}
                -g ${CASTXML_EXECUTABLE}
                --snake-case-file "${snake_case_config_file}"
                --interface-output-dir "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}"
                --library-output-dir "${WRAPPER_LIBRARY_OUTPUT_DIR}"
                --submodule-order "${WRAPPER_SUBMODULE_ORDER}"
                --pyi_dir "${ITK_STUB_DIR}"
                DEPENDS ${ITK_WRAP_DOC_DOCSTRING_FILES} ${xml_files} ${IGENERATOR} ${typedef_in_files}
                WORKING_DIRECTORY "${WRAPPING_CONFIG_WORKING_DIR}" # Arguments to WORKING_DIRECTORY may use generator expressions
                VERBATIM
        )
      else()
        unset(ITK_STUB_DIR)
        unset(snake_case_config_file)
        add_custom_command(
                OUTPUT ${i_files} ${typedef_files} ${idx_files}
                COMMAND ${Python3_EXECUTABLE} ${IGENERATOR}
                ${mdx_opts}
                ${swig_libs}
                -w1 -w3 -w51 -w52 -w53 -w54
                -A protected -A private
                -p ${PYGCCXML_DIR}
                -g ${CASTXML_EXECUTABLE}
                --interface-output-dir "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}"
                --library-output-dir "${WRAPPER_LIBRARY_OUTPUT_DIR}"
                --submodule-order "${WRAPPER_SUBMODULE_ORDER}"
                DEPENDS ${ITK_WRAP_DOC_DOCSTRING_FILES} ${xml_files} ${IGENERATOR} ${typedef_in_files}
                WORKING_DIRECTORY "${WRAPPING_CONFIG_WORKING_DIR}" # Arguments to WORKING_DIRECTORY may use generator expressions
                VERBATIM
        )
      endif()
    endif()
    unset(mdx_opts)
    # the ${WRAPPER_LIBRARY_NAME}Swig target
    if(NOT TARGET ${WRAPPER_LIBRARY_NAME}Swig)
      add_custom_target(${WRAPPER_LIBRARY_NAME}Swig DEPENDS ${mdx_file} ${i_files} ${typedef_files} ${idx_files})
      add_dependencies(${WRAPPER_LIBRARY_NAME}Swig ${WRAPPER_LIBRARY_NAME}CastXML)
    endif()

    if(NOT EXTERNAL_WRAP_ITK_PROJECT)
      # don't depend on the targets from wrapitk in external projects
      foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
        add_dependencies(${WRAPPER_LIBRARY_NAME}Swig ${dep}Swig)
      endforeach()
    endif()
    unset(ITK_STUB_PYI_FILES)

    set(${WRAPPER_LIBRARY_NAME}IdxFiles ${idx_files} CACHE INTERNAL "Internal ${WRAPPER_LIBRARY_NAME}Idx file list.")
    set(${WRAPPER_LIBRARY_NAME}SwigFiles ${i_files} CACHE INTERNAL "Internal ${WRAPPER_LIBRARY_NAME}Swig file list.")
  endif()
  if(${module_prefix}_WRAP_PYTHON AND WRAPPER_LIBRARY_PYTHON)
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
      set(ITK_WRAP_PYTHON_CONFIGURATION_DEPENDS "'${dep}', ${ITK_WRAP_PYTHON_CONFIGURATION_DEPENDS}")
      set(ITK_WRAP_PYTHON_LIBRARY_IMPORTS "import itk.${dep}Python\n${ITK_WRAP_PYTHON_LIBRARY_IMPORTS}")
    endforeach()

    # ITKPyBase is always included, excepted ITKPyBase itself
    if(NOT "${WRAPPER_LIBRARY_NAME}" STREQUAL "ITKPyBase")
      set(ITK_WRAP_PYTHON_CONFIGURATION_DEPENDS "'ITKPyBase', ${ITK_WRAP_PYTHON_CONFIGURATION_DEPENDS}")
      set(ITK_WRAP_PYTHON_LIBRARY_IMPORTS "import itk.ITKPyBasePython\n${ITK_WRAP_PYTHON_LIBRARY_IMPORTS}")
      set(ITK_WRAP_PYTHON_SNAKE_CASE "${ITK_WRAP_PYTHON_ROOT_BINARY_DIR}/itk/Configuration/${WRAPPER_LIBRARY_NAME}_snake_case.py")
    else()
      unset(ITK_WRAP_PYTHON_SNAKE_CASE)
    endif()
    set(ITK_WRAP_PYTHON_LIBRARY_CONFIG_FILE "${ITK_WRAP_PYTHON_ROOT_BINARY_DIR}/itk/Configuration/${WRAPPER_LIBRARY_NAME}Config.py")

    # Pass module factory names into module configuration
    set(ITK_WRAP_PYTHON_CONFIGURATION_FACTORIES)
    foreach(factory IN LISTS ITK_MODULE_${WRAPPER_LIBRARY_NAME}_FACTORY_NAMES)
      string(REPLACE "::" "\",\"" factory_list ${factory})
      set(ITK_WRAP_PYTHON_CONFIGURATION_FACTORIES "${ITK_WRAP_PYTHON_CONFIGURATION_FACTORIES}(\"${factory_list}\"),")
    endforeach()

    # and create the file, with the var ITK_WRAP_PYTHON_CONFIGURATION_TEMPLATES and
    # ITK_WRAP_PYTHON_CONFIGURATION_DEPENDS created earlier
    configure_file("${ITK_WRAP_PYTHON_SOURCE_DIR}/itk/support/ModuleConfig.py.in"
      "${ITK_WRAP_PYTHON_LIBRARY_CONFIG_FILE}"
      @ONLY)
    unset(ITK_WRAP_PYTHON_CONFIGURATION_DEPENDS)

    WRAP_ITK_PYTHON_BINDINGS_INSTALL(/itk/Configuration
      "${WRAPPER_LIBRARY_NAME}"
      "${ITK_WRAP_PYTHON_LIBRARY_CONFIG_FILE}"
      "${ITK_WRAP_PYTHON_SNAKE_CASE}"
    )
    unset(ITK_WRAP_PYTHON_LIBRARY_CONFIG_FILE)
    unset(ITK_WRAP_PYTHON_SNAKE_CASE)

    set(ITK_WRAP_PYTHON_GLOBAL_TIMESTAMP_DECLS )
    set(ITK_WRAP_PYTHON_GLOBAL_TIMESTAMP_CALLS )
    if(NOT BUILD_SHARED_LIBS)
      if(WRAPPER_LIBRARY_NAME STREQUAL "ITKCommon")

        if(WIN32)
          set(DO_NOT_WAIT_FOR_THREADS_DECLS "#include \"itkThreadPool.h\"")
          set(DO_NOT_WAIT_FOR_THREADS_CALLS "itk::ThreadPool::SetDoNotWaitForThreads( true );")
        endif()

        set(ITK_WRAP_PYTHON_GLOBAL_TIMESTAMP_DECLS "
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

")

        set(ITK_WRAP_PYTHON_GLOBAL_TIMESTAMP_CALLS "
  static void * _ITKCommonPython_API[_ITKCommonPython_API_pointers];

  /* Initialize the C API pointer array */
  _ITKCommonPython_API[_ITKCommonPython_GetGlobalSingletonIndex_NUM] = (void *)_ITKCommonPython_GetGlobalSingletonIndex;

  /* Create a Capsule containing the API pointer array's address */
  PyObject * cAPIObject = PyCapsule_New((void *)_ITKCommonPython_API,
    \"_ITKCommonPython._C_API\", NULL);

  if( cAPIObject != NULL )
    {
    PyModule_AddObject( m, \"_C_API\", cAPIObject );
    }
  ${DO_NOT_WAIT_FOR_THREADS_CALLS}
")
      elseif("ITKCommon" IN_LIST WRAPPER_LIBRARY_LINK_LIBRARIES)
        set(ITK_WRAP_PYTHON_GLOBAL_TIMESTAMP_DECLS "
#include \"itkPyITKCommonCAPI.h\"
${DO_NOT_WAIT_FOR_THREADS_DECLS}
")
        set(ITK_WRAP_PYTHON_GLOBAL_TIMESTAMP_CALLS "
  if( import__ITKCommonPython() < 0 )
    {
#if PY_VERSION_HEX >= 0x03000000
    return NULL;
#else
    return;
#endif
    }
  itk::SingletonIndex::SetInstance( _ITKCommonPython_GetGlobalSingletonIndex() );
  itk::ObjectFactoryBase::Initialize();
  ${DO_NOT_WAIT_FOR_THREADS_CALLS}
")
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
    configure_file("${ITK_WRAP_PYTHON_SOURCE_DIR}/main_module_ext.i.in"
      "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/python/${WRAPPER_LIBRARY_NAME}_ext.i"
      @ONLY)

    # set some var reused later
    set(interface_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${WRAPPER_LIBRARY_NAME}.i")
    set(lib ${WRAPPER_LIBRARY_NAME}Python)
    set(python_file "${ITK_PYTHON_PACKAGE_DIR}/${lib}.py")
    set(cpp_file "${CMAKE_CURRENT_BINARY_DIR}/${lib}.cpp")

    # if this is for an external library, let the user add extra swig args
    if(EXTERNAL_WRAP_ITK_PROJECT)
      set(WRAP_ITK_SWIG_ARGS_PYTHON "" CACHE STRING "Extra user-defined swig arguments to be to the swig executable.")
      mark_as_advanced(WRAP_ITK_SWIG_ARGS_PYTHON)
    endif()

    # Run swig to produce the *Python.cpp and the *Python.py file
    itk_setup_swig_python("Module" ${base_name} ${interface_file} ${python_file} ${cpp_file} "")

    # build all the c++ files from this module in a common lib
    if(NOT TARGET ${lib})
      add_library(${lib} MODULE ${cpp_file} ${ITK_WRAP_PYTHON_CXX_FILES} ${WRAPPER_LIBRARY_CXX_SOURCES})
      set_target_properties(${lib} PROPERTIES PREFIX "_")

      # gcc 4.4 complains a lot without this flag when building in release mode
      if (CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
        set_target_properties(${lib} PROPERTIES COMPILE_FLAGS "-fno-strict-aliasing -w")
      endif()
      # extension is not the same on windows
      if(WIN32)
        # normally need *.pyd
        # python_d requires libraries named *_d.pyd
        set_target_properties(${lib} PROPERTIES SUFFIX .pyd)
        set_target_properties(${lib} PROPERTIES DEBUG_POSTFIX "_d")

        if(MSVC)
          # Disables 'conversion from 'type1' to 'type2', possible loss of data warnings
          set_target_properties(${lib} PROPERTIES COMPILE_FLAGS "/wd4244")
        endif()
      endif()
      if (NOT MSVC)
        include(CheckIPOSupported)
        check_ipo_supported(RESULT ipo_is_supported)
        if (ipo_is_supported)
          set_property(TARGET ${lib} PROPERTY INTERPROCEDURAL_OPTIMIZATION_RELEASE TRUE)
        endif()
      endif()

      # Link the modules together
      target_link_libraries(${lib} LINK_PUBLIC ${WRAPPER_LIBRARY_LINK_LIBRARIES})
      itk_target_link_libraries_with_dynamic_lookup(${lib} LINK_PUBLIC ${PYTHON_LIBRARY})

      if(USE_COMPILER_HIDDEN_VISIBILITY)
        # Prefer to use target properties supported by newer cmake
        set_target_properties(${lib} PROPERTIES CXX_VISIBILITY_PRESET hidden)
        set_target_properties(${lib} PROPERTIES C_VISIBILITY_PRESET hidden)
        set_target_properties(${lib} PROPERTIES VISIBILITY_INLINES_HIDDEN 1)
      endif()

      add_dependencies(${lib} ${WRAPPER_LIBRARY_NAME}Swig)
      if(${module_prefix}_WRAP_DOC)
        add_dependencies(${lib} ${WRAPPER_LIBRARY_NAME}Doxygen)
      endif()
      set(_component_module "")
      if(WRAP_ITK_INSTALL_COMPONENT_PER_MODULE)
        if("${WRAPPER_LIBRARY_NAME}" MATCHES "^ITK(PyUtils|PyBase)$")
          set(_component_module "ITKCommon")
        else()
          set(_component_module "${WRAPPER_LIBRARY_NAME}")
        endif()
      endif()
      install(TARGETS "${lib}"
        DESTINATION "${PY_SITE_PACKAGES_PATH}/itk"
        COMPONENT ${_component_module}${WRAP_ITK_INSTALL_COMPONENT_IDENTIFIER}RuntimeLibraries
        )
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

  endif()
  if(${module_prefix}_WRAP_DOC)
    itk_end_wrap_module_DOC()
  endif()

  # Add testing
  set(wrapping_test_directory ${CMAKE_CURRENT_SOURCE_DIR}/test)
  if(BUILD_TESTING AND EXISTS ${wrapping_test_directory}/CMakeLists.txt)
    add_subdirectory(${wrapping_test_directory})
  endif()
endmacro()


################################################################################
# Macros for finding and processing *.wrap files.
################################################################################

macro(itk_auto_load_submodules)
  # Global vars used: WRAPPER_LIBRARY_SOURCE_DIR
  # Global vars modified: WRAPPER_SUBMODULE_ORDER

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
  foreach(_file ${_wrap_cmake_files})
    # get the module name from module.wrap
    get_filename_component(_module "${_file}" NAME_WE)
    list(APPEND WRAPPER_SUBMODULE_ORDER "${_module}")
  endforeach()
  list(REMOVE_DUPLICATES WRAPPER_SUBMODULE_ORDER)
  foreach(_module ${WRAPPER_SUBMODULE_ORDER})
    itk_load_submodule("${_module}")
  endforeach()
endmacro()


macro(itk_load_submodule module)
  # include a cmake module file and generate the associated wrap_*.cxx file.
  # This basically sets the global vars that will be added to or modified
  # by the commands in the included *.wrap module.
  #
  # Global vars used: WRAPPER_LIBRARY_NAME WRAPPER_DEFAULT_INCLUDE
  # Global vars modified: WRAPPER_TYPEDEFS
  #                       WRAPPER_INCLUDE_FILES WRAPPER_AUTO_INCLUDE_HEADERS
  message(STATUS "${WRAPPER_LIBRARY_NAME}: Creating ${module} submodule.")

  # We run into some trouble if there's a module with the same name as the
  # wrapper library. Fix this.
  string(TOUPPER "${module}" upper_module)
  string(TOUPPER "${WRAPPER_LIBRARY_NAME}" upper_lib)
  if("${upper_module}" STREQUAL "${upper_lib}")
    message(FATAL_ERROR "The module ${module} can't have the same name as its library. Note that the names are not case sensitive.")
  endif()

  # call generators specific macros which set several associated global variables
  if(${module_prefix}_WRAP_CASTXML)
    # clear the typedefs and the includes
    set(CASTXML_TYPEDEFS )
    set(CASTXML_INCLUDES )
    set(CASTXML_FORCE_INSTANTIATE )
  endif()
  if(${module_prefix}_WRAP_SWIGINTERFACE)
    # store the content of the SwigInterface.h files - a set of #includes for that module
    set(SWIG_INTERFACE_INCLUDES )
    # typedefs for swig
    set(SWIG_INTERFACE_TYPEDEFS )
  endif()
  if(${module_prefix}_WRAP_DOC)
    set(ITK_WRAP_DOC_DOXY2SWIG_INPUT )  # the c++ name - swig names definitions
  endif()
  if(${module_prefix}_WRAP_PYTHON AND WRAPPER_LIBRARY_PYTHON)
    itk_wrap_submodule_python("${module}" "${WRAPPER_LIBRARY_NAME}")
  endif()

  # WRAPPER_INCLUDE_FILES: contains a list of all files to include in the final cxx file
  set(WRAPPER_INCLUDE_FILES )

  # Add  to the list of files
  # to be #included in the final cxx file.
  foreach(inc ${WRAPPER_DEFAULT_INCLUDE})
    itk_wrap_include("${inc}")
  endforeach()

  # Indicates that the appropriate itk header for this class will be automatically included
  # in later stages of the wrapping process
  set(WRAPPER_AUTO_INCLUDE_HEADERS ON)

  # Now include the .wrap file.
  if(EXISTS "${WRAPPER_LIBRARY_SOURCE_DIR}/${module}.wrap")
      include("${WRAPPER_LIBRARY_SOURCE_DIR}/${module}.wrap")
  else()
    # for backward compatibility, to be removed in ITKv6
    if(EXISTS "${WRAPPER_LIBRARY_SOURCE_DIR}/wrap_${module}.cmake")
        message(FATAL_ERROR "INCORRECT FILE NAME PATTERN: ${WRAPPER_LIBRARY_SOURCE_DIR}/wrap_${module}.cmake should be named ${WRAPPER_LIBRARY_SOURCE_DIR}/${module}.cmake")
    endif()
    message(SEND_ERROR "Module ${WRAPPER_LIBRARY_SOURCE_DIR}/${module}.wrap not found.")
  endif()

  if(${module_prefix}_WRAP_CASTXML)
    # write the wrap_*.cxx file
    # Create the cxx file which will be given to castxml.
    set(cxx_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module}.cxx")
    configure_file("${ITK_WRAP_CASTXML_SOURCE_DIR}/wrap_.cxx.in" "${cxx_file}" @ONLY)

    # the xml file to be generated
    set(xml_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module}.xml")

    set(_castxml_depends)
    if(NOT ITK_USE_SYSTEM_CASTXML)
      # ExternalProject target for CastXML.
      set(_castxml_depends castxml)
    endif()

    set(_ccache_cmd)
    if(ITK_USE_CCACHE)
      set(_ccache_cmd ${CCACHE_EXECUTABLE})
    endif()

    # Avoid missing omp.h include
    set(_castxml_cc_flags ${CMAKE_CXX_FLAGS})
    if(CMAKE_CXX_EXTENSIONS)
      set(_castxml_cc_flags "${_castxml_cc_flags} ${CMAKE_CXX14_EXTENSION_COMPILE_OPTION}")
    else()
      set(_castxml_cc_flags "${_castxml_cc_flags} ${CMAKE_CXX14_STANDARD_COMPILE_OPTION}")
    endif()

    # Aggressive optimization flags cause cast_xml to give invalid error conditions
    set(INVALID_OPTIMIZATION_FLAGS "-fopenmp;-march=[a-zA-Z0-9\-]*;-mtune=[a-zA-Z0-9\-]*;-mfma")
    foreach( rmmatch ${INVALID_OPTIMIZATION_FLAGS})
      string(REGEX REPLACE ${rmmatch} "" _castxml_cc_flags "${_castxml_cc_flags}")
    endforeach()
    unset(INVALID_OPTIMIZATION_FLAGS)

    # Configure the internal Clang preprocessor and target platform to match that of the given compiler command.
    separate_arguments(_castxml_cc_flags)
    set(_castxml_cc)
    if(MSVC)
      set(_castxml_cc --castxml-cc-msvc ( "${CMAKE_CXX_COMPILER}" ${_castxml_cc_flags} ) -fexceptions)
      if(MSVC90)
        # needed for VS2008 64 bit
        set(_castxml_cc ${_castxml_cc} "-D_HAS_TR1=0")
      endif()
    else()
      set(_castxml_cc --castxml-cc-gnu ( "${CMAKE_CXX_COMPILER}" ${_castxml_cc_flags} ))
    endif()

    # Override castxml target platform when cross compiling
    set(_target)
    if(CMAKE_CROSSCOMPILING)
      if(NOT CMAKE_CXX_COMPILER_TARGET)
        message(FATAL_ERROR "Set the target triple in CMAKE_CXX_COMPILER_TARGET "
                " as described in http://clang.llvm.org/docs/CrossCompilation.html")
      endif()
      set(_target "--target=${CMAKE_CXX_COMPILER_TARGET}")
    endif()

    set(_build_env)
    if(APPLE)
      # If building on OS X, make sure that CastXML's calls to the compiler have the
      # settings that the output files will be compiled with.  This prevents headers
      # from one version of OS X from being used when building for another version.
      list(APPEND _build_env
              env
              "SDKROOT=${CMAKE_OSX_SYSROOT}"
              "MACOSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}"
              )
    endif()

    set(_include ${${WRAPPER_LIBRARY_NAME}_SOURCE_DIR}/include)
    set(_hdrs)
    set(glob_hdrs)
    if(EXISTS ${_include})
      file(GLOB_RECURSE glob_hdrs ${_include}/*.h)
    endif()
    foreach(header IN LISTS glob_hdrs)
      get_filename_component(header_name ${header} NAME)
      if(${header_name} IN_LIST WRAPPER_INCLUDE_FILES)
        list(APPEND _hdrs ${header})
      endif()
    endforeach()
    unset(glob_hdrs)
    unset(_include)

    # write the module.xml file using castxml
    add_custom_command(
            OUTPUT ${xml_file}
            COMMAND ${_build_env} ${_ccache_cmd} ${CASTXML_EXECUTABLE}
            -o ${xml_file}
            --castxml-gccxml
            ${_target}
            --castxml-start _wrapping_
            ${_castxml_cc}
            -w
            -c # needed for ccache to think we are not calling for link
            @${castxml_inc_file}
            ${cxx_file}
            VERBATIM
            DEPENDS ${_castxml_depends} ${cxx_file} ${castxml_inc_file} ${_hdrs}
    )

    list(APPEND CastXML_OUTPUT_FILES ${xml_file})
  endif()

  if(${module_prefix}_WRAP_SWIGINTERFACE)
    itk_end_wrap_submodule_swig_interface("${module}")
  endif()
  if(${module_prefix}_WRAP_PYTHON AND WRAPPER_LIBRARY_PYTHON)
    itk_end_wrap_submodule_python("${module}")
  endif()
  if(${module_prefix}_WRAP_DOC)
    itk_end_wrap_submodule_DOC("${module}")
  endif()

  unset(_hdrs)
  unset(_build_env)
  unset(_target)
  unset(_castxml_cc)
  unset(_castxml_cc_flags)
  unset(_ccache_cmd)
  unset(_castxml_depends)
  unset(xml_file)
  unset(cxx_file)
endmacro()


################################################################################
# Macros to be used in the *.wrap files themselves.
# These macros specify that a class is to be wrapped, that certain itk headers
# are to be included, and what specific template instatiations are to be wrapped.
################################################################################

macro(itk_wrap_class class)
  # Wraps the c++ class 'class'. This parameter must be a fully-qualified c++
  # name.
  # The class will be named in the SWIG wrappers as the top-level namespace
  # concatenated to the base class name. E.g. itk::Image -> itkImage or
  # itk::Statistics::Sample -> itkSample.
  # If the top-level namespace is 'itk' and WRAPPER_AUTO_INCLUDE_HEADERS is ON
  # then the appropriate itk header for this class will be included. Otherwise
  # itk_wrap_include should be manually called from the *.wrap file that calls
  # this macro.
  # Lastly, this class takes an optional 'wrap method' parameter. Valid values are:
  # POINTER POINTER_WITH_CONST_POINTER POINTER_WITH_SUPERCLASS POINTER_WITH_2_SUPERCLASSES
  # EXPLICIT_SPECIALIZATION POINTER_WITH_EXPLICIT_SPECIALIZATION ENUM AUTOPOINTER
  #
  # Global vars used: none
  # Global vars modified: WRAPPER_INCLUDE_FILES
  # drop the namespace prefix
  if("${class}" MATCHES "::")
    # there's at least one namespace in the name
    string(REGEX REPLACE ".*::" "" base_name "${class}")
    string(REGEX REPLACE "^([^:]*::)?.+" "\\1" top_namespace "${class}")
    string(REGEX REPLACE "::" "" top_namespace "${top_namespace}") # drop the :: from the namespace
    set(swig_name "${top_namespace}${base_name}")
  else()
    # no namespaces
    set(swig_name "${class}")
  endif()

  # Call the itk_wrap_named_class macro, including any optional arguments
  itk_wrap_named_class("${class}" "${swig_name}" ${ARGN})

  # and include the class's header
  if(WRAPPER_AUTO_INCLUDE_HEADERS)
    itk_wrap_include("${swig_name}.h")
  endif()
endmacro()

macro(itk_wrap_named_class class swig_name)
  # Begin the wrapping of a new templated class. The 'class' parameter is a
  # fully-qualified C++ type name, including the namespace. Between itk_wrap_class
  # and itk_end_wrap_class various macros should be called to cause certain template
  # instances to be automatically added to the wrap_*.cxx file. itk_end_wrap_class
  # actually parses through the template instaces that have been recorded and
  # creates the content of that cxx file. itk_wrap_simple_class should be used
  # to create a definition for a non-templated class. (Note that internally,
  # itk_wrap_simple_class eventually calls this macro. This macro should never
  # be called directly for a non-templated class though.)
  #
  # The second parameter of this macro is the name that the class should be given
  # in SWIG (with template definitions providing additional mangled suffixes to this name)
  #
  # Lastly, this class takes an optional 'wrap method' parameter. Valid values are:
  # POINTER POINTER_WITH_CONST_POINTER POINTER_WITH_SUPERCLASS POINTER_WITH_2_SUPERCLASSES
  # EXPLICIT_SPECIALIZATION POINTER_WITH_EXPLICIT_SPECIALIZATION ENUM AUTOPOINTER
  # If no parameter is given, the class is simply wrapped as-is. If the parameter
  # is "POINTER" then the class is wrapped and so is the SmartPointer template type
  # that is typedef'd as class::Pointer.
  # If POINTER_WITH_SUPERCLASS is given, class, class::Pointer, class::Superclass,
  # and class::Superclass::Pointer are wrapped. This requires that the class
  # has a typedef'd "Superclass" and that that superclass has Pointer and Self
  # typedefs.
  #
  # Global vars used: none
  # Global vars modified: WRAPPER_CLASS WRAPPER_TEMPLATES WRAPPER_INCLUDE_FILES
  #                       WRAPPER_WRAP_METHOD WRAPPER_SWIG_NAME

  # first, we must be sure the wrap method is valid
  if("${ARGC}" EQUAL 2)
    # store the wrap method
    set(WRAPPER_WRAP_METHOD "")
  endif()

  if("${ARGC}" EQUAL 3)
    set(WRAPPER_WRAP_METHOD "${ARGV2}")
    set(VALID_WRAP_METHODS POINTER POINTER_WITH_CONST_POINTER POINTER_WITH_SUPERCLASS POINTER_WITH_2_SUPERCLASSES EXPLICIT_SPECIALIZATION POINTER_WITH_EXPLICIT_SPECIALIZATION ENUM AUTOPOINTER)
    if( NOT "${WRAPPER_WRAP_METHOD}" IN_LIST VALID_WRAP_METHODS)
      message(SEND_ERROR "itk_wrap_class: Invalid option '${WRAPPER_WRAP_METHOD}'. Possible values are ${VALID_WRAP_METHODS}")
    endif()
  endif()

  if("${ARGC}" GREATER 3)
    message(SEND_ERROR "Too many arguments")
  endif()

  set(WRAPPER_CLASS "${class}")
  set(WRAPPER_SWIG_NAME "${swig_name}")
  set(WRAPPER_WARN_ABOUT_NO_TEMPLATE ON)
  # clear the wrap parameters
  # TODO: It shouldn't be used with the new architecture!!
  set(WRAPPER_TEMPLATES)

  if(${module_prefix}_WRAP_PYTHON AND WRAPPER_LIBRARY_PYTHON)
    # store the current class wrapped, so we can generate the typemaps for itk::ImageSource
    set(ITK_WRAP_PYTHON_CURRENT_CLASS "${class}")
    set(ITK_WRAP_PYTHON_CURRENT_SWIG_NAME "${swig_name}")
  endif()
  if(${module_prefix}_WRAP_DOC)
    itk_wrap_named_class_DOC("${class}" "${swig_name}")
  endif()
endmacro()

macro(itk_wrap_simple_class class)
  # Similar to itk_wrap_class in that it generates typedefs for Swig input.
  # However, since no templates need to be declared, there's no need for
  # itk_wrap_class ... (declare templates) .. itk_end_wrap_class. Instead
  # itk_wrap_simple_class takes care of it all.
  # A fully-qualified 'class' parameter is required as above. The swig name for
  # this class is generated as in itk_wrap_class.
  # Lastly, this class takes an optional 'wrap method' parameter. Valid values are:
  # POINTER POINTER_WITH_CONST_POINTER and POINTER_WITH_SUPERCLASS.

  itk_wrap_class("${class}" ${ARGN})
  # to avoid useless warning: no template can be defined in
  set(WRAPPER_WARN_ABOUT_NO_TEMPLATE OFF)
  itk_wrap_one_type("${WRAPPER_WRAP_METHOD}" "${WRAPPER_CLASS}" "${WRAPPER_SWIG_NAME}")
  itk_end_wrap_class()
endmacro()


macro(itk_wrap_named_simple_class class swig_name)
  # Similar to itk_wrap_named_class in that it generates typedefs for Swig input.
  # However, since no templates need to be declared, there's no need for
  # itk_wrap_class ... (declare templates) .. itk_end_wrap_class. Instead
  # itk_wrap_named_simple_class takes care of it all.
  # A fully-qualified 'class' parameter is required as above. The swig name for
  # this class is provided by the second parameter.
  # Lastly, this class takes an optional 'wrap method' parameter. Valid values are:
  # POINTER POINTER_WITH_CONST_POINTER and POINTER_WITH_SUPERCLASS.

  itk_wrap_named_class("${class}" "${swig_name}" ${ARGN})
  # to avoid useless warning: no template can be defined in
  set(WRAPPER_WARN_ABOUT_NO_TEMPLATE OFF)
  itk_wrap_one_type("${WRAPPER_WRAP_METHOD}" "${WRAPPER_CLASS}" "${WRAPPER_SWIG_NAME}")
endmacro()


macro(itk_wrap_include include_file)
  # Add a header file to the list of files to be #included in the final cxx file.
  # Global vars used: WRAPPER_INCLUDE_FILES
  # Global vars modified: WRAPPER_INCLUDE_FILES
  set(already_included 0)
  foreach(included ${WRAPPER_INCLUDE_FILES})
    if("${include_file}" STREQUAL "${included}")
      set(already_included 1)
    endif()
  endforeach()

  if(NOT already_included)
    # include order IS important. Default values must be before the other ones
    set(WRAPPER_INCLUDE_FILES
      ${WRAPPER_INCLUDE_FILES}
      ${include_file}
    )

    if(${module_prefix}_WRAP_CASTXML)
      if("${include_file}" MATCHES "<.*>")
        set(CASTXML_INCLUDES "${CASTXML_INCLUDES}#include ${include_file}\n")
      else()
        set(CASTXML_INCLUDES "${CASTXML_INCLUDES}#include \"${include_file}\"\n")
      endif()
    endif()
    if(${module_prefix}_WRAP_SWIGINTERFACE)
      list(APPEND SWIG_INTERFACE_INCLUDES ${include_file})
    endif()
  endif()

  unset(already_included)
endmacro()


macro(itk_end_wrap_class)
  # Parse through the list of WRAPPER_TEMPLATES set up by the macros at the bottom
  # of this file, turning them into proper C++ type definitions suitable for
  # input to Swig. The C++ definitions are stored in WRAPPER_TYPEDEFS.
  #
  # Global vars used: WRAPPER_CLASS WRAPPER_WRAP_METHOD WRAPPER_TEMPLATES WRAPPER_SWIG_NAME
  # Global vars modified: WRAPPER_TYPEDEFS

  # the regexp used to get the values separated by a #
  if(NOT "${WRAPPER_TEMPLATES}" STREQUAL "")
    set(sharp_regexp "([0-9A-Za-z_]*)[ ]*#[ ]*(.*)")
    foreach(wrap ${WRAPPER_TEMPLATES})
      string(REGEX REPLACE "${sharp_regexp}" "\\1" mangled_suffix "${wrap}")
      string(REGEX REPLACE "${sharp_regexp}" "\\2" template_params "${wrap}")
      itk_wrap_one_type("${WRAPPER_WRAP_METHOD}" "${WRAPPER_CLASS}" "${WRAPPER_SWIG_NAME}${mangled_suffix}" "${template_params}")
    endforeach()
  else()
    if(WRAPPER_WARN_ABOUT_NO_TEMPLATE AND NOT EXTERNAL_WRAP_ITK_PROJECT)
      # display a warning if the class is empty
      message("Warning: No template declared for ${WRAPPER_CLASS}. Perhaps you should turn on more WRAP_* options?")
    endif()
  endif()
endmacro()


macro(itk_wrap_simple_type wrap_class swig_name)
  # Add a typedef, without support for any option
  if(${module_prefix}_WRAP_CASTXML)
    set(CASTXML_TYPEDEFS "${CASTXML_TYPEDEFS}    typedef ${wrap_class} ${swig_name};\n")
    set(CASTXML_FORCE_INSTANTIATE "${CASTXML_FORCE_INSTANTIATE}    (void)sizeof(${swig_name});\n")
  endif()
  if(${module_prefix}_WRAP_PYTHON AND WRAPPER_LIBRARY_PYTHON)
    itk_wrap_simple_type_python("${wrap_class}" "${swig_name}")
  endif()
endmacro()


macro(itk_wrap_one_type wrap_method wrap_class swig_name)
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

  itk_wrap_one_type_all_generators("${wrap_method}" "${wrap_class}" "${swig_name}" "${ARGV3}")

  # Add a typedef for the class. We have this funny looking full_name::base_name
  # thing (it expands to, for example "typedef itk::Foo<baz, 2>::Foo"), to
  # trick castxml into creating code for the class. If we left off the trailing
  # base_name, then castxml wouldn't see the typedef as a class instantiation,
  # and thus wouldn't create XML for any of the methods, etc.

  if("${wrap_method}" MATCHES "2_SUPERCLASSES")
    itk_wrap_simple_type("${full_class_name}::Superclass::Superclass" "${swig_name}_Superclass_Superclass")
    itk_wrap_simple_type("${full_class_name}::Superclass::Superclass::Pointer" "${swig_name}_Superclass_Superclass_Pointer")
  endif()

  if("${wrap_method}" MATCHES "SUPERCLASS")
    itk_wrap_simple_type("${full_class_name}::Superclass" "${swig_name}_Superclass")
    itk_wrap_simple_type("${full_class_name}::Superclass::Pointer" "${swig_name}_Superclass_Pointer")
  endif()

  if("${wrap_method}" MATCHES "CONST_POINTER")
    # add a const pointer typedef if we are so asked
    itk_wrap_simple_type("${full_class_name}::ConstPointer" "${swig_name}_ConstPointer")
  endif()

  itk_wrap_simple_type("${full_class_name}" "${swig_name}")

  if("${wrap_method}" MATCHES "POINTER")
    if("${wrap_method}" STREQUAL "AUTOPOINTER")
      # add a pointer typedef if we are so asked
      itk_wrap_simple_type("${full_class_name}::SelfAutoPointer" "${swig_name}_AutoPointer")
    else()
      # add a pointer typedef if we are so asked
      itk_wrap_simple_type("${full_class_name}::Pointer" "${swig_name}_Pointer")
    endif()
  endif()

endmacro()



################################################################################
# Macros which cause one or more template instantiations to be added to the
# WRAPPER_TEMPLATES list. This list is initialized by the macro itk_wrap_class above,
# and used by the macro itk_end_wrap_class to produce the wrap_xxx.cxx files with
# the correct templates. These cxx files serve as the Swig inputs.
################################################################################

macro(itk_wrap_template name types)
  # This is the fundamental macro for adding a template to be wrapped.
  # 'name' is a mangled suffix to be added to the class name (defined in itk_wrap_class)
  # to uniquely identify this instantiation.
  # 'types' is a comma-separated list of the template parameters (in C++ form),
  # some common parameters (e.g. for images) are stored in variables by
  # WrapBasicTypes.cmake and WrapITKTypes.cmake.
  #
  # The format of the WRAPPER_TEMPLATES list is a series of "name # types" strings
  # (because there's no CMake support for nested lists, name and types are
  # separated out from the strings with a regex).
  #
  # Global vars used: WRAPPER_TEMPLATES
  # Global vars modified: WRAPPER_TEMPLATES

#   list(APPEND WRAPPER_TEMPLATES "${name} # ${types}")
  set(WRAPPER_WARN_ABOUT_NO_TEMPLATE OFF)
  itk_wrap_one_type("${WRAPPER_WRAP_METHOD}" "${WRAPPER_CLASS}" "${WRAPPER_SWIG_NAME}${name}" "${types}")
  if(${module_prefix}_WRAP_PYTHON AND WRAPPER_LIBRARY_PYTHON)
    itk_wrap_template_python("${name}" "${types}")
  endif()
endmacro()

###################################
# Macros for wrapping image filters
###################################

# First, a set of convenience macros for wrapping an image filter with all
# user-selected image types of a given class. These macros take a 'param_count'
# parameter which indicates how many template parameters the current image filter
# takes. The parameters are filled with the exact same image type. To wrap image
# filters which take different image types as different template parameters, use
# itk_wrap_image_filter_types or itk_wrap_image_filter_combinations.
# These macros also take an optional second parameter which is a "dimensionality
# condition" to restrict the dimensions that theis filter will be instantiated
# for. The condition can either be a single number indicating the one dimension
# allowed, a list of dimensions that are allowed (either as a single ;-delimited
# string or just a set of separate parameters), or something of the form "n+"
# (where n is a number) indicating that instantiations are allowed for dimension
# n and above.

macro(itk_wrap_image_filter param_types param_count)
  # itk_wrap_image_filter is a more general macro for wrapping image filters that
  # need one or more image parameters of the same type. The first parameter to this
  # macro is a list of image pixel types for which filter instantiations should be
  # created. The second is a 'param_count' parameter which controls how many image
  # template parameters are created (see above). The optional third parameter is
  # a dimensionality condition (see above also).
  #
  # E.g. itk_wrap_image_filter("${WRAP_ITK_ALL}" 2) will create template instantiations
  # of the filter for every pixel type that the user has selected.

  set(have_dim_cond OFF)
  if(NOT "${ARGN}" STREQUAL "")
    set(have_dim_cond ON)
  endif()

  foreach(param_type ${param_types})
    set(param_list "")
    foreach(i RANGE 1 ${param_count})
      list(APPEND param_list ${param_type})
    endforeach()
    if(have_dim_cond)
      itk_wrap_image_filter_types(${param_list} "${ARGN}")
    else()
      itk_wrap_image_filter_types(${param_list})
    endif()
  endforeach()
endmacro()

macro(itk_wrap_image_filter_combinations)
  # itk_wrap_image_filter_combinations takes a variable number of parameters. Each
  # parameter is a list of image pixel types. Filter instantiations are created
  # for every combination of different pixel types in different parameters.
  # A dimensionality condition may be optionally specified as the last parameter.
  #
  # E.g. itk_wrap_image_filter_combinations("UC;US" "UC;US") will create:
  # filter<itk::Image<unsigned char, d>, itk::Image<unsigned char, d> >
  # filter<itk::Image<unsigned char, d>, itk::Image<unsigned short, d> >
  # filter<itk::Image<unsigned short, d>, itk::Image<unsigned char, d> >
  # filter<itk::Image<unsigned short, d>, itk::Image<unsigned short, d> >
  # where 'd' is the image dimension, for each selected image dimension.

  # First, store the variable args in real varables, not the macro parameters.
  # Parameters can't be looked up like this: ${ARGV${num}} because they are
  # textually substituted before the macro is evaluated.
  set(arg0 ${ARGV0})
  set(arg1 ${ARGV1})
  set(arg2 ${ARGV2})
  set(arg3 ${ARGV3})
  set(arg4 ${ARGV4})
  set(arg5 ${ARGV5})
  set(arg6 ${ARGV6})
  set(arg7 ${ARGV7})
  set(arg8 ${ARGV8})
  set(arg9 ${ARGV9})
  DECREMENT(last_arg_number ${ARGC})

  # Now see if we have a dimension condition, and if so, note it and remove it
  # from the list of args that we will process later
  set(have_dim_cond OFF)
  set(last_arg "${arg${last_arg_number}}")
  if("${last_arg}" MATCHES "^[0-9]")
    # We have a dimensionality condition
    set(have_dim_cond ON)
    DECREMENT(last_arg_number ${last_arg_number})
  endif()

  # Build up a list of all of the combinations of all of the elements in each
  # argument. Each combinarion is stored as a #-delimited list of pixel types.
  # The #-delimiter is needed because CMake can't store nested lists.
  # Also note the need to check for empty lists and note invalidity if so.
  set(all_args_valid ON)
  if(NOT arg0)
    set(all_args_valid OFF)
  else()
    set(template_combinations ${arg0})
  endif()

  foreach(num RANGE 1 ${last_arg_number})
    set(types "${arg${num}}")
    if(NOT types)
      set(all_args_valid OFF)
    else()
      set(temp "")
      foreach(type_list ${template_combinations})
        foreach(type ${types})
          list(APPEND temp "${type_list}#${type}")
        endforeach()
      endforeach()
      set(template_combinations ${temp})
    endif()
  endforeach()

  if(all_args_valid)
    foreach(param_set ${template_combinations})
      # Each param_set is a #-delimited list of pixel types. First thing, we unpack
      # param_set back to a CMake list (;-delimited). Then we instantiate the filter
      # for that combination of image pixel types.
      string(REPLACE "#" ";" param_list "${param_set}")
      if(have_dim_cond)
        itk_wrap_image_filter_types(${param_list} "${last_arg}")
      else()
        itk_wrap_image_filter_types(${param_list})
      endif()
    endforeach()
  endif()
endmacro()


macro(itk_wrap_image_filter_types)
  # itk_wrap_image_filter_types creates template instantiations of the current image
  # filter, for all the selected dimensions (or dimensions that meet the optional
  # dimensionality condition). This macro takes a variable number of arguments,
  # which should correspond to the image pixel types of the images in the filter's
  # template parameter list. The optional dimensionality condition should be
  # placed in the last parameter.

  # First, store the variable args in real varables, not the macro parameters.
  # Parameters can't be looked up like this: ${ARGV${num}} because they are
  # textually substituted before the macro is evaluated.
  set(arg0 ${ARGV0})
  set(arg1 ${ARGV1})
  set(arg2 ${ARGV2})
  set(arg3 ${ARGV3})
  set(arg4 ${ARGV4})
  set(arg5 ${ARGV5})
  set(arg6 ${ARGV6})
  set(arg7 ${ARGV7})
  set(arg8 ${ARGV8})
  set(arg9 ${ARGV9})
  DECREMENT(last_arg_number ${ARGC})

  set(last_arg "${arg${last_arg_number}}")
  if("${last_arg}" MATCHES "^[0-9]")
    # We have a dimensionality condition
    itk_wrap_filter_dims(dims "${last_arg}")
    DECREMENT(last_arg_number ${last_arg_number})
  else()
    set(dims ${ITK_WRAP_IMAGE_DIMS})
  endif()

  set(vec_dims 1)
  foreach(num RANGE 0 ${last_arg_number})
    set(type "${arg${num}}")
    if("${WRAP_ITK_VECTOR}" MATCHES "(^|;)${type}(;|$)")
      set(vec_dims ${ITK_WRAP_VECTOR_COMPONENTS})
    endif()
  endforeach()

  foreach(vec_dim ${vec_dims})
    foreach(d ${dims})
      set(template_params "")
      set(mangled_name "")
      set(comma "") # Don't add a comma before the first template param!
      foreach(num RANGE 0 ${last_arg_number})
        set(type "${arg${num}}")
        if("${WRAP_ITK_VECTOR}" MATCHES "(^|;)${type}(;|$)")
          set(type "${type}${vec_dim}")
        endif()
        set(image_type ${ITKT_I${type}${d}})
        set(mangle_type ${ITKM_I${type}${d}})
        if(NOT DEFINED image_type)
          message(FATAL_ERROR "Wrapping ${WRAPPER_CLASS}: No image type for '${type}' pixels is known.")
        endif()

        set(template_params "${template_params}${comma}${image_type}")
        set(mangled_name "${mangled_name}${mangle_type}")
        set(comma ", ") # now add commas after the subsequent template params
      endforeach()
      itk_wrap_template("${mangled_name}" "${template_params}")
    endforeach()
  endforeach()
endmacro()


macro(itk_wrap_filter_dims var_name dimension_condition)
  # itk_wrap_filter_dims processes a dimension_condition and returns a list of the dimensions
  # that (a) meet the condition, and (b) were selected to be wrapped. Recall
  # that the condition is either a CMake list of dimensions, or a string of the
  # form "n+" where n is a number.

  if("${dimension_condition}" MATCHES "^[0-9]+\\+$")
    # The condition is of the form "n+". Make a list of the
    # selected wrapping dims that are >= that number.
    string(REGEX REPLACE "^([0-9]+)\\+$" "\\1" min_dim "${dimension_condition}")
    DECREMENT(max_disallowed ${min_dim})
    set(${var_name} "")
    foreach(d ${ITK_WRAP_IMAGE_DIMS})
      if("${d}" GREATER "${max_disallowed}")
        set(${var_name} ${${var_name}} ${d})
      endif()
    endforeach()
  else()
    # The condition is just a list of dims. Return the intersection of these
    # dims with the selected ones.
    INTERSECTION(${var_name} "${dimension_condition}" "${ITK_WRAP_IMAGE_DIMS}")
  endif()
endmacro()
