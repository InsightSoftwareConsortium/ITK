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

## The functions dump_cmake_variables and write_changed_cmake_variables_to_file
## are useful for debugging the wrapping macro behaviors
# They are loosely based off suggestions from:
# https://stackoverflow.com/questions/9298278/cmake-print-out-all-accessible-variables-in-a-script
set(DO_DEBUG_MACROS OFF)
function(dump_cmake_variables output_var_name this_module_name)
  if(DO_DEBUG_MACROS)
    if(this_module_name STREQUAL "itkEuler3DTransform")
      get_cmake_property(_variableNames VARIABLES)
      list(FILTER _variableNames EXCLUDE REGEX "itk_auto_load_submodules.*")
      list(FILTER _variableNames EXCLUDE REGEX "^ARG.*")
      list(FILTER _variableNames EXCLUDE REGEX "^CMAKE_MATCH.*")
      list(SORT _variableNames)
      foreach(_variableName ${_variableNames})
        list(
          APPEND
          output_equalities
          "^^${_variableName}=${${_variableName}}\n-----------------------------------------\n"
        )
      endforeach()
      set(${output_var_name} ${output_equalities} PARENT_SCOPE)
      #message(STATUS "AAA\n:${output_var_name}===:${_variableNames}:\n\n")
      unset(output_var_name)
    endif()
  endif()
endfunction()

function(
  write_changed_cmake_variables_to_file
  output_filename
  pre_list
  post_list
  this_module_name
)
  if(DO_DEBUG_MACROS)
    if(this_module_name STREQUAL "itkEuler3DTransform")
      #message(STATUS "\n\n\nXXX PRE : ${pre_list}")
      #message(STATUS "\nYYYPOST : ${itk_auto_load_submodules_${WRAPPER_LIBRARY_NAME}_post}")
      set(changed_post_list ${post_list})
      #message(STATUS "YYY\n:${changed_post_list}:\n\n")
      #message(STATUS "\nYYYPOST : ${itk_auto_load_submodules_${WRAPPER_LIBRARY_NAME}_post}")
      foreach(pre_list_item ${pre_list})
        list(REMOVE_ITEM changed_post_list "${pre_list_item}")
      endforeach()
      set(
        write_string
        "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n"
      )
      message(STATUS "ZZZ\n:${changed_post_list}:${this_module_name}\n\n")
      foreach(changed_post_list_item ${changed_post_list})
        set(write_string "${write_string}${changed_post_list_item}")
      endforeach()
      file(WRITE ${output_filename} ${write_string})
      unset(write_string)
    endif()
  endif()
endfunction()

macro(itk_wrap_submodule_python submodule module)
  # Start wrapping the modules
  #ITK_WRAP_PYTHON_SWIG_EXT ITK_WRAP_PYTHON_LIBRARY_IMPORTS
  set(
    ITK_WRAP_PYTHON_SWIG_EXT
    "%pythonbegin %{\nfrom . import _${module}Python\n%}\n\n"
  )

  # register the module for the lib module
  string(
    PREPEND
    ITK_WRAP_PYTHON_LIBRARY_IMPORTS
    "from itk.${submodule}Python import *\n"
  )
endmacro()

# Support for additional include directories of each module
# WARNING: Each module must set this variable BEFORE calling itk_wrap_module
# TODO: is this the place place for this?
set(
  WRAPPER_LIBRARY_INCLUDE_DIRECTORIES
  ""
  CACHE INTERNAL
  "additional include directories for each module"
)

###############################################################################
# Define fundamental wrapping macro which sets up the global variables used
# across all of the wrapping macros included at the end of this file.
# All variables set here are optional and have sensible default values.
# Also define some other global defaults like WRAPPER_MASTER_INDEX_OUTPUT_DIR.
###############################################################################
macro(itk_wrap_module library_name)
  # Initialize for wrapping a new module.  Clear many variables.
  if("${ARGC}" EQUAL 2)
    # First turn everything off
    foreach(lang ${WRAP_ITK_GENERATORS})
      string(TOUPPER ${lang} LANG)
      set(WRAPPER_LIBRARY_${LANG} OFF)
    endforeach()
    # Explicitly turn requested items ON
    set(REQUESTED_WRAPPINGS "${ARGV1}")
    foreach(lang ${REQUESTED_WRAPPINGS})
      string(TOUPPER ${lang} LANG)
      set(WRAPPER_LIBRARY_${LANG} ON)
    endforeach()
    unset(REQUESTED_WRAPPINGS)
  else()
    # Otherwise turn everything on
    foreach(lang ${WRAP_ITK_GENERATORS})
      string(TOUPPER ${lang} LANG)
      set(WRAPPER_LIBRARY_${LANG} ON)
    endforeach()
  endif()
  if(NOT WRAPPER_LIBRARY_PYTHON)
    message(
      FATAL_ERROR
      "PYTHON WRAPPING IS REQUIRED for any other wrapping to work"
    )
  endif()
  unset(LANG)

  set(WRAPPER_LIBRARY_NAME "${library_name}")
  message(STATUS "${WRAPPER_LIBRARY_NAME}: Creating module.")

  # Mark the current source dir for inclusion because it may contain header files.
  include_directories(BEFORE "${CMAKE_CURRENT_SOURCE_DIR}")
  include_directories(BEFORE ${WRAPPER_LIBRARY_INCLUDE_DIRECTORIES})

  # WRAPPER_LIBRARY_SOURCE_DIR. Directory to be scanned for *.wrap files.
  set(WRAPPER_LIBRARY_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

  # WRAPPER_LIBRARY_DEPENDS. List of names of other wrapper libraries that
  # define symbols used by this wrapper library.
  intersection(WRAPPER_LIBRARY_DEPENDS "${ITK_MODULE_${library_name}_DEPENDS}" "${WRAP_ITK_MODULES}")

  # WRAPPER_LIBRARY_LINK_LIBRARIES. List of other libraries that should
  # be linked to the wrapper library.
  set(
    WRAPPER_LIBRARY_LINK_LIBRARIES
    ${ITK_LIBRARIES}
    ${${itk-module}_LIBRARIES}
  )

  # WRAPPER_SUBMODULE_ORDER. List of *.wrap submodules in the source dir
  # that should be included/wrapped before the rest in the given order.
  # Just the submodule group name is needed, not the full path or file name.
  unset(WRAPPER_SUBMODULE_ORDER)

  # WRAPPER_LIBRARY_SWIG_INPUTS. List of C++ source files to be used
  # as input for Swig. This list is then appended to by
  # WRAPPER_LIBRARY_AUTO_INCLUDE_WRAP_FILES. A full path to each input is required.
  unset(WRAPPER_LIBRARY_SWIG_INPUTS)

  # WRAPPER_SWIG_LIBRARY_FILES. List of swig .swg files to pass to cswig to control
  # type handling and so forth. A full path to each include is required.
  # The itk.swg file and the library file for the current library are implicitly added.
  unset(WRAPPER_SWIG_LIBRARY_FILES)

  # WRAPPER_LIBRARY_CXX_SOURCES. C++ sources to be compiled and linked in
  # to the wrapper library (with no prior processing by swig, etc.)
  # A full path to each input is required.
  unset(WRAPPER_LIBRARY_CXX_SOURCES)

  # Call the language support initialization function

  # store the content of the mdx file
  unset(SWIG_INTERFACE_MDX_CONTENT)
  # store the content of the .i file for the module - a set of import of all the .i files generated for the module
  unset(SWIG_INTERFACE_MODULE_CONTENT)
  # build a list of modules to create the igenerator custom command
  unset(SWIG_INTERFACE_MODULES)

  if(${module_prefix}_WRAP_DOC)
    unset(ITK_WRAP_DOC_DOXYGEN_HEADERS) # doxygen headers to process in this lib
    unset(ITK_WRAP_DOC_DOXYGEN_XML_FILES) # xml files produced by doxygen in this lib
    unset(ITK_WRAP_DOC_DOCSTRING_FILES) # swig docstring files produced by doxygen in this lib
  endif()

  unset(ITK_WRAP_PYTHON_CONFIGURATION_TEMPLATES)
  unset(ITK_WRAP_PYTHON_LIBRARY_IMPORTS)
  unset(ITK_WRAP_PYTHON_LIBRARY_DEPS)
  unset(ITK_WRAP_PYTHON_LIBRARY_DECLS)
  set(
    ITK_WRAP_PYTHON_LIBRARY_CALLS
    "\nPyObject * sysModules = PyImport_GetModuleDict();\n"
  )
  unset(ITK_WRAP_PYTHON_CXX_FILES)
  unset(ITK_WRAP_PYTHON_FILES)
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
  unset(swig_name)
  unset(top_namespace)
  unset(base_name)
endmacro()

macro(itk_wrap_named_class class swig_name)
  # Begin the wrapping of a new templated class. The 'class' parameter is a
  # fully-qualified C++ type name, including the namespace. Between itk_wrap_class
  # and itk_end_wrap_class various macros should be called to cause certain template
  # instances to be automatically added to the wrap_*.cxx file. itk_end_wrap_class
  # actually parses through the template instances that have been recorded and
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
    set(
      VALID_WRAP_METHODS
      POINTER
      POINTER_WITH_CONST_POINTER
      POINTER_WITH_SUPERCLASS
      POINTER_WITH_2_SUPERCLASSES
      EXPLICIT_SPECIALIZATION
      POINTER_WITH_EXPLICIT_SPECIALIZATION
      ENUM
      AUTOPOINTER
    )
    if(NOT "${WRAPPER_WRAP_METHOD}" IN_LIST VALID_WRAP_METHODS)
      message(
        SEND_ERROR
        "itk_wrap_class: Invalid option '${WRAPPER_WRAP_METHOD}'. Possible values are ${VALID_WRAP_METHODS}"
      )
    endif()
    unset(VALID_WRAP_METHODS)
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

  # store the current class wrapped, so we can generate the typemaps for itk::ImageSource
  set(ITK_WRAP_PYTHON_CURRENT_CLASS "${class}")
  set(ITK_WRAP_PYTHON_CURRENT_SWIG_NAME "${swig_name}")

  if(${module_prefix}_WRAP_DOC)
    if("${WRAPPER_WRAP_METHOD}" STREQUAL "ENUM")
      # doc is not generated in the same way for enum. Just ignore it
      set(ITK_WRAP_DOC_GENERATE_DOXY2SWIG_INPUT OFF)
    else()
      set(ITK_WRAP_DOC_GENERATE_DOXY2SWIG_INPUT OFF)
      get_directory_property(dirs INCLUDE_DIRECTORIES)
      set(paths)
      foreach(dir ${dirs})
        list(APPEND paths "${dir}/${swig_name}.h")
      endforeach()
      file(GLOB doc_path ${paths})
      if(doc_path AND "${class}" MATCHES "^itk::")
        # store the header
        list(APPEND ITK_WRAP_DOC_DOXYGEN_HEADERS "${doc_path}")
        # and the produced file
        string(REPLACE "::" "_" base_name "${class}")
        # some simple computations to find the xml file produced for this class
        string(REGEX REPLACE "([A-Z])" "\\1" xmlname ${class})
        string(REGEX REPLACE ":" "_1" xmlname ${xmlname})
        # Converts camel case names to snake case.
        string(REGEX REPLACE "([A-Z])" "_\\1" xmlname ${xmlname})
        string(TOLOWER ${xmlname} xmlname)
        list(
          APPEND
          ITK_WRAP_DOC_DOXYGEN_XML_FILES
          "${CMAKE_CURRENT_BINARY_DIR}/Doc/xml/class${xmlname}.xml"
        )

        # the doxy2swig input
        set(
          ITK_WRAP_DOC_DOXY2SWIG_INPUT
          "${ITK_WRAP_DOC_DOXY2SWIG_INPUT}\n${CMAKE_CURRENT_BINARY_DIR}/Doc/xml/class${xmlname}.xml\t${class}"
        )
        set(ITK_WRAP_DOC_GENERATE_DOXY2SWIG_INPUT ON)
        unset(xmlname)
        unset(doc_path)
        unset(paths)
      endif()
    endif()
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
  # Add a header file to the WRAPPER_INCLUDE_FILES list of files
  # WRAPPER_INCLUDE_FILES is used for both SWIG and CASTXML
  # Global vars modified: WRAPPER_INCLUDE_FILES
  list(FIND WRAPPER_INCLUDE_FILES "${include_file}" _index)
  if(${_index} EQUAL -1)
    # include order IS important. Default values must be before the other ones
    list(APPEND WRAPPER_INCLUDE_FILES ${include_file})
  endif()
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
      string(
        REGEX
        REPLACE
        "${sharp_regexp}"
        "\\2"
        template_parameters
        "${wrap}"
      )
      itk_wrap_one_type(
              "${WRAPPER_WRAP_METHOD}"
              "${WRAPPER_CLASS}"
              "${WRAPPER_SWIG_NAME}${mangled_suffix}"
              "${template_parameters}"
      )
    endforeach()
    unset(template_parameters)
    unset(mangled_suffix)
    unset(sharp_regexp)
  else()
    if(WRAPPER_WARN_ABOUT_NO_TEMPLATE AND NOT EXTERNAL_WRAP_ITK_PROJECT)
      # display a warning if the class is empty
      message(
        "Warning: No template declared for ${WRAPPER_CLASS}. Perhaps you should turn on more WRAP_* options?"
      )
    endif()
  endif()
endmacro()

macro(itk_wrap_simple_type wrap_class swig_name)
  # Add a typedef, without support for any option
  # CASTXML_TYPEDEFS Is input and output modified by this function
  # CASTXML_FORCE_INSTANTIATE is input and output modified by this function

  set(
    CASTXML_TYPEDEFS
    "${CASTXML_TYPEDEFS}    using ${swig_name}=${wrap_class};\n"
  )
  set(
    CASTXML_FORCE_INSTANTIATE
    "${CASTXML_FORCE_INSTANTIATE}    (void)sizeof(${swig_name});\n"
  )

  # split the class name and the template parameters
  if("${wrap_class}" MATCHES "<.*>")
    string(
      REGEX
      REPLACE
      "^([^<]+)< *(.+) *>([^>]*)$"
      "\\1"
      cpp_name
      "${wrap_class}"
    )
    string(
      REGEX
      REPLACE
      "^([^<]+)< *(.+) *>([^>]*)$"
      "\\2"
      template_params
      "${wrap_class}"
    )
    string(
      REGEX
      REPLACE
      "^([^<]+)< *(.+) *>([^>]*)$"
      "\\3"
      ext_def
      "${wrap_class}"
    )
  else()
    set(cpp_name "${wrap_class}")
    set(template_params NO_TEMPLATE)
    set(ext_def "")
  endif()
  string(REGEX REPLACE ".*::" "" simple_name "${cpp_name}")

  # must be done first so the typemap are used in the %template commands
  if("${swig_name}" MATCHES "_Pointer$")
    string(REGEX REPLACE "_Pointer$" "" smart_pointed "${swig_name}")
    string(
      REGEX
      REPLACE
      "(.)([A-Z][a-z]+)"
      "\\1_\\2"
      snake_name
      "${simple_name}"
    )
    string(
      REGEX
      REPLACE
      "([a-z0-9])([A-Z])"
      "\\1_\\2"
      snake_name
      "${snake_name}"
    )
    string(REGEX REPLACE "__" "_" snake_name "${snake_name}")
    string(TOLOWER "${snake_name}" snake_name)
    add_python_pointer_typemap("${smart_pointed}" ${simple_name} ${snake_name})
    unset(snake_name)
    unset(smart_pointed)
    unset(simple_name)
  endif()

  # and now, generate the typemaps and other customizations
  if("${cpp_name}" STREQUAL "itk::Matrix")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "DECL_PYTHON_ITK_MATRIX(${swig_name})\n"
    )
  endif()

  if("${cpp_name}" STREQUAL "std::complex")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "DECL_PYTHON_STD_COMPLEX_CLASS(${swig_name})\n"
    )
  endif()

  if("${swig_name}" STREQUAL "itkLightObject")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "%template(listitkLightObject) std::list< itkLightObject_Pointer >;\n\n"
    )
    add_python_config_template(
          "list"
          "std::list"
          "listitkLightObject"
          "itk::LightObject"
    )
  endif()

  if("${cpp_name}" STREQUAL "itk::VariableLengthVector")
    if(NOT ("${template_params}" MATCHES "std::complex")) # TODO cover complex types
      string(
        APPEND
        ITK_WRAP_PYTHON_SWIG_EXT
        "DECL_PYTHON_VARIABLELENGTHVECTOR_CLASS(${swig_name}, ${template_params})\n"
      )
    endif()
  endif()

  if("${swig_name}" STREQUAL "itkObject")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "DECL_PYTHON_OBJECT_CLASS(${swig_name})\n"
    )
  endif()

  if("${swig_name}" STREQUAL "itkProcessObject")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "DECL_PYTHON_PROCESSOBJECT_CLASS(${swig_name})\n\n"
    )
  endif()

  if("${swig_name}" STREQUAL "itkDataObject")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "%template(vectoritkDataObject) std::vector< itkDataObject_Pointer >;\n"
    )
    add_python_config_template(
          "vector"
          "std::vector"
          "vectoritkDataObject"
          "itk::DataObject"
    )
  endif()

  if("${swig_name}" STREQUAL "itkObjectFactoryBase")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "%template(listitkObjectFactoryBase) std::list< itkObjectFactoryBase * >;\n"
    )
    add_python_config_template(
          "list"
          "std::list"
          "listitkObjectFactoryBase"
          "itk::ObjectFactoryBase"
    )
  endif()

  if("${swig_name}" STREQUAL "itkMetaDataDictionary")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "%template(vectoritkMetaDataDictionary) std::vector< itkMetaDataDictionary * >;\n"
    )
    add_python_config_template(
          "vector"
          "std::vector"
          "vectoritkMetaDataDictionary"
          "itk::MetaDataDictionary"
    )
  endif()

  if("${swig_name}" STREQUAL "itkCommand")
    # make itk::Command hineritable in python
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "%feature(\"director\") itkCommand;\n"
    )
  endif()

  if(
    "${cpp_name}"
      STREQUAL
      "itk::ImageBase"
    AND
      NOT
        "${swig_name}"
          MATCHES
          "Pointer$"
  )
    # add the templated method non seen by gccxml, in a more python-friendly way
    # than the c++ version
    add_python_output_return_by_value_class("${swig_name}" "GetBufferedRegion")
    add_python_output_return_by_value_class("${swig_name}" "GetLargestPossibleRegion")
    add_python_output_return_by_value_class("${swig_name}" "GetRequestedRegion")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "DECL_PYTHON_IMAGEBASE_CLASS(${swig_name}, ${template_params})\n"
    )
    string(APPEND ITK_WRAP_PYTHON_SWIG_EXT "%inline %{\n")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "#include \"itkContinuousIndexSwigInterface.h\"\n"
    )
    string(APPEND ITK_WRAP_PYTHON_SWIG_EXT "%}\n")
  endif()

  if("${cpp_name}" STREQUAL "itk::Image")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "DECL_PYTHON_IMAGE_CLASS(${swig_name})\n\n"
    )
  endif()

  if("${cpp_name}" STREQUAL "itk::PointSetBase")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "DECL_PYTHON_POINTSETBASE_CLASS(${swig_name})\n\n"
    )
  endif()

  if("${cpp_name}" STREQUAL "itk::PointSet")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "DECL_PYTHON_POINTSET_CLASS(${swig_name})\n\n"
    )
  endif()

  if("${cpp_name}" STREQUAL "itk::PolyLineParametricPath")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "DECL_PYTHON_POLYLINEPARAMETRICPATH_CLASS(${swig_name})\n\n"
    )
  endif()

  if("${cpp_name}" STREQUAL "itk::Mesh")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "DECL_PYTHON_MESH_CLASS(${swig_name})\n\n"
    )
  endif()

  if("${cpp_name}" STREQUAL "itk::TransformBaseTemplate")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "DECL_PYTHON_TRANSFORMBASETEMPLATE_CLASS(${swig_name})\n\n"
    )
  endif()

  if(
    "${cpp_name}"
      STREQUAL
      "itk::PyImageFilter"
    AND
      NOT
        "${swig_name}"
          MATCHES
          "Pointer$"
  )
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "DECL_PYIMAGEFILTER_CLASS(${swig_name})\n\n"
    )
  endif()

  if(
    "${cpp_name}"
      STREQUAL
      "itk::StatisticsLabelObject"
    AND
      NOT
        "${swig_name}"
          MATCHES
          "Pointer$"
  )
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "%template(map${swig_name}) std::map< unsigned long, ${swig_name}_Pointer, std::less< unsigned long > >;\n"
    )
    add_python_config_template(
          "map"
          "std::map"
          "map${swig_name}"
          "unsigned long, ${cpp_name}< ${template_params} >"
    )
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "%template(vector${swig_name}) std::vector< ${swig_name}_Pointer >;\n"
    )
    add_python_config_template(
          "vector"
          "std::vector"
          "vector${swig_name}"
          "${cpp_name}< ${template_params} >"
    )
  endif()

  if(
    "${cpp_name}"
      STREQUAL
      "itk::LabelMap"
    AND
      NOT
        "${swig_name}"
          MATCHES
          "Pointer$"
  )
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "DECL_PYTHON_LABELMAP_CLASS(${swig_name})\n"
    )
  endif()

  if("${cpp_name}" STREQUAL "itk::ComponentTreeNode")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "%template(list${swig_name}) std::list< ${swig_name}* >;\n"
    )
    add_python_config_template(
          "list"
          "std::list"
          "list${swig_name}"
          "${cpp_name}< ${template_params} > *"
    )
  endif()

  if("${cpp_name}" STREQUAL "itk::ImageRegion")
    add_python_output_return_by_value_class("${swig_name}" "GetIndex")
    add_python_output_return_by_value_class("${swig_name}" "GetModifiableIndex")
    add_python_output_return_by_value_class("${swig_name}" "GetSize")
    add_python_output_return_by_value_class("${swig_name}" "GetModifiableSize")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "DECL_PYTHON_IMAGEREGION_CLASS(${swig_name})%template(vector${swig_name}) std::vector< ${swig_name} >;\n"
    )
    add_python_config_template(
          "vector"
          "std::vector"
          "vector${swig_name}"
          "${cpp_name}< ${template_params} >"
    )
  endif()

  if(
    "${cpp_name}"
      STREQUAL
      "itk::Image"
    AND
      NOT
        "${swig_name}"
          MATCHES
          "Pointer$"
  )
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "DECL_PYTHON_STD_VEC_RAW_TO_SMARTPTR_TYPEMAP(${swig_name}, ${swig_name}_Pointer)\n"
    )
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "%template(vector${swig_name}) std::vector< ${swig_name}_Pointer >;\n"
    )
    add_python_config_template(
          "vector"
          "std::vector"
          "vector${swig_name}"
          "${cpp_name}< ${template_params} > "
    )
  endif()

  if("${cpp_name}" STREQUAL "itk::Index")
    add_python_seq_typemap("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::Size")
    add_python_seq_typemap("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::RGBPixel")
    # number of elements is not in the template parameters so use the
    # macro which get it with Size() instead
    add_python_variable_length_seq_typemap("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::RGBAPixel")
    # number of elements is not in the template parameters so use the
    # macro which get it with Size() instead
    add_python_variable_length_seq_typemap("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::Offset")
    add_python_seq_typemap("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::FixedArray")
    add_python_vec_typemap("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::Vector")
    add_python_vec_typemap("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::CovariantVector")
    add_python_vec_typemap("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::Point")
    add_python_vec_typemap("${swig_name}" "${template_params}")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "%template(vector${swig_name}) std::vector< ${swig_name} >;\n"
    )
    add_python_config_template(
          "vector"
          "std::vector"
          "vector${swig_name}"
          "${cpp_name}< ${template_params} >"
    )
  endif()

  if("${cpp_name}" STREQUAL "itk::ContinuousIndex")
    add_python_vec_typemap("${swig_name}" "${template_params}")
  endif()

  if("${cpp_name}" STREQUAL "itk::Array")
    add_python_variable_length_seq_typemap("${swig_name}" "${template_params}")
  endif()

  if(
    "${cpp_name}"
      STREQUAL
      "itk::TransformBaseTemplate"
    AND
      NOT
        "${ext_def}"
          MATCHES
          "Pointer"
  )
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "%template(list${swig_name}_Pointer) std::list< ${swig_name}_Pointer >;\n"
    )
    add_python_config_template(
          "list"
          "std::list"
          "list${swig_name}_Pointer"
          "${cpp_name}< ${template_params} >"
    )
  endif()

  if("${cpp_name}" STREQUAL "itk::SpatialObjectPoint")
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "DECL_PYTHON_SPATIALOBJECTPPOINT_CLASS(${swig_name})%template(vector${swig_name}) std::vector< ${swig_name} >;\n"
    )
    add_python_config_template(
          "vector"
          "std::vector"
          "vector${swig_name}"
          "${cpp_name}< ${template_params} >"
    )
  endif()

  foreach(
    sopClassName
    IN
    ITEMS
      "itk::ContourSpatialObjectPoint"
      "itk::DTITubeSpatialObjectPoint"
      "itk::LineSpatialObjectPoint"
      "itk::SurfaceSpatialObjectPoint"
      "itk::TubeSpatialObjectPoint"
  )
    if("${cpp_name}" STREQUAL "${sopClassName}")
      string(
        APPEND
        ITK_WRAP_PYTHON_SWIG_EXT
        "%template(vector${swig_name}) std::vector< ${swig_name} >;\n"
      )
      add_python_config_template(
              "vector"
              "std::vector"
              "vector${swig_name}"
              "${cpp_name}< ${template_params} >"
      )
    endif()
  endforeach()

  if(
    "${cpp_name}"
      STREQUAL
      "itk::SpatialObject"
    AND
      NOT
        "${ext_def}"
          MATCHES
          "Pointer"
  )
    string(
      APPEND
      ITK_WRAP_PYTHON_SWIG_EXT
      "%template(list${swig_name}_Pointer) std::list< ${swig_name}_Pointer >;\n"
    )
    add_python_config_template(
          "list"
          "std::list"
          "list${swig_name}_Pointer"
          "${cpp_name}< ${template_params} >"
    )
  endif()

  foreach(
    soClassName
    IN
    ITEMS
      "itk::ArrowSpatialObjectPoint"
      "itk::BlogSpatialObject"
      "itk::BoxSpatialObject"
      "itk::ContourSpatialObject"
      "itk::EllipseSpatialObject"
      "itk::GaussianSpatialObject"
      "itk::GroupSpatialObject"
      "itk::ImageMaskSpatialObject"
      "itk::ImageSpatialObject"
      "itk::LandmarkSpatialObject"
      "itk::LineSpatialObject"
      "itk::PointBasedSpatialObject"
      "itk::PolygonSpatialObject"
      "itk::SurfaceSpatialObject"
      "itk::TubeSpatialObject"
  )
    if(
      "${cpp_name}"
        STREQUAL
        "${soClassName}"
      AND
        NOT
          "${ext_def}"
            MATCHES
            "Pointer"
    )
      string(
        APPEND
        ITK_WRAP_PYTHON_SWIG_EXT
        "%template(list${swig_name}_Pointer) std::list< ${swig_name}_Pointer >;\n"
      )
      add_python_config_template(
              "list"
              "std::list"
              "list${swig_name}_Pointer"
              "${cpp_name}< ${template_params} >"
      )
    endif()
  endforeach()
  unset(ext_def)
  unset(template_params)
  unset(cpp_name)
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

  set(template_parameters "${ARGV3}")
  if(template_parameters)
    set(full_class_name "${wrap_class}< ${template_parameters} >")
  else()
    set(full_class_name "${wrap_class}")
  endif()

  # Add a typedef for the class. We have this funny looking full_name::base_name
  # thing (it expands to, for example "typedef itk::Foo<baz, 2>::Foo") used
  # for gccxml typedefs

  if("${wrap_method}" MATCHES "2_SUPERCLASSES")
    itk_wrap_simple_type_swig_interface("${full_class_name}::Superclass::Superclass"
                                            "${swig_name}_Superclass_Superclass"
    )
    itk_wrap_simple_type_swig_interface("${full_class_name}::Superclass::Superclass::Pointer"
                                            "${swig_name}_Superclass_Superclass_Pointer"
    )
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
  if(${module_prefix}_WRAP_DOC)
    if(ITK_WRAP_DOC_GENERATE_DOXY2SWIG_INPUT)
      set(
        ITK_WRAP_DOC_DOXY2SWIG_INPUT
        "${ITK_WRAP_DOC_DOXY2SWIG_INPUT}\t${swig_name}"
      )
    endif()
  endif()

  if(
    NOT
      "${wrap_class}"
        STREQUAL
        "MetaEvent"
    AND
      NOT
        "${wrap_method}"
          MATCHES
          "ENUM"
  )
    add_python_config_template(
          "${base_name}"
          "${wrap_class}"
          "${swig_name}"
          "${template_parameters}"
    )
  endif()
  unset(template_parameters)

  # Add a typedef for the class. We have this funny looking full_name::base_name
  # thing (it expands to, for example "typedef itk::Foo<baz, 2>::Foo"), to
  # trick castxml into creating code for the class. If we left off the trailing
  # base_name, then castxml wouldn't see the typedef as a class instantiation,
  # and thus wouldn't create XML for any of the methods, etc.

  if("${wrap_method}" MATCHES "2_SUPERCLASSES")
    itk_wrap_simple_type("${full_class_name}::Superclass::Superclass" "${swig_name}_Superclass_Superclass")
    itk_wrap_simple_type("${full_class_name}::Superclass::Superclass::Pointer"
                             "${swig_name}_Superclass_Superclass_Pointer"
    )
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
  unset(full_class_name)
  unset(base_name)
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
  itk_wrap_one_type(
      "${WRAPPER_WRAP_METHOD}"
      "${WRAPPER_CLASS}"
      "${WRAPPER_SWIG_NAME}${name}"
      "${types}"
  )

  if("${ITK_WRAP_PYTHON_CURRENT_CLASS}" STREQUAL "itk::ImageSource")
    # generate the typemap which let pass an ImageSource instead of an Image
    set(image_source "${ITK_WRAP_PYTHON_CURRENT_SWIG_NAME}${name}")
    set(image "${ITKN_${name}}")
    # An empty value for ${ITKN_${name}} means that the mangled type ${name}
    # was not requested when wrapping ITK. But we want to allow external
    # modules to redefine those missing types if they use it internally.
    if(image STREQUAL "")
      # Replace the mangled type I with itkImage
      string(REPLACE "I" "itkImage" imageTemplate ${name})
      set(image ${imageTemplate})
    endif()

    set(text "\n\n")
    set(text "${text}%typemap(in) ${image} * {\n")
    #    set(text "${text}  // ======================\n")
    set(text "${text}  ${image_source} * imgsrc;\n")
    set(text "${text}  ${image} * img;\n")
    set(
      text
      "${text}  if($input != Py_None && SWIG_ConvertPtr($input,(void **)(&imgsrc),\$descriptor(${image_source} *), 0) == 0)\n"
    )
    set(text "${text}    {\n")
    set(text "${text}    \$1 = imgsrc->GetOutput(0);\n")
    set(text "${text}    }\n")
    set(
      text
      "${text}  else if(SWIG_ConvertPtr($input,(void **)(&img),\$descriptor(${image} *), 0) == 0)\n"
    )
    set(text "${text}    {\n")
    set(text "${text}    \$1 = img;\n")
    set(text "${text}    }\n")
    set(text "${text}  else\n")
    set(text "${text}    {\n")
    set(
      text
      "${text}    PyErr_SetString(PyExc_TypeError, \"Expecting argument of type ${image} or ${image_source}.\");\n"
    )
    set(text "${text}    SWIG_fail;\n")
    set(text "${text}    }\n")
    set(text "${text}}\n")
    set(text "${text}\n")
    set(text "${text}\n")
    set(text "${text}%typemap(typecheck) ${image} * {\n")
    #    set(text "${text}  // //////////////////////////\n")
    set(text "${text}  ${image_source} * imgsrc;\n")
    set(text "${text}  ${image} * img;\n")
    set(
      text
      "${text}  if($input != Py_None && SWIG_ConvertPtr($input,(void **)(&imgsrc),\$descriptor(${image_source} *), 0) == 0)\n"
    )
    set(text "${text}    {\n")
    set(text "${text}    \$1 = 1;\n")
    set(text "${text}    }\n")
    set(
      text
      "${text}  else if(SWIG_ConvertPtr($input,(void **)(&img),\$descriptor(${image} *), 0) == 0)\n"
    )
    set(text "${text}    {\n")
    set(text "${text}    \$1 = 1;\n")
    set(text "${text}    }\n")
    set(text "${text}  else\n")
    set(text "${text}    {\n")
    set(text "${text}    PyErr_Clear();\n")
    set(text "${text}    \$1 = 0;\n")
    set(text "${text}    }\n")
    set(text "${text}}\n")
    string(APPEND ITK_WRAP_PYTHON_SWIG_EXT "${text}")
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
# condition" to restrict the dimensions that this filter will be instantiated
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

  # First, store the variable args in real variables, not the macro parameters.
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
  decrement(last_arg_number ${ARGC})

  # Now see if we have a dimension condition, and if so, note it and remove it
  # from the list of args that we will process later
  set(have_dim_cond OFF)
  set(last_arg "${arg${last_arg_number}}")
  if("${last_arg}" MATCHES "^[0-9]")
    # We have a dimensionality condition
    set(have_dim_cond ON)
    decrement(last_arg_number ${last_arg_number})
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

  # First, store the variable args in real variables, not the macro parameters.
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
  decrement(last_arg_number ${ARGC})

  set(last_arg "${arg${last_arg_number}}")
  if("${last_arg}" MATCHES "^[0-9]")
    # We have a dimensionality condition
    itk_wrap_filter_dims(dims "${last_arg}")
    decrement(last_arg_number ${last_arg_number})
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
      set(template_parameters "")
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
          message(
            FATAL_ERROR
            "Wrapping ${WRAPPER_CLASS}: No image type for '${type}' pixels is known."
          )
        endif()

        set(template_parameters "${template_parameters}${comma}${image_type}")
        set(mangled_name "${mangled_name}${mangle_type}")
        set(comma ", ") # now add commas after the subsequent template params
      endforeach()
      itk_wrap_template("${mangled_name}" "${template_parameters}")
      unset(template_parameters)
      unset(mangled_name)
      unset(comma)
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
    decrement(max_disallowed ${min_dim})
    set(${var_name} "")
    foreach(d ${ITK_WRAP_IMAGE_DIMS})
      if("${d}" GREATER "${max_disallowed}")
        set(
          ${var_name}
          ${${var_name}}
          ${d}
        )
      endif()
    endforeach()
  else()
    # The condition is just a list of dims. Return the intersection of these
    # dims with the selected ones.
    intersection(${var_name} "${dimension_condition}" "${ITK_WRAP_IMAGE_DIMS}")
  endif()
endmacro()

include(${CMAKE_CURRENT_LIST_DIR}/macro_files/itk_auto_load_submodules.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/macro_files/itk_end_wrap_module.cmake)

#################
function(itk_auto_load_and_end_wrap_submodules)
  # CMake functions introduce new scopes, a variable created or
  # modified inside the function won’t be accessible outside the function.
  #
  # Wrapping these two huge macros in a function makes them
  # operate inside a new scope to prevent polluting the
  # global namespace with changed variables.

  itk_auto_load_submodules()
  itk_end_wrap_module()
endfunction()
