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
  itk_wrap_modules_all_generators()
endmacro()

macro(itk_end_wrap_modules)
  itk_end_wrap_modules_all_generators()
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

  # WRAPPER_LIBRARY_OUTPUT_DIR. Directory in which generated cxx, xml, and idx
  # files will be placed.
  set(WRAPPER_LIBRARY_OUTPUT_DIR "${ITK_DIR}/Wrapping")

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
  # WRAPPER_LIBRARY_GROUPS is a deprecated variable for this specification.
  unset(WRAPPER_LIBRARY_GROUPS )

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

  if("${WRAPPER_LIBRARY_itk_wrap_modules_STATUS}" STREQUAL "NOT_EXECUTED")
    itk_wrap_modules()
    # change the status of WRAPPER_LIBRARY_itk_wrap_modules_STATUS, so we can call itk_end_wrap_modules when
    # itk_end_wrap_module will be called
    set(WRAPPER_LIBRARY_itk_wrap_modules_STATUS "EXECUTED_IN_itk_wrap_module" CACHE INTERNAL "status var used to avoid the use of itk_wrap_modules in simple contributions.")
  endif()

  # Call the language support initialization function
  itk_wrap_module_all_generators("${library_name}")

endmacro()


macro(itk_end_wrap_module)
  if("${WRAPPER_LIBRARY_itk_wrap_modules_STATUS}" STREQUAL "EXECUTED_IN_itk_wrap_module")
    itk_end_wrap_modules()
  endif()

  itk_end_wrap_module_all_generators()
endmacro()


macro(INCLUDE_LIBRARY library)
  # TODO: that macro is buggy !!!!
  # it doesn't store the vars where in the lib sub dir

  itk_wrap_module("${library}")
  # change some default values

  # WRAPPER_LIBRARY_SOURCE_DIR. Directory to be scanned for *.wrap files.
  set(WRAPPER_LIBRARY_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/${library}")

  # WRAPPER_LIBRARY_OUTPUT_DIR. Directory in which generated cxx, xml, idx,
  # and mdx files will be placed.
  set(WRAPPER_LIBRARY_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/${library}")

  add_subdirectory("${library}")
  itk_end_wrap_module()
endmacro()


################################################################################
# Macros for finding and processing *.wrap files.
################################################################################

macro(itk_auto_load_submodules)

  # Include the *.wrap files in WRAPPER_LIBRARY_SOURCE_DIR. This causes
  # corresponding wrap_*.cxx files to be generated WRAPPER_LIBRARY_OUTPUT_DIR,
  # and added to the WRAPPER_LIBRARY_SWIG_INPUTS list.
  # In addition, this causes the other required wrap_*.cxx files for the entire
  # library and each wrapper language to be created.
  # Finally, this macro causes the language support files for the templates and
  # library here defined to be created.

  # For backwards compatibility
  if(WRAPPER_LIBRARY_GROUPS)
    set(WRAPPER_SUBMODULE_ORDER ${WRAPPER_LIBRARY_GROUPS})
  endif()

  # Next, include modules already in WRAPPER_SUBMODULE_ORDER, because those are
  # guaranteed to be processed first.
  foreach(module ${WRAPPER_SUBMODULE_ORDER})
    itk_load_submodule("${module}")
  endforeach()

  # Now search for other *.wrap files to include
  file(GLOB wrap_cmake_files "${WRAPPER_LIBRARY_SOURCE_DIR}/*.wrap")
  # sort the list of files so we are sure to always get the same order on all system
  # and for all builds. That's important for several reasons:
  # - the order is important for the order of creation of python template
  # - the typemaps files are always the same, and the rebuild can be avoided
  list(SORT wrap_cmake_files)
  foreach(_file ${wrap_cmake_files})
    # get the module name from module.wrap
    get_filename_component(module "${_file}" NAME_WE)

    # if the module is already in the list, it means that it is already included
    # ... and do not include excluded modules
    set(will_include 1)
    foreach(already_included ${WRAPPER_SUBMODULE_ORDER})
      if("${already_included}" STREQUAL "${module}")
        set(will_include 0)
      endif()
    endforeach()

    if(${will_include})
      # Add the module name to the list. WRITE_MODULE_FILES uses this list
      # to create the master library wrapper file.
      list(APPEND WRAPPER_SUBMODULE_ORDER "${module}")
      itk_load_submodule("${module}")
    endif()
  endforeach()
endmacro()


macro(itk_load_submodule module)
  # include a cmake module file and generate the associated wrap_*.cxx file.
  # This basically sets the global vars that will be added to or modified
  # by the commands in the included *.wrap module.
  #
  # Global vars used: none
  # Global vars modified: WRAPPER_MODULE_NAME WRAPPER_TYPEDEFS
  #                       WRAPPER_INCLUDE_FILES WRAPPER_AUTO_INCLUDE_HEADERS

  itk_wrap_submodule(${module})

  # Now include the file.
  if(EXISTS "${WRAPPER_LIBRARY_SOURCE_DIR}/${module}.wrap")
      include("${WRAPPER_LIBRARY_SOURCE_DIR}/${module}.wrap")
  else()
    # for backward compatibility
    if(EXISTS "${WRAPPER_LIBRARY_SOURCE_DIR}/wrap_${module}.cmake")
        include("${WRAPPER_LIBRARY_SOURCE_DIR}/wrap_${module}.cmake")
    else()
      message(SEND_ERROR "Module ${WRAPPER_LIBRARY_SOURCE_DIR}/${module}.wrap or ${WRAPPER_LIBRARY_SOURCE_DIR}/wrap_${module}.cmake not found.")
    endif()
  endif()

  # Call generator specific macros
  itk_end_wrap_submodule_all_generators("${WRAPPER_MODULE_NAME}")

endmacro()

macro(itk_wrap_submodule module)

  message(STATUS "${WRAPPER_LIBRARY_NAME}: Creating ${module} submodule.")

  # We run into some trouble if there's a module with the same name as the
  # wrapper library. Fix this.
  string(TOUPPER "${module}" upper_module)
  string(TOUPPER "${WRAPPER_LIBRARY_NAME}" upper_lib)
  if("${upper_module}" STREQUAL "${upper_lib}")
    message(FATAL_ERROR "The module ${module} can't have the same name than its library. Note that the names are not case sensitive.")
  endif()

  # preset the vars before include the file
  set(WRAPPER_MODULE_NAME "${module}")

  # call generators specific macros
  itk_wrap_submodule_all_generators("${module}")

  set(WRAPPER_INCLUDE_FILES )
  foreach(inc ${WRAPPER_DEFAULT_INCLUDE})
    itk_wrap_include("${inc}")
  endforeach()
  set(WRAPPER_AUTO_INCLUDE_HEADERS ON)

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

  itk_wrap_class_all_generators("${class}")
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
    set(ok 0)
    foreach(opt POINTER POINTER_WITH_CONST_POINTER POINTER_WITH_SUPERCLASS POINTER_WITH_2_SUPERCLASSES EXPLICIT_SPECIALIZATION POINTER_WITH_EXPLICIT_SPECIALIZATION ENUM AUTOPOINTER)
      if("${opt}" STREQUAL "${WRAPPER_WRAP_METHOD}")
        set(ok 1)
      endif()
    endforeach()
    if(ok EQUAL 0)
      message(SEND_ERROR "itk_wrap_class: Invalid option '${WRAPPER_WRAP_METHOD}'. Possible values are POINTER, POINTER_WITH_CONST_POINTER, POINTER_WITH_SUPERCLASS, POINTER_WITH_2_SUPERCLASSES, EXPLICIT_SPECIALIZATION, POINTER_WITH_EXPLICIT_SPECIALIZATION, ENUM and AUTOPOINTER")
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

  itk_wrap_named_class_all_generators("${class}" "${swig_name}")
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

  itk_wrap_simple_class_all_generators(${class})
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

  itk_wrap_named_simple_class_all_generators("${class}" "${swig_name}")
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
    itk_wrap_include_all_generators("${include_file}")
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
      string(REGEX REPLACE "${sharp_regexp}" "\\2" template_params "${wrap}")
      itk_wrap_one_type("${WRAPPER_WRAP_METHOD}" "${WRAPPER_CLASS}" "${WRAPPER_SWIG_NAME}${mangled_suffix}" "${template_params}")
    endforeach()
  else()
    if(WRAPPER_WARN_ABOUT_NO_TEMPLATE AND NOT EXTERNAL_WRAP_ITK_PROJECT)
      # display a warning if the class is empty
      message("Warning: No template declared for ${WRAPPER_CLASS}. Perhaps you should turn on more WRAP_* options?")
    endif()
  endif()

  itk_end_wrap_class_all_generators()
endmacro()


macro(itk_wrap_simple_type wrap_class swig_name)
  # Add a typedef, without support for any option
  itk_wrap_simple_type_all_generators("${wrap_class}" "${swig_name}")
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
  itk_wrap_template_all_generators("${name}" "${types}")
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
