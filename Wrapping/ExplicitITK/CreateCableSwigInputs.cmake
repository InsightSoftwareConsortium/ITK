################################################################################
# Macro definitions for creating proper CableSwig input files from wrap_*.cmake
# files.
# This file includes definitions for the macros to call from a CMakeList file
# to cause wrap_*.cmake files to be turned into CXX files, and definitions for
# the macros to use in the wrap_*.cmake files themselves to declare that certain
# classes and template instantiations be wrapped.
# Note on convention: variable names in ALL_CAPS are global, and shared between
# macros or between CMake and files that are configured. Variable names in
# lower_case are local to a given macro.
################################################################################


################################################################################
# Macros for finding and processing wrap_*.cmake files.
################################################################################

macro(WRAPPER_LIBRARY_CREATE_WRAP_FILES library_name)

  set(WRAPPER_LIBRARY_OUTPUT_DIR "${ITK_BINARY_DIR}/Code/${library_name}/Templates")
  # Include the wrap_*.cmake files in WRAPPER_LIBRARY_SOURCE_DIR. This causes
  # corresponding wrap_*.cxx files to be generated WRAPPER_LIBRARY_OUTPUT_DIR,
  # and added to the WRAPPER_LIBRARY_CABLESWIG_INPUTS list.
  # In addition, this causes the other required wrap_*.cxx files for the entire
  # library and each wrapper language to be created.
  # Finally, this macro causes the language support files for the templates and
  # library here defined to be created.

  # Next, include modules already in WRAPPER_LIBRARY_GROUPS, because those are
  # guaranteed to be processed first.
  foreach(module ${WRAPPER_LIBRARY_GROUPS})
    # EXISTS test is to allow groups to be declared in WRAPPER_LIBRARY_GROUPS
    # which aren't represented by cmake files: e.g. groups that are created in
    # custom cableswig cxx inputs stored in WRAPPER_LIBRARY_CABLESWIG_INPUTS.
    if(EXISTS "${WRAPPER_LIBRARY_SOURCE_DIR}/wrap_${module}.cmake")
        INCLUDE_WRAP_CMAKE("${module}")
    endif(EXISTS "${WRAPPER_LIBRARY_SOURCE_DIR}/wrap_${module}.cmake")
  endforeach(module)

  # Now search for other wrap_*.cmake files to include
  file(GLOB wrap_cmake_files "${WRAPPER_LIBRARY_SOURCE_DIR}/wrap_*.cmake")
  # sort the list of files so we are sure to always get the same order on all system
  # and for all builds. That's important for several reasons:
  # - the order is important for the order of creation of python template
  # - the typemaps files are always the same, and the rebuild can be avoided
  SORT(sorted_cmake_files "${wrap_cmake_files}")
  foreach(file ${sorted_cmake_files})
    # get the module name from wrap_module.cmake
    get_filename_component(module "${file}" NAME_WE)
    string(REGEX REPLACE "^wrap_" "" module "${module}")

    # if the module is already in the list, it means that it is already included
    # ... and do not include excluded modules
    set(will_include 1)
    foreach(already_included ${WRAPPER_LIBRARY_GROUPS})
      if("${already_included}" STREQUAL "${module}")
        set(will_include 0)
      endif("${already_included}" STREQUAL "${module}")
    endforeach(already_included)

    if(${will_include})
      # Add the module name to the list. WRITE_MODULE_FILES uses this list
      # to create the master library wrapper file.
      set(WRAPPER_LIBRARY_GROUPS ${WRAPPER_LIBRARY_GROUPS} "${module}")
      INCLUDE_WRAP_CMAKE("${module}")
    endif(${will_include})
  endforeach(file)

  WRITE_MODULE_FILES()
endmacro(WRAPPER_LIBRARY_CREATE_WRAP_FILES)

macro(INCLUDE_WRAP_CMAKE module)
  # include a cmake module file and generate the associated wrap_*.cxx file.
  # This basically sets the global vars that will be added to or modified
  # by the commands in the included wrap_*.cmake module.
  #
  # Global vars used: none
  # Global vars modified: WRAPPER_MODULE_NAME WRAPPER_TYPEDEFS
  #                       WRAPPER_INCLUDE_FILES WRAPPER_AUTO_INCLUDE_HEADERS
  #                       WRAPPER_DO_NOT_CREATE_CXX
  message(STATUS "${WRAPPER_LIBRARY_NAME}: Creating ${module} wrappers.")

  # We run into some trouble if there's a module with the same name as the
  # wrapper library. Fix this.
  string(TOUPPER "${module}" upper_module)
  string(TOUPPER "${WRAPPER_LIBRARY_NAME}" upper_lib)
  if("${upper_module}" STREQUAL "${upper_lib}")
    set(module "${module}_module")
  endif("${upper_module}" STREQUAL "${upper_lib}")

  # preset the vars before include the file
  set(WRAPPER_MODULE_NAME "${module}")
  set(WRAPPER_TYPEDEFS)
  set(WRAPPER_INCLUDE_FILES ${WRAPPER_DEFAULT_INCLUDE})
  set(WRAPPER_AUTO_INCLUDE_HEADERS ON)
  set(WRAPPER_DO_NOT_CREATE_CXX OFF)

  # Now include the file.
  include("${WRAPPER_LIBRARY_SOURCE_DIR}/wrap_${module}.cmake")

  # Write the file, inless the included cmake file told us not to.
  # A file might declare WRAPPER_DO_NOT_CREATE_CXX if that cmake file
  # provides a custom wrap_*.cxx file and manually appends it to the
  # WRAPPER_LIBRARY_CABLESWIG_INPUTS list; thus that file would not
  # need or want any cxx file generated.
  if(NOT WRAPPER_DO_NOT_CREATE_CXX)
    WRITE_WRAP_CXX("wrap_${module}.cxx")
  endif(NOT WRAPPER_DO_NOT_CREATE_CXX)
endmacro(INCLUDE_WRAP_CMAKE)


macro(WRITE_WRAP_CXX file_name library_name)
  set(WRAPPER_LIBRARY_OUTPUT_DIR "${ITK_BINARY_DIR}/Code/${library_name}/Templates")
  # write the wrap_*.cxx file
  #
  # Global vars used: WRAPPER_INCLUDE_FILES WRAPPER_MODULE_NAME and WRAPPER_TYPEDEFS
  # Global vars modified: none

  # Create the '#include' statements.
  set(CONFIG_WRAPPER_INCLUDES)
  foreach(inc ${WRAPPER_INCLUDE_FILES})
    if("${inc}" MATCHES "<.*>")
      # if the include file is a <stdlib> include file, don't surround the name with qotes.
      set(include "${inc}")
    else("${inc}" MATCHES "<.*>")
      set(include "\"${inc}\"")
    endif("${inc}" MATCHES "<.*>")
    set(CONFIG_WRAPPER_INCLUDES "${CONFIG_WRAPPER_INCLUDES}#include ${include}\n")
  endforeach(inc)
  set(CONFIG_WRAPPER_MODULE_NAME "${WRAPPER_MODULE_NAME}")
  set(CONFIG_WRAPPER_TYPEDEFS "${WRAPPER_TYPEDEFS}")

  # Create the cxx file.
  set(cxx_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${file_name}")

  configure_file("${EXPLICIT_ITK_CONFIG_DIR}/wrap_.cxx.in"
    "${cxx_file}" @ONLY IMMEDIATE)

  # And add the cxx file to the list of cableswig inputs.
  set(WRAPPER_LIBRARY_CABLESWIG_INPUTS
    ${WRAPPER_LIBRARY_CABLESWIG_INPUTS} "${cxx_file}")
endmacro(WRITE_WRAP_CXX)


################################################################################
# Macros for writing the global module CableSwig inputs which specify all the
# groups to be bundled together into one module.
################################################################################

macro(WRITE_MODULE_FILES)
  # Write the wrap_LIBRARY_NAME.cxx file which specifies all the wrapped groups.

  message(STATUS "${WRAPPER_LIBRARY_NAME}: Creating module wrapper files.")


  set(group_list "")
  foreach(group_name ${WRAPPER_LIBRARY_GROUPS})
    set(group_list "${group_list}    \"${group_name}\",\n")
  endforeach(group_name ${group})
  string(REGEX REPLACE ",\n$" "\n" group_list "${group_list}")

  set(CONFIG_GROUP_LIST "${group_list}")

  # Create the cxx file.
  set(cxx_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/wrap_${WRAPPER_LIBRARY_NAME}.cxx")
  configure_file("${EXPLICIT_ITK_CONFIG_DIR}/wrap_ITK.cxx.in"
    "${cxx_file}" @ONLY IMMEDIATE)


  if(EXPLICIT_ITK_TCL)
    WRITE_MODULE_FOR_LANGUAGE("Tcl")
  endif(EXPLICIT_ITK_TCL)
  if(EXPLICIT_ITK_PYTHON)
    WRITE_MODULE_FOR_LANGUAGE("Python")
  endif(EXPLICIT_ITK_PYTHON)
  if(EXPLICIT_ITK_JAVA)
    WRITE_MODULE_FOR_LANGUAGE("Java")
  endif(EXPLICIT_ITK_JAVA)
  if(EXPLICIT_ITK_PERL)
    WRITE_MODULE_FOR_LANGUAGE("Perl")
  endif(EXPLICIT_ITK_PERL)
endmacro(WRITE_MODULE_FILES)

macro(WRITE_MODULE_FOR_LANGUAGE language)
  # Write the language specific CableSwig input which declares which language is
  # to be used and includes the general module cableswig input.
  set(CONFIG_LANGUAGE "${language}")
  set(CONFIG_MODULE_NAME ${WRAPPER_LIBRARY_NAME})
  string(TOUPPER ${language} CONFIG_UPPER_LANG)

  # Create the cxx file.
  set(cxx_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/wrap_${WRAPPER_LIBRARY_NAME}${language}.cxx")
  configure_file("${EXPLICIT_ITK_CONFIG_DIR}/wrap_ITKLang.cxx.in"
    "${cxx_file}" @ONLY IMMEDIATE)

endmacro(WRITE_MODULE_FOR_LANGUAGE)


################################################################################
# Macros to be used in the wrap_*.cmake files themselves.
# These macros specify that a class is to be wrapped, that certain itk headers
# are to be included, and what specific template instatiations are to be wrapped.
################################################################################

macro(WRAP_CLASS class)
  # Wraps the c++ class 'class'. This parameter must be a fully-qualified c++
  # name.
  # The class will be named in the SWIG wrappers as the top-level namespace
  # concatenated to the base class name. E.g. itk::Image -> itkImage or
  # itk::Statistics::Sample -> itkSample.
  # If the top-level namespace is 'itk' amd WRAPPER_AUTO_INCLUDE_HEADERS is ON
  # then the appropriate itk header for this class will be included. Otherwise
  # WRAP_INCLUDE should be manually called from the wrap_*.cmake file that calls
  # this macro.
  # Lastly, this class takes an optional 'wrap method' parameter. Valid values are:
  # POINTER, POINTER_WITH_SUPERCLASS, DEREF and SELF.
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
  else("${class}" MATCHES "::")
    # no namespaces
    set(swig_name "${class}")
  endif("${class}" MATCHES "::")

  # Call the WRAP_NAMED_CLASS macro, including any optional arguments
  WRAP_NAMED_CLASS("${class}" "${swig_name}" ${ARGN})

  # and include the class's header
  if(WRAPPER_AUTO_INCLUDE_HEADERS)
    WRAP_INCLUDE("${swig_name}.h")
  endif(WRAPPER_AUTO_INCLUDE_HEADERS)
endmacro(WRAP_CLASS)

macro(WRAP_NAMED_CLASS class swig_name)
  # Begin the wrapping of a new templated class. The 'class' parameter is a
  # fully-qualified C++ type name, including the namespace. Between WRAP_CLASS
  # and END_WRAP_CLASS various macros should be called to cause certain template
  # instances to be automatically added to the wrap_*.cxx file. END_WRAP_CLASS
  # actually parses through the template instaces that have been recorded and
  # creates the content of that cxx file. WRAP_NON_TEMPLATE_CLASS should be used
  # to create a definition for a non-templated class. (Note that internally,
  # WRAP_NON_TEMPLATE_CLASS eventually calls this macro. This macro should never
  # be called directly for a non-templated class though.)
  #
  # The second parameter of this macro is the name that the class should be given
  # in SWIG (with template definitions providing additional mangled suffixes to this name)
  #
  # Lastly, this class takes an optional 'wrap method' parameter. Valid values are:
  # POINTER and POINTER_WITH_SUPERCLASS.
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
  endif("${ARGC}" EQUAL 2)

  if("${ARGC}" EQUAL 3)
    set(WRAPPER_WRAP_METHOD "${ARGV2}")
    set(ok 0)
    foreach(opt POINTER POINTER_WITH_SUPERCLASS)
      if("${opt}" STREQUAL "${WRAPPER_WRAP_METHOD}")
        set(ok 1)
      endif("${opt}" STREQUAL "${WRAPPER_WRAP_METHOD}")
    endforeach(opt)
    if(ok EQUAL 0)
      message(SEND_ERROR "WRAP_CLASS: Invalid option '${WRAPPER_WRAP_METHOD}'. Possible values are POINTER and POINTER_WITH_SUPERCLASS")
    endif(ok EQUAL 0)
  endif("${ARGC}" EQUAL 3)

  if("${ARGC}" GREATER 3)
    message(SEND_ERROR "Too many arguments")
  endif("${ARGC}" GREATER 3)

  set(WRAPPER_CLASS "${class}")
  set(WRAPPER_SWIG_NAME "${swig_name}")
  # clear the wrap parameters
  set(WRAPPER_TEMPLATES)
endmacro(WRAP_NAMED_CLASS)

macro(WRAP_NON_TEMPLATE_CLASS class)
  # Similar to WRAP_CLASS in that it generates typedefs for CableSwig input.
  # However, since no templates need to be declared, there's no need for
  # WRAP_CLASS ... (declare templates) .. END_WRAP_CLASS. Instead
  # WRAP_NON_TEMPLATE_CLASS takes care of it all.
  # A fully-qualified 'class' parameter is required as above. The swig name for
  # this class is generated as in WRAP_CLASS.
  # Lastly, this class takes an optional 'wrap method' parameter. Valid values are:
  # POINTER and POINTER_WITH_SUPERCLASS.

  WRAP_CLASS("${class}" ${ARGN})
  ADD_ONE_TYPEDEF("${WRAPPER_WRAP_METHOD}" "${WRAPPER_CLASS}" "${WRAPPER_SWIG_NAME}")
endmacro(WRAP_NON_TEMPLATE_CLASS class)


macro(WRAP_NAMED_NON_TEMPLATE_CLASS class swig_name)
  # Similar to WRAP_NAMED_CLASS in that it generates typedefs for CableSwig input.
  # However, since no templates need to be declared, there's no need for
  # WRAP_CLASS ... (declare templates) .. END_WRAP_CLASS. Instead
  # WRAP_NAMED_NON_TEMPLATE_CLASS takes care of it all.
  # A fully-qualified 'class' parameter is required as above. The swig name for
  # this class is provided by the second parameter.
  # Lastly, this class takes an optional 'wrap method' parameter. Valid values are:
  # POINTER and POINTER_WITH_SUPERCLASS.

  WRAP_NAMED_CLASS("${class}" "${swig_name}" ${ARGN})
  ADD_ONE_TYPEDEF("${WRAPPER_WRAP_METHOD}" "${WRAPPER_CLASS}" "${WRAPPER_SWIG_NAME}")
endmacro(WRAP_NAMED_NON_TEMPLATE_CLASS class)


macro(WRAP_INCLUDE include_file)
  # Add a header file to the list of files to be #included in the final
  # cxx file. This list is actually processed in WRITE_WRAP_CXX.
  #
  # Global vars used: WRAPPER_INCLUDE_FILES
  # Global vars modified: WRAPPER_INCLUDE_FILES
  set(already_included 0)
  foreach(included ${WRAPPER_INCLUDE_FILES})
    if("${include_file}" STREQUAL "${already_included}")
      set(already_included 1)
    endif("${include_file}" STREQUAL "${already_included}")
  endforeach(included)

  if(NOT already_included)
    # include order IS important. Default values must be before the other ones
    set(WRAPPER_INCLUDE_FILES
      ${WRAPPER_INCLUDE_FILES}
      ${include_file}
    )
  endif(NOT already_included)
endmacro(WRAP_INCLUDE)

macro(END_WRAP_CLASS)
  # Parse through the list of WRAPPER_TEMPLATES set up by the macros at the bottom
  # of this file, turning them into proper C++ type definitions suitable for
  # input to CableSwig. The C++ definitions are stored in WRAPPER_TYPEDEFS.
  #
  # Global vars used: WRAPPER_CLASS WRAPPER_WRAP_METHOD WRAPPER_TEMPLATES WRAPPER_SWIG_NAME
  # Global vars modified: WRAPPER_TYPEDEFS

  # the regexp used to get the values separated by a #
  set(sharp_regexp "([0-9A-Za-z_]*)[ ]*#[ ]*(.*)")
  foreach(wrap ${WRAPPER_TEMPLATES})
    string(REGEX REPLACE "${sharp_regexp}" "\\1" mangled_suffix "${wrap}")
    string(REGEX REPLACE "${sharp_regexp}" "\\2" template_params "${wrap}")
    #ADD_ONE_TYPEDEF("${WRAPPER_WRAP_METHOD}" "${WRAPPER_CLASS}" "${WRAPPER_SWIG_NAME}${mangled_suffix}" "${template_params}")
  endforeach(wrap)
endmacro(END_WRAP_CLASS)

macro(ADD_ONE_TYPEDEF wrap_method wrap_class swig_name)
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

  # Add a typedef for the class. We have this funny looking full_name::base_name
  # thing (it expands to, for example "typedef itk::Foo<baz, 2>::Foo"), to
  # trick gcc_xml into creating code for the class. If we left off the trailing
  # base_name, then gcc_xml wouldn't see the typedef as a class instantiation,
  # and thus wouldn't create XML for any of the methods, etc.
  set(typedefs "typedef ${full_class_name}::${base_name} ${swig_name}")

  if("${wrap_method}" MATCHES "POINTER")
    # add a pointer typedef if we are so asked
    set(typedefs ${typedefs} "typedef ${full_class_name}::Pointer::SmartPointer ${swig_name}_Pointer")
  endif("${wrap_method}" MATCHES "POINTER")

  if("${wrap_method}" MATCHES "SUPERCLASS")
    set(typedefs ${typedefs} "typedef ${full_class_name}::Superclass::Self ${swig_name}_Superclass")
    set(typedefs ${typedefs} "typedef ${full_class_name}::Superclass::Pointer::SmartPointer ${swig_name}_Superclass_Pointer")
  endif("${wrap_method}" MATCHES "SUPERCLASS")

  # insert a blank line to separate the classes
  set(WRAPPER_TYPEDEFS "${WRAPPER_TYPEDEFS}\n")
  foreach(typedef ${typedefs})
    set(WRAPPER_TYPEDEFS "${WRAPPER_TYPEDEFS}      ${typedef};\n")
  endforeach(typedef)

  # Note: if there's no template_parameters set, this will just pass an empty
  # list as the template_params parameter of LANGUAGE_SUPPORT_ADD_CLASS, as required
  # in non-template cases.
  #LANGUAGE_SUPPORT_ADD_CLASS("${base_name}" "${wrap_class}" "${swig_name}" "${template_parameters}")

  #if("${wrap_method}" MATCHES "POINTER")
  #  LANGUAGE_SUPPORT_ADD_CLASS("SmartPointer" "itk::SmartPointer" "${swig_name}_Pointer" "${full_class_name}")
  #endif("${wrap_method}" MATCHES "POINTER")
endmacro(ADD_ONE_TYPEDEF)



################################################################################
# Macros which cause one or more template instantiations to be added to the
# WRAPPER_TEMPLATES list. This list is initialized by the macro WRAP_CLASS above,
# and used by the macro END_WRAP_CLASS to produce the wrap_xxx.cxx files with
# the correct templates. These cxx files serve as the CableSwig inputs.
################################################################################

macro(WRAP_TEMPLATE name types)
  # This is the fundamental macro for adding a template to be wrapped.
  # 'name' is a mangled suffix to be added to the class name (defined in WRAP_CLASS)
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

  set(WRAPPER_TEMPLATES ${WRAPPER_TEMPLATES} "${name} # ${types}")
endmacro(WRAP_TEMPLATE)

###################################
# Macros for wrapping image filters
###################################

# First, a set of convenience macros for wrapping an image filter with all
# user-selected image types of a given class. These macros take a 'param_count'
# parameter which indicates how many template parameters the current image filter
# takes. The parameters are filled with the exact same image type. To wrap image
# filters which take different image types as different template parameters, use
# WRAP_IMAGE_FILTER_TYPES or WRAP_IMAGE_FILTER_COMBINATIONS.
# These macros also take an optional second parameter which is a "dimensionality
# condition" to restrict the dimensions that theis filter will be instantiated
# for. The condition can either be a single number indicating the one dimension
# allowed, a list of dimensions that are allowed (either as a single ;-delimited
# string or just a set of separate parameters), or something of the form "n+"
# (where n is a number) indicating that instantiations are allowed for dimension
# n and above.
#
# E.g., if only EXPLICIT_unsigned_char is selected and 2- and 3-dimensional images
# are selected, then WRAP_IMAGE_FILTER_INT(2)  will create instantiations for
# filter<itk::Image<unsigned char, 2>, itk::Image<unsigned char, 2> >
# and
# filter<itk::Image<unsigned char, 3>, itk::Image<unsigned char, 3> >

macro(WRAP_IMAGE_FILTER_ALL param_count)
  WRAP_IMAGE_FILTER("${EXPLICIT_ITK_ALL}" ${param_count} "${ARGN}")
endmacro(WRAP_IMAGE_FILTER_ALL)

macro(WRAP_IMAGE_FILTER_SCALAR param_count)
  WRAP_IMAGE_FILTER("${EXPLICIT_ITK_SCALAR}" ${param_count} "${ARGN}")
endmacro(WRAP_IMAGE_FILTER_SCALAR)

macro(WRAP_IMAGE_FILTER_VECTOR param_count)
  WRAP_IMAGE_FILTER("${EXPLICIT_ITK_VECTOR}" ${param_count} "${ARGN}")
endmacro(WRAP_IMAGE_FILTER_VECTOR)

macro(WRAP_IMAGE_FILTER_INT param_count)
  WRAP_IMAGE_FILTER("${EXPLICIT_ITK_INT}" ${param_count} "${ARGN}")
endmacro(WRAP_IMAGE_FILTER_INT)

macro(WRAP_IMAGE_FILTER_SIGN_INT param_count)
  WRAP_IMAGE_FILTER("${EXPLICIT_ITK_SIGN_INT}" ${param_count} "${ARGN}")
endmacro(WRAP_IMAGE_FILTER_SIGN_INT)

macro(WRAP_IMAGE_FILTER_REAL param_count)
  WRAP_IMAGE_FILTER("${EXPLICIT_ITK_REAL}" ${param_count} "${ARGN}")
endmacro(WRAP_IMAGE_FILTER_REAL)

macro(WRAP_IMAGE_FILTER_RGB param_count)
  WRAP_IMAGE_FILTER("${EXPLICIT_ITK_RGB}" ${param_count} "${ARGN}")
endmacro(WRAP_IMAGE_FILTER_RGB)

macro(WRAP_IMAGE_FILTER_VECTOR_REAL param_count)
  WRAP_IMAGE_FILTER("${EXPLICIT_ITK_VECTOR_REAL}" ${param_count} "${ARGN}")
endmacro(WRAP_IMAGE_FILTER_VECTOR_REAL)

macro(WRAP_IMAGE_FILTER_COV_VECTOR_REAL param_count)
  WRAP_IMAGE_FILTER("${EXPLICIT_ITK_COV_VECTOR_REAL}" ${param_count} "${ARGN}")
endmacro(WRAP_IMAGE_FILTER_COV_VECTOR_REAL)


macro(WRAP_IMAGE_FILTER param_types param_count)
  # WRAP_IMAGE_FILTER is a more general macro for wrapping image filters that
  # need one or more image parameters of the same type. The first parameter to this
  # macro is a list of image pixel types for which filter instantiations should be
  # created. The second is a 'param_count' parameter which controls how many image
  # template parameters are created (see above). The optional third parameter is
  # a dimensionality condition (see above also).
  #
  # E.g. WRAP_IMAGE_FILTER("${EXPLICIT_ITK_ALL}" 2) will create template instantiations
  # of the filter for every pixel type that the user has selected.

  set(have_dim_cond OFF)
  if("${ARGN}")
    set(have_dim_cond ON)
  endif("${ARGN}")

  foreach(param_type ${param_types})
    set(param_list "")
    foreach(i RANGE 1 ${param_count})
      set(param_list ${param_list} ${param_type})
    endforeach(i)
    if(have_dim_cond)
      WRAP_IMAGE_FILTER_TYPES("${ARGN}" ${param_list})
    else(have_dim_cond)
      WRAP_IMAGE_FILTER_TYPES(${param_list})
    endif(have_dim_cond)
  endforeach(param_type)
endmacro(WRAP_IMAGE_FILTER)

macro(WRAP_IMAGE_FILTER_COMBINATIONS)
  # WRAP_IMAGE_FILTER_COMBINATIONS takes a variable number of parameters. Each
  # parameter is a list of image pixel types. Filter instantiations are created
  # for every combination of different pixel types in different parameters.
  # A dimensionality condition may be optionally specified as the first parameter.
  #
  # E.g. WRAP_IMAGE_FILTER_COMBINATIONS("UC;US" "UC;US") will create:
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
  endif("${last_arg}" MATCHES "^[0-9]")

  # Build up a list of all of the combinations of all of the elements in each
  # argument. Each combinarion is stored as a #-delimited list of pixel types.
  # The #-delimiter is needed because CMake can't store nested lists.
  # Also note the need to check for empty lists and note invalidity if so.
  set(all_args_valid ON)
  if(NOT arg0)
    set(all_args_valid OFF)
  else(NOT arg0)
    set(template_combinations ${arg0})
  endif(NOT arg0)

  foreach(num RANGE 1 ${last_arg_number})
    set(types "${arg${num}}")
    if(NOT types)
      set(all_args_valid OFF)
    else(NOT types)
      set(temp "")
      foreach(type_list ${template_combinations})
        foreach(type ${types})
          set(temp ${temp} "${type_list}#${type}")
        endforeach(type)
      endforeach(type_list)
      set(template_combinations ${temp})
    endif(NOT types)
  endforeach(num)

  if(all_args_valid)
    foreach(param_set ${template_combinations})
      # Each param_set is a #-delimited list of pixel types. First thing, we unpack
      # param_set back to a CMake list (;-delimited). Then we instantiate the filter
      # for that combination of image pixel types.
      string(REPLACE "#" ";" param_list "${param_set}")
      if(have_dim_cond)
        WRAP_IMAGE_FILTER_TYPES(${param_list} "${last_arg}")
      else(have_dim_cond)
        WRAP_IMAGE_FILTER_TYPES(${param_list})
      endif(have_dim_cond)
    endforeach(param_set)
  endif(all_args_valid)
endmacro(WRAP_IMAGE_FILTER_COMBINATIONS)


macro(WRAP_IMAGE_FILTER_TYPES)
  # WRAP_IMAGE_FILTER_TYPES creates template instantiations of the current image
  # filter, for all the selected dimensions (or dimensions that meet the optional
  # dimensionality condition). This macro takes a variable number of arguments,
  # which should correspond to the image pixel types of the images in the filter's
  # template parameter list. The optional dimensionality condition should be
  # placed in the first parameter.

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
    FILTER_DIMS(dim_list "${last_arg}")
    DECREMENT(last_arg_number ${last_arg_number})
  else("${last_arg}" MATCHES "^[0-9]")
    set(dim_list ${EXPLICIT_ITK_DIMS})
  endif("${last_arg}" MATCHES "^[0-9]")

  foreach(d ${dim_list})
    set(template_params "")
    set(mangled_name "")
    set(comma "") # Don't add a comma before the first template param!
    foreach(num RANGE 0 ${last_arg_number})
      set(type "${arg${num}}")
      if("${EXPLICIT_ITK_VECTOR}" MATCHES "(^|;)${type}(;|$)")
        # if the type is a vector type with no dimension specified, make the
        # vector dimension match the image dimension.
        set(type "${type}${d}")
      endif("${EXPLICIT_ITK_VECTOR}" MATCHES "(^|;)${type}(;|$)")
      set(image_type ${ITKT_I${type}${d}})
      set(mangle_type ${ITKM_I${type}${d}})
      if(NOT DEFINED image_type)
        message(FATAL_ERROR "Wrapping ${WRAPPER_CLASS}: No image type for '${type}' pixels is known.")
      endif(NOT DEFINED image_type)

      set(template_params "${template_params}${comma}${image_type}")
      set(mangled_name "${mangled_name}${mangle_type}")
      set(comma ", ") # now add commas after the subsequent template params
    endforeach(num)
    WRAP_TEMPLATE("${mangled_name}" "${template_params}")
  endforeach(d)
endmacro(WRAP_IMAGE_FILTER_TYPES)


macro(FILTER_DIMS var_name dimension_condition)
  # FILTER_DIMS processes a dimension_condition and returns a list of the dimensions
  # that (a) meet the condition, and (b) were selected to be wrapped. Recall
  # that the condition is either a CMake list of dimensions, or a string of the
  # form "n+" where n is a number.

  if("${dimension_condition}" MATCHES "^[0-9]+\\+$")
    # The condition is of the form "n+". Make a list of the
    # selected wrapping dims that are >= that number.
    string(REGEX REPLACE "^([0-9]+)\\+$" "\\1" min_dim "${dimension_condition}")
    DECREMENT(max_disallowed ${min_dim})
    foreach(d ${EXPLICIT_ITK_DIMS})
      if("${d}" GREATER "${max_disallowed}")
        set(${var_name} ${${var_name}} ${d})
      endif("${d}" GREATER "${max_disallowed}")
    endforeach(d)
  else("${dimension_condition}" MATCHES "^[0-9]+\\+$")
    # The condition is just a list of dims. Return the intersection of these
    # dims with the selected ones.
    INTERSECTION(${var_name} "${dimension_condition}" "${EXPLICIT_ITK_DIMS}")
  endif("${dimension_condition}" MATCHES "^[0-9]+\\+$")
endmacro(FILTER_DIMS)
