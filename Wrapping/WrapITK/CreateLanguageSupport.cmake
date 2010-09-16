################################################################################
# Macros to collect information about wrapped types and module dependencies
# so that support files for given languages can be created to automatically
# load the modules in the right order and provide support for looking up
# templated types.
# Currently only Python support is implemented.
################################################################################


macro(LANGUAGE_SUPPORT_INITIALIZE)
  # Re-set the WRAPPED_CLASSES variable used to collect class and template data.
  # This variable holds a list of strings of the one of two forms:
  #
  # "simple name # c++ name # swig name # c++ template parameters"
  # where 'simple name' is the name the class should have in the wrapped code
  # (e.g. drop the itk), 'c++ name' is the name of the templated class in c++
  # (not including the template parameters!), 'swig name' is the name of this
  # particular template instantiation in the swig wrappers (e.g. itkImageF2),
  # and 'c++ template parameters' are the raw text between the template angle
  # brackets (e.g. the ... in itk::Image<...>) for this template instantiation.
  #
  # or "simple name # c++ name # swig name # NO_TEMPLATE"
  # where simple name is the same as above, 'c++ name' is the name of the class
  # in c++, and 'swig name' is the name of the class in the swig wrappers.
  #
  # Also re-set the WRAPPER_TYPEMAPS variable, which holds the text of SWIG
  # typemaps generated for smart pointers wrapped in this library.

  set(WRAPPED_CLASSES)
  set(WRAPPER_TYPEMAPS)
endmacro(LANGUAGE_SUPPORT_INITIALIZE)


macro(LANGUAGE_SUPPORT_CONFIGURE_FILES)
  # Create the various files to make it easier to use the ITK wrappers, especially
  # with reference to the multitude of templates.
  # Currently, only Python is supported.

  message(STATUS "${WRAPPER_LIBRARY_NAME}: Creating language support files.")

  CONFIGURE_TYPEMAPS("${WRAPPER_SWIG_LIBRARY_OUTPUT_DIR}")

  if(WRAP_ITK_PYTHON AND WRAPPER_LIBRARY_PYTHON)
    PYTHON_SUPPORT_CONFIGURE_FILES()
  endif(WRAP_ITK_PYTHON AND WRAPPER_LIBRARY_PYTHON)

  if(WRAP_ITK_TCL AND WRAPPER_LIBRARY_TCL)
    TCL_SUPPORT_CONFIGURE_FILES()
  endif(WRAP_ITK_TCL AND WRAPPER_LIBRARY_TCL)

  if(WRAP_ITK_JAVA AND WRAPPER_LIBRARY_JAVA)
    JAVA_SUPPORT_CONFIGURE_FILES()
  endif(WRAP_ITK_JAVA AND WRAPPER_LIBRARY_JAVA)

endmacro(LANGUAGE_SUPPORT_CONFIGURE_FILES)


macro(LANGUAGE_SUPPORT_ADD_CLASS simple_name cpp_name swig_name template_parameters)
  # Add the template definitions to the WRAPPED_CLASSES list,
  # where 'simple_name' is the name the class should have in the wrapped code
  # (e.g. drop the itk), 'cpp_name' is the name of the templated class in c++
  # (not including the template parameters!), 'swig_name' is the name of this
  # particular template instantiation in the swig wrappers (e.g. itkImageF2)
  # or just the base name if this isn't a templated class,  and
  # 'template_params' are the raw text between the template angle brackets
  # (e.g. the ... in itk::Image<...>) for this template instantiation.
  # Leave template params empty (e.g. "") for non-template classes.
  #
  # We use this data to create our string of the form
  # "simple name # c++ name # swig name # c++ template parameters"
  # or "simple name # c++ name # swig name # NO_TEMPLATE"
  # as required above.

  # the var passed in parameters of the macro can't be modified, so use a different name
  if("${template_parameters}" STREQUAL "")
    set(template_params "NO_TEMPLATE")
  else("${template_parameters}" STREQUAL "")
    set(template_params "${template_parameters}")
  endif("${template_parameters}" STREQUAL "")

  set(WRAPPED_CLASSES ${WRAPPED_CLASSES} "${simple_name} # ${cpp_name} # ${swig_name} # ${template_params}")
  ADD_TYPEMAP("${simple_name}" "${cpp_name}" "${swig_name}" "${template_params}")
endmacro(LANGUAGE_SUPPORT_ADD_CLASS)


macro(ADD_TYPEMAP simple_name cpp_name swig_name template_params)
  if(WRAP_ITK_PYTHON AND WRAPPER_LIBRARY_PYTHON)
    ADD_PYTHON_TYPEMAP("${simple_name}" "${cpp_name}" "${swig_name}" "${template_params}")
  endif(WRAP_ITK_PYTHON AND WRAPPER_LIBRARY_PYTHON)

  if(WRAP_ITK_TCL AND WRAPPER_LIBRARY_TCL)
    ADD_TCL_TYPEMAP("${simple_name}" "${cpp_name}" "${swig_name}" "${template_params}")
  endif(WRAP_ITK_TCL AND WRAPPER_LIBRARY_TCL)

  if(WRAP_ITK_JAVA AND WRAPPER_LIBRARY_JAVA)
    ADD_JAVA_TYPEMAP("${simple_name}" "${cpp_name}" "${swig_name}" "${template_params}")
  endif(WRAP_ITK_JAVA AND WRAPPER_LIBRARY_JAVA)
endmacro(ADD_TYPEMAP)





macro(CONFIGURE_TYPEMAPS outdir)
  set(CONFIG_TYPEMAP_TEXT "${WRAPPER_TYPEMAPS}")
  configure_file("${WRAP_ITK_CONFIG_DIR}/typemaps.swg.in"
    "${outdir}/${WRAPPER_LIBRARY_NAME}.swg"
    @ONLY IMMEDIATE)
   WRAP_ITK_INSTALL("/SWIG" "${outdir}/${WRAPPER_LIBRARY_NAME}.swg")
endmacro(CONFIGURE_TYPEMAPS)





# load language specific code in language specific dir.

include(${WRAP_ITK_CMAKE_DIR}/Python/CreateLanguageSupport.cmake)
include(${WRAP_ITK_CMAKE_DIR}/Java/CreateLanguageSupport.cmake)
include(${WRAP_ITK_CMAKE_DIR}/Tcl/CreateLanguageSupport.cmake)


