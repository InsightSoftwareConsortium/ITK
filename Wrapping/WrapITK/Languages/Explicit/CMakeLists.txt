###############################################################################
# install the files requires for explicit
if(NOT EXPLICITAL_WRAP_ITK_PROJECT)
  WRAP_ITK_INSTALL(/Configuration/Languages/Explicit CMakeLists.txt)
  WRAP_ITK_INSTALL(/Configuration/Languages/Explicit explicit.in)
endif(NOT EXPLICITAL_WRAP_ITK_PROJECT)


###############################################################################
# hide cable swig dir in explicital projects - it is safer to use the same as the
# one used to build wrapitk
if(EXPLICITAL_WRAP_ITK_PROJECT)
  mark_as_advanced(CableSwig_DIR)
endif(EXPLICITAL_WRAP_ITK_PROJECT)


###############################################################################
# store the current dir, so it can be reused later
set(WRAP_ITK_EXPLICIT_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}" CACHE INTERNAL "explicit source dir")
set(WRAP_ITK_EXPLICIT_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}" CACHE INTERNAL "explicit binary dir")


set(WRAPPER_MASTER_INDEX_OUTPUT_DIR "${PROJECT_BINARY_DIR}/Typedefs")
set(WRAPPER_SWIG_LIBRARY_OUTPUT_DIR "${PROJECT_BINARY_DIR}/SWIG")



macro(WRAP_INCLUDE_EXPLICIT include_file)
  if("${include_file}" MATCHES "<.*>")
    set(EXPLICIT_EXPLICITS "${EXPLICIT_EXPLICITS}#include ${include_file}\n")
  else("${include_file}" MATCHES "<.*>")
    set(EXPLICIT_EXPLICITS "${EXPLICIT_EXPLICITS}#include \"${include_file}\"\n")
  endif("${include_file}" MATCHES "<.*>")
endmacro(WRAP_INCLUDE_EXPLICIT)


macro(ADD_SIMPLE_TYPEDEF_EXPLICIT wrap_class swig_name)
  if("${wrap_class}" MATCHES "<.*>")
    set(EXPLICIT_EXPLICITS "${EXPLICIT_EXPLICITS}extern template class ${wrap_class};\n")
    set(EXPLICIT_INSTANTIATES "${EXPLICIT_INSTANTIATES}template class ${wrap_class};\n")
  endif("${wrap_class}" MATCHES "<.*>")
endmacro(ADD_SIMPLE_TYPEDEF_EXPLICIT)


macro(WRAP_MODULE_EXPLICIT module)
  # clear the typedefs and the includes
  set(EXPLICIT_INSTANTIATES )
endmacro(WRAP_MODULE_EXPLICIT)

macro(END_WRAP_MODULE_EXPLICIT module)
  # Create the cxx file.
  set(file_name "${module}.explicit.cxx")
  set(cxx_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${file_name}")
  set(CONFIG_EXPLICIT_CONTENT "#include \"${WRAPPER_LIBRARY_NAME}.explicit.h\"\n")
  set(CONFIG_EXPLICIT_CONTENT "${CONFIG_EXPLICIT_CONTENT}${EXPLICIT_INSTANTIATES}")
  configure_file("${WRAP_ITK_EXPLICIT_SOURCE_DIR}/explicit.in"
    "${cxx_file}" @ONLY IMMEDIATE)
  set(EXPLICIT_EXPLICITS_FILES ${EXPLICIT_EXPLICITS_FILES} ${cxx_file})
endmacro(END_WRAP_MODULE_EXPLICIT)


macro(ADD_ONE_TYPEDEF_EXPLICIT  wrap_method wrap_class swig_name template_params)
  # insert a blank line to separate the classes
  set(EXPLICIT_EXPLICITS "${EXPLICIT_EXPLICITS}\n")
  set(EXPLICIT_INSTANTIATES "${EXPLICIT_INSTANTIATES}\n")
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

# don't do superclasses for now

#   if("${wrap_method}" MATCHES "2_SUPERCLASSES")
#     ADD_SIMPLE_TYPEDEF_EXPLICIT("${full_class_name}::Superclass::Superclass" "${swig_name}_Superclass_Superclass")
#     ADD_SIMPLE_TYPEDEF_EXPLICIT("${full_class_name}::Superclass::Superclass::Pointer::SmartPointer" "${swig_name}_Superclass_Superclass_Pointer")
#   endif("${wrap_method}" MATCHES "2_SUPERCLASSES")
#
#   if("${wrap_method}" MATCHES "SUPERCLASS")
#     ADD_SIMPLE_TYPEDEF_EXPLICIT("${full_class_name}::Superclass" "${swig_name}_Superclass")
#     ADD_SIMPLE_TYPEDEF_EXPLICIT("${full_class_name}::Superclass::Pointer::SmartPointer" "${swig_name}_Superclass_Pointer")
#   endif("${wrap_method}" MATCHES "SUPERCLASS")

  # the same output with or without FORCE_INSTANTIATE
  ADD_SIMPLE_TYPEDEF_EXPLICIT("${full_class_name}" "${swig_name}")

# don't do pointers for now

#   if("${wrap_method}" MATCHES "POINTER")
#     if("${wrap_method}" STREQUAL "AUTOPOINTER")
#       # add a pointer typedef if we are so asked
#       ADD_SIMPLE_TYPEDEF_EXPLICIT("${full_class_name}::SelfAutoPointer" "${swig_name}_AutoPointer")
#     else("${wrap_method}" STREQUAL "AUTOPOINTER")
#       # add a pointer typedef if we are so asked
#       ADD_SIMPLE_TYPEDEF_EXPLICIT("itk::SmartPointer< ${full_class_name} >" "${swig_name}_Pointer")
#     endif("${wrap_method}" STREQUAL "AUTOPOINTER")
#   endif("${wrap_method}" MATCHES "POINTER")
endmacro(ADD_ONE_TYPEDEF_EXPLICIT)

macro(WRAP_LIBRARY_EXPLICIT library_name)
  set(EXPLICIT_EXPLICITS )
  set(EXPLICIT_EXPLICITS_FILES )
endmacro(WRAP_LIBRARY_EXPLICIT)

macro(END_WRAP_LIBRARY_EXPLICIT)
  set(file_name "${WRAPPER_LIBRARY_NAME}.explicit.h")
  set(cxx_file "${WRAPPER_MASTER_INDEX_OUTPUT_DIR}/${file_name}")
  set(CONFIG_EXPLICIT_CONTENT )
  foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
    set(CONFIG_EXPLICIT_CONTENT "${CONFIG_EXPLICIT_CONTENT}#include \"${dep}.explicit.h\"\n")
  endforeach(dep)
  set(CONFIG_EXPLICIT_CONTENT "${CONFIG_EXPLICIT_CONTENT}${EXPLICIT_EXPLICITS}")
  configure_file("${WRAP_ITK_EXPLICIT_SOURCE_DIR}/explicit.in"
    "${cxx_file}" @ONLY IMMEDIATE)
   WRAP_ITK_INSTALL(/Configuration/Typedefs "${cxx_file}")
  if(EXPLICIT_EXPLICITS_FILES)
    add_library(${WRAPPER_LIBRARY_NAME}Explicit SHARED ${EXPLICIT_EXPLICITS_FILES})
    foreach(dep ${WRAPPER_LIBRARY_DEPENDS})
      target_link_libraries(${WRAPPER_LIBRARY_NAME}Explicit ${dep}Explicit)
    endforeach(dep)
    target_link_libraries(${WRAPPER_LIBRARY_NAME}Explicit ${WRAPPER_LIBRARY_LINK_LIBRARIES} )

    install(TARGETS ${WRAPPER_LIBRARY_NAME}Explicit DESTINATION "${WRAP_ITK_INSTALL_PREFIX}/lib")
  endif(EXPLICIT_EXPLICITS_FILES)
endmacro(END_WRAP_LIBRARY_EXPLICIT)
