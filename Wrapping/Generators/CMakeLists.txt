###############################################################################
# the macros in charge of dispatching to the language specific macros


macro(itk_wrap_one_type_all_generators wrap_method wrap_class swig_name template_params)
  if(${module_prefix}_WRAP_CASTXML)
    # insert a blank line to separate the classes
    set(CASTXML_TYPEDEFS "${CASTXML_TYPEDEFS}\n")
    # add a piece of code for type instantiation
    set(CASTXML_FORCE_INSTANTIATE "${CASTXML_FORCE_INSTANTIATE}\n")
  endif()
  if(${module_prefix}_WRAP_SWIGINTERFACE)
    itk_wrap_one_type_swig_interface("${wrap_method}" "${wrap_class}" "${swig_name}" "${template_params}")
  endif()
  if(${module_prefix}_WRAP_DOC)
    if(ITK_WRAP_DOC_GENERATE_DOXY2SWIG_INPUT)
      set(ITK_WRAP_DOC_DOXY2SWIG_INPUT "${ITK_WRAP_DOC_DOXY2SWIG_INPUT}\t${swig_name}")
    endif()
  endif()
  if(${module_prefix}_WRAP_PYTHON AND WRAPPER_LIBRARY_PYTHON)
    string(REGEX REPLACE "(.*::)" "" base_name "${wrap_class}")

    if(NOT "${wrap_class}" STREQUAL "MetaEvent" AND NOT "${wrap_method}" MATCHES "ENUM")
      ADD_PYTHON_CONFIG_TEMPLATE("${base_name}" "${wrap_class}" "${swig_name}" "${template_params}")
    endif()
  endif()
endmacro()


###############################################################################
# ccache management. This option is shared by most of generators, so put it here.
option(ITK_USE_CCACHE "Use ccache to cache swig/castxml/... output and speedup the rebuild." OFF)
mark_as_advanced(ITK_USE_CCACHE)
if(ITK_USE_CCACHE)
  find_program(CCACHE_EXECUTABLE NAMES ccache-swig ccache DOC "ccache executable.")
endif()



###############################################################################
# build the generators list
if(EXTERNAL_WRAP_ITK_PROJECT)
  # generators there have been turned on while building wrapitk
else()
  if(DEFINED ITK_WRAP_GCCXML)
    # Keep ITK_WRAP_GCCXML for backward compatibility.
    set(${module_prefix}_WRAP_CASTXML "${ITK_WRAP_GCCXML}" CACHE STRING "Build xml files.")
  else()
    set(${module_prefix}_WRAP_CASTXML ON CACHE BOOL "Build xml files.")
  endif()
  option(${module_prefix}_WRAP_SWIGINTERFACE "Build swig interfaces." ON)
  mark_as_advanced(${module_prefix}_WRAP_CASTXML ${module_prefix}_WRAP_SWIGINTERFACE)
endif()

set(WRAP_ITK_GENERATORS CACHE INTERNAL "Internal generators list.")
set(WRAP_ITK_UNUSED_GENERATORS CACHE INTERNAL "Internal unused generators list.")

# The following list of directories int the "ITK/Wrapping/Generators
set(_candidate_generators "SwigInterface" "Doc" "CastXML" "Python")

foreach(lang ${_candidate_generators})
  string(TOUPPER "${lang}" lang_upper)

  # add the language to the options
  if(NOT DEFINED ${module_prefix}_WRAP_${lang_upper})
    option(${module_prefix}_WRAP_${lang_upper} "Build swig ${lang} wrapper support." OFF)
  endif()
  # add the language to the list, if the user have turn it on, and include
  # the subdirectory
  if(${module_prefix}_WRAP_${lang_upper})
    list(APPEND WRAP_ITK_GENERATORS "${lang}")
    add_subdirectory("${lang}")
  else()
    list(APPEND WRAP_ITK_UNUSED_GENERATORS "${lang}")
  endif()
endforeach()
mark_as_advanced(${module_prefix}_WRAP_DOC)
set(WRAP_ITK_GENERATORS ${WRAP_ITK_GENERATORS} CACHE INTERNAL "Internal generators list.")
set(WRAP_ITK_UNUSED_GENERATORS ${WRAP_ITK_UNUSED_GENERATORS} CACHE INTERNAL "Internal unused generators list.")

# The include_directories are added here to be used by Wrapping/ConfigureWrapping.cmake
foreach(lang ${_candidate_generators})
  string(TOUPPER "${lang}" lang_upper)
  if(${module_prefix}_WRAP_${lang_upper})
    get_directory_property(inc DIRECTORY "${lang}" INCLUDE_DIRECTORIES)
    include_directories(${inc})
  endif()
endforeach()
unset(_candidate_generators)
