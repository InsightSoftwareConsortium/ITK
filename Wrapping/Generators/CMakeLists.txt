###############################################################################
# the macros in charge of dispatching to the language specific macros

macro(itk_wrap_module_all_generators library_name)
  if(${module_prefix}_WRAP_CASTXML)
    itk_wrap_module_castxml("${library_name}")
  endif()
  if(${module_prefix}_WRAP_SWIGINTERFACE)
    itk_wrap_module_swig_interface("${library_name}")
  endif()
  if(${module_prefix}_WRAP_DOC)
    itk_wrap_module_DOC("${library_name}")
  endif()
  if(${module_prefix}_WRAP_PYTHON AND WRAPPER_LIBRARY_PYTHON)
    itk_wrap_module_python("${library_name}")
  endif()
endmacro()


macro(itk_wrap_submodule_all_generators module)
  if(${module_prefix}_WRAP_CASTXML)
    itk_wrap_submodule_castxml("${module}")
  endif()
  if(${module_prefix}_WRAP_SWIGINTERFACE)
    itk_wrap_submodule_swig_interface("${module}")
  endif()
  if(${module_prefix}_WRAP_DOC)
    itk_wrap_submodule_DOC("${module}")
  endif()
  if(${module_prefix}_WRAP_PYTHON AND WRAPPER_LIBRARY_PYTHON)
    itk_wrap_submodule_python("${module}" "${WRAPPER_LIBRARY_NAME}")
  endif()
endmacro()


macro(itk_wrap_named_class_all_generators class swig_name)
  if(${module_prefix}_WRAP_PYTHON AND WRAPPER_LIBRARY_PYTHON)
    itk_wrap_named_class_python("${class}" "${swig_name}")
  endif()
  if(${module_prefix}_WRAP_DOC)
    itk_wrap_named_class_DOC("${class}" "${swig_name}")
  endif()
endmacro()

macro(itk_end_wrap_class_all_generators)
endmacro()


macro(itk_wrap_simple_class_all_generators class)
endmacro()


macro(itk_wrap_named_simple_class_all_generators class)
endmacro()


macro(itk_wrap_include_all_generators inc)
  if(${module_prefix}_WRAP_CASTXML)
    itk_wrap_include_castxml("${inc}")
  endif()
  if(${module_prefix}_WRAP_SWIGINTERFACE)
    itk_wrap_include_swig_interface("${inc}")
  endif()
endmacro()

macro(itk_wrap_template_all_generators name types)
  if(${module_prefix}_WRAP_PYTHON AND WRAPPER_LIBRARY_PYTHON)
    itk_wrap_template_python("${name}" "${types}")
  endif()
endmacro()


macro(itk_wrap_one_type_all_generators wrap_method wrap_class swig_name template_params)
  if(${module_prefix}_WRAP_CASTXML)
    itk_wrap_one_type_castxml("${wrap_method}" "${wrap_class}" "${swig_name}" "${template_params}")
  endif()
  if(${module_prefix}_WRAP_SWIGINTERFACE)
    itk_wrap_one_type_swig_interface("${wrap_method}" "${wrap_class}" "${swig_name}" "${template_params}")
  endif()
  if(${module_prefix}_WRAP_DOC)
    itk_wrap_one_type_DOC("${wrap_method}" "${wrap_class}" "${swig_name}" "${template_params}")
  endif()
  if(${module_prefix}_WRAP_PYTHON AND WRAPPER_LIBRARY_PYTHON)
    itk_wrap_one_type_python("${wrap_method}" "${wrap_class}" "${swig_name}" "${template_params}")
  endif()
endmacro()


macro(itk_wrap_simple_type_all_generators wrap_class swig_name)
  if(${module_prefix}_WRAP_CASTXML)
    itk_wrap_simple_type_castxml("${wrap_class}" "${swig_name}")
  endif()
  if(${module_prefix}_WRAP_PYTHON AND WRAPPER_LIBRARY_PYTHON)
    itk_wrap_simple_type_python("${wrap_class}" "${swig_name}")
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
file(GLOB local_files *)
# filter and reorder the list
set(generators )
foreach(f ${local_files})
  file(RELATIVE_PATH lang "${CMAKE_CURRENT_SOURCE_DIR}" "${f}")
  if(IS_DIRECTORY "${f}")
    if("${lang}" MATCHES "^CastXML$|^SwigInterface$|^Doc$")
      # put those generators before the others
      list(PREPEND generators ${lang})
    else()
      list(APPEND generators ${lang})
    endif()
  endif()
endforeach()

foreach(lang ${generators})
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
foreach(lang ${generators})
  string(TOUPPER "${lang}" lang_upper)
  if(${module_prefix}_WRAP_${lang_upper})
    get_directory_property(inc DIRECTORY "${lang}" INCLUDE_DIRECTORIES)
    include_directories(${inc})
  endif()
endforeach()
