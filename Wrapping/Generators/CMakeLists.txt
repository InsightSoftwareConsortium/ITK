
# Macro to set the module prefix
macro(itk_wrap_modules_set_prefix)
  if(EXTERNAL_WRAP_ITK_PROJECT)
    set(module_prefix ${itk-module})
  else()
    set(module_prefix ITK)
  endif()
endmacro()

###############################################################################
# the macros in charge of dispatching to the language specific macros

macro(itk_wrap_modules_all_generators)
  itk_wrap_modules_set_prefix()
  if(${module_prefix}_WRAP_PYTHON)
    # Wrap PyBase
    if(NOT EXTERNAL_WRAP_ITK_PROJECT)
        add_subdirectory(${ITK_WRAP_PYTHON_SOURCE_DIR}/PyBase)
    endif()
  endif()
endmacro()


macro(itk_wrap_module_all_generators library_name)
  itk_wrap_modules_set_prefix()
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
  itk_wrap_modules_set_prefix()
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

macro(itk_end_wrap_submodule_all_generators module)
  itk_wrap_modules_set_prefix()
  if(${module_prefix}_WRAP_CASTXML)
   # write the wrap_*.cxx file
   #
   # Global vars used: WRAPPER_INCLUDE_FILES WRAPPER_MODULE_NAME and WRAPPER_TYPEDEFS
   # Global vars modified: none

   # Create the cxx file which will be given to castxml.
   set(cxx_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module}.cxx")
   configure_file("${ITK_WRAP_CASTXML_SOURCE_DIR}/wrap_.cxx.in" "${cxx_file}" @ONLY)

   # generate the xml file
   set(xml_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module}.xml")

   set(_castxml_depends)
   if(NOT ITK_USE_SYSTEM_CASTXML)
     # ExternalProject target for CastXML.
     set(_castxml_depends castxml)
   endif()
   set(ccache_cmd)
   if(ITK_USE_CCACHE)
     set(_ccache_cmd ${CCACHE_EXECUTABLE})
   endif()
   set(_castxml_cc_flags ${CMAKE_CXX_FLAGS})
   # Avoid missing omp.h include
   if(CMAKE_CXX_EXTENSIONS)
     set(_castxml_cc_flags "${_castxml_cc_flags} ${CMAKE_CXX14_EXTENSION_COMPILE_OPTION}")
   else()
     set(_castxml_cc_flags "${_castxml_cc_flags} ${CMAKE_CXX14_STANDARD_COMPILE_OPTION}")
   endif()

   # Agressive optimization flags cause cast_xml to give invalid error conditions
   set(INVALID_OPTIMIZATION_FLAGS "-fopenmp;-march=[a-zA-Z0-9\-]*;-mtune=[a-zA-Z0-9\-]*;-mfma")
   foreach( rmmatch ${INVALID_OPTIMIZATION_FLAGS})
     string(REGEX REPLACE ${rmmatch} "" _castxml_cc_flags "${_castxml_cc_flags}")
   endforeach()
   unset(INVALID_OPTIMIZATION_FLAGS)

   separate_arguments(_castxml_cc_flags)
   if(MSVC)
     set(_castxml_cc --castxml-cc-msvc ( "${CMAKE_CXX_COMPILER}" ${_castxml_cc_flags} ) -fexceptions)
     if(MSVC90)
       # needed for VS2008 64 bit
       set(_castxml_cc ${_castxml_cc} "-D_HAS_TR1=0")
     endif()
   else()
     set(_castxml_cc --castxml-cc-gnu ( "${CMAKE_CXX_COMPILER}" ${_castxml_cc_flags} ))
   endif()
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
   set(_hdrs )
   set(glob_hdrs )
   if(EXISTS ${_include})
     file(GLOB_RECURSE glob_hdrs ${_include}/*.h)
   endif()
   foreach(header IN LISTS glob_hdrs)
     get_filename_component(header_name ${header} NAME)
     if(${header_name} IN_LIST WRAPPER_INCLUDE_FILES)
       list(APPEND _hdrs ${header})
     endif()
   endforeach()

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
    itk_end_wrap_submodule_DOC()
  endif()
endmacro()


macro(itk_wrap_named_class_all_generators class swig_name)
  itk_wrap_modules_set_prefix()
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
  itk_wrap_modules_set_prefix()
  if(${module_prefix}_WRAP_CASTXML)
    itk_wrap_include_castxml("${inc}")
  endif()
  if(${module_prefix}_WRAP_SWIGINTERFACE)
    itk_wrap_include_swig_interface("${inc}")
  endif()
endmacro()

macro(itk_wrap_template_all_generators name types)
  itk_wrap_modules_set_prefix()
  if(${module_prefix}_WRAP_PYTHON AND WRAPPER_LIBRARY_PYTHON)
    itk_wrap_template_python("${name}" "${types}")
  endif()
endmacro()


macro(itk_wrap_one_type_all_generators wrap_method wrap_class swig_name template_params)
  itk_wrap_modules_set_prefix()
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
  itk_wrap_modules_set_prefix()
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
itk_wrap_modules_set_prefix()
if(EXTERNAL_WRAP_ITK_PROJECT)
  # generators there have been turned on while building wrapitk
  set(default OFF)
else()
  if(DEFINED ITK_WRAP_GCCXML)
    # Keep ITK_WRAP_GCCXML for backward compatibility.
    set(${module_prefix}_WRAP_CASTXML "${ITK_WRAP_GCCXML}" CACHE STRING "Build xml files.")
  else()
    set(${module_prefix}_WRAP_CASTXML ON CACHE BOOL "Build xml files.")
  endif()
  option(${module_prefix}_WRAP_SWIGINTERFACE "Build swig interfaces." ON)
  set(default OFF)
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
      set(generators ${lang} ${generators})
    else()
      list(APPEND generators ${lang})
    endif()
  endif()
endforeach()

foreach(lang ${generators})
  string(TOUPPER "${lang}" lang_upper)

  # add the language to the options
  if(NOT DEFINED ${module_prefix}_WRAP_${lang_upper})
    option(${module_prefix}_WRAP_${lang_upper} "Build swig ${lang} wrapper support." ${default})
  endif()
  # add the language to the list, if the user have turn it on, and include
  # the subdirectory
  if(${module_prefix}_WRAP_${lang_upper})
    set(WRAP_ITK_GENERATORS ${WRAP_ITK_GENERATORS} "${lang}")
    add_subdirectory("${lang}")
  else()
    set(WRAP_ITK_UNUSED_GENERATORS ${WRAP_ITK_UNUSED_GENERATORS} "${lang}")
  endif()
endforeach()
mark_as_advanced(${module_prefix}_WRAP_DOC)
set(WRAP_ITK_GENERATORS ${WRAP_ITK_GENERATORS} CACHE INTERNAL "Internal generators list.")
set(WRAP_ITK_UNUSED_GENERATORS ${WRAP_ITK_UNUSED_GENERATORS} CACHE INTERNAL "Internal unused generators list.")

# The include_directories are added here to be used by
# Wrapping/ConfigureWrapping.cmake
foreach(lang ${generators})
  string(TOUPPER "${lang}" lang_upper)
  if(${module_prefix}_WRAP_${lang_upper})
    get_directory_property(inc DIRECTORY "${lang}" INCLUDE_DIRECTORIES)
    include_directories(${inc})
  endif()
endforeach()
