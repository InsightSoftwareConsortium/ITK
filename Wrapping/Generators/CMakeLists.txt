###############################################################################
# the macros in charge of dispatching to the language specific macros



###############################################################################
# ccache management. This option is shared by most of generators, so put it here.
cmake_dependent_option(ITK_USE_CCACHE "Use ccache to cache swig/castxml/... output and speedup the rebuild." OFF ITK_WRAP_PYTHON OFF)
mark_as_advanced(ITK_USE_CCACHE)
if(ITK_USE_CCACHE)
  find_program(CCACHE_EXECUTABLE NAMES ccache-swig ccache DOC "ccache executable.")
endif()



if(DEFINED ITK_WRAP_GCCXML)
  message(FATAL_ERROR "ITK_WRAP_GCCXML is deprecated.  Use ${module_prefix}_WRAP_CASTXML instead.")
endif()
###############################################################################
# build the generators list
if(EXTERNAL_WRAP_ITK_PROJECT)
  # generators there have been turned on while building wrapitk
else()
  cmake_dependent_option(${module_prefix}_WRAP_CASTXML       "Build xml files."       ON ITK_WRAP_PYTHON OFF)
  cmake_dependent_option(${module_prefix}_WRAP_SWIGINTERFACE "Build swig interfaces." ON ITK_WRAP_PYTHON OFF)
  mark_as_advanced(${module_prefix}_WRAP_CASTXML ${module_prefix}_WRAP_SWIGINTERFACE)
endif()
if(NOT ${module_prefix}_WRAP_CASTXML)
  message(FATAL_ERROR "${module_prefix}_WRAP_CASTXML is required to be on for wrapping python")
endif()
if(NOT ${module_prefix}_WRAP_SWIGINTERFACE)
  message(FATAL_ERROR "${module_prefix}_WRAP_SWIGINTERFACE is required to be on for wrapping python")
endif()


set(WRAP_ITK_GENERATORS CACHE INTERNAL "Internal generators list.")
set(WRAP_ITK_UNUSED_GENERATORS CACHE INTERNAL "Internal unused generators list.")

# The following list of directories int the "ITK/Wrapping/Generators
set(_candidate_generators "SwigInterface" "Doc" "CastXML" "Python")

foreach(lang ${_candidate_generators})
  string(TOUPPER "${lang}" lang_upper)

  # add the language to the options
  if(NOT DEFINED ${module_prefix}_WRAP_${lang_upper})
    cmake_dependent_option(${module_prefix}_WRAP_${lang_upper} "Build swig ${lang} wrapper support." OFF ITK_WRAP_PYTHON OFF)
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
