################################################################################
# Macro definitions for creating proper Explicit Instantiation files
# from wrap_*.cmake files.
# This file includes definitions for the macros to call from a CMakeList file
# to cause wrap_*.cmake files to be turned into H and XX files, and definitions
# for the macros to use in the wrap_*.cmake files themselves to declare that
# certain classes and template instantiations be wrapped.
# Note on convention: variable names in ALL_CAPS are global, and shared between
# macros or between CMake and files that are configured. Variable names in
# lower_case are local to a given macro.
################################################################################

################################################################################
# Macros for finding and processing wrap_*.cmake files.
################################################################################
macro(WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE type)
 set(itk_Wrap_${type}_temp ${itk_Wrap_${type}})
 set(itk_Wrap_${type} ${itk_Wrap_Explicit_${type}})
endmacro(WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE)

macro(WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE type)
 set(itk_Wrap_${type} ${itk_Wrap_${type}_temp})
endmacro(WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE)

macro(WRAPPER_LIBRARY_CREATE_EXPLICIT_INSTANTIATION_FILES library_name)

  # Store the WrapTypes to restore them after generation
  WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE(Image)
  WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE(Offset)
  WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE(Vector)
  WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE(CovariantVector)
  WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE(ContinuousIndex)
  WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE(Array)
  WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE(Array2D)
  WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE(FixedArray)
  WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE(RGBPixel)
  WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE(VectorImage)
  WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE(VariableLengthVector)
  WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE(Point)
  WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE(LevelSetNode)
  WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE(StructuringElement)
  WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE(SpatialObject)

  # Include the wrap_*.cmake files in WRAPPER_LIBRARY_SOURCE_DIR. This causes
  # corresponding wrap_*.cxx files to be generated WRAPPER_LIBRARY_OUTPUT_DIR,
  # and added to the WRAPPER_LIBRARY_CABLESWIG_INPUTS list.
  # In addition, this causes the other required wrap_*.cxx files for the entire
  # library and each wrapper language to be created.
  # Finally, this macro causes the language support files for the templates and
  # library here defined to be created.
  set(WRAPPER_EXPLICIT_LIBRARY_NAME "ITK${library_name}")
  set(WRAPPER_LIBRARY_OUTPUT_DIR "${ITK_BINARY_DIR}/Code/${library_name}/Templates")
  # WRAPPER_LIBRARY_OUTPUT_DIR. Directory in which generated cxx, xml, and idx
  # files will be placed.
  set(WRAPPER_LIBRARY_OUTPUT_DIR "${WRAPPER_LIBRARY_OUTPUT_DIR}")

  include_directories("${ITK_BINARY_DIR}/Code/${library_name}")
  set(WRAPPER_EXPLICIT_INSTANTIATION_SOURCES)

  # Next, include modules already in WRAPPER_LIBRARY_GROUPS, because those are
  # guaranteed to be processed first.
  foreach(module ${WRAPPER_LIBRARY_GROUPS})
    # EXISTS test is to allow groups to be declared in WRAPPER_LIBRARY_GROUPS
    # which aren't represented by cmake files: e.g. groups that are created in
    # custom cableswig cxx inputs stored in WRAPPER_LIBRARY_CABLESWIG_INPUTS.
    if(EXISTS "${WRAPPER_LIBRARY_SOURCE_DIR}/wrap_${module}.cmake")
    if("${module}" STREQUAL "SwigExtras")
    else("${module}" STREQUAL "SwigExtras")
       INCLUDE_EXPLICIT_INSTANTIATION_CMAKE("${module}" "${library_name}")
    endif("${module}" STREQUAL "SwigExtras")
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

    if("${module}" STREQUAL "SwigExtras")
      set(will_include 0)
    endif("${module}" STREQUAL "SwigExtras")

    if(${will_include})
      # Add the module name to the list. WRITE_MODULE_FILES uses this list
      # to create the master library wrapper file.
      set(WRAPPER_LIBRARY_GROUPS ${WRAPPER_LIBRARY_GROUPS} "${module}")
      INCLUDE_EXPLICIT_INSTANTIATION_CMAKE("${module}" "${library_name}")
    endif(${will_include})
  endforeach(file)

  # Restore the current state of the variables for other wrapping
  WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE(Image)
  WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE(Offset)
  WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE(Vector)
  WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE(CovariantVector)
  WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE(ContinuousIndex)
  WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE(Array)
  WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE(Array2D)
  WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE(FixedArray)
  WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE(RGBPixel)
  WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE(VectorImage)
  WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE(VariableLengthVector)
  WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE(Point)
  WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE(LevelSetNode)
  WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE(StructuringElement)
  WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE(SpatialObject)

endmacro(WRAPPER_LIBRARY_CREATE_EXPLICIT_INSTANTIATION_FILES)

macro(INCLUDE_EXPLICIT_INSTANTIATION_CMAKE module library_name)
  # include a cmake module file and generate the associated wrap_*.cxx file.
  # This basically sets the global vars that will be added to or modified
  # by the commands in the included wrap_*.cmake module.
  #
  # Global vars used: none
  # Global vars modified: WRAPPER_MODULE_NAME WRAPPER_TYPEDEFS
  #                       WRAPPER_INCLUDE_FILES WRAPPER_AUTO_INCLUDE_HEADERS
  #                       WRAPPER_DO_NOT_CREATE_CXX

  message(STATUS "${WRAPPER_EXPLICIT_LIBRARY_NAME}: Creating ${module} explicit instantiation.")

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
  set(WRAPPER_EXPLICIT_INSTANTIATION_INCLUDES)
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
    WRITE_EXPLICIT_INSTANTIATION_H_CXX("${module}")
    WRITE_EXPLICIT_INSTANTIATION_FILE("${module}" "${library_name}")
  endif(NOT WRAPPER_DO_NOT_CREATE_CXX)
endmacro(INCLUDE_EXPLICIT_INSTANTIATION_CMAKE)

# Keep a list of files that shouldn't be included
macro(WRAP_NO_INCLUDE type_name)
  set(EXPLICIT_ITK_NO_INCLUDES ${EXPLICIT_ITK_NO_INCLUDES} ${WRAPPER_MODULE_NAME}${type_name})
endmacro(WRAP_NO_INCLUDE module_name)

macro(WRITE_EXPLICIT_INSTANTIATION_H_CXX module_name)

  set(CONFIG_MODULE_NAME "${WRAPPER_MODULE_NAME}")

  foreach(wrap ${WRAPPER_TEMPLATES})
    string(REGEX REPLACE "${sharp_regexp}" "\\1" mangled_suffix "${wrap}")
    string(REGEX REPLACE "${sharp_regexp}" "\\2" template_params "${wrap}")

    string(REGEX REPLACE "itk" "" class_name "${module_name}")

    set(CONFIG_MANGLED_SUFFIX ${mangled_suffix})
    set(CONFIG_TEMPLATE_PARAMETERS ${template_params})
    set(CONFIG_CLASS_NAME ${class_name})

    # Add extra includes if the templates has already been defined
    string(REGEX REPLACE "," ";" template_params_list "${template_params}")
    string(REGEX REPLACE "Templates::" "" template_params_list "${template_params_list}")

    set(CONFIG_EXTRA_INCLUDES "")
    foreach(param ${template_params_list})
      string(REGEX REPLACE " " "" param_nospace "${param}")
      if(${param_nospace} MATCHES "Image*")
        set(CONFIG_EXTRA_INCLUDES "${CONFIG_EXTRA_INCLUDES}#include \"Templates/itk${param_nospace}.h\"\n")
      endif(${param_nospace} MATCHES "Image*")
      if(${param_nospace} MATCHES "Vector*")
        set(CONFIG_EXTRA_INCLUDES "${CONFIG_EXTRA_INCLUDES}#include \"Templates/itk${param_nospace}.h\"\n")
      endif(${param_nospace} MATCHES "Vector*")
      if(${param_nospace} MATCHES "Pixel*")
        set(CONFIG_EXTRA_INCLUDES "${CONFIG_EXTRA_INCLUDES}#include \"Templates/itk${param_nospace}.h\"\n")
      endif(${param_nospace} MATCHES "Pixel*")
      if(${param_nospace} MATCHES "Point*")
        set(CONFIG_EXTRA_INCLUDES "${CONFIG_EXTRA_INCLUDES}#include \"Templates/itk${param_nospace}.h\"\n")
      endif(${param_nospace} MATCHES "Point*")
      if(${param_nospace} MATCHES "Matrix*")
        set(CONFIG_EXTRA_INCLUDES "${CONFIG_EXTRA_INCLUDES}#include \"Templates/itk${param_nospace}.h\"\n")
      endif(${param_nospace} MATCHES "Matrix*")
      if(${param_nospace} MATCHES "FixedArray*")
        set(CONFIG_EXTRA_INCLUDES "${CONFIG_EXTRA_INCLUDES}#include \"Templates/itk${param_nospace}.h\"\n")
      endif(${param_nospace} MATCHES "FixedArray*")
      if(${param_nospace} MATCHES "Neighborhood*")
        set(CONFIG_EXTRA_INCLUDES "${CONFIG_EXTRA_INCLUDES}#include \"Templates/itk${param_nospace}.h\"\n")
      endif(${param_nospace} MATCHES "Neighborhood*")
 if(${param_nospace} MATCHES "TreeContainer*")
        set(CONFIG_EXTRA_INCLUDES "${CONFIG_EXTRA_INCLUDES}#include \"Templates/itk${param_nospace}.h\"\n")
      endif(${param_nospace} MATCHES "TreeContainer*")
  if(${param_nospace} MATCHES "EllipsoidInteriorExteriorSpatialFunction*")
        set(CONFIG_EXTRA_INCLUDES "${CONFIG_EXTRA_INCLUDES}#include \"Templates/itk${param_nospace}.h\"\n")
      endif(${param_nospace} MATCHES "EllipsoidInteriorExteriorSpatialFunction*")
  if(${param_nospace} MATCHES "ZeroFluxNeumann*")
        set(CONFIG_EXTRA_INCLUDES "${CONFIG_EXTRA_INCLUDES}#include \"Templates/itk${param_nospace}.h\"\n")
      endif(${param_nospace} MATCHES "ZeroFluxNeumann*")
 if(${param_nospace} MATCHES "Item*")
        set(CONFIG_EXTRA_INCLUDES "${CONFIG_EXTRA_INCLUDES}#include \"Templates/itk${param_nospace}.h\"\n")
      endif(${param_nospace} MATCHES "Item*")
    endforeach(param ${template_params_list})


    # Create the h file.
    set(h_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module_name}${mangled_suffix}.h")
    configure_file("${EXPLICIT_ITK_CONFIG_DIR}/explicit_.h.in"
      "${h_file}" @ONLY IMMEDIATE)

    # Create the cxx file.
    set(cxx_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module_name}${mangled_suffix}.cxx")

    configure_file("${EXPLICIT_ITK_CONFIG_DIR}/explicit_.cxx.in"
      "${cxx_file}" @ONLY IMMEDIATE)

   set(add_to_include 1)
   foreach(no_include ${EXPLICIT_ITK_NO_INCLUDES})
      set(word ${module_name}${mangled_suffix})
      if(${word} MATCHES ${no_include})
      set(add_to_include 0)
      endif(${word} MATCHES ${no_include})
   endforeach(no_include ${EXPLICIT_ITK_NO_INCLUDES})

    # And add the h file to the list of includes.
    if(add_to_include)
      set(WRAPPER_EXPLICIT_INSTANTIATION_INCLUDES
        ${WRAPPER_EXPLICIT_INSTANTIATION_INCLUDES} "${module_name}${mangled_suffix}.h")
    endif(add_to_include)

    # Add the source
    set(WRAPPER_EXPLICIT_INSTANTIATION_SOURCES
      ${WRAPPER_EXPLICIT_INSTANTIATION_SOURCES} ${cxx_file})
  endforeach(wrap)
endmacro(WRITE_EXPLICIT_INSTANTIATION_H_CXX)

macro(WRITE_EXPLICIT_INSTANTIATION_FILE module_name library_name)

  set(CONFIG_WRAPPER_INCLUDES)
  foreach(include_file ${WRAPPER_EXPLICIT_INSTANTIATION_INCLUDES})
    set(new_include "#include \"${include_file}\"\n")
    set(CONFIG_WRAPPER_INCLUDES ${CONFIG_WRAPPER_INCLUDES}${new_include})
  endforeach(include_file)

  # Create the +-.h file.
  set(file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module_name}+-.h")

  configure_file("${EXPLICIT_ITK_CONFIG_DIR}/explicit_+-.h.in" "${file}" @ONLY IMMEDIATE)

  install_files("${ITK_INSTALL_INCLUDE_DIR}/${library_name}/Templates" FILES "${file}")

endmacro(WRITE_EXPLICIT_INSTANTIATION_FILE)

macro(WRAPPER_LIBRARY_CREATE_EXPLICIT_INSTANTIATION_LIBRARY)
  add_library(${WRAPPER_LIBRARY_NAME}Explicit ${WRAPPER_EXPLICIT_INSTANTIATION_SOURCES})
endmacro(WRAPPER_LIBRARY_CREATE_EXPLICIT_INSTANTIATION_LIBRARY)

macro(WRAPPER_LIBRARY_CREATE_EXPLICIT_INSTANTIATION_SOURCES)
  set(WRAPPER_EXPLICIT_${WRAPPER_LIBRARY_NAME}_SRCS ${WRAPPER_EXPLICIT_INSTANTIATION_SOURCES})
endmacro(WRAPPER_LIBRARY_CREATE_EXPLICIT_INSTANTIATION_SOURCES)
