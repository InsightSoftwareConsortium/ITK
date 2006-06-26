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
MACRO(WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE type)
 SET(itk_Wrap_${type}_temp ${itk_Wrap_${type}})
 SET(itk_Wrap_${type} ${itk_Wrap_Explicit_${type}})
ENDMACRO(WRAPPER_LIBRARY_STORE_CURRENT_WRAP_TYPE)

MACRO(WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE type)
 SET(itk_Wrap_${type} ${itk_Wrap_${type}_temp})
ENDMACRO(WRAPPER_LIBRARY_RESTORE_CURRENT_WRAP_TYPE)

MACRO(WRAPPER_LIBRARY_CREATE_EXPLICIT_INSTANTIATION_FILES libName)
  
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
  SET(WRAPPER_EXPLICIT_LIBRARY_NAME ${libName})
  SET(WRAPPER_EXPLICIT_INSTANTIATION_SOURCES)
  
  # Next, include modules already in WRAPPER_LIBRARY_GROUPS, because those are
  # guaranteed to be processed first.
  FOREACH(module ${WRAPPER_LIBRARY_GROUPS})
    # EXISTS test is to allow groups to be declared in WRAPPER_LIBRARY_GROUPS
    # which aren't represented by cmake files: e.g. groups that are created in
    # custom cableswig cxx inputs stored in WRAPPER_LIBRARY_CABLESWIG_INPUTS.
    IF(EXISTS "${WRAPPER_LIBRARY_SOURCE_DIR}/wrap_${module}.cmake")
    IF("${module}" STREQUAL "SwigExtras")
    ELSE("${module}" STREQUAL "SwigExtras")
       INCLUDE_EXPLICIT_INSTANTIATION_CMAKE("${module}")
    ENDIF("${module}" STREQUAL "SwigExtras")
    ENDIF(EXISTS "${WRAPPER_LIBRARY_SOURCE_DIR}/wrap_${module}.cmake")
  ENDFOREACH(module)

  # Now search for other wrap_*.cmake files to include
  FILE(GLOB wrap_cmake_files "${WRAPPER_LIBRARY_SOURCE_DIR}/wrap_*.cmake")
  # sort the list of files so we are sure to always get the same order on all system
  # and for all builds. That's important for several reasons:
  # - the order is important for the order of creation of python template
  # - the typemaps files are always the same, and the rebuild can be avoided
  SORT(sorted_cmake_files "${wrap_cmake_files}")
  FOREACH(file ${sorted_cmake_files})  
    # get the module name from wrap_module.cmake
    GET_FILENAME_COMPONENT(module "${file}" NAME_WE)
    STRING(REGEX REPLACE "^wrap_" "" module "${module}")

    # if the module is already in the list, it means that it is already included
    # ... and do not include excluded modules
    SET(will_include 1)
    FOREACH(already_included ${WRAPPER_LIBRARY_GROUPS})
      IF("${already_included}" STREQUAL "${module}")
        SET(will_include 0)
      ENDIF("${already_included}" STREQUAL "${module}")
    ENDFOREACH(already_included)

    IF("${module}" STREQUAL "SwigExtras")
      SET(will_include 0)
    ENDIF("${module}" STREQUAL "SwigExtras")

    IF(${will_include})
      # Add the module name to the list. WRITE_MODULE_FILES uses this list
      # to create the master library wrapper file.
      SET(WRAPPER_LIBRARY_GROUPS ${WRAPPER_LIBRARY_GROUPS} "${module}")
      INCLUDE_EXPLICIT_INSTANTIATION_CMAKE("${module}")
    ENDIF(${will_include})
  ENDFOREACH(file)

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

ENDMACRO(WRAPPER_LIBRARY_CREATE_EXPLICIT_INSTANTIATION_FILES)

MACRO(INCLUDE_EXPLICIT_INSTANTIATION_CMAKE module)
  # include a cmake module file and generate the associated wrap_*.cxx file.
  # This basically sets the global vars that will be added to or modified
  # by the commands in the included wrap_*.cmake module.
  #
  # Global vars used: none
  # Global vars modified: WRAPPER_MODULE_NAME WRAPPER_TYPEDEFS
  #                       WRAPPER_INCLUDE_FILES WRAPPER_AUTO_INCLUDE_HEADERS
  #                       WRAPPER_DO_NOT_CREATE_CXX

  MESSAGE(STATUS "${WRAPPER_EXPLICIT_LIBRARY_NAME}: Creating ${module} explicit instantiation.")

  # We run into some trouble if there's a module with the same name as the
  # wrapper library. Fix this.
  STRING(TOUPPER "${module}" upper_module)
  STRING(TOUPPER "${WRAPPER_LIBRARY_NAME}" upper_lib)
  IF("${upper_module}" STREQUAL "${upper_lib}")
    SET(module "${module}_module")
  ENDIF("${upper_module}" STREQUAL "${upper_lib}")
 
  # preset the vars before include the file
  SET(WRAPPER_MODULE_NAME "${module}")
  SET(WRAPPER_TYPEDEFS)
  SET(WRAPPER_EXPLICIT_INSTANTIATION_INCLUDES)
  SET(WRAPPER_INCLUDE_FILES ${WRAPPER_DEFAULT_INCLUDE})
  SET(WRAPPER_AUTO_INCLUDE_HEADERS ON)
  SET(WRAPPER_DO_NOT_CREATE_CXX OFF)

  # Now include the file.
  INCLUDE("${WRAPPER_LIBRARY_SOURCE_DIR}/wrap_${module}.cmake")

  # Write the file, inless the included cmake file told us not to.
  # A file might declare WRAPPER_DO_NOT_CREATE_CXX if that cmake file
  # provides a custom wrap_*.cxx file and manually appends it to the 
  # WRAPPER_LIBRARY_CABLESWIG_INPUTS list; thus that file would not
  # need or want any cxx file generated.
  IF(NOT WRAPPER_DO_NOT_CREATE_CXX)
    WRITE_EXPLICIT_INSTANTIATION_H_CXX("${module}")
    WRITE_EXPLICIT_INSTANTIATION_FILE("${module}")
  ENDIF(NOT WRAPPER_DO_NOT_CREATE_CXX)
ENDMACRO(INCLUDE_EXPLICIT_INSTANTIATION_CMAKE)

MACRO(WRITE_EXPLICIT_INSTANTIATION_H_CXX module_name)

  SET(CONFIG_MODULE_NAME "${WRAPPER_MODULE_NAME}")

  FOREACH(wrap ${WRAPPER_TEMPLATES})
    STRING(REGEX REPLACE "${sharp_regexp}" "\\1" mangled_suffix "${wrap}")
    STRING(REGEX REPLACE "${sharp_regexp}" "\\2" template_params "${wrap}")

    STRING(REGEX REPLACE "itk" "" class_name "${module_name}")

    SET(CONFIG_MANGLED_SUFFIX ${mangled_suffix})
    SET(CONFIG_TEMPLATE_PARAMETERS ${template_params})
    SET(CONFIG_CLASS_NAME ${class_name})

    # Add extra includes if the templates has already been defined
    STRING(REGEX REPLACE "," ";" template_params_list "${template_params}")
    STRING(REGEX REPLACE "Templates::" "" template_params_list "${template_params_list}")
    
    SET(CONFIG_EXTRA_INCLUDES "")
    FOREACH(param ${template_params_list})
      STRING(REGEX REPLACE " " "" param_nospace "${param}")
      IF(${param_nospace} MATCHES "Image*")
        SET(CONFIG_EXTRA_INCLUDES "${CONFIG_EXTRA_INCLUDES}#include \"itk${param_nospace}.h\"\n")
      ENDIF(${param_nospace} MATCHES "Image*")
      IF(${param_nospace} MATCHES "Vector*")
        SET(CONFIG_EXTRA_INCLUDES "${CONFIG_EXTRA_INCLUDES}#include \"itk${param_nospace}.h\"\n")
      ENDIF(${param_nospace} MATCHES "Vector*")
      IF(${param_nospace} MATCHES "Pixel*")
        SET(CONFIG_EXTRA_INCLUDES "${CONFIG_EXTRA_INCLUDES}#include \"itk${param_nospace}.h\"\n")
      ENDIF(${param_nospace} MATCHES "Pixel*")
    ENDFOREACH(param ${template_params_list})
   

    # Create the h file.
    SET(h_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module_name}${mangled_suffix}.h")

    CONFIGURE_FILE("${EXPLICIT_ITK_CONFIG_DIR}/explicit_.h.in"
      "${h_file}" @ONLY IMMEDIATE)

    # Create the cxx file.
    SET(cxx_file "${WRAPPER_LIBRARY_OUTPUT_DIR}/${module_name}${mangled_suffix}.cxx")

    CONFIGURE_FILE("${EXPLICIT_ITK_CONFIG_DIR}/explicit_.cxx.in"
      "${cxx_file}" @ONLY IMMEDIATE)

    # And add the h file to the list of includes.
    SET(WRAPPER_EXPLICIT_INSTANTIATION_INCLUDES
      ${WRAPPER_EXPLICIT_INSTANTIATION_INCLUDES} "${h_file}")
    SET(WRAPPER_EXPLICIT_INSTANTIATION_SOURCES
      ${WRAPPER_EXPLICIT_INSTANTIATION_SOURCES} ${cxx_file})
  ENDFOREACH(wrap)
ENDMACRO(WRITE_EXPLICIT_INSTANTIATION_H_CXX)

MACRO(WRITE_EXPLICIT_INSTANTIATION_FILE module_name)
  
  SET(CONFIG_WRAPPER_INCLUDES)
  FOREACH(include_file ${WRAPPER_EXPLICIT_INSTANTIATION_INCLUDES})
    SET(new_include "#include \"${include_file}\"\n")  
    SET(CONFIG_WRAPPER_INCLUDES ${CONFIG_WRAPPER_INCLUDES}${new_include})
  ENDFOREACH(include_file)

  # Create the +-.h file.
  SET(file "${EXPLICIT_TEMPLATES_OUTPUT_DIR}/${module_name}+-.h")

  CONFIGURE_FILE("${EXPLICIT_ITK_CONFIG_DIR}/explicit_+-.h.in"
      "${file}" @ONLY IMMEDIATE)
  
ENDMACRO(WRITE_EXPLICIT_INSTANTIATION_FILE)

MACRO(WRAPPER_LIBRARY_CREATE_EXPLICIT_INSTANTIATION_LIBRARY)
  ADD_LIBRARY(${WRAPPER_LIBRARY_NAME}Explicit ${WRAPPER_EXPLICIT_INSTANTIATION_SOURCES})
ENDMACRO(WRAPPER_LIBRARY_CREATE_EXPLICIT_INSTANTIATION_LIBRARY)
