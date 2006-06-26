################################################################################
# Macro definitions for some simple CMake utility functions.
################################################################################

################################################################################
# Functions for list operations.
################################################################################

MACRO(SORT var_name list)
  # Sort the given list and store it in var_name.
  SET(sort_tmp1 "")
  FOREACH(l ${list})
    SET(sort_inserted 0)
    SET(sort_tmp2 "")
    FOREACH(l1 ${sort_tmp1})
      IF("${l}" STRLESS "${l1}" AND ${sort_inserted} EQUAL 0)
        SET(sort_tmp2 ${sort_tmp2} "${l}" "${l1}")
        SET(sort_inserted 1)
      ELSE("${l}" STRLESS "${l1}" AND ${sort_inserted} EQUAL 0)
        SET(sort_tmp2 ${sort_tmp2} "${l1}")
      ENDIF("${l}" STRLESS "${l1}" AND ${sort_inserted} EQUAL 0)
    ENDFOREACH(l1)
    IF(${sort_inserted} EQUAL 0)
      SET(sort_tmp1 ${sort_tmp1} "${l}")
    ELSE(${sort_inserted} EQUAL 0)
      SET(sort_tmp1 ${sort_tmp2})
    ENDIF(${sort_inserted} EQUAL 0)
  ENDFOREACH(l)
  SET(${var_name} ${sort_tmp1})
ENDMACRO(SORT)

MACRO(UNIQUE var_name list)
  # Make the given list have only one instance of each unique element and
  # store it in var_name.
  SET(unique_tmp "")
  FOREACH(l ${list})
    IF(NOT "${unique_tmp}" MATCHES "(^|;)${l}(;|$)")
      SET(unique_tmp ${unique_tmp} ${l})
    ENDIF(NOT "${unique_tmp}" MATCHES "(^|;)${l}(;|$)")
  ENDFOREACH(l)
  SET(${var_name} ${unique_tmp})
ENDMACRO(UNIQUE)

MACRO(INTERSECTION var_name list1 list2)
  # Store the intersection between the two given lists in var_name.
  SET(intersect_tmp "")
  FOREACH(l ${list1})
    IF("${list2}" MATCHES "(^|;)${l}(;|$)")
      SET(intersect_tmp ${intersect_tmp} ${l})
    ENDIF("${list2}" MATCHES "(^|;)${l}(;|$)")
  ENDFOREACH(l)
  SET(${var_name} ${intersect_tmp})
ENDMACRO(INTERSECTION)

MACRO(FILTER var_name list1 list2)
  # Remove elements in list2 from list1 and store the result in var_name.
  SET(filter_tmp "")
  FOREACH(l ${list1})
    IF(NOT "${list2}" MATCHES "(^|;)${l}(;|$)")
      SET(filter_tmp ${filter_tmp} ${l})
    ENDIF(NOT "${list2}" MATCHES "(^|;)${l}(;|$)")
  ENDFOREACH(l)
  SET(${var_name} ${filter_tmp})
ENDMACRO(FILTER)


################################################################################
# Simple arithmetic.
################################################################################

MACRO(INCREMENT var_name input)
  # Increment the input variable (must be in [0,8]) and store the result in var_name.
  SET(${var_name} ${increment${input}})
  IF(NOT DEFINED ${var_name})
    MESSAGE(FATAL_ERROR "Could not increment. Input ${input} out of range 0-8?")
  ENDIF(NOT DEFINED ${var_name})
ENDMACRO(INCREMENT)

MACRO(DECREMENT var_name input)
  # Decrement the input variable (must be in [1,9]) and store the result in var_name.
  SET(${var_name} ${decrement${input}})
  IF(NOT DEFINED ${var_name})
    MESSAGE(FATAL_ERROR "Could not decrement. Input ${input} out of range 1-9?")
  ENDIF(NOT DEFINED ${var_name})
ENDMACRO(DECREMENT)

SET(increment0 1)
SET(increment1 2)
SET(increment2 3)
SET(increment3 4)
SET(increment4 5)
SET(increment5 6)
SET(increment6 7)
SET(increment7 8)
SET(increment8 9)

SET(decrement1 0)
SET(decrement2 1)
SET(decrement3 2)
SET(decrement4 3)
SET(decrement5 4)
SET(decrement6 5)
SET(decrement7 6)
SET(decrement8 7)
SET(decrement9 8)

################################################################################
# Macros to install files at absolute locations.
################################################################################

MACRO(INSTALL_AT_ABSOLUTE_PATH target path)
  # USAGE: 
  # INSTALL_AT_ABSOLUTE_PATH(custom_install_target "/path/to/install" "/path/to/file1" ... "path/to/fileN")
  # where the custom_install_target parameter is the name of a target that has
  # previously been created with CREATE_INSTALL_AT_ABSOLUTE_PATH_TARGET.
  # This macro then installs the listed files in the provided install path
  
  SET(install_file_name "${PROJECT_BINARY_DIR}/${target}.cmake")
    
  FOREACH(file ${ARGN})
    GET_FILENAME_COMPONENT(filename "${file}" NAME)
    STRING(REGEX REPLACE "/$" "" stripped_path "${path}")
    FILE(APPEND "${install_file_name}" 
     "MESSAGE(STATUS \"Installing ${stripped_path}/${filename}\")\n")
    FILE(APPEND "${install_file_name}" 
     "FILE(INSTALL DESTINATION \"${path}\" TYPE FILE FILES \"${file}\")\n")
  ENDFOREACH(file)
ENDMACRO(INSTALL_AT_ABSOLUTE_PATH)

MACRO(CREATE_INSTALL_AT_ABSOLUTE_PATH_TARGET target type comment)
  # Creates a target to hang an absolute-path install procedure on.
  # The 'target' parameter is the name of the target. The 'type' parameter
  # must be either 'DEFAULT' or 'ON_DEMAND', where the former means that the
  # files are installed by default when 'make install' is run and the latter
  # means that the files will only be installed when 'make target' is run (where
  # 'target' is replaced with the name of the target that is being created).
  # The 'comment' parameter is a string that will be printed at the start of
  # the install process process.
  
  SET(install_file_name "${PROJECT_BINARY_DIR}/${target}.cmake")
  FILE(WRITE "${install_file_name}" "MESSAGE(STATUS \"${comment}\")\n")

  IF("${type}" MATCHES "DEFAULT")
    ADD_CUSTOM_TARGET(${target} ALL)
    SET_TARGET_PROPERTIES(${target} PROPERTIES 
      POST_INSTALL_SCRIPT "${install_file_name}")
  ELSE("${type}" MATCHES "DEFAULT")
    ADD_CUSTOM_TARGET(${target})
    ADD_CUSTOM_COMMAND(TARGET ${target}
      PRE_BUILD
      COMMAND ${CMAKE_COMMAND}
      ARGS -P "${install_file_name}"
      COMMENT "Manual installation of files from target ${target}")
  ENDIF("${type}" MATCHES "DEFAULT")
ENDMACRO(CREATE_INSTALL_AT_ABSOLUTE_PATH_TARGET)
