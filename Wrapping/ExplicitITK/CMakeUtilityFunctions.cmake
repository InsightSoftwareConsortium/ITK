################################################################################
# Macro definitions for some simple CMake utility functions.
################################################################################

################################################################################
# Functions for list operations.
################################################################################

macro(SORT var_name list)
  # Sort the given list and store it in var_name.
  set(sort_tmp1 "")
  foreach(l ${list})
    set(sort_inserted 0)
    set(sort_tmp2 "")
    foreach(l1 ${sort_tmp1})
      if("${l}" STRLESS "${l1}" AND ${sort_inserted} EQUAL 0)
        set(sort_tmp2 ${sort_tmp2} "${l}" "${l1}")
        set(sort_inserted 1)
      else("${l}" STRLESS "${l1}" AND ${sort_inserted} EQUAL 0)
        set(sort_tmp2 ${sort_tmp2} "${l1}")
      endif("${l}" STRLESS "${l1}" AND ${sort_inserted} EQUAL 0)
    endforeach(l1)
    if(${sort_inserted} EQUAL 0)
      set(sort_tmp1 ${sort_tmp1} "${l}")
    else(${sort_inserted} EQUAL 0)
      set(sort_tmp1 ${sort_tmp2})
    endif(${sort_inserted} EQUAL 0)
  endforeach(l)
  set(${var_name} ${sort_tmp1})
endmacro(SORT)

macro(UNIQUE var_name list)
  # Make the given list have only one instance of each unique element and
  # store it in var_name.
  set(unique_tmp "")
  foreach(l ${list})
    if(NOT "${unique_tmp}" MATCHES "(^|;)${l}(;|$)")
      set(unique_tmp ${unique_tmp} ${l})
    endif(NOT "${unique_tmp}" MATCHES "(^|;)${l}(;|$)")
  endforeach(l)
  set(${var_name} ${unique_tmp})
endmacro(UNIQUE)

macro(INTERSECTION var_name list1 list2)
  # Store the intersection between the two given lists in var_name.
  set(intersect_tmp "")
  foreach(l ${list1})
    if("${list2}" MATCHES "(^|;)${l}(;|$)")
      set(intersect_tmp ${intersect_tmp} ${l})
    endif("${list2}" MATCHES "(^|;)${l}(;|$)")
  endforeach(l)
  set(${var_name} ${intersect_tmp})
endmacro(INTERSECTION)

macro(FILTER var_name list1 list2)
  # Remove elements in list2 from list1 and store the result in var_name.
  set(filter_tmp "")
  foreach(l ${list1})
    if(NOT "${list2}" MATCHES "(^|;)${l}(;|$)")
      set(filter_tmp ${filter_tmp} ${l})
    endif(NOT "${list2}" MATCHES "(^|;)${l}(;|$)")
  endforeach(l)
  set(${var_name} ${filter_tmp})
endmacro(FILTER)


################################################################################
# Simple arithmetic.
################################################################################

macro(INCREMENT var_name input)
  # Increment the input variable (must be in [0,8]) and store the result in var_name.
  set(${var_name} ${increment${input}})
  if(NOT DEFINED ${var_name})
    message(FATAL_ERROR "Could not increment. Input ${input} out of range 0-8?")
  endif(NOT DEFINED ${var_name})
endmacro(INCREMENT)

macro(DECREMENT var_name input)
  # Decrement the input variable (must be in [1,9]) and store the result in var_name.
  set(${var_name} ${decrement${input}})
  if(NOT DEFINED ${var_name})
    message(FATAL_ERROR "Could not decrement. Input ${input} out of range 1-9?")
  endif(NOT DEFINED ${var_name})
endmacro(DECREMENT)

set(increment0 1)
set(increment1 2)
set(increment2 3)
set(increment3 4)
set(increment4 5)
set(increment5 6)
set(increment6 7)
set(increment7 8)
set(increment8 9)

set(decrement1 0)
set(decrement2 1)
set(decrement3 2)
set(decrement4 3)
set(decrement5 4)
set(decrement6 5)
set(decrement7 6)
set(decrement8 7)
set(decrement9 8)

################################################################################
# Macros to install files at absolute locations.
################################################################################

macro(INSTALL_AT_ABSOLUTE_PATH target path)
  # USAGE:
  # INSTALL_AT_ABSOLUTE_PATH(custom_install_target "/path/to/install" "/path/to/file1" ... "path/to/fileN")
  # where the custom_install_target parameter is the name of a target that has
  # previously been created with CREATE_INSTALL_AT_ABSOLUTE_PATH_TARGET.
  # This macro then installs the listed files in the provided install path

  set(install_file_name "${PROJECT_BINARY_DIR}/${target}.cmake")

  foreach(file ${ARGN})
    get_filename_component(filename "${file}" NAME)
    string(REGEX REPLACE "/$" "" stripped_path "${path}")
    file(APPEND "${install_file_name}"
     "message(STATUS \"Installing ${stripped_path}/${filename}\")\n")
    file(APPEND "${install_file_name}"
     "file(INSTALL DESTINATION \"${path}\" TYPE FILE FILES \"${file}\")\n")
  endforeach(file)
endmacro(INSTALL_AT_ABSOLUTE_PATH)

macro(CREATE_INSTALL_AT_ABSOLUTE_PATH_TARGET target type comment)
  # Creates a target to hang an absolute-path install procedure on.
  # The 'target' parameter is the name of the target. The 'type' parameter
  # must be either 'DEFAULT' or 'ON_DEMAND', where the former means that the
  # files are installed by default when 'make install' is run and the latter
  # means that the files will only be installed when 'make target' is run (where
  # 'target' is replaced with the name of the target that is being created).
  # The 'comment' parameter is a string that will be printed at the start of
  # the install process process.

  set(install_file_name "${PROJECT_BINARY_DIR}/${target}.cmake")
  file(WRITE "${install_file_name}" "message(STATUS \"${comment}\")\n")

  if("${type}" MATCHES "DEFAULT")
    add_custom_target(${target} ALL)
    set_target_properties(${target} PROPERTIES
      POST_INSTALL_SCRIPT "${install_file_name}")
  else("${type}" MATCHES "DEFAULT")
    add_custom_target(${target})
    add_custom_command(TARGET ${target}
      PRE_BUILD
      COMMAND ${CMAKE_COMMAND}
      ARGS -P "${install_file_name}"
      COMMENT "Manual installation of files from target ${target}")
  endif("${type}" MATCHES "DEFAULT")
endmacro(CREATE_INSTALL_AT_ABSOLUTE_PATH_TARGET)
