################################################################################
# Macro definitions for some simple CMake utility functions.
################################################################################

################################################################################
# Functions for list operations.
################################################################################

macro(UNIQUE var_name list)
  # Make the given list have only one instance of each unique element and
  # store it in var_name.
  set(${var_name} ${list})
  list(REMOVE_DUPLICATES ${var_name})
endmacro()

macro(INTERSECTION var_name list1 list2)
  # Store the intersection between the two given lists in var_name.
  set(intersect_tmp "")
  foreach(l ${list1})
    if("${list2}" MATCHES "(^|;)${l}(;|$)")
      list(APPEND intersect_tmp ${l})
    endif()
  endforeach()
  set(${var_name} ${intersect_tmp})
endmacro()

macro(REMOVE var_name list1 list2)
  # Remove elements in list2 from list1 and store the result in var_name.
  set(${var_name} ${list1})
  list(REMOVE_ITEM ${var_name} ${list2})
endmacro()


################################################################################
# Simple arithmetic.
################################################################################

macro(INCREMENT var_name input)
  # Increment the input variable and store the result in var_name.
  math(EXPR ${var_name} "${input} + 1")
endmacro()

macro(DECREMENT var_name input)
  # Decrement the input variable and store the result in var_name.
  math(EXPR ${var_name} "${input} - 1")
endmacro()
