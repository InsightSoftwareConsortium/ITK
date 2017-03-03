# When adding new configuration tests that cache results, please make
# sure they honour VXL_UPDATE_CONFIGURATION: if this variable is ON,
# redo the test and update the cached value.

#
# The tests results will be cached. Allow the user to re-run the tests
# if necessary This flag will be reset to "OFF" every time, so that
# the tests are typically just run once. Set it to ON to refresh your
# machine's configuration if you believe the test results are stale or
# wrong.
#
option( VXL_UPDATE_CONFIGURATION "Re-run the configuration tests to update cached results?" "OFF" )
mark_as_advanced( VXL_UPDATE_CONFIGURATION )

# The serial number below will allow the maintainers to force builds
# to update cached results. Whenever you make a change that makes it
# necessary for cached values to be updated, increment the serial
# number. The format follows a DNS-style numbering: the current date
# followed by a modification count within the day.
#
set( VXL_CONFIG_SERIAL_CURRENT "2014-12-09-002" )

if( ${VXL_CONFIG_SERIAL_CURRENT} MATCHES "^${VXL_CONFIG_SERIAL_LAST}$" )
  # The configuration is current
else()
  set( VXL_UPDATE_CONFIGURATION "ON" )
  # Record that we've done the new config.
  set( VXL_CONFIG_SERIAL_LAST ${VXL_CONFIG_SERIAL_CURRENT} CACHE INTERNAL "Serial number of last configuration" )
endif()

set(vxl_config_SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR})
set(VXL_PLFM_TEST_FILE ${vxl_config_SOURCE_DIR}/vxl_platform_tests.cxx)
########################################################################
# START MACRO DEFINITIONS
#
# Include necessary modules
#
include(CheckSymbolExists)
include(CheckCXXSymbolExists)
include(CheckFunctionExists)
include(CheckTypeSize)
include(CheckIncludeFiles)
include(CheckIncludeFileCXX)
include(TestBigEndian)
#
# Perform the VXL specific test with status output
#
# Sets the TEST to 1 if the corresponding program could be compiled
# and linked
#
macro(PERFORM_CMAKE_TEST PLFM_TEST_FILE TEST)
  if( VXL_UPDATE_CONFIGURATION )
    unset( ${TEST} )
  endif()
  if(NOT DEFINED "${TEST}")
    # Perform test
    if(CMAKE_REQUIRED_LIBRARIES)
      set(TEST_ADD_LIBRARIES "-DLINK_LIBRARIES:STRING=${CMAKE_REQUIRED_LIBRARIES}")
    endif()
    message(STATUS "Performing Test ${TEST}")

    if(CMAKE_CXX_STANDARD)
      set(TRY_COMP_CXX_STANDARD
        -DCMAKE_CXX_STANDARD:STRING=${CMAKE_CXX_STANDARD})
    endif()
    try_compile(${TEST}
                ${CMAKE_BINARY_DIR}
                ${PLFM_TEST_FILE}
                CMAKE_FLAGS
                  -DCOMPILE_DEFINITIONS:STRING=${CMAKE_REQUIRED_FLAGS} "${TEST_ADD_LIBRARIES}"
                  ${TRY_COMP_CXX_STANDARD}
                COMPILE_DEFINITIONS -D${TEST}
                OUTPUT_VARIABLE OUTPUT)
    if(${TEST})
      set(${TEST} 1 CACHE INTERNAL "VXL test ${FUNCTION}")
      message(STATUS "Performing Test ${TEST} - Success")
    else()
      message(STATUS "Performing Test ${TEST} - Failed")
      set(${TEST} 0 CACHE INTERNAL "Test ${FUNCTION}")
      file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
                 "Performing Test ${TEST} failed with the following output:\n"
                 "${OUTPUT}\n")
    endif()
  else()
    # Have result
    #foreach(tst ${TEST})
    #  message("Test ${TEST} resulted in ${${tst}}")
    #endforeach()
  endif()
endmacro()

#
# Perform a custom VXL try compile test with status output
#
# DIR is the directory containing the test project
#
# Sets the TEST to 1 if the corresponding program could be compiled
# and linked
#
macro(PERFORM_CMAKE_TEST_CUSTOM DIR TEST)
  if( VXL_UPDATE_CONFIGURATION )
    unset( ${TEST} )
  endif()
  if(NOT DEFINED "${TEST}")
    # Perform test
    set(MACRO_CHECK_FUNCTION_DEFINITIONS
        "-D${TEST} ${CMAKE_REQUIRED_FLAGS}")
    if(CMAKE_REQUIRED_LIBRARIES)
      set(TEST_ADD_LIBRARIES
          "-DLINK_LIBRARIES:STRING=${CMAKE_REQUIRED_LIBRARIES}")
    endif()
    message(STATUS "Performing Test ${TEST}")

    try_compile(${TEST}
                ${CMAKE_BINARY_DIR}/config/${DIR}
                ${vxl_config_SOURCE_DIR}/${DIR}
                ${TEST}
                CMAKE_FLAGS -DCOMPILE_DEFINITIONS:STRING=${MACRO_CHECK_FUNCTION_DEFINITIONS}
                -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
                -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
                "${TEST_ADD_LIBRARIES}"
                OUTPUT_VARIABLE OUTPUT)
    if(${TEST})
      set(${TEST} 1 CACHE INTERNAL "VXL test ${FUNCTION}")
      message(STATUS "Performing Test ${TEST} - Success")
    else()
      message(STATUS "Performing Test ${TEST} - Failed")
      set(${TEST} 0 CACHE INTERNAL "Test ${FUNCTION}")
      file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
                 "Performing Test ${TEST} failed with the following output:\n"
                 "${OUTPUT}\n")
    endif()
  endif()
  file(REMOVE_RECURSE ${CMAKE_BINARY_DIR}/config/${DIR})
endmacro()

#
# Perform the VXL specific try-run test with status output
#
# Sets TEST to 1 if the corresponding program compiles, links, run,
# and returns 0 (indicating success).
#
macro(PERFORM_CMAKE_TEST_RUN PLFM_TEST_FILE TEST)
  if( VXL_UPDATE_CONFIGURATION )
    unset( ${TEST} )
  endif()
  if(NOT DEFINED "${TEST}")
    # Perform test
    set(MACRO_CHECK_FUNCTION_DEFINITIONS
        "-D${TEST} ${CMAKE_REQUIRED_FLAGS}")
    if(CMAKE_REQUIRED_LIBRARIES)
      set(TEST_ADD_LIBRARIES
          "-DLINK_LIBRARIES:STRING=${CMAKE_REQUIRED_LIBRARIES}")
    endif()
    message(STATUS "Performing Test ${TEST}")

    try_run(${TEST} ${TEST}_COMPILED
            ${CMAKE_BINARY_DIR}
            ${PLFM_TEST_FILE}
            CMAKE_FLAGS -DCOMPILE_DEFINITIONS:STRING=${MACRO_CHECK_FUNCTION_DEFINITIONS}
            "${TEST_ADD_LIBRARIES}"
            OUTPUT_VARIABLE OUTPUT)
    if(${TEST}_COMPILED)
      if(${TEST})
        message(STATUS "Performing Test ${TEST} - Failed")
        set(${TEST} 0 CACHE INTERNAL "Test ${FUNCTION} (failed to run)")
        file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
                   "Performing Test ${TEST} failed with the following output:\n"
                   "${OUTPUT}\n")
      else()
        set(${TEST} 1 CACHE INTERNAL "VXL test ${FUNCTION} (successful run)")
        message(STATUS "Performing Test ${TEST} - Success")
        file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
                   "Performing Test ${TEST} succeeded with the following output:\n"
                   "${OUTPUT}\n")
      endif()
    else()
      message(STATUS "Performing Try-Run Test ${TEST} - Test Compilation Failed")
      set(${TEST} 0 CACHE INTERNAL "Test ${FUNCTION} (failed to compile)")
      file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
                 "Performing Try-Run Test ${TEST} failed to compile with the following output:\n"
                 "${OUTPUT}\n")
    endif()
  else()
    # Have result
    #foreach(tst ${TEST})
    #  message("Test ${TEST} resulted in ${${tst}}")
    #endforeach()
  endif()
endmacro()

#
# Check for include file and if not found, set variable to 0
#
macro(PERFORM_CHECK_HEADER PLFM_TEST_FILE VARIABLE)
  if( VXL_UPDATE_CONFIGURATION )
    unset(${VARIABLE})
  endif()
  CHECK_INCLUDE_FILE_CXX(${PLFM_TEST_FILE} ${VARIABLE})
  if("x${${VARIABLE}}" STREQUAL "x")
    set(${VARIABLE} 0)
  endif()
endmacro()

macro(PERFORM_CHECK_C_HEADER PLFM_TEST_FILE VARIABLE)
  if( VXL_UPDATE_CONFIGURATION )
    unset(${VARIABLE})
  endif()
  CHECK_INCLUDE_FILES(${PLFM_TEST_FILE} ${VARIABLE})
  if("x${${VARIABLE}}" STREQUAL "x")
    set(${VARIABLE} 0)
  endif()
endmacro()

#
# Check value of variable and if true, set to VALUE_TRUE, otherwise to
# VALUE_FALSE
#
macro(SET_BOOL VAR VALUE_TRUE VALUE_FALSE)
  set(SET_BOOL_VAR "${VAR}")
  if(${SET_BOOL_VAR})
    set(${VAR} ${VALUE_TRUE})
  else()
    set(${VAR} ${VALUE_FALSE})
  endif()
endmacro()

#
# Set the variable to inverse of the given value
#
macro(SET_INVERT VAR VALUE)
  set(SET_INVERT_VAR "${VALUE}")
  if(SET_INVERT_VAR)
    set(${VAR} "0")
  else()
    set(${VAR} "1")
  endif()
endmacro()

#
# Check if the type exists (should really go to CMake/Modules)
#
macro(CHECK_TYPE_EXISTS TYPE FILES VARIABLE)
  if( VXL_UPDATE_CONFIGURATION )
    unset( ${VARIABLE} )
  endif()
  if(NOT DEFINED "${VARIABLE}")
    set(CHECK_TYPE_EXISTS_CONTENT "/* */\n")
    set(MACRO_CHECK_TYPE_EXISTS_FLAGS ${CMAKE_REQUIRED_FLAGS})
    if(CMAKE_REQUIRED_LIBRARIES)
      set(CHECK_TYPE_EXISTS_LIBS
          "-DLINK_LIBRARIES:STRING=${CMAKE_REQUIRED_LIBRARIES}")
    endif()
    foreach(PLFM_TEST_FILE ${FILES})
      set(CHECK_TYPE_EXISTS_CONTENT
          "${CHECK_TYPE_EXISTS_CONTENT}#include <${PLFM_TEST_FILE}>\n")
    endforeach()
    set(CHECK_TYPE_EXISTS_CONTENT
        "${CHECK_TYPE_EXISTS_CONTENT}\nvoid cmakeRequireSymbol(${TYPE} dummy){(void)dummy;}\nint main()\n{return 0;\n}\n")

    file(WRITE ${CMAKE_BINARY_DIR}/CMakeTmp/CheckTypeExists.cxx
         "${CHECK_TYPE_EXISTS_CONTENT}")

    message(STATUS "Looking for ${TYPE}")
    try_compile(${VARIABLE}
                ${CMAKE_BINARY_DIR}
                ${CMAKE_BINARY_DIR}/CMakeTmp/CheckTypeExists.cxx
                CMAKE_FLAGS
                -DCOMPILE_DEFINITIONS:STRING=${MACRO_CHECK_TYPE_EXISTS_FLAGS}
                "${CHECK_TYPE_EXISTS_LIBS}"
                OUTPUT_VARIABLE OUTPUT)
    if(${VARIABLE})
      message(STATUS "Looking for ${TYPE} - found")
      set(${VARIABLE} 1 CACHE INTERNAL "Have symbol ${TYPE}")
      file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
           "Determining if the ${TYPE} "
           "exist passed with the following output:\n"
           "${OUTPUT}\nFile ${CMAKE_BINARY_DIR}/CMakeTmp/CheckTypeExists.cxx:\n"
           "${CHECK_TYPE_EXISTS_CONTENT}\n")
    else()
      message(STATUS "Looking for ${TYPE} - not found.")
      set(${VARIABLE} "" CACHE INTERNAL "Have symbol ${TYPE}")
      file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
           "Determining if the ${TYPE} "
           "exist failed with the following output:\n"
           "${OUTPUT}\nFile ${CMAKE_BINARY_DIR}/CMakeTmp/CheckTypeExists.c:\n"
           "${CHECK_TYPE_EXISTS_CONTENT}\n")
    endif()
  endif()
endmacro()

#
# Check if the type exists and if not make result 0
#
macro(CHECK_TYPE_EXISTS_ZERO SYMBOL FILES VARIABLE)
  if( VXL_UPDATE_CONFIGURATION )
    unset( ${VARIABLE} )
  endif()
  CHECK_TYPE_EXISTS("${SYMBOL}" "${FILES}" "${VARIABLE}")
  if(NOT ${VARIABLE})
    set(${VARIABLE} 0)
  endif()
endmacro()

#
# Check if the function exists and if not make result 0
#
macro(CHECK_FUNCTION_EXISTS_ZERO FUNCTION VARIABLE)
  if( VXL_UPDATE_CONFIGURATION )
    unset(${VARIABLE})
  endif()
  CHECK_FUNCTION_EXISTS("${FUNCTION}" "${VARIABLE}")
  if(NOT ${VARIABLE})
    set(${VARIABLE} 0)
  endif()
endmacro()

#
# Determine which C++ type matches the given size
#
macro( DETERMINE_TYPE VAR INTEGRAL_TYPE SIZE TYPE_LIST )
  if( VXL_UPDATE_CONFIGURATION )
    set( VXL_${VAR} "" )
  endif()
  # If we've tested this before, use the cached result and don't re-run
  if( NOT VXL_${VAR} )
    # We can't have IF commands on a macro parameter. For example,
    # if( INTEGRAL_TYPE ) doesn't seem to work. I think the
    # expansion is done at the wrong time. A macro is not a procedure
    # call. This is a workaround.
    set( MSG1 "Looking for ${SIZE}-bit int." )
    set( MSG0 "Looking for ${SIZE}-bit float." )
    set( MSG ${MSG${INTEGRAL_TYPE}} )

    set( VXL_${VAR} "void" )
    set( VXL_HAS_${VAR} 0 )
    foreach( TYPE ${TYPE_LIST} )
      # Write the config to a file instead of passing on the command
      # line to avoid issues with spaces. (In "long double", for
      # example)
      file(WRITE ${CMAKE_BINARY_DIR}/CMakeTmp/config.h "#define THE_TYPE ${TYPE}\n#define THE_SIZE ${SIZE}\n#define INTEGRAL_TYPE ${INTEGRAL_TYPE}")
      set( MACRO_DETERMINE_TYPE_FLAGS "-DVXL_HAS_TYPE_OF_SIZE" )
      message( STATUS "${MSG} [Checking ${TYPE}...]" )
      try_compile(COMPILE_RESULT
            ${CMAKE_BINARY_DIR}
            ${VXL_PLFM_TEST_FILE}
            CMAKE_FLAGS -DCOMPILE_DEFINITIONS:STRING=${MACRO_DETERMINE_TYPE_FLAGS}
                        -DINCLUDE_DIRECTORIES:STRING=${CMAKE_BINARY_DIR}/CMakeTmp
                        -DLINK_LIBRARIES:STRING=${CMAKE_REQUIRED_LIBRARIES}
            OUTPUT_VARIABLE OUTPUT )
      if( COMPILE_RESULT )
        set( VXL_${VAR} ${TYPE} )
        set( VXL_HAS_${VAR} 1 )
      else()
        file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
          "${MSG} Failed to compile with the following output:\n(FLAGS=${MACRO_DETERMINE_TYPE_FLAGS})\n${OUTPUT}\n")
      endif()
    endforeach()
    if( VXL_HAS_${VAR} )
      message( STATUS "${MSG} Found ${VXL_${VAR}}." )
    else()
      message( STATUS "${MSG} Not found." )
    endif()
    # Cache the value to prevent a second run of the test
    set( VXL_${VAR} ${VXL_${VAR}} CACHE INTERNAL "VXL test result" )
    set( VXL_HAS_${VAR} ${VXL_HAS_${VAR}} CACHE INTERNAL "VXL test result" )
  endif()
endmacro()

#
# Determine if a particular function is declared in the given header.
macro(PERFORM_C_CHECK_FUNCTION SYMBOL PLFM_TEST_FILE VARIABLE)
  if( VXL_UPDATE_CONFIGURATION )
    unset(${VARIABLE})
  endif()
  CHECK_CXX_SYMBOL_EXISTS(${SYMBOL} ${PLFM_TEST_FILE} ${VARIABLE})
  if(${VARIABLE})
    set(${VARIABLE} "1")
  else()
    set(${VARIABLE} "0")
  endif()
endmacro()
# END MACRO DEFINITIONS
########################################################################
#
# Perform all the specific tests
#

PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_HAS_BOOL)
if(NOT VCL_HAS_BOOL)
  message(FATAL_ERROR "Compilers that do not support bool are not supported")
endif()
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_HAS_LONG_LONG)
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_HAS_TYPENAME)
if(NOT VCL_HAS_TYPENAME)
  message(FATAL_ERROR "Compilers that do not support typename are not supported")
endif()
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_HAS_EXPORT)
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_HAS_MUTABLE)
if(NOT VCL_HAS_MUTABLE)
  message(FATAL_ERROR "Compilers that do not support 'mutable' are not supported")
endif()
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_HAS_EXPLICIT)
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_HAS_DYNAMIC_CAST)
if(NOT VCL_HAS_DYNAMIC_CAST)
  message(FATAL_ERROR "Compilers that do not support dynamic_cast are not supported")
endif()
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_HAS_RTTI)
if(NOT VCL_HAS_RTTI)
  message(FATAL_ERROR "Compilers that do not support RTTI are not supported")
endif()
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_DEFAULT_VALUE)
if(NOT VCL_DEFAULT_VALUE)
  message(FATAL_ERROR "Compilers that do not support default values are not supported")
endif()
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_HAS_MEMBER_TEMPLATES)
if(NOT VCL_HAS_MEMBER_TEMPLATES)
  message(FATAL_ERROR "Compilers that do not support member templates are not supported")
endif()
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_CAN_DO_PARTIAL_SPECIALIZATION)
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_ALLOWS_INLINE_INSTANTIATION)
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_NEEDS_INLINE_INSTANTIATION)
  SET_INVERT(VCL_NEEDS_INLINE_INSTANTIATION "${VCL_NEEDS_INLINE_INSTANTIATION}")

PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_CAN_DO_STATIC_TEMPLATE_MEMBER)
if(NOT VCL_CAN_DO_STATIC_TEMPLATE_MEMBER)
  message(FATAL_ERROR "Compilers that can not do static template members are not supported")
endif()
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_CAN_DO_NON_TYPE_FUNCTION_TEMPLATE_PARAMETER)
if(NOT VCL_CAN_DO_NON_TYPE_FUNCTION_TEMPLATE_PARAMETER)
  message(FATAL_ERROR "Compilers that can not do non-type function template are not supported")
endif()
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_NEED_FRIEND_FOR_TEMPLATE_OVERLOAD)
if(NOT VCL_NEED_FRIEND_FOR_TEMPLATE_OVERLOAD)
  message(FATAL_ERROR "Compilers that require friend for template overload are not supported")
endif()
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_OVERLOAD_CAST)
  SET_BOOL(VCL_OVERLOAD_CAST "(x)" "((T)(x))")
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_NULL_TMPL_ARGS)
  SET_BOOL(VCL_NULL_TMPL_ARGS "/* <> */" "<>")
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_NO_STATIC_DATA_MEMBERS)
  SET_INVERT(VCL_NO_STATIC_DATA_MEMBERS "${VCL_NO_STATIC_DATA_MEMBERS}")
PERFORM_CMAKE_TEST_RUN(${VXL_PLFM_TEST_FILE} VCL_HAS_TEMPLATE_SYMBOLS)
  SET_INVERT(VCL_HAS_TEMPLATE_SYMBOLS "${VCL_HAS_TEMPLATE_SYMBOLS}")
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_CAN_DO_COMPLETE_DEFAULT_TYPE_PARAMETER)
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_CAN_DO_TEMPLATE_DEFAULT_TYPE_PARAMETER)
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_TEMPLATE_MATCHES_TOO_OFTEN)
  SET_INVERT(VCL_TEMPLATE_MATCHES_TOO_OFTEN "${VCL_TEMPLATE_MATCHES_TOO_OFTEN}")
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_HAS_EXCEPTIONS)
if(NOT VCL_HAS_EXCEPTIONS)
  message(FATAL_ERROR "Compiler does not support exceptions")
endif()
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_HAS_NAMESPACES)
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_ALLOWS_NAMESPACE_STD)
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_NEEDS_NAMESPACE_STD)
  SET_INVERT(VCL_NEEDS_NAMESPACE_STD "${VCL_NEEDS_NAMESPACE_STD}")
PERFORM_CMAKE_TEST_RUN(${VXL_PLFM_TEST_FILE} VCL_HAS_SLICED_DESTRUCTOR_BUG)
  SET_INVERT(VCL_HAS_SLICED_DESTRUCTOR_BUG "${VCL_HAS_SLICED_DESTRUCTOR_BUG}")
PERFORM_CMAKE_TEST_RUN(${VXL_PLFM_TEST_FILE} VCL_HAS_WORKING_STRINGSTREAM)
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VXL_UNISTD_USLEEP_IS_VOID)
  SET_INVERT(VXL_UNISTD_USLEEP_IS_VOID "${VXL_UNISTD_USLEEP_IS_VOID}")
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VXL_STDLIB_HAS_QSORT)
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VXL_C_MATH_HAS_LROUND)
PERFORM_CMAKE_TEST_RUN(${VXL_PLFM_TEST_FILE} VCL_HAS_LFS)
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VXL_HAS_DBGHELP_H)

# Test how to define a template specialization, and whether
# specializations can be properly distinguished by top-level
# cv-qualifiers.
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_DEFINE_SPECIALIZATION)
if(NOT VCL_DEFINE_SPECIALIZATION)
  message(FATAL_ERROR "Compiler does not support ISO compatible template speicalization")
endif()
PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_CANNOT_SPECIALIZE_CV)
if(NOT VCL_CANNOT_SPECIALIZE_CV)
  message(FATAL_ERROR "Compiler does not support ISO separation of types based on const")
endif()
set(CMAKE_REQUIRED_FLAGS)

# Check if Windows have wchar_t defined
if(WIN32)
  PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VXL_HAS_WIN_WCHAR_T)
endif()

# Test the ability of shared libraries to link to static vxl libriares
PERFORM_CMAKE_TEST_CUSTOM(vxl_shared_link_test VXL_PIC_COMPATIBLE)

#
# Find header files
#

PERFORM_CHECK_HEADER(cassert VCL_CXX_HAS_HEADER_CASSERT)
PERFORM_CHECK_HEADER(ciso646 VCL_CXX_HAS_HEADER_CISO646)
if(NOT VCL_CXX_HAS_HEADER_CISO646)
  message(FATAL_ERROR "Compiler must have #include <ciso646>")
endif()
PERFORM_CHECK_HEADER(csetjmp VCL_CXX_HAS_HEADER_CSETJMP)
PERFORM_CHECK_HEADER(cstdio VCL_CXX_HAS_HEADER_CSTDIO)
PERFORM_CHECK_HEADER(ctime VCL_CXX_HAS_HEADER_CTIME)
if(NOT VCL_CXX_HAS_HEADER_CTIME)
  message(FATAL_ERROR "Compiler must have #include <ctime>")
endif()
PERFORM_CHECK_HEADER(cctype VCL_CXX_HAS_HEADER_CCTYPE)
PERFORM_CHECK_HEADER(climits VCL_CXX_HAS_HEADER_CLIMITS)
PERFORM_CHECK_HEADER(csignal VCL_CXX_HAS_HEADER_CSIGNAL)
PERFORM_CHECK_HEADER(cstdlib VCL_CXX_HAS_HEADER_CSTDLIB)
if(NOT VCL_CXX_HAS_HEADER_CSTDLIB)
  message(FATAL_ERROR "Compiler must have #include <cstdlib>")
endif()
PERFORM_CHECK_HEADER(cwchar VCL_CXX_HAS_HEADER_CWCHAR)
PERFORM_CHECK_HEADER(cerrno VCL_CXX_HAS_HEADER_CERRNO)
PERFORM_CHECK_HEADER(clocale VCL_CXX_HAS_HEADER_CLOCALE)
PERFORM_CHECK_HEADER(cstdarg VCL_CXX_HAS_HEADER_CSTDARG)
PERFORM_CHECK_HEADER(cstring VCL_CXX_HAS_HEADER_CSTRING)
PERFORM_CHECK_HEADER(cwctype VCL_CXX_HAS_HEADER_CWCTYPE)
PERFORM_CHECK_HEADER(cfloat VCL_CXX_HAS_HEADER_CFLOAT)
PERFORM_CHECK_HEADER(cmath VCL_CXX_HAS_HEADER_CMATH)
PERFORM_CHECK_HEADER(cstddef VCL_CXX_HAS_HEADER_CSTDDEF)
PERFORM_CHECK_HEADER(algorithm VCL_CXX_HAS_HEADER_ALGORITHM)
PERFORM_CHECK_HEADER(iomanip VCL_CXX_HAS_HEADER_IOMANIP)
PERFORM_CHECK_HEADER(list VCL_CXX_HAS_HEADER_LIST)
PERFORM_CHECK_HEADER(ostream VCL_CXX_HAS_HEADER_OSTREAM)
PERFORM_CHECK_HEADER(streambuf VCL_CXX_HAS_HEADER_STREAMBUF)
PERFORM_CHECK_HEADER(bitset VCL_CXX_HAS_HEADER_BITSET)
PERFORM_CHECK_HEADER(ios VCL_CXX_HAS_HEADER_IOS)
PERFORM_CHECK_HEADER(locale VCL_CXX_HAS_HEADER_LOCALE)
PERFORM_CHECK_HEADER(queue VCL_CXX_HAS_HEADER_QUEUE)
PERFORM_CHECK_HEADER(string VCL_CXX_HAS_HEADER_STRING)
PERFORM_CHECK_HEADER(complex VCL_CXX_HAS_HEADER_COMPLEX)
PERFORM_CHECK_HEADER(iosfwd VCL_CXX_HAS_HEADER_IOSFWD)
PERFORM_CHECK_HEADER(map VCL_CXX_HAS_HEADER_MAP)
PERFORM_CHECK_HEADER(set VCL_CXX_HAS_HEADER_SET)
PERFORM_CHECK_HEADER(typeinfo VCL_CXX_HAS_HEADER_TYPEINFO)
PERFORM_CHECK_HEADER(deque VCL_CXX_HAS_HEADER_DEQUE)
PERFORM_CHECK_HEADER(iostream VCL_CXX_HAS_HEADER_IOSTREAM)
PERFORM_CHECK_HEADER(memory VCL_CXX_HAS_HEADER_MEMORY)
PERFORM_CHECK_HEADER(sstream VCL_CXX_HAS_HEADER_SSTREAM)
PERFORM_CHECK_HEADER(utility VCL_CXX_HAS_HEADER_UTILITY)
PERFORM_CHECK_HEADER(exception VCL_CXX_HAS_HEADER_EXCEPTION)
PERFORM_CHECK_HEADER(istream VCL_CXX_HAS_HEADER_ISTREAM)
PERFORM_CHECK_HEADER(new VCL_CXX_HAS_HEADER_NEW)
PERFORM_CHECK_HEADER(stack VCL_CXX_HAS_HEADER_STACK)
PERFORM_CHECK_HEADER(valarray VCL_CXX_HAS_HEADER_VALARRAY)
PERFORM_CHECK_HEADER(fstream VCL_CXX_HAS_HEADER_FSTREAM)
PERFORM_CHECK_HEADER(iterator VCL_CXX_HAS_HEADER_ITERATOR)
PERFORM_CHECK_HEADER(numeric VCL_CXX_HAS_HEADER_NUMERIC)
PERFORM_CHECK_HEADER(stdexcept VCL_CXX_HAS_HEADER_STDEXCEPT)
PERFORM_CHECK_HEADER(vector VCL_CXX_HAS_HEADER_VECTOR)
PERFORM_CHECK_HEADER(functional VCL_CXX_HAS_HEADER_FUNCTIONAL)
PERFORM_CHECK_HEADER(strstream VCL_CXX_HAS_HEADER_STRSTREAM)
PERFORM_CHECK_HEADER(iso646.h VCL_CXX_HAS_HEADER_ISO646_H)
if(NOT VCL_CXX_HAS_HEADER_ISO646_H)
  message(FATAL_ERROR "Compiler must have #include <iso646.h>")
endif()
PERFORM_CHECK_HEADER(emmintrin.h VXL_HAS_EMMINTRIN_H)

PERFORM_CHECK_HEADER(windows.h   VXL_CXX_HAS_WINDOWS_H)

PERFORM_CHECK_C_HEADER(assert.h    VCL_HAS_HEADER_ASSERT_H)
PERFORM_CHECK_C_HEADER(pthread.h   VXL_HAS_PTHREAD_H)
PERFORM_CHECK_C_HEADER(semaphore.h VXL_HAS_SEMAPHORE_H)
PERFORM_CHECK_C_HEADER(dlfcn.h     VCL_HAS_DLFCN_H)
PERFORM_CHECK_C_HEADER(fcntl.h     VCL_HAS_FCNTL_H)
PERFORM_CHECK_C_HEADER(inttypes.h  VCL_HAS_INTTYPES_H)
PERFORM_CHECK_C_HEADER(limits.h    VCL_HAS_LIMITS_H)
PERFORM_CHECK_C_HEADER(malloc.h    VCL_HAS_MALLOC_H)
PERFORM_CHECK_C_HEADER(memory.h    VCL_HAS_MEMORY_H)
PERFORM_CHECK_C_HEADER(search.h    VCL_HAS_SEARCH_H)
PERFORM_CHECK_C_HEADER(stdint.h    VCL_HAS_STDINT_H)
PERFORM_CHECK_C_HEADER(stdlib.h    VCL_HAS_STDLIB_H)
PERFORM_CHECK_C_HEADER(strings.h   VCL_HAS_STRINGS_H)
PERFORM_CHECK_C_HEADER(string.h    VCL_HAS_STRING_H)
PERFORM_CHECK_C_HEADER(sys/stat.h  VCL_HAS_SYS_STAT_H)
PERFORM_CHECK_C_HEADER(sys/times.h VCL_HAS_SYS_TIME_H)
PERFORM_CHECK_C_HEADER(sys/types.h VCL_HAS_SYS_TYPES_H)
PERFORM_CHECK_C_HEADER(unistd.h   VCL_HAS_UNISTD_H)

#
# Check for new C++0x standard additions (including TR1)
#

PERFORM_CHECK_HEADER(tr1/memory VCL_CXX_HAS_TR1)
CHECK_TYPE_EXISTS_ZERO(std::shared_ptr<void> memory VCL_MEMORY_HAS_SHARED_PTR)
CHECK_TYPE_EXISTS_ZERO(std::tr1::shared_ptr<void> memory VCL_MEMORY_HAS_TR1_SHARED_PTR)
CHECK_TYPE_EXISTS_ZERO(std::tr1::shared_ptr<void> tr1/memory VCL_TR1_MEMORY_HAS_SHARED_PTR)

if(VCL_MEMORY_HAS_SHARED_PTR OR VCL_TR1_MEMORY_HAS_SHARED_PTR)
  option(VCL_INCLUDE_CXX_0X "Enable C++0x standard extensions" ${BUILD_FOR_VXL_DASHBOARD})
else()
  set(VCL_INCLUDE_CXX_0X 0)
endif()

# Make sure boolean values are (0,1) not (NO,YES)
if(VCL_INCLUDE_CXX_0X)
  set(VCL_INCLUDE_CXX_0X 1)
else()
  set(VCL_INCLUDE_CXX_0X 0)
endif()



# check for hardware support for sse2 with the current compiler flags
PERFORM_CMAKE_TEST_RUN(${VXL_PLFM_TEST_FILE} VXL_HAS_SSE2_HARDWARE_SUPPORT)

# if no support right now, see if the support exists if some flags
# are added.  This can be used to give the user some useful info.
if(NOT VXL_HAS_SSE2_HARDWARE_SUPPORT)
  if(CMAKE_COMPILER_IS_GNUCXX)
    set(VXL_SSE_TEST_FLAG_BACKUP ${CMAKE_REQUIRED_FLAGS})
    set(CMAKE_REQUIRED_FLAGS " -msse2 ${VXL_SSE_TEST_FLAG_BACKUP} ")
    PERFORM_CMAKE_TEST_RUN(${VXL_PLFM_TEST_FILE} VXL_SSE2_HARDWARE_SUPPORT_POSSIBLE)
    set( VXL_SSE2_HARDWARE_SUPPORT_POSSIBLE_HELP
      "The current compiler flags do not allow the SSE2 instructions to be used. "
      "It looks like if you add the flag '-msse2' you will be able to use the "
      "SSE2 instructions. If you make this change and still see this message, "
      " you may need to set VXL_UPDATE_CONFIGURATION to ON."
      CACHE INTERNAL "help string for how to enable SSE2 support" )
    set(CMAKE_REQUIRED_FLAGS ${VXL_SSE_TEST_FLAG_BACKUP})
  endif()
endif()


#
# Check for aligned dynamic memory allocation support, useful for sse
#

if(VXL_HAS_EMMINTRIN_H)
  # check for memory allocation operations
  PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VXL_HAS_MM_MALLOC)
  PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VXL_HAS_ALIGNED_MALLOC)
  PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VXL_HAS_MINGW_ALIGNED_MALLOC)
  PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VXL_HAS_POSIX_MEMALIGN)

else()
  set( VXL_HAS_MM_MALLOC 0 )
  set( VXL_HAS_ALIGNED_MALLOC 0 )
  set( VXL_HAS_MINGW_ALIGNED_MALLOC 0 )
  set( VXL_HAS_POSIX_MEMALIGN 0 )
endif()


#
# Check complex pow stuff
#

if(VCL_CXX_HAS_HEADER_COMPLEX)
  PERFORM_CMAKE_TEST_RUN(${VXL_PLFM_TEST_FILE} VCL_COMPLEX_POW_WORKS)
endif()



# Tests of math.h may need math library on UNIX.
if(UNIX)
  set(CMAKE_REQUIRED_LIBRARIES "${CMAKE_REQUIRED_LIBRARIES};m")
endif()

# Check C++ <cmath> first, where the C++11 standard says these must be.
include(${CMAKE_CURRENT_LIST_DIR}/CheckCXXExpressionCompiles.cmake) ## From VTK
check_cxx_expression_compiles("std::isnan(0.0)" "cmath" VXL_HAS_STD_ISNAN)
check_cxx_expression_compiles("std::isinf(0.0)" "cmath" VXL_HAS_STD_ISINF)
check_cxx_expression_compiles("std::isfinite(0.0)" "cmath" VXL_HAS_STD_ISFINITE)
check_cxx_expression_compiles("std::isnormal(0.0)" "cmath" VXL_HAS_STD_ISNORMAL)

PERFORM_C_CHECK_FUNCTION(sqrtf "math.h" VXL_C_MATH_HAS_SQRTF)

PERFORM_C_CHECK_FUNCTION(finite "math.h" VXL_C_MATH_HAS_FINITE)
PERFORM_C_CHECK_FUNCTION(finitef "math.h" VXL_C_MATH_HAS_FINITEF)
PERFORM_C_CHECK_FUNCTION(finitel "math.h" VXL_C_MATH_HAS_FINITEL)
PERFORM_C_CHECK_FUNCTION(finite "ieeefp.h" VXL_IEEEFP_HAS_FINITE)
#PERFORM_C_CHECK_FUNCTION(qsort "stdlib.h" VXL_STDLIB_HAS_QSORT)
PERFORM_C_CHECK_FUNCTION(lrand48 "stdlib.h" VXL_STDLIB_HAS_LRAND48)
PERFORM_C_CHECK_FUNCTION(drand48 "stdlib.h" VXL_STDLIB_HAS_DRAND48)
PERFORM_C_CHECK_FUNCTION(srand48 "stdlib.h" VXL_STDLIB_HAS_SRAND48)

TEST_BIG_ENDIAN(VXL_BIG_ENDIAN)
SET_INVERT(VXL_LITTLE_ENDIAN "${VXL_BIG_ENDIAN}")

PERFORM_CMAKE_TEST(${VXL_PLFM_TEST_FILE} VCL_CAN_DO_IMPLICIT_TEMPLATES)
if(NOT VCL_CAN_DO_IMPLICIT_TEMPLATES)
  message(FATAL_ERROR "Implicit template instantiation must be supported by the compiler.")
endif()
unset(VCL_CAN_DO_IMPLICIT_TEMPLATES)

PERFORM_CMAKE_TEST_RUN(${VXL_PLFM_TEST_FILE} VCL_CHAR_IS_SIGNED)

#
# Check type sizes
#

set(CMAKE_REQUIRED_FLAGS ${CMAKE_ANSI_CFLAGS})

# The types are listed in reverse order of preference. That is, the
# last type is should be the most preferred type name.
#
DETERMINE_TYPE(BYTE     1 8   "char")
DETERMINE_TYPE(INT_8    1 8   "short;char")
DETERMINE_TYPE(INT_16   1 16  "char;int;short")
DETERMINE_TYPE(INT_32   1 32  "short;long;int")
DETERMINE_TYPE(INT_64   1 64  "__int64;long long;long")
DETERMINE_TYPE(IEEE_32  0 32  "long double;double;float")
DETERMINE_TYPE(IEEE_64  0 64  "float;long double;double")
DETERMINE_TYPE(IEEE_96  0 96  "float;double;long double")
DETERMINE_TYPE(IEEE_128 0 128 "float;double;long double")
if(${VXL_INT_64} MATCHES "^long$")
  set(VXL_INT_64_IS_LONG 1)
else()
  set(VXL_INT_64_IS_LONG 0)
endif()

#
# Check unistd stuff
#

CHECK_INCLUDE_FILE_CXX("unistd.h" HAVE_UNISTD_H)
if(HAVE_UNISTD_H)
  CHECK_TYPE_EXISTS_ZERO(useconds_t "unistd.h" VXL_UNISTD_HAS_USECONDS_T)
  CHECK_TYPE_EXISTS_ZERO(intptr_t "unistd.h" VXL_UNISTD_HAS_INTPTR_T)
  CHECK_FUNCTION_EXISTS_ZERO(ualarm VXL_UNISTD_HAS_UALARM)
  CHECK_FUNCTION_EXISTS_ZERO(usleep VXL_UNISTD_HAS_USLEEP)
  CHECK_FUNCTION_EXISTS_ZERO(lchown VXL_UNISTD_HAS_LCHOWN)
  CHECK_FUNCTION_EXISTS_ZERO(pread VXL_UNISTD_HAS_PREAD)
  CHECK_FUNCTION_EXISTS_ZERO(pwrite VXL_UNISTD_HAS_PWRITE)
  CHECK_FUNCTION_EXISTS_ZERO(tell VXL_UNISTD_HAS_TELL)
  CHECK_FUNCTION_EXISTS_ZERO(getpid VXL_UNISTD_HAS_GETPID)
  CHECK_FUNCTION_EXISTS_ZERO(gethostname VXL_UNISTD_HAS_GETHOSTNAME)
else()
  # If there is not unistd.h, assume windows and therefore hardcode results.
  set(VXL_UNISTD_HAS_USECONDS_T 0)
  set(VXL_UNISTD_HAS_INTPTR_T 0)
  set(VXL_UNISTD_HAS_UALARM 1)
  set(VXL_UNISTD_HAS_USLEEP 1)
  set(VXL_UNISTD_HAS_LCHOWN 1)
  set(VXL_UNISTD_HAS_PREAD 1)
  set(VXL_UNISTD_HAS_PWRITE 1)
  set(VXL_UNISTD_HAS_TELL 1)
  set(VXL_UNISTD_HAS_GETPID 1)
  set(VXL_UNISTD_HAS_GETHOSTNAME 1)
endif()

#
# Check the address model of the build, i.e. 32-bit (4-byte) or 64-bit (8-byte).
# The type of void * is directly related to address model on most machines and compilers.
# Hence, we use the size of void * instead.
#
math(EXPR VXL_ADDRESS_BITS 8*${CMAKE_SIZEOF_VOID_P} )

#
# Sanity Check numeric_limits infinity stuff
#
PERFORM_CHECK_HEADER(limits VCL_CXX_HAS_HEADER_LIMITS)
if(NOT VCL_CXX_HAS_HEADER_LIMITS)
  message(FATAL_ERROR "Compiler is required to have limits.h")
else()
  PERFORM_CMAKE_TEST_RUN(${VXL_PLFM_TEST_FILE} VCL_NUMERIC_LIMITS_HAS_INFINITY)
  # ITK does not require has_infinity
  #if(NOT VCL_NUMERIC_LIMITS_HAS_INFINITY)
  #message(FATAL_ERROR "Compiler is required to have has_infinity.")
  #endif()
  #unset(VCL_NUMERIC_LIMITS_HAS_INFINITY)
endif()
unset(VCL_CXX_HAS_HEADER_LIMITS)

if(NOT VCL_CXX_HAS_HEADER_CFLOAT)
  message(FATAL_ERROR "Compiler is required to have cfloat")
endif()
unset(VCL_CXX_HAS_HEADER_CFLOAT)

# Reset the update configuration flag
set( VXL_UPDATE_CONFIGURATION "OFF" CACHE BOOL "Re-run the configuration tests?" FORCE )

if(CMAKE_CXX_STANDARD)
  set(CMAKE_TEST_FLAGS "-DCMAKE_CXX_STANDARD:STRING=${CMAKE_CXX_STANDARD}")
endif()
if(CMAKE_CXX_STANDARD)
   set(TRY_COMP_CXX_STANDARD
       -DCMAKE_CXX_STANDARD:STRING=${CMAKE_CXX_STANDARD})
endif()
# Identify the version of CXX compiler used when VXL was built. This needs to be
# identified so that external applications can identify how VXL was built.
set(VXL_COMPILED_CXX_STANDARD_VERSION 1)
foreach(CXX_TEST_VERSION 199711L 201103L 201402L)
  try_compile(VXL_MIN_CXX_LEVEL_TEST
    ${CMAKE_CURRENT_BINARY_DIR}/CMakeTmp
    ${CMAKE_CURRENT_LIST_DIR}/vxlGetCXXCompilerVersion.cxx
    CMAKE_FLAGS
        -DCOMPILE_DEFINITIONS:STRING=${CMAKE_REQUIRED_FLAGS} "${TEST_ADD_LIBRARIES}"
        ${TRY_COMP_CXX_STANDARD}
    COMPILE_DEFINITIONS -DVXL_CXX_TEST_VERSION=${CXX_TEST_VERSION}
    OUTPUT_VARIABLE VXL_COMPILED_CXX_STANDARD_VERSION_LOG
  )
  if(VXL_MIN_CXX_LEVEL_TEST)
     set(VXL_COMPILED_CXX_STANDARD_VERSION ${CXX_TEST_VERSION})
  endif()
endforeach()

#These variables should no longer be used
unset(vxl_config_SOURCE_DIR)
unset(VXL_PLFM_TEST_FILE)
