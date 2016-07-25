#-----------------------------------------------------------------------------
# Include all the necessary files for macros
#-----------------------------------------------------------------------------
set (HDF_PREFIX "H5")
include (${HDF_RESOURCES_EXT_DIR}/ConfigureChecks.cmake)
include (${CMAKE_ROOT}/Modules/TestForSTDNamespace.cmake)

#-----------------------------------------------------------------------------
# Option to Clear File Buffers before write --enable-clear-file-buffers
#-----------------------------------------------------------------------------
option (HDF5_Enable_Clear_File_Buffers "Securely clear file buffers before writing to file" ON)
if (HDF5_Enable_Clear_File_Buffers)
  set (H5_CLEAR_MEMORY 1)
endif (HDF5_Enable_Clear_File_Buffers)
MARK_AS_ADVANCED (HDF5_Enable_Clear_File_Buffers)

#-----------------------------------------------------------------------------
# Option for --enable-strict-format-checks
#-----------------------------------------------------------------------------
option (HDF5_STRICT_FORMAT_CHECKS "Whether to perform strict file format checks" OFF)
if (HDF5_STRICT_FORMAT_CHECKS)
  set (H5_STRICT_FORMAT_CHECKS 1)
endif (HDF5_STRICT_FORMAT_CHECKS)
MARK_AS_ADVANCED (HDF5_STRICT_FORMAT_CHECKS)

#-----------------------------------------------------------------------------
# Option for --enable-metadata-trace-file
#-----------------------------------------------------------------------------
option (HDF5_METADATA_TRACE_FILE "Enable metadata trace file collection" OFF)
if (HDF5_METADATA_TRACE_FILE)
  set (H5_METADATA_TRACE_FILE 1)
endif (HDF5_METADATA_TRACE_FILE)
MARK_AS_ADVANCED (HDF5_METADATA_TRACE_FILE)

# ----------------------------------------------------------------------
# Decide whether the data accuracy has higher priority during data
# conversions.  If not, some hard conversions will still be prefered even
# though the data may be wrong (for example, some compilers don't
# support denormalized floating values) to maximize speed.
#
option (HDF5_WANT_DATA_ACCURACY "IF data accuracy is guaranteed during data conversions" ON)
if (HDF5_WANT_DATA_ACCURACY)
  set (H5_WANT_DATA_ACCURACY 1)
endif (HDF5_WANT_DATA_ACCURACY)
MARK_AS_ADVANCED (HDF5_WANT_DATA_ACCURACY)

# ----------------------------------------------------------------------
# Decide whether the presence of user's exception handling functions is
# checked and data conversion exceptions are returned.  This is mainly
# for the speed optimization of hard conversions.  Soft conversions can
# actually benefit little.
#
option (HDF5_WANT_DCONV_EXCEPTION "exception handling functions is checked during data conversions" ON)
if (HDF5_WANT_DCONV_EXCEPTION)
  set (H5_WANT_DCONV_EXCEPTION 1)
endif (HDF5_WANT_DCONV_EXCEPTION)
MARK_AS_ADVANCED (HDF5_WANT_DCONV_EXCEPTION)

# ----------------------------------------------------------------------
# Check if they would like the function stack support compiled in
#
option (HDF5_ENABLE_CODESTACK "Enable the function stack tracing (for developer debugging)." OFF)
if (HDF5_ENABLE_CODESTACK)
  set (H5_HAVE_CODESTACK 1)
endif (HDF5_ENABLE_CODESTACK)
MARK_AS_ADVANCED (HDF5_ENABLE_CODESTACK)

#-----------------------------------------------------------------------------
#  Are we going to use HSIZE_T
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_HSIZET "Enable datasets larger than memory" ON)
if (HDF5_ENABLE_HSIZET)
  set (${HDF_PREFIX}_HAVE_LARGE_HSIZET 1)
endif (HDF5_ENABLE_HSIZET)

# so far we have no check for this
set (H5_HAVE_TMPFILE 1)

# TODO --------------------------------------------------------------------------
# Should the Default Virtual File Driver be compiled?
# This is hard-coded now but option should added to match configure
#
set (H5_DEFAULT_VFD H5FD_SEC2)

if (NOT DEFINED "H5_DEFAULT_PLUGINDIR")
  if (WINDOWS)
    set (H5_DEFAULT_PLUGINDIR "%ALLUSERSPROFILE%\\\\hdf5\\\\lib\\\\plugin")
  else (WINDOWS)
    set (H5_DEFAULT_PLUGINDIR "/usr/local/hdf5/lib/plugin")
  endif (WINDOWS)
endif (NOT DEFINED "H5_DEFAULT_PLUGINDIR")

if (WINDOWS)
  set (H5_HAVE_WINDOWS 1)
  # ----------------------------------------------------------------------
  # Set the flag to indicate that the machine has window style pathname,
  # that is, "drive-letter:\" (e.g. "C:") or "drive-letter:/" (e.g. "C:/").
  # (This flag should be _unset_ for all machines, except for Windows)
  set (H5_HAVE_WINDOW_PATH 1)
endif (WINDOWS)

# ----------------------------------------------------------------------
# END of WINDOWS Hard code Values
# ----------------------------------------------------------------------

CHECK_FUNCTION_EXISTS (difftime          H5_HAVE_DIFFTIME)

# Find the library containing clock_gettime()
if (NOT WINDOWS)
  CHECK_FUNCTION_EXISTS(clock_gettime CLOCK_GETTIME_IN_LIBC)
  CHECK_LIBRARY_EXISTS(rt clock_gettime "" CLOCK_GETTIME_IN_LIBRT)
  CHECK_LIBRARY_EXISTS(posix4 clock_gettime "" CLOCK_GETTIME_IN_LIBPOSIX4)
  if (CLOCK_GETTIME_IN_LIBC)
    set (H5_HAVE_CLOCK_GETTIME 1)
  elseif (CLOCK_GETTIME_IN_LIBRT)
    set (H5_HAVE_CLOCK_GETTIME 1)
    list (APPEND LINK_LIBS rt)
  elseif (CLOCK_GETTIME_IN_LIBPOSIX4)
    set (H5_HAVE_CLOCK_GETTIME 1)
    list (APPEND LINK_LIBS posix4)
  endif (CLOCK_GETTIME_IN_LIBC)
endif (NOT WINDOWS)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#  Check if Direct I/O driver works
#-----------------------------------------------------------------------------
if (NOT WINDOWS)
  option (HDF5_ENABLE_DIRECT_VFD "Build the Direct I/O Virtual File Driver" ON)
  if (HDF5_ENABLE_DIRECT_VFD)
    set (msg "Performing TEST_DIRECT_VFD_WORKS")
    set (MACRO_CHECK_FUNCTION_DEFINITIONS "-DTEST_DIRECT_VFD_WORKS -D_GNU_SOURCE ${CMAKE_REQUIRED_FLAGS}")
    TRY_RUN (TEST_DIRECT_VFD_WORKS_RUN   TEST_DIRECT_VFD_WORKS_COMPILE
        ${CMAKE_BINARY_DIR}
        ${HDF_RESOURCES_EXT_DIR}/HDFTests.c
        CMAKE_FLAGS -DCOMPILE_DEFINITIONS:STRING=${MACRO_CHECK_FUNCTION_DEFINITIONS}
        OUTPUT_VARIABLE OUTPUT
    )
    if (TEST_DIRECT_VFD_WORKS_COMPILE)
      if (TEST_DIRECT_VFD_WORKS_RUN  MATCHES 0)
        HDF_FUNCTION_TEST (HAVE_DIRECT)
        set (CMAKE_REQUIRED_DEFINITIONS "${CMAKE_REQUIRED_DEFINITIONS} -D_GNU_SOURCE")
        add_definitions ("-D_GNU_SOURCE")
      else (TEST_DIRECT_VFD_WORKS_RUN  MATCHES 0)
        set (TEST_DIRECT_VFD_WORKS "" CACHE INTERNAL ${msg})
        message (STATUS "${msg}... no")
        file (APPEND ${CMAKE_BINARY_DIR}/CMakeFiles/CMakeError.log
              "Test TEST_DIRECT_VFD_WORKS Run failed with the following output and exit code:\n ${OUTPUT}\n"
        )
      endif (TEST_DIRECT_VFD_WORKS_RUN  MATCHES 0)
    else (TEST_DIRECT_VFD_WORKS_COMPILE )
      set (TEST_DIRECT_VFD_WORKS "" CACHE INTERNAL ${msg})
      message (STATUS "${msg}... no")
      file (APPEND ${CMAKE_BINARY_DIR}/CMakeFiles/CMakeError.log
          "Test TEST_DIRECT_VFD_WORKS Compile failed with the following output:\n ${OUTPUT}\n"
      )
    endif (TEST_DIRECT_VFD_WORKS_COMPILE)
  endif (HDF5_ENABLE_DIRECT_VFD)
endif (NOT WINDOWS)


#-----------------------------------------------------------------------------
# Macro to determine the various conversion capabilities
#-----------------------------------------------------------------------------
MACRO (H5ConversionTests TEST msg)
  if ("${TEST}" MATCHES "^${TEST}$")
   # message (STATUS "===> ${TEST}")
    TRY_RUN (${TEST}_RUN   ${TEST}_COMPILE
        ${CMAKE_BINARY_DIR}
        ${HDF_RESOURCES_DIR}/ConversionTests.c
        CMAKE_FLAGS -DCOMPILE_DEFINITIONS:STRING=-D${TEST}_TEST
        OUTPUT_VARIABLE OUTPUT
    )
    if (${TEST}_COMPILE)
      if (${TEST}_RUN  MATCHES 0)
        set (${TEST} 1 CACHE INTERNAL ${msg})
        message (STATUS "${msg}... yes")
      else (${TEST}_RUN  MATCHES 0)
        set (${TEST} "" CACHE INTERNAL ${msg})
        message (STATUS "${msg}... no")
        file (APPEND ${CMAKE_BINARY_DIR}/CMakeFiles/CMakeError.log
              "Test ${TEST} Run failed with the following output and exit code:\n ${OUTPUT}\n"
        )
      endif (${TEST}_RUN  MATCHES 0)
    else (${TEST}_COMPILE )
      set (${TEST} "" CACHE INTERNAL ${msg})
      message (STATUS "${msg}... no")
      file (APPEND ${CMAKE_BINARY_DIR}/CMakeFiles/CMakeError.log
          "Test ${TEST} Compile failed with the following output:\n ${OUTPUT}\n"
      )
    endif (${TEST}_COMPILE)

  endif ("${TEST}" MATCHES "^${TEST}$")
ENDMACRO (H5ConversionTests)

#-----------------------------------------------------------------------------
# Macro to make some of the conversion tests easier to write/read
#-----------------------------------------------------------------------------
MACRO (H5MiscConversionTest  VAR TEST msg)
  if ("${TEST}" MATCHES "^${TEST}$")
    if (${VAR})
      set (${TEST} 1 CACHE INTERNAL ${msg})
      message (STATUS "${msg}... yes")
    else (${VAR})
      set (${TEST} "" CACHE INTERNAL ${msg})
      message (STATUS "${msg}... no")
    endif (${VAR})
  endif ("${TEST}" MATCHES "^${TEST}$")
ENDMACRO (H5MiscConversionTest)

#-----------------------------------------------------------------------------
# Check various conversion capabilities
#-----------------------------------------------------------------------------

# ----------------------------------------------------------------------
# Set the flag to indicate that the machine is using a special algorithm to convert
# 'long double' to '(unsigned) long' values.  (This flag should only be set for 
# the IBM Power6 Linux.  When the bit sequence of long double is 
# 0x4351ccf385ebc8a0bfcc2a3c3d855620, the converted value of (unsigned)long 
# is 0x004733ce17af227f, not the same as the library's conversion to 0x004733ce17af2282.
# The machine's conversion gets the correct value.  We define the macro and disable
# this kind of test until we figure out what algorithm they use.
#
H5ConversionTests (H5_LDOUBLE_TO_LONG_SPECIAL  "Checking IF your system converts long double to (unsigned) long values with special algorithm")
# ----------------------------------------------------------------------
# Set the flag to indicate that the machine is using a special algorithm
# to convert some values of '(unsigned) long' to 'long double' values.  
# (This flag should be off for all machines, except for IBM Power6 Linux, 
# when the bit sequences are 003fff..., 007fff..., 00ffff..., 01ffff..., 
# ..., 7fffff..., the compiler uses a unknown algorithm.  We define a 
# macro and skip the test for now until we know about the algorithm.
#
H5ConversionTests (H5_LONG_TO_LDOUBLE_SPECIAL "Checking IF your system can convert (unsigned) long to long double values with special algorithm")
# ----------------------------------------------------------------------
# Set the flag to indicate that the machine can accurately convert
# 'long double' to '(unsigned) long long' values.  (This flag should be set for
# all machines, except for Mac OS 10.4 and SGI IRIX64 6.5.  When the bit sequence
# of long double is 0x4351ccf385ebc8a0bfcc2a3c..., the values of (unsigned)long long
# start to go wrong on these two machines.  Adjusting it higher to
# 0x4351ccf385ebc8a0dfcc... or 0x4351ccf385ebc8a0ffcc... will make the converted
# values wildly wrong.  This test detects this wrong behavior and disable the test.
#
H5ConversionTests (H5_LDOUBLE_TO_LLONG_ACCURATE "Checking IF correctly converting long double to (unsigned) long long values")
# ----------------------------------------------------------------------
# Set the flag to indicate that the machine can accurately convert
# '(unsigned) long long' to 'long double' values.  (This flag should be set for
# all machines, except for Mac OS 10.4, when the bit sequences are 003fff...,
# 007fff..., 00ffff..., 01ffff..., ..., 7fffff..., the converted values are twice
# as big as they should be.
#
H5ConversionTests (H5_LLONG_TO_LDOUBLE_CORRECT "Checking IF correctly converting (unsigned) long long to long double values")
# ----------------------------------------------------------------------
# Check if pointer alignments are enforced
#
H5ConversionTests (H5_NO_ALIGNMENT_RESTRICTIONS "Checking IF alignment restrictions are strictly enforced")

# -----------------------------------------------------------------------
# wrapper script variables
# 
set (prefix ${CMAKE_INSTALL_PREFIX})
set (exec_prefix "\${prefix}")
set (libdir "${exec_prefix}/lib")
set (includedir "\${prefix}/include")
set (host_os ${CMAKE_HOST_SYSTEM_NAME})
set (CC ${CMAKE_C_COMPILER})
set (CXX ${CMAKE_CXX_COMPILER})
set (FC ${CMAKE_Fortran_COMPILER})
foreach (LINK_LIB ${LINK_LIBS})
  set (LIBS "${LIBS} -l${LINK_LIB}")
endforeach (LINK_LIB ${LINK_LIBS})
