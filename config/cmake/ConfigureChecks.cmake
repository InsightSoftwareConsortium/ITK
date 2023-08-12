#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
#-----------------------------------------------------------------------------
# Include all the necessary files for macros
#-----------------------------------------------------------------------------
include (CheckFunctionExists)
include (CheckIncludeFile)
include (CheckIncludeFiles)
include (CheckLibraryExists)
include (CheckSymbolExists)
include (CheckTypeSize)
include (CheckVariableExists)
include (TestBigEndian)
include (CheckStructHasMember)

set (HDF_PREFIX "H5")

# Check for Darwin (not just Apple - we also want to catch OpenDarwin)
if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set (${HDF_PREFIX}_HAVE_DARWIN 1)
endif ()

# Check for Solaris
if (${CMAKE_SYSTEM_NAME} MATCHES "SunOS")
    set (${HDF_PREFIX}_HAVE_SOLARIS 1)
endif ()

#-----------------------------------------------------------------------------
# This MACRO checks IF the symbol exists in the library and IF it
# does, it appends library to the list.
#-----------------------------------------------------------------------------
set (LINK_LIBS "")
macro (CHECK_LIBRARY_EXISTS_CONCAT LIBRARY SYMBOL VARIABLE)
  CHECK_LIBRARY_EXISTS ("${LIBRARY};${LINK_LIBS}" ${SYMBOL} "" ${VARIABLE})
  if (${VARIABLE})
    set (LINK_LIBS ${LINK_LIBS} ${LIBRARY})
  endif ()
endmacro ()

# ----------------------------------------------------------------------
# WINDOWS Hard code Values
# ----------------------------------------------------------------------
set (WINDOWS)

if (MINGW)
  set (${HDF_PREFIX}_HAVE_MINGW 1)
  set (WINDOWS 1) # MinGW tries to imitate Windows
  set (CMAKE_REQUIRED_FLAGS "-DWIN32_LEAN_AND_MEAN=1 -DNOGDI=1")
  set (__USE_MINGW_ANSI_STDIO 1)
endif ()

if (WIN32 AND NOT MINGW)
  if (NOT UNIX)
    set (WINDOWS 1)
    set (CMAKE_REQUIRED_FLAGS "/DWIN32_LEAN_AND_MEAN=1 /DNOGDI=1")
    if (MSVC)
      set (${HDF_PREFIX}_HAVE_VISUAL_STUDIO 1)
    endif ()
  endif ()
endif ()

if (WINDOWS)
  set (HDF5_REQUIRED_LIBRARIES "ws2_32.lib;wsock32.lib")
  set (${HDF_PREFIX}_HAVE_WIN32_API 1)
  set (${HDF_PREFIX}_HAVE_LIBM 1)
  set (${HDF_PREFIX}_HAVE_STRDUP 1)
  if (NOT MINGW)
    set (${HDF_PREFIX}_HAVE_GETHOSTNAME 1)
  endif ()
  if (NOT UNIX AND NOT CYGWIN)
    set (${HDF_PREFIX}_HAVE_GETCONSOLESCREENBUFFERINFO 1)
    set (${HDF_PREFIX}_GETTIMEOFDAY_GIVES_TZ 1)
    set (${HDF_PREFIX}_HAVE_TIMEZONE 1)
    set (${HDF_PREFIX}_HAVE_GETTIMEOFDAY 1)
    set (${HDF_PREFIX}_HAVE_LIBWS2_32 1)
    set (${HDF_PREFIX}_HAVE_LIBWSOCK32 1)
  endif ()
endif ()

# ----------------------------------------------------------------------
# END of WINDOWS Hard code Values
# ----------------------------------------------------------------------

if (NOT WINDOWS)
  TEST_BIG_ENDIAN (${HDF_PREFIX}_WORDS_BIGENDIAN)
endif ()

#-----------------------------------------------------------------------------
# Check IF header file exists and add it to the list.
#-----------------------------------------------------------------------------
macro (CHECK_INCLUDE_FILE_CONCAT FILE VARIABLE)
  CHECK_INCLUDE_FILES ("${USE_INCLUDES};${FILE}" ${VARIABLE})
  if (${VARIABLE})
    set (USE_INCLUDES ${USE_INCLUDES} ${FILE})
  endif ()
endmacro ()

#-----------------------------------------------------------------------------
#  Check for the existence of certain header files
#-----------------------------------------------------------------------------
CHECK_INCLUDE_FILE_CONCAT ("sys/file.h"      ${HDF_PREFIX}_HAVE_SYS_FILE_H)
CHECK_INCLUDE_FILE_CONCAT ("sys/ioctl.h"     ${HDF_PREFIX}_HAVE_SYS_IOCTL_H)
CHECK_INCLUDE_FILE_CONCAT ("sys/resource.h"  ${HDF_PREFIX}_HAVE_SYS_RESOURCE_H)
CHECK_INCLUDE_FILE_CONCAT ("sys/socket.h"    ${HDF_PREFIX}_HAVE_SYS_SOCKET_H)
CHECK_INCLUDE_FILE_CONCAT ("sys/stat.h"      ${HDF_PREFIX}_HAVE_SYS_STAT_H)
CHECK_INCLUDE_FILE_CONCAT ("sys/time.h"      ${HDF_PREFIX}_HAVE_SYS_TIME_H)
CHECK_INCLUDE_FILE_CONCAT ("sys/types.h"     ${HDF_PREFIX}_HAVE_SYS_TYPES_H)
CHECK_INCLUDE_FILE_CONCAT ("features.h"      ${HDF_PREFIX}_HAVE_FEATURES_H)
CHECK_INCLUDE_FILE_CONCAT ("dirent.h"        ${HDF_PREFIX}_HAVE_DIRENT_H)
CHECK_INCLUDE_FILE_CONCAT ("unistd.h"        ${HDF_PREFIX}_HAVE_UNISTD_H)
CHECK_INCLUDE_FILE_CONCAT ("pwd.h"           ${HDF_PREFIX}_HAVE_PWD_H)
CHECK_INCLUDE_FILE_CONCAT ("globus/common.h" ${HDF_PREFIX}_HAVE_GLOBUS_COMMON_H)
CHECK_INCLUDE_FILE_CONCAT ("pdb.h"           ${HDF_PREFIX}_HAVE_PDB_H)
CHECK_INCLUDE_FILE_CONCAT ("pthread.h"       ${HDF_PREFIX}_HAVE_PTHREAD_H)
CHECK_INCLUDE_FILE_CONCAT ("srbclient.h"     ${HDF_PREFIX}_HAVE_SRBCLIENT_H)
CHECK_INCLUDE_FILE_CONCAT ("dlfcn.h"         ${HDF_PREFIX}_HAVE_DLFCN_H)
CHECK_INCLUDE_FILE_CONCAT ("netinet/in.h"    ${HDF_PREFIX}_HAVE_NETINET_IN_H)
CHECK_INCLUDE_FILE_CONCAT ("netdb.h"         ${HDF_PREFIX}_HAVE_NETDB_H)
CHECK_INCLUDE_FILE_CONCAT ("arpa/inet.h"     ${HDF_PREFIX}_HAVE_ARPA_INET_H)
if (WINDOWS)
  CHECK_INCLUDE_FILE_CONCAT ("shlwapi.h"         ${HDF_PREFIX}_HAVE_SHLWAPI_H)
  # Checking for StrStrIA in the library is not reliable for mingw32 to stdcall
  set (LINK_LIBS ${LINK_LIBS} "shlwapi")
endif ()

## Check for non-standard extension quadmath.h

CHECK_INCLUDE_FILES(quadmath.h C_HAVE_QUADMATH)
if (C_HAVE_QUADMATH)
  set(${HDF_PREFIX}_HAVE_QUADMATH_H 1)
else ()
  set(${HDF_PREFIX}_HAVE_QUADMATH_H 0)
endif ()

if (CYGWIN)
  set (${HDF_PREFIX}_HAVE_LSEEK64 0)
endif ()

#-----------------------------------------------------------------------------
#  Check for the math library "m"
#-----------------------------------------------------------------------------
if (MINGW OR NOT WINDOWS)
  CHECK_LIBRARY_EXISTS_CONCAT ("m" ceil     ${HDF_PREFIX}_HAVE_LIBM)
  CHECK_LIBRARY_EXISTS_CONCAT ("dl" dlopen     ${HDF_PREFIX}_HAVE_LIBDL)
  CHECK_LIBRARY_EXISTS_CONCAT ("ws2_32" WSAStartup  ${HDF_PREFIX}_HAVE_LIBWS2_32)
  CHECK_LIBRARY_EXISTS_CONCAT ("wsock32" gethostbyname ${HDF_PREFIX}_HAVE_LIBWSOCK32)
endif ()

# UCB (BSD) compatibility library
CHECK_LIBRARY_EXISTS_CONCAT ("ucb"    gethostname  ${HDF_PREFIX}_HAVE_LIBUCB)

# For other tests to use the same libraries
set (HDF5_REQUIRED_LIBRARIES ${HDF5_REQUIRED_LIBRARIES} ${LINK_LIBS})

set (USE_INCLUDES "")
if (WINDOWS)
  set (USE_INCLUDES ${USE_INCLUDES} "windows.h")
endif ()

# For other specific tests, use this MACRO.
macro (HDF_FUNCTION_TEST OTHER_TEST)
  if (NOT DEFINED ${HDF_PREFIX}_${OTHER_TEST})
    set (MACRO_CHECK_FUNCTION_DEFINITIONS "-D${OTHER_TEST} ${CMAKE_REQUIRED_FLAGS}")

    foreach (def
        HAVE_SYS_TIME_H
        HAVE_UNISTD_H
        HAVE_SYS_TYPES_H
        HAVE_SYS_SOCKET_H
    )
      if ("${${HDF_PREFIX}_${def}}")
        set (MACRO_CHECK_FUNCTION_DEFINITIONS "${MACRO_CHECK_FUNCTION_DEFINITIONS} -D${def}")
      endif ()
    endforeach ()

    if (LARGEFILE)
      set (MACRO_CHECK_FUNCTION_DEFINITIONS
          "${MACRO_CHECK_FUNCTION_DEFINITIONS} -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE"
      )
    endif ()

    message (TRACE "Performing ${OTHER_TEST}")
    try_compile (${OTHER_TEST}
        ${CMAKE_BINARY_DIR}
        ${HDF_RESOURCES_DIR}/HDFTests.c
        COMPILE_DEFINITIONS "${MACRO_CHECK_FUNCTION_DEFINITIONS}"
        LINK_LIBRARIES "${HDF5_REQUIRED_LIBRARIES}"
        OUTPUT_VARIABLE OUTPUT
    )
    if (${OTHER_TEST})
      set (${HDF_PREFIX}_${OTHER_TEST} 1 CACHE INTERNAL "Other test ${FUNCTION}")
      message (VERBOSE "Performing Other Test ${OTHER_TEST} - Success")
    else ()
      message (VERBOSE "Performing Other Test ${OTHER_TEST} - Failed")
      set (${HDF_PREFIX}_${OTHER_TEST} "" CACHE INTERNAL "Other test ${FUNCTION}")
      file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
          "Performing Other Test ${OTHER_TEST} failed with the following output:\n"
          "${OUTPUT}\n"
      )
    endif ()
  endif ()
endmacro ()

#-----------------------------------------------------------------------------
#  Check for large file support
#-----------------------------------------------------------------------------

# The linux-lfs option is deprecated.
set (LINUX_LFS 0)

set (HDF_EXTRA_C_FLAGS)
set (HDF_EXTRA_FLAGS)
if (MINGW OR NOT WINDOWS)
  if (CMAKE_SYSTEM_NAME MATCHES "Linux")
    # Linux Specific flags
    # This was originally defined as _POSIX_SOURCE which was updated to
    # _POSIX_C_SOURCE=199506L to expose a greater amount of POSIX
    # functionality so clock_gettime and CLOCK_MONOTONIC are defined
    # correctly. This was later updated to 200112L so that
    # posix_memalign() is visible for the direct VFD code on Linux
    # systems.
    # POSIX feature information can be found in the gcc manual at:
    # http://www.gnu.org/s/libc/manual/html_node/Feature-Test-Macros.html
    set (HDF_EXTRA_C_FLAGS -D_POSIX_C_SOURCE=200809L)

    # Need to add this so that O_DIRECT is visible for the direct
    # VFD on Linux systems.
    set (HDF_EXTRA_C_FLAGS ${HDF_EXTRA_C_FLAGS} -D_GNU_SOURCE)

    option (HDF_ENABLE_LARGE_FILE "Enable support for large (64-bit) files on Linux." ON)
    mark_as_advanced (HDF_ENABLE_LARGE_FILE)
    if (HDF_ENABLE_LARGE_FILE AND NOT DEFINED TEST_LFS_WORKS_RUN)
      set (msg "Performing TEST_LFS_WORKS")
      try_run (TEST_LFS_WORKS_RUN   TEST_LFS_WORKS_COMPILE
          ${CMAKE_BINARY_DIR}
          ${HDF_RESOURCES_DIR}/HDFTests.c
          COMPILE_DEFINITIONS "-DTEST_LFS_WORKS"
      )

      # The LARGEFILE definitions were from the transition period
      # and are probably no longer needed. The FILE_OFFSET_BITS
      # check should be generalized for all POSIX systems as it
      # is in the Autotools.
      if (TEST_LFS_WORKS_COMPILE)
        if (TEST_LFS_WORKS_RUN MATCHES 0)
          set (TEST_LFS_WORKS 1 CACHE INTERNAL ${msg})
          set (LARGEFILE 1)
          set (HDF_EXTRA_FLAGS ${HDF_EXTRA_FLAGS} -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE)
          message (VERBOSE "${msg}... yes")
        else ()
          set (TEST_LFS_WORKS "" CACHE INTERNAL ${msg})
          message (VERBOSE "${msg}... no")
          file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
                "Test TEST_LFS_WORKS Run failed with the following exit code:\n ${TEST_LFS_WORKS_RUN}\n"
          )
        endif ()
      else ()
        set (TEST_LFS_WORKS "" CACHE INTERNAL ${msg})
        message (VERBOSE "${msg}... no")
        file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
            "Test TEST_LFS_WORKS Compile failed\n"
        )
      endif ()
    endif ()
    set (CMAKE_REQUIRED_DEFINITIONS ${CMAKE_REQUIRED_DEFINITIONS} ${HDF_EXTRA_FLAGS})
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Check for HAVE_OFF64_T functionality
#-----------------------------------------------------------------------------
if (MINGW OR NOT WINDOWS)
  HDF_FUNCTION_TEST (HAVE_OFF64_T)
  if (${HDF_PREFIX}_HAVE_OFF64_T)
    CHECK_FUNCTION_EXISTS (lseek64            ${HDF_PREFIX}_HAVE_LSEEK64)
  endif ()

  CHECK_FUNCTION_EXISTS (fseeko               ${HDF_PREFIX}_HAVE_FSEEKO)

  CHECK_STRUCT_HAS_MEMBER("struct stat64" st_blocks "sys/types.h;sys/stat.h" HAVE_STAT64_STRUCT)
  if (HAVE_STAT64_STRUCT)
    CHECK_FUNCTION_EXISTS (stat64             ${HDF_PREFIX}_HAVE_STAT64)
  endif ()
endif ()

#-----------------------------------------------------------------------------
#  Check the size in bytes of all the int and float types
#-----------------------------------------------------------------------------
macro (HDF_CHECK_TYPE_SIZE type var)
  set (aType ${type})
  set (aVar  ${var})
  message (TRACE "Checking size of ${aType} and storing into ${aVar}")
  CHECK_TYPE_SIZE (${aType}   ${aVar})
  if (NOT ${aVar})
    set (${aVar} 0 CACHE INTERNAL "SizeOf for ${aType}")
    message (TRACE "Size of ${aType} was NOT Found")
  endif ()
endmacro ()

HDF_CHECK_TYPE_SIZE (char           ${HDF_PREFIX}_SIZEOF_CHAR)
HDF_CHECK_TYPE_SIZE (short          ${HDF_PREFIX}_SIZEOF_SHORT)
HDF_CHECK_TYPE_SIZE (int            ${HDF_PREFIX}_SIZEOF_INT)
HDF_CHECK_TYPE_SIZE (unsigned       ${HDF_PREFIX}_SIZEOF_UNSIGNED)
if (NOT APPLE)
  HDF_CHECK_TYPE_SIZE (long         ${HDF_PREFIX}_SIZEOF_LONG)
endif ()
HDF_CHECK_TYPE_SIZE ("long long"    ${HDF_PREFIX}_SIZEOF_LONG_LONG)

HDF_CHECK_TYPE_SIZE (float          ${HDF_PREFIX}_SIZEOF_FLOAT)
HDF_CHECK_TYPE_SIZE (double         ${HDF_PREFIX}_SIZEOF_DOUBLE)
HDF_CHECK_TYPE_SIZE ("long double"  ${HDF_PREFIX}_SIZEOF_LONG_DOUBLE)

HDF_CHECK_TYPE_SIZE (int8_t         ${HDF_PREFIX}_SIZEOF_INT8_T)
HDF_CHECK_TYPE_SIZE (uint8_t        ${HDF_PREFIX}_SIZEOF_UINT8_T)
HDF_CHECK_TYPE_SIZE (int_least8_t   ${HDF_PREFIX}_SIZEOF_INT_LEAST8_T)
HDF_CHECK_TYPE_SIZE (uint_least8_t  ${HDF_PREFIX}_SIZEOF_UINT_LEAST8_T)
HDF_CHECK_TYPE_SIZE (int_fast8_t    ${HDF_PREFIX}_SIZEOF_INT_FAST8_T)
HDF_CHECK_TYPE_SIZE (uint_fast8_t   ${HDF_PREFIX}_SIZEOF_UINT_FAST8_T)

HDF_CHECK_TYPE_SIZE (int16_t        ${HDF_PREFIX}_SIZEOF_INT16_T)
HDF_CHECK_TYPE_SIZE (uint16_t       ${HDF_PREFIX}_SIZEOF_UINT16_T)
HDF_CHECK_TYPE_SIZE (int_least16_t  ${HDF_PREFIX}_SIZEOF_INT_LEAST16_T)
HDF_CHECK_TYPE_SIZE (uint_least16_t ${HDF_PREFIX}_SIZEOF_UINT_LEAST16_T)
HDF_CHECK_TYPE_SIZE (int_fast16_t   ${HDF_PREFIX}_SIZEOF_INT_FAST16_T)
HDF_CHECK_TYPE_SIZE (uint_fast16_t  ${HDF_PREFIX}_SIZEOF_UINT_FAST16_T)

HDF_CHECK_TYPE_SIZE (int32_t        ${HDF_PREFIX}_SIZEOF_INT32_T)
HDF_CHECK_TYPE_SIZE (uint32_t       ${HDF_PREFIX}_SIZEOF_UINT32_T)
HDF_CHECK_TYPE_SIZE (int_least32_t  ${HDF_PREFIX}_SIZEOF_INT_LEAST32_T)
HDF_CHECK_TYPE_SIZE (uint_least32_t ${HDF_PREFIX}_SIZEOF_UINT_LEAST32_T)
HDF_CHECK_TYPE_SIZE (int_fast32_t   ${HDF_PREFIX}_SIZEOF_INT_FAST32_T)
HDF_CHECK_TYPE_SIZE (uint_fast32_t  ${HDF_PREFIX}_SIZEOF_UINT_FAST32_T)

HDF_CHECK_TYPE_SIZE (int64_t        ${HDF_PREFIX}_SIZEOF_INT64_T)
HDF_CHECK_TYPE_SIZE (uint64_t       ${HDF_PREFIX}_SIZEOF_UINT64_T)
HDF_CHECK_TYPE_SIZE (int_least64_t  ${HDF_PREFIX}_SIZEOF_INT_LEAST64_T)
HDF_CHECK_TYPE_SIZE (uint_least64_t ${HDF_PREFIX}_SIZEOF_UINT_LEAST64_T)
HDF_CHECK_TYPE_SIZE (int_fast64_t   ${HDF_PREFIX}_SIZEOF_INT_FAST64_T)
HDF_CHECK_TYPE_SIZE (uint_fast64_t  ${HDF_PREFIX}_SIZEOF_UINT_FAST64_T)

HDF_CHECK_TYPE_SIZE (size_t       ${HDF_PREFIX}_SIZEOF_SIZE_T)
HDF_CHECK_TYPE_SIZE (ssize_t      ${HDF_PREFIX}_SIZEOF_SSIZE_T)
if (NOT ${HDF_PREFIX}_SIZEOF_SSIZE_T)
  set (${HDF_PREFIX}_SIZEOF_SSIZE_T 0)
endif ()
if (MINGW OR NOT WINDOWS)
  HDF_CHECK_TYPE_SIZE (ptrdiff_t    ${HDF_PREFIX}_SIZEOF_PTRDIFF_T)
endif ()

HDF_CHECK_TYPE_SIZE (off_t          ${HDF_PREFIX}_SIZEOF_OFF_T)
HDF_CHECK_TYPE_SIZE (off64_t        ${HDF_PREFIX}_SIZEOF_OFF64_T)
if (NOT ${HDF_PREFIX}_SIZEOF_OFF64_T)
  set (${HDF_PREFIX}_SIZEOF_OFF64_T 0)
endif ()
HDF_CHECK_TYPE_SIZE (time_t          ${HDF_PREFIX}_SIZEOF_TIME_T)

#-----------------------------------------------------------------------------
# Extra C99 types
#-----------------------------------------------------------------------------

# Size of bool
set (CMAKE_EXTRA_INCLUDE_FILES stdbool.h)
HDF_CHECK_TYPE_SIZE (_Bool        ${HDF_PREFIX}_SIZEOF_BOOL)

if (MINGW OR NOT WINDOWS)
  #-----------------------------------------------------------------------------
  # Check if the dev_t type is a scalar type
  #-----------------------------------------------------------------------------
  HDF_FUNCTION_TEST (DEV_T_IS_SCALAR)

  # ----------------------------------------------------------------------
  # Check for MONOTONIC_TIMER support (used in clock_gettime).  This has
  # to be done after any POSIX/BSD defines to ensure that the test gets
  # the correct POSIX level on linux.
  CHECK_VARIABLE_EXISTS (CLOCK_MONOTONIC HAVE_CLOCK_MONOTONIC)

  #-----------------------------------------------------------------------------
  # Check a bunch of time functions
  #-----------------------------------------------------------------------------
  CHECK_STRUCT_HAS_MEMBER("struct tm" tm_gmtoff "time.h" ${HDF_PREFIX}_HAVE_TM_GMTOFF)
  CHECK_STRUCT_HAS_MEMBER("struct tm" __tm_gmtoff "time.h" ${HDF_PREFIX}_HAVE___TM_GMTOFF)
  if (${HDF_PREFIX}_HAVE_SYS_TIME_H)
    CHECK_STRUCT_HAS_MEMBER("struct tm" tz_minuteswest "sys/types.h;sys/time.h;time.h" ${HDF_PREFIX}_HAVE_STRUCT_TIMEZONE)
  else ()
    CHECK_STRUCT_HAS_MEMBER("struct tm" tz_minuteswest "sys/types.h;time.h" ${HDF_PREFIX}_HAVE_STRUCT_TIMEZONE)
  endif ()
  CHECK_FUNCTION_EXISTS (gettimeofday      ${HDF_PREFIX}_HAVE_GETTIMEOFDAY)
  foreach (time_test
#      HAVE_TIMEZONE
      GETTIMEOFDAY_GIVES_TZ
      HAVE_TM_ZONE
      HAVE_STRUCT_TM_TM_ZONE
  )
    HDF_FUNCTION_TEST (${time_test})
  endforeach ()
  if (NOT CYGWIN AND NOT MINGW)
      HDF_FUNCTION_TEST (HAVE_TIMEZONE)
  endif ()

  # ----------------------------------------------------------------------
  # Does the struct stat have the st_blocks field?  This field is not POSIX.
  #
  CHECK_STRUCT_HAS_MEMBER("struct stat" st_blocks "sys/types.h;sys/stat.h" ${HDF_PREFIX}_HAVE_STAT_ST_BLOCKS)

  # ----------------------------------------------------------------------
  # How do we figure out the width of a tty in characters?
  #
  CHECK_FUNCTION_EXISTS (ioctl             ${HDF_PREFIX}_HAVE_IOCTL)
  CHECK_STRUCT_HAS_MEMBER ("struct videoconfig" numtextcols "" ${HDF_PREFIX}_HAVE_STRUCT_VIDEOCONFIG)
  CHECK_STRUCT_HAS_MEMBER ("struct text_info" screenwidth "" ${HDF_PREFIX}_HAVE_STRUCT_TEXT_INFO)
  CHECK_FUNCTION_EXISTS (_getvideoconfig   ${HDF_PREFIX}_HAVE__GETVIDEOCONFIG)
  CHECK_FUNCTION_EXISTS (gettextinfo       ${HDF_PREFIX}_HAVE_GETTEXTINFO)
  CHECK_FUNCTION_EXISTS (_scrsize          ${HDF_PREFIX}_HAVE__SCRSIZE)
  if (NOT CYGWIN)
    CHECK_FUNCTION_EXISTS (GetConsoleScreenBufferInfo    ${HDF_PREFIX}_HAVE_GETCONSOLESCREENBUFFERINFO)
  endif ()
  CHECK_SYMBOL_EXISTS (TIOCGWINSZ "sys/ioctl.h" ${HDF_PREFIX}_HAVE_TIOCGWINSZ)
  CHECK_SYMBOL_EXISTS (TIOCGETD   "sys/ioctl.h" ${HDF_PREFIX}_HAVE_TIOCGETD)

  # ----------------------------------------------------------------------
  # cygwin user credentials are different then on linux
  #
  if (NOT CYGWIN AND NOT MINGW)
    CHECK_FUNCTION_EXISTS (getpwuid        ${HDF_PREFIX}_HAVE_GETPWUID)
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Check for some functions that are used
#
CHECK_FUNCTION_EXISTS (alarm             ${HDF_PREFIX}_HAVE_ALARM)
CHECK_FUNCTION_EXISTS (fcntl             ${HDF_PREFIX}_HAVE_FCNTL)
CHECK_FUNCTION_EXISTS (flock             ${HDF_PREFIX}_HAVE_FLOCK)
CHECK_FUNCTION_EXISTS (fork              ${HDF_PREFIX}_HAVE_FORK)

CHECK_FUNCTION_EXISTS (gethostname       ${HDF_PREFIX}_HAVE_GETHOSTNAME)
CHECK_FUNCTION_EXISTS (getrusage         ${HDF_PREFIX}_HAVE_GETRUSAGE)

CHECK_FUNCTION_EXISTS (pread             ${HDF_PREFIX}_HAVE_PREAD)
CHECK_FUNCTION_EXISTS (pwrite            ${HDF_PREFIX}_HAVE_PWRITE)
CHECK_FUNCTION_EXISTS (rand_r            ${HDF_PREFIX}_HAVE_RAND_R)
CHECK_FUNCTION_EXISTS (random            ${HDF_PREFIX}_HAVE_RANDOM)

CHECK_FUNCTION_EXISTS (strcasestr        ${HDF_PREFIX}_HAVE_STRCASESTR)
CHECK_FUNCTION_EXISTS (strdup            ${HDF_PREFIX}_HAVE_STRDUP)
CHECK_FUNCTION_EXISTS (symlink           ${HDF_PREFIX}_HAVE_SYMLINK)

CHECK_FUNCTION_EXISTS (tmpfile           ${HDF_PREFIX}_HAVE_TMPFILE)
CHECK_FUNCTION_EXISTS (asprintf          ${HDF_PREFIX}_HAVE_ASPRINTF)
CHECK_FUNCTION_EXISTS (vasprintf         ${HDF_PREFIX}_HAVE_VASPRINTF)
CHECK_FUNCTION_EXISTS (waitpid           ${HDF_PREFIX}_HAVE_WAITPID)

#-----------------------------------------------------------------------------
# sigsetjmp is special; may actually be a macro
#-----------------------------------------------------------------------------
if (NOT ${HDF_PREFIX}_HAVE_SIGSETJMP)
  CHECK_SYMBOL_EXISTS (sigsetjmp "setjmp.h" ${HDF_PREFIX}_HAVE_MACRO_SIGSETJMP)
  if (${HDF_PREFIX}_HAVE_MACRO_SIGSETJMP)
    set (${HDF_PREFIX}_HAVE_SIGSETJMP 1)
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Check a bunch of other functions
#-----------------------------------------------------------------------------
if (MINGW OR NOT WINDOWS)
  foreach (other_test
      HAVE_ATTRIBUTE
      SYSTEM_SCOPE_THREADS
      HAVE_SOCKLEN_T
  )
    HDF_FUNCTION_TEST (${other_test})
  endforeach ()
endif ()

#-----------------------------------------------------------------------------
# Check if InitOnceExecuteOnce is available
#-----------------------------------------------------------------------------
if (WINDOWS)
  if (NOT HDF_NO_IOEO_TEST)
    message (VERBOSE "Checking for InitOnceExecuteOnce:")
  if (NOT DEFINED ${HDF_PREFIX}_HAVE_IOEO)
    if (LARGEFILE)
      set (CMAKE_REQUIRED_DEFINITIONS
          "${CURRENT_TEST_DEFINITIONS} -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE"
      )
    endif ()
    set (MACRO_CHECK_FUNCTION_DEFINITIONS "-DHAVE_IOEO ${CMAKE_REQUIRED_FLAGS}")
    if (CMAKE_REQUIRED_INCLUDES)
      set (CHECK_C_SOURCE_COMPILES_ADD_INCLUDES "-DINCLUDE_DIRECTORIES:STRING=${CMAKE_REQUIRED_INCLUDES}")
    else ()
      set (CHECK_C_SOURCE_COMPILES_ADD_INCLUDES)
    endif ()

    TRY_RUN(HAVE_IOEO_EXITCODE HAVE_IOEO_COMPILED
        ${CMAKE_BINARY_DIR}
        ${HDF_RESOURCES_DIR}/HDFTests.c
        COMPILE_DEFINITIONS "${CMAKE_REQUIRED_DEFINITIONS} ${MACRO_CHECK_FUNCTION_DEFINITIONS}"
        LINK_LIBRARIES "${HDF5_REQUIRED_LIBRARIES}"
        CMAKE_FLAGS "${CHECK_C_SOURCE_COMPILES_ADD_INCLUDES} -DCMAKE_SKIP_RPATH:BOOL=${CMAKE_SKIP_RPATH}"
        COMPILE_OUTPUT_VARIABLE OUTPUT
    )
    # if it did not compile make the return value fail code of 1
    if (NOT HAVE_IOEO_COMPILED)
      set (HAVE_IOEO_EXITCODE 1)
    endif ()
    # if the return value was 0 then it worked
    if ("${HAVE_IOEO_EXITCODE}" EQUAL 0)
      set (${HDF_PREFIX}_HAVE_IOEO 1 CACHE INTERNAL "Test InitOnceExecuteOnce")
      message (VERBOSE "Performing Test InitOnceExecuteOnce - Success")
      file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
        "Performing C SOURCE FILE Test InitOnceExecuteOnce succeeded with the following output:\n"
        "${OUTPUT}\n"
        "Return value: ${HAVE_IOEO}\n")
    else ()
      if (CMAKE_CROSSCOMPILING AND "${HAVE_IOEO_EXITCODE}" MATCHES  "FAILED_TO_RUN")
        set (${HDF_PREFIX}_HAVE_IOEO "${HAVE_IOEO_EXITCODE}")
      else ()
        set (${HDF_PREFIX}_HAVE_IOEO "" CACHE INTERNAL "Test InitOnceExecuteOnce")
      endif ()

      message (VERBOSE "Performing Test InitOnceExecuteOnce - Failed")
      file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
        "Performing InitOnceExecuteOnce Test  failed with the following output:\n"
        "${OUTPUT}\n"
        "Return value: ${HAVE_IOEO_EXITCODE}\n")
    endif ()
  endif ()
  endif ()
endif ()

# ----------------------------------------------------------------------
# Set the flag to indicate that the machine can handle converting
# denormalized floating-point values.
# (This flag should be set for all machines, except for the Crays, where
# the cache value is set in its config file)
#-----------------------------------------------------------------------------
set (${HDF_PREFIX}_CONVERT_DENORMAL_FLOAT 1)

# ----------------------------------------------------------------------
# Set a special flag when using memory sanity checkers like Valgrind.
# This disables the free lists, as the memory reuse scheme they implement
# can hide memory problems.
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_USING_MEMCHECKER)
  set (${HDF_PREFIX}_USING_MEMCHECKER 1)
endif ()

#-----------------------------------------------------------------------------
# Option for --enable-strict-format-checks
#-----------------------------------------------------------------------------
option (HDF5_STRICT_FORMAT_CHECKS "Whether to perform strict file format checks" OFF)
mark_as_advanced (HDF5_STRICT_FORMAT_CHECKS)
if (HDF5_STRICT_FORMAT_CHECKS)
  set (${HDF_PREFIX}_STRICT_FORMAT_CHECKS 1)
endif ()
MARK_AS_ADVANCED (HDF5_STRICT_FORMAT_CHECKS)

# ----------------------------------------------------------------------
# Decide whether the data accuracy has higher priority during data
# conversions.  If not, some hard conversions will still be preferred even
# though the data may be wrong (for example, some compilers don't
# support denormalized floating values) to maximize speed.
#-----------------------------------------------------------------------------
option (HDF5_WANT_DATA_ACCURACY "IF data accuracy is guaranteed during data conversions" ON)
mark_as_advanced (HDF5_WANT_DATA_ACCURACY)
if (HDF5_WANT_DATA_ACCURACY)
  set (${HDF_PREFIX}_WANT_DATA_ACCURACY 1)
endif ()
MARK_AS_ADVANCED (HDF5_WANT_DATA_ACCURACY)

# ----------------------------------------------------------------------
# Decide whether the presence of user's exception handling functions is
# checked and data conversion exceptions are returned.  This is mainly
# for the speed optimization of hard conversions.  Soft conversions can
# actually benefit little.
#-----------------------------------------------------------------------------
option (HDF5_WANT_DCONV_EXCEPTION "exception handling functions is checked during data conversions" ON)
mark_as_advanced (HDF5_WANT_DCONV_EXCEPTION)
if (HDF5_WANT_DCONV_EXCEPTION)
  set (${HDF_PREFIX}_WANT_DCONV_EXCEPTION 1)
endif ()
MARK_AS_ADVANCED (HDF5_WANT_DCONV_EXCEPTION)

# ----------------------------------------------------------------------
# Check if they would like the function stack support compiled in
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_CODESTACK "Enable the function stack tracing (for developer debugging)." OFF)
mark_as_advanced (HDF5_ENABLE_CODESTACK)
if (HDF5_ENABLE_CODESTACK)
  set (${HDF_PREFIX}_HAVE_CODESTACK 1)
endif ()
MARK_AS_ADVANCED (HDF5_ENABLE_CODESTACK)

# ----------------------------------------------------------------------
# Check if they would like to use file locking by default
#-----------------------------------------------------------------------------
option (HDF5_USE_FILE_LOCKING "Use file locking by default (mainly for SWMR)" ON)
if (HDF5_USE_FILE_LOCKING)
  set (${HDF_PREFIX}_USE_FILE_LOCKING 1)
endif ()

# ----------------------------------------------------------------------
# Check if they would like to ignore file locks when disabled on a file system
#-----------------------------------------------------------------------------
option (HDF5_IGNORE_DISABLED_FILE_LOCKS "Ignore file locks when disabled on file system" ON)
if (HDF5_IGNORE_DISABLED_FILE_LOCKS)
  set (${HDF_PREFIX}_IGNORE_DISABLED_FILE_LOCKS 1)
endif ()

# Set the libhdf5.settings file variable
if (HDF5_IGNORE_DISABLED_FILE_LOCKS AND HDF5_USE_FILE_LOCKING)
  set (HDF5_FILE_LOCKING_SETTING "best-effort")
elseif (HDF5_IGNORE_DISABLED_FILE_LOCKS)
  set (HDF5_FILE_LOCKING_SETTING "yes")
else ()
  set (HDF5_FILE_LOCKING_SETTING "no")
endif ()

# so far we have no check for this
set (${HDF_PREFIX}_HAVE_TMPFILE 1)

# TODO --------------------------------------------------------------------------
# Should the Default Virtual File Driver be compiled?
# This is hard-coded now but option should added to match configure
#-----------------------------------------------------------------------------
set (${HDF_PREFIX}_DEFAULT_VFD H5FD_SEC2)

if (NOT DEFINED "${HDF_PREFIX}_DEFAULT_PLUGINDIR")
  if (WINDOWS)
    set (${HDF_PREFIX}_DEFAULT_PLUGINDIR "%ALLUSERSPROFILE%\\\\hdf5\\\\lib\\\\plugin")
  else ()
    set (${HDF_PREFIX}_DEFAULT_PLUGINDIR "/usr/local/hdf5/lib/plugin")
  endif ()
endif ()

if (WINDOWS)
  set (${HDF_PREFIX}_HAVE_WINDOWS 1)
  # ----------------------------------------------------------------------
  # Set the flag to indicate that the machine has window style pathname,
  # that is, "drive-letter:\" (e.g. "C:") or "drive-letter:/" (e.g. "C:/").
  # (This flag should be _unset_ for all machines, except for Windows)
  #-----------------------------------------------------------------------
  set (${HDF_PREFIX}_HAVE_WINDOW_PATH 1)
endif ()

# ----------------------------------------------------------------------
# END of WINDOWS Hard code Values
# ----------------------------------------------------------------------

# Find the library containing clock_gettime()
if (MINGW OR NOT WINDOWS)
  CHECK_FUNCTION_EXISTS (clock_gettime CLOCK_GETTIME_IN_LIBC)
  CHECK_LIBRARY_EXISTS (rt clock_gettime "" CLOCK_GETTIME_IN_LIBRT)
  CHECK_LIBRARY_EXISTS (posix4 clock_gettime "" CLOCK_GETTIME_IN_LIBPOSIX4)
  if (CLOCK_GETTIME_IN_LIBC)
    set (${HDF_PREFIX}_HAVE_CLOCK_GETTIME 1)
  elseif (CLOCK_GETTIME_IN_LIBRT)
    set (${HDF_PREFIX}_HAVE_CLOCK_GETTIME 1)
    list (APPEND LINK_LIBS rt)
  elseif (CLOCK_GETTIME_IN_LIBPOSIX4)
    set (${HDF_PREFIX}_HAVE_CLOCK_GETTIME 1)
    list (APPEND LINK_LIBS posix4)
  endif ()
endif ()

# Check for clock_gettime() CLOCK_MONOTONIC_COARSE
set (CMAKE_EXTRA_INCLUDE_FILES time.h)
check_type_size(CLOCK_MONOTONIC_COARSE CLOCK_MONOTONIC_COARSE_SIZE)
if (HAVE_CLOCK_MONOTONIC_COARSE_SIZE)
  set (${HDF_PREFIX}_HAVE_CLOCK_MONOTONIC_COARSE 1)
endif ()
unset (CMAKE_EXTRA_INCLUDE_FILES)

#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#  Check if Direct I/O driver works
#-----------------------------------------------------------------------------
if (NOT WINDOWS)
  option (HDF5_ENABLE_DIRECT_VFD "Build the Direct I/O Virtual File Driver" OFF)
  if (HDF5_ENABLE_DIRECT_VFD)
    set (msg "Performing TEST_DIRECT_VFD_WORKS")
    set (MACRO_CHECK_FUNCTION_DEFINITIONS "-DTEST_DIRECT_VFD_WORKS -D_GNU_SOURCE ${CMAKE_REQUIRED_FLAGS}")
    TRY_RUN (TEST_DIRECT_VFD_WORKS_RUN   TEST_DIRECT_VFD_WORKS_COMPILE
        ${CMAKE_BINARY_DIR}
        ${HDF_RESOURCES_DIR}/HDFTests.c
        CMAKE_FLAGS -DCOMPILE_DEFINITIONS:STRING=${MACRO_CHECK_FUNCTION_DEFINITIONS}
        OUTPUT_VARIABLE OUTPUT
    )
    if (TEST_DIRECT_VFD_WORKS_COMPILE)
      if (TEST_DIRECT_VFD_WORKS_RUN EQUAL "0")
        HDF_FUNCTION_TEST (HAVE_DIRECT)
        set (CMAKE_REQUIRED_DEFINITIONS "${CMAKE_REQUIRED_DEFINITIONS} -D_GNU_SOURCE")
        add_definitions ("-D_GNU_SOURCE")
      else ()
        set (TEST_DIRECT_VFD_WORKS "" CACHE INTERNAL ${msg})
        message (VERBOSE "${msg}... no")
        file (APPEND ${CMAKE_BINARY_DIR}/CMakeFiles/CMakeError.log
              "Test TEST_DIRECT_VFD_WORKS Run failed with the following output and exit code:\n ${OUTPUT}\n"
        )
      endif ()
    else ()
      set (TEST_DIRECT_VFD_WORKS "" CACHE INTERNAL ${msg})
      message (VERBOSE "${msg}... no")
      file (APPEND ${CMAKE_BINARY_DIR}/CMakeFiles/CMakeError.log
          "Test TEST_DIRECT_VFD_WORKS Compile failed with the following output:\n ${OUTPUT}\n"
      )
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
#  Check if ROS3 driver can be built
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_ROS3_VFD "Build the ROS3 Virtual File Driver" OFF)
  if (HDF5_ENABLE_ROS3_VFD)
    find_package(CURL REQUIRED)
    find_package(OpenSSL REQUIRED)
    if (${CURL_FOUND} AND ${OPENSSL_FOUND})
      set (${HDF_PREFIX}_HAVE_ROS3_VFD 1)
      list (APPEND LINK_LIBS ${CURL_LIBRARIES} ${OPENSSL_LIBRARIES})
      INCLUDE_DIRECTORIES (${CURL_INCLUDE_DIRS} ${OPENSSL_INCLUDE_DIR})
    else ()
      message (WARNING "The Read-Only S3 VFD was requested but cannot be built.\nPlease check that openssl and cURL are available on your\nsystem, and/or re-configure without option HDF5_ENABLE_ROS3_VFD.")
    endif ()
endif ()

# ----------------------------------------------------------------------
# Check whether we can build the Mirror VFD
# ----------------------------------------------------------------------
option (HDF5_ENABLE_MIRROR_VFD "Build the Mirror Virtual File Driver" OFF)
if (HDF5_ENABLE_MIRROR_VFD)
  if ( ${HDF_PREFIX}_HAVE_NETINET_IN_H AND
       ${HDF_PREFIX}_HAVE_NETDB_H      AND
       ${HDF_PREFIX}_HAVE_ARPA_INET_H  AND
       ${HDF_PREFIX}_HAVE_SYS_SOCKET_H AND
       ${HDF_PREFIX}_HAVE_FORK)
      set (${HDF_PREFIX}_HAVE_MIRROR_VFD 1)
  else()
    message(WARNING "The socket-based Mirror VFD was requested but cannot be built. System prerequisites are not met.")
  endif()
endif()

#-----------------------------------------------------------------------------
# Check if C has __float128 extension (used for Fortran only)
#-----------------------------------------------------------------------------

if (HDF5_BUILD_FORTRAN)
  HDF_CHECK_TYPE_SIZE(__float128 _SIZEOF___FLOAT128)
  if (_SIZEOF___FLOAT128)
    set (${HDF_PREFIX}_HAVE_FLOAT128 1)
    set (${HDF_PREFIX}_SIZEOF___FLOAT128 ${_SIZEOF___FLOAT128})
  else ()
    set (${HDF_PREFIX}_HAVE_FLOAT128 0)
    set (${HDF_PREFIX}_SIZEOF___FLOAT128 0)
  endif ()

  HDF_CHECK_TYPE_SIZE(_Quad _SIZEOF__QUAD)
  if (NOT _SIZEOF__QUAD)
    set (${HDF_PREFIX}_SIZEOF__QUAD 0)
  else ()
    set (${HDF_PREFIX}_SIZEOF__QUAD ${_SIZEOF__QUAD})
  endif ()

  if (NOT CMAKE_CROSSCOMPILING)
    #-----------------------------------------------------------------------------
    # The provided CMake C macros don't provide a general compile/run function
    # so this one is used.
    #-----------------------------------------------------------------------------
    set (RUN_OUTPUT_PATH_DEFAULT ${CMAKE_BINARY_DIR})
    macro (C_RUN FUNCTION_NAME SOURCE_CODE RETURN_VAR RETURN_OUTPUT_VAR)
        message (VERBOSE "Detecting C ${FUNCTION_NAME}")
        file (WRITE
            ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testCCompiler1.c
            ${SOURCE_CODE}
        )
        if (CMAKE_VERSION VERSION_LESS 3.25)
          set (_RUN_OUTPUT_VARIABLE "RUN_OUTPUT_VARIABLE")
        else ()
          set (_RUN_OUTPUT_VARIABLE  "RUN_OUTPUT_STDOUT_VARIABLE")
        endif()
        TRY_RUN (RUN_RESULT_VAR COMPILE_RESULT_VAR
            ${CMAKE_BINARY_DIR}
            ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testCCompiler1.c
            COMPILE_DEFINITIONS "-D_SIZEOF___FLOAT128=${H5_SIZEOF___FLOAT128};-D_HAVE_QUADMATH_H=${H5_HAVE_QUADMATH_H}"
            COMPILE_OUTPUT_VARIABLE COMPILEOUT
            ${_RUN_OUTPUT_VARIABLE} OUTPUT_VAR
        )

        set (${RETURN_OUTPUT_VAR} ${OUTPUT_VAR})

        message (VERBOSE "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ")
        message (VERBOSE "Test COMPILE_RESULT_VAR ${COMPILE_RESULT_VAR} ")
        message (VERBOSE "Test COMPILE_OUTPUT ${COMPILEOUT} ")
        message (VERBOSE "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ")
        message (VERBOSE "Test RUN_RESULT_VAR ${RUN_RESULT_VAR} ")
        message (VERBOSE "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ")

        if (COMPILE_RESULT_VAR)
          if (RUN_RESULT_VAR EQUAL "0")
            set (${RETURN_VAR} 1 CACHE INTERNAL "Have C function ${FUNCTION_NAME}")
            message (VERBOSE "Testing C ${FUNCTION_NAME} - OK")
            file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
                "Determining if the C ${FUNCTION_NAME} exists passed with the following output:\n"
                "${OUTPUT_VAR}\n\n"
            )
          else ()
            message (VERBOSE "Testing C ${FUNCTION_NAME} - Fail")
            set (${RETURN_VAR} 0 CACHE INTERNAL "Have C function ${FUNCTION_NAME}")
            file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
                "Determining if the C ${FUNCTION_NAME} exists failed with the following output:\n"
                "${OUTPUT_VAR}\n\n")
          endif ()
        else ()
            message (FATAL_ERROR "Compilation of C ${FUNCTION_NAME} - Failed")
        endif ()
    endmacro ()
    set (PROG_SRC
        "
#include <float.h>\n\
#include <stdio.h>\n\
#define CHECK_FLOAT128 _SIZEOF___FLOAT128\n\
#if CHECK_FLOAT128!=0\n\
#if _HAVE_QUADMATH_H!=0\n\
#include <quadmath.h>\n\
#endif\n\
#ifdef FLT128_DIG\n\
#define C_FLT128_DIG FLT128_DIG\n\
#else\n\
#define C_FLT128_DIG 0\n\
#endif\n\
#else\n\
#define C_FLT128_DIG 0\n\
#endif\n\
#if defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L\n\
#define C_LDBL_DIG DECIMAL_DIG\n\
#else\n\
#define C_LDBL_DIG LDBL_DIG\n\
#endif\n\nint main(void) {\nprintf(\"\\%d\\\;\\%d\\\;\", C_LDBL_DIG, C_FLT128_DIG)\\\;\n\nreturn 0\\\;\n}\n
         "
    )

    C_RUN ("maximum decimal precision for C" ${PROG_SRC} PROG_RES PROG_OUTPUT4)
    message (STATUS "Testing maximum decimal precision for C - ${PROG_OUTPUT4}")

    # dnl The output from the above program will be:
    # dnl  -- long double decimal precision  --  __float128 decimal precision

    list (GET PROG_OUTPUT4 0 H5_LDBL_DIG)
    list (GET PROG_OUTPUT4 1 H5_FLT128_DIG)

    if (${HDF_PREFIX}_SIZEOF___FLOAT128 EQUAL "0" OR FLT128_DIG EQUAL "0")
      set (${HDF_PREFIX}_HAVE_FLOAT128 0)
      set (${HDF_PREFIX}_SIZEOF___FLOAT128 0)
      set (_PAC_C_MAX_REAL_PRECISION ${H5_LDBL_DIG})
    else ()
      set (_PAC_C_MAX_REAL_PRECISION ${H5_FLT128_DIG})
    endif ()
    if (NOT ${_PAC_C_MAX_REAL_PRECISION})
      set (${HDF_PREFIX}_PAC_C_MAX_REAL_PRECISION 0)
    else ()
      set (${HDF_PREFIX}_PAC_C_MAX_REAL_PRECISION ${_PAC_C_MAX_REAL_PRECISION})
    endif ()
    message (STATUS "maximum decimal precision for C var - ${${HDF_PREFIX}_PAC_C_MAX_REAL_PRECISION}")
  else ()
    set (${HDF_PREFIX}_PAC_C_MAX_REAL_PRECISION 0)
  endif ()

endif()

#-----------------------------------------------------------------------------
# Macro to determine the various conversion capabilities
#-----------------------------------------------------------------------------
macro (H5ConversionTests TEST msg)
  if (NOT DEFINED ${TEST})
    TRY_RUN (${TEST}_RUN   ${TEST}_COMPILE
        ${CMAKE_BINARY_DIR}
        ${HDF_RESOURCES_DIR}/ConversionTests.c
        CMAKE_FLAGS -DCOMPILE_DEFINITIONS:STRING=-D${TEST}_TEST
        OUTPUT_VARIABLE OUTPUT
    )
    if (${TEST}_COMPILE)
      if (${TEST}_RUN EQUAL "0")
        set (${TEST} 1 CACHE INTERNAL ${msg})
        message (VERBOSE "${msg}... yes")
      else ()
        set (${TEST} "" CACHE INTERNAL ${msg})
        message (VERBOSE "${msg}... no")
        file (APPEND ${CMAKE_BINARY_DIR}/CMakeFiles/CMakeError.log
              "Test ${TEST} Run failed with the following output and exit code:\n ${OUTPUT}\n"
        )
      endif ()
    else ()
      set (${TEST} "" CACHE INTERNAL ${msg})
      message (VERBOSE "${msg}... no")
      file (APPEND ${CMAKE_BINARY_DIR}/CMakeFiles/CMakeError.log
          "Test ${TEST} Compile failed with the following output:\n ${OUTPUT}\n"
      )
    endif ()

  endif ()
endmacro ()

#-----------------------------------------------------------------------------
# Check various conversion capabilities
#-----------------------------------------------------------------------------

# ----------------------------------------------------------------------
# Set the flag to indicate that the machine is using a special algorithm toconvert
# 'long double' to '(unsigned) long' values.  (This flag should only be set for
# the IBM Power Linux.  When the bit sequence of long double is
# 0x4351ccf385ebc8a0bfcc2a3c3d855620, the converted value of (unsigned)long
# is 0x004733ce17af227f, not the same as the library's conversion to 0x004733ce17af2282.
# The machine's conversion gets the correct value.  We define the macro and disable
# this kind of test until we figure out what algorithm they use.
#-----------------------------------------------------------------------------
H5ConversionTests (${HDF_PREFIX}_LDOUBLE_TO_LONG_SPECIAL  "Checking IF your system converts long double to (unsigned) long values with special algorithm")
# ----------------------------------------------------------------------
# Set the flag to indicate that the machine is using a special algorithm
# to convert some values of '(unsigned) long' to 'long double' values.
# (This flag should be off for all machines, except for IBM Power Linux,
# when the bit sequences are 003fff..., 007fff..., 00ffff..., 01ffff...,
# ..., 7fffff..., the compiler uses a unknown algorithm.  We define a
# macro and skip the test for now until we know about the algorithm.
#-----------------------------------------------------------------------------
H5ConversionTests (${HDF_PREFIX}_LONG_TO_LDOUBLE_SPECIAL "Checking IF your system can convert (unsigned) long to long double values with special algorithm")
# ----------------------------------------------------------------------
# Set the flag to indicate that the machine can accurately convert
# 'long double' to '(unsigned) long long' values.  (This flag should be set for
# all machines, except for Mac OS 10.4 and SGI IRIX64 6.5.  When the bit sequence
# of long double is 0x4351ccf385ebc8a0bfcc2a3c..., the values of (unsigned)long long
# start to go wrong on these two machines.  Adjusting it higher to
# 0x4351ccf385ebc8a0dfcc... or 0x4351ccf385ebc8a0ffcc... will make the converted
# values wildly wrong.  This test detects this wrong behavior and disable the test.
#-----------------------------------------------------------------------------
H5ConversionTests (${HDF_PREFIX}_LDOUBLE_TO_LLONG_ACCURATE "Checking IF correctly converting long double to (unsigned) long long values")
# ----------------------------------------------------------------------
# Set the flag to indicate that the machine can accurately convert
# '(unsigned) long long' to 'long double' values.  (This flag should be set for
# all machines, except for Mac OS 10.4, when the bit sequences are 003fff...,
# 007fff..., 00ffff..., 01ffff..., ..., 7fffff..., the converted values are twice
# as big as they should be.
#-----------------------------------------------------------------------------
H5ConversionTests (${HDF_PREFIX}_LLONG_TO_LDOUBLE_CORRECT "Checking IF correctly converting (unsigned) long long to long double values")
# ----------------------------------------------------------------------
# Set the flag to indicate that the machine can accurately convert
# some long double values
#-----------------------------------------------------------------------------
H5ConversionTests (${HDF_PREFIX}_DISABLE_SOME_LDOUBLE_CONV "Checking IF the cpu is power9 and cannot correctly converting long double values")
