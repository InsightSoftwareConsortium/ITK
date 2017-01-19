#-----------------------------------------------------------------------------
# Compiler specific flags : Shouldn't there be compiler tests for these
#-----------------------------------------------------------------------------
if (CMAKE_COMPILER_IS_GNUCC)
  if (CMAKE_BUILD_TYPE MATCHES Debug)
    set (CMAKE_C_FLAGS "${CMAKE_ANSI_CFLAGS} ${CMAKE_C_FLAGS} -std=c99")
    if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 5.0)
      set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Og -ftrapv -fno-common")
    endif ()
  else (CMAKE_BUILD_TYPE MATCHES Debug)
    set (CMAKE_C_FLAGS "${CMAKE_ANSI_CFLAGS} ${CMAKE_C_FLAGS} -std=c99")
    if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 5.0)
      set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fstdarg-opt")
    endif ()
  endif (CMAKE_BUILD_TYPE MATCHES Debug)
endif (CMAKE_COMPILER_IS_GNUCC)
if (CMAKE_COMPILER_IS_GNUCXX)
  if (CMAKE_BUILD_TYPE MATCHES Debug)
    set (CMAKE_CXX_FLAGS "${CMAKE_ANSI_CFLAGS} ${CMAKE_CXX_FLAGS}")
    if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 5.0)
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Og -ftrapv -fno-common")
    endif ()
  else (CMAKE_BUILD_TYPE MATCHES Debug)
    set (CMAKE_CXX_FLAGS "${CMAKE_ANSI_CFLAGS} ${CMAKE_CXX_FLAGS}")
    if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 5.0)
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fstdarg-opt")
    endif ()
  endif (CMAKE_BUILD_TYPE MATCHES Debug)
endif (CMAKE_COMPILER_IS_GNUCXX)

#-----------------------------------------------------------------------------
# Option to allow the user to disable compiler warnings
#-----------------------------------------------------------------------------
option (HDF5_DISABLE_COMPILER_WARNINGS "Disable compiler warnings" OFF)
if (HDF5_DISABLE_COMPILER_WARNINGS)
  # MSVC uses /w to suppress warnings.  It also complains if another
  # warning level is given, so remove it.
  if (MSVC)
    set (HDF5_WARNINGS_BLOCKED 1)
    string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /w")
    string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /w")
  endif (MSVC)
  if (WIN32)
    add_definitions (-D_CRT_SECURE_NO_WARNINGS)
  endif (WIN32)
  # Borland uses -w- to suppress warnings.
  if (BORLAND)
    set (HDF5_WARNINGS_BLOCKED 1)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w-")
  endif (BORLAND)

  # Most compilers use -w to suppress warnings.
  if (NOT HDF5_WARNINGS_BLOCKED)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w")
  endif (NOT HDF5_WARNINGS_BLOCKED)
endif (HDF5_DISABLE_COMPILER_WARNINGS)

#-----------------------------------------------------------------------------
# CDash is configured to only allow 3000 warnings, so
# break into groups (from the config/gnu-flags file)
#-----------------------------------------------------------------------------
if (NOT MSVC AND CMAKE_COMPILER_IS_GNUCC)
  if (NOT ${CMAKE_SYSTEM_NAME} MATCHES "SunOS")
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wundef -Wshadow -Wpointer-arith -Wbad-function-cast -Wcast-qual -Wcast-align -Wwrite-strings -Wconversion -Waggregate-return -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wredundant-decls -Wnested-externs -Winline")
  else (NOT ${CMAKE_SYSTEM_NAME} MATCHES "SunOS")
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -erroff=%none -DBSD_COMP")
  endif (NOT ${CMAKE_SYSTEM_NAME} MATCHES "SunOS")
    # Append warning flags
    # Don't use the '-Wtraditional' flag, we're way past having K&R C code
    # set (H5_CFLAGS "${H5_CFLAGS} -Wtraditional")
    # Don't use the '-Wtraditional-conversion' flag, there's too many warnings
    #  from GCC's assert macro
    # set (H5_CFLAGS "${H5_CFLAGS} -Wtraditional-conversion")

    # Append warning flags from gcc-3* case
    # (don't use -Wpadded flag for normal builds, many of the warnings its
    #   issuing can't be fixed and they are making it hard to detect other,
    #   more important warnings)
    #set (H5_CFLAGS "${H5_CFLAGS} -Wfloat-equal -Wmissing-format-attribute -Wpadded")
    set (H5_CFLAGS1 "${H5_CFLAGS1} -Wfloat-equal -Wmissing-format-attribute")

    # Append warning flags from gcc-3.2* case
    set (H5_CFLAGS1 "${H5_CFLAGS1} -Wmissing-noreturn -Wpacked -Wdisabled-optimization")

    # Enable more format checking flags, beyond the basic -Wformat included
    # in -Wall
    set (H5_CFLAGS1 "${H5_CFLAGS1} -Wformat=2")

    # The "unreachable code" warning appears to be reliable now...
    # (this warning was removed in gcc 4.5+)
    if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 4.7)
      set (H5_CFLAGS1 "${H5_CFLAGS1} -Wunreachable-code")
    endif()

    # Append warning flags from gcc-3.3* case
    set (H5_CFLAGS1 "${H5_CFLAGS1} -Wendif-labels")

    # Append warning flags from gcc-3.4* case
    set (H5_CFLAGS2 "${H5_CFLAGS2} -Wdeclaration-after-statement -Wold-style-definition -Winvalid-pch")

    # Append more extra warning flags that only gcc4.0+ know about
    set (H5_CFLAGS2 "${H5_CFLAGS2} -Wvariadic-macros -Winit-self -Wmissing-include-dirs -Wswitch-default -Wswitch-enum -Wunused-macros")

    # Append more extra warning flags that only gcc 4.1+ know about
    set (H5_CFLAGS3 "${H5_CFLAGS3} -Wunsafe-loop-optimizations -Wc++-compat")

    # Append more extra warning flags that only gcc 4.2+ know about
    set (H5_CFLAGS3 "${H5_CFLAGS3} -Wstrict-overflow")

    # Append more extra warning flags that only gcc 4.3+ know about
    #
    # Technically, variable-length arrays are part of the C99 standard, but
    #   we should approach them a bit cautiously... -QAK
    set (H5_CFLAGS3 "${H5_CFLAGS3} -Wlogical-op -Wlarger-than=2048 -Wvla")

    # Append more extra warning flags that only gcc 4.4+ know about
    set (H5_CFLAGS4 "${H5_CFLAGS4} -Wsync-nand -Wframe-larger-than=16384 -Wpacked-bitfield-compat")

    # Append more extra warning flags that only gcc 4.5+ know about
    if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 4.5)
      set (H5_CFLAGS4 "${H5_CFLAGS4} -Wstrict-overflow=5 -Wjump-misses-init -Wunsuffixed-float-constants")
    endif()

    # Append more extra warning flags that only gcc 4.6+ know about
    if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 4.6)
      set (H5_CFLAGS5 "${H5_CFLAGS5} -Wdouble-promotion -Wsuggest-attribute=const -Wtrampolines")
    endif()

    # Append more extra warning flags that only gcc 4.7+ know about
    if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 4.7)
      set (H5_CFLAGS5 "${H5_CFLAGS5} -Wstack-usage=8192 -Wvector-operation-performance -Wsuggest-attribute=pure -Wsuggest-attribute=noreturn")
    endif()

    # Append more extra warning flags that only gcc 4.8+ know about
    if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 4.8)
      set (H5_CFLAGS5 "${H5_CFLAGS5} -Wsuggest-attribute=format")
    endif()

    # Append more extra warning flags that only gcc 4.9+ know about
    if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 4.9)
      set (H5_CFLAGS5 "${H5_CFLAGS5} -Wdate-time -Wopenmp-simd")
    endif()

    # (There was no release of gcc 5.0)

    # Append more extra warning flags that only gcc 5.1+ know about
    if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 5.1)
      set (H5_CFLAGS6 "${H5_CFLAGS6} -Warray-bounds=2 -Wc99-c11-compat")
    endif()

endif (NOT MSVC AND CMAKE_COMPILER_IS_GNUCC)

#-----------------------------------------------------------------------------
# Option to allow the user to enable all warnings
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_ALL_WARNINGS "Enable all warnings" OFF)
if (HDF5_ENABLE_ALL_WARNINGS)
  if (MSVC)
    string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /Wall")
    string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Wall")
  else (MSVC)
    if (CMAKE_COMPILER_IS_GNUCC)
      set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -pedantic ${H5_CFLAGS1} ${H5_CFLAGS2} ${H5_CFLAGS3} ${H5_CFLAGS4}")
    endif (CMAKE_COMPILER_IS_GNUCC)
  endif (MSVC)
endif (HDF5_ENABLE_ALL_WARNINGS)

#-----------------------------------------------------------------------------
# Option to allow the user to enable warnings by groups
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_GROUPZERO_WARNINGS "Enable group zero warnings" OFF)
if (HDF5_ENABLE_GROUPZERO_WARNINGS)
  if (MSVC)
    string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /W1")
    string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W1")
  else (MSVC)
    if (CMAKE_COMPILER_IS_GNUCC)
      set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -pedantic")
    endif (CMAKE_COMPILER_IS_GNUCC)
  endif (MSVC)
endif (HDF5_ENABLE_GROUPZERO_WARNINGS)

#-----------------------------------------------------------------------------
# Option to allow the user to enable warnings by groups
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_GROUPONE_WARNINGS "Enable group one warnings" OFF)
if (HDF5_ENABLE_GROUPONE_WARNINGS)
  if (MSVC)
    string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /W2")
    string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W2")
  else (MSVC)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${H5_CFLAGS1}")
  endif (MSVC)
endif (HDF5_ENABLE_GROUPONE_WARNINGS)

#-----------------------------------------------------------------------------
# Option to allow the user to enable warnings by groups
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_GROUPTWO_WARNINGS "Enable group two warnings" OFF)
if (HDF5_ENABLE_GROUPTWO_WARNINGS)
  if (MSVC)
    string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /W3")
    string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W3")
  else (MSVC)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${H5_CFLAGS2}")
  endif (MSVC)
endif (HDF5_ENABLE_GROUPTWO_WARNINGS)

#-----------------------------------------------------------------------------
# Option to allow the user to enable warnings by groups
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_GROUPTHREE_WARNINGS "Enable group three warnings" OFF)
if (HDF5_ENABLE_GROUPTHREE_WARNINGS)
  if (MSVC)
    string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /W4")
    string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
  else (MSVC)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${H5_CFLAGS3}")
  endif (MSVC)
endif (HDF5_ENABLE_GROUPTHREE_WARNINGS)

#-----------------------------------------------------------------------------
# Option to allow the user to enable warnings by groups
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_GROUPFOUR_WARNINGS "Enable group four warnings" OFF)
if (HDF5_ENABLE_GROUPFOUR_WARNINGS)
  if (NOT MSVC)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${H5_CFLAGS4}")
  endif (NOT MSVC)
endif (HDF5_ENABLE_GROUPFOUR_WARNINGS)

#-----------------------------------------------------------------------------
# Option to allow the user to enable warnings by groups
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_GROUPFIVE_WARNINGS "Enable group five warnings" OFF)
if (HDF5_ENABLE_GROUPFIVE_WARNINGS)
  if (NOT MSVC)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${H5_CFLAGS5}")
  endif (NOT MSVC)
endif (HDF5_ENABLE_GROUPFIVE_WARNINGS)

#-----------------------------------------------------------------------------
# Option to allow the user to enable warnings by groups
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_GROUPSIX_WARNINGS "Enable group six warnings" OFF)
if (HDF5_ENABLE_GROUPSIX_WARNINGS)
  if (NOT MSVC)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${H5_CFLAGS6}")
  endif (NOT MSVC)
endif (HDF5_ENABLE_GROUPSIX_WARNINGS)

#-----------------------------------------------------------------------------
# This is in here to help some of the GCC based IDES like Eclipse
# and code blocks parse the compiler errors and warnings better.
#-----------------------------------------------------------------------------
if (CMAKE_COMPILER_IS_GNUCC)
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fmessage-length=0")
endif (CMAKE_COMPILER_IS_GNUCC)
if (CMAKE_COMPILER_IS_GNUCXX)
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fmessage-length=0")
endif (CMAKE_COMPILER_IS_GNUCXX)
