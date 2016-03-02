# microsoft specific config file
set(WORDS_BIGENDIAN )
set(HAVE_LIMITS_H   1)
set(HAVE_UNISTD_H   1)
set(CMAKE_CXX_COMPILER  VC++60 CACHE STRING
     "Name of C++ compiler used.")
set(CMAKE_CXX_FLAGS_RELEASE "/MD /O2" CACHE STRING
        "Flags used by the compiler during release builds (/MD /Ob1 /Oi /Ot /Oy /Gs will produce slightly less optimized but smaller files)")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "/MD /Zi /O2" CACHE STRING
        "Flags used by the compiler during Release with Debug Info builds")
set(CMAKE_CXX_FLAGS_MINSIZEREL "/MD /O1" CACHE STRING
        "Flags used by the compiler during release minsize builds")
set(CMAKE_CXX_FLAGS_DEBUG "/MDd /Zi /Od /GZ" CACHE STRING
        "Flags used by the compiler during debug builds")
set(CMAKE_CXX_FLAGS "/W3 /Zm1000 /GX /GR" CACHE STRING
        "Flags used by the compiler during all build types, /GX /GR are for exceptions and rtti in VC++, /Zm1000 increases the compiler's memory allocation to support ANSI C++/stdlib")
set(CMAKE_USE_WIN32_THREADS 1 CACHE BOOL "Use the win32 thread library")
set(CMAKE_MAKE_PROGRAM   "msdev" CACHE STRING "Program used to build from dsp files.")
mark_as_advanced(
WORDS_BIGENDIAN
HAVE_UNISTD_H
HAVE_LIMITS_H
CMAKE_CXX_COMPILER
CMAKE_CXX_FLAGS_RELEASE
CMAKE_CXX_FLAGS_RELWITHDEBINFO
CMAKE_CXX_FLAGS_MINSIZEREL
CMAKE_CXX_FLAGS_DEBUG
CMAKE_USE_WIN32_THREADS
CMAKE_MAKE_PROGRAM
)
