# microsoft specific config file
set(WORDS_BIGENDIAN )
set(HAVE_LIMITS_H   1)
set(HAVE_UNISTD_H   1)
set(CMAKE_CXX_COMPILER  cl CACHE FILEPATH
     "Name of C++ compiler used.")
set(CMAKE_C_COMPILER  cl CACHE FILEPATH
     "Name of C compiler used.")
set(CMAKE_CFLAGS  "/W3 /Zm1000" CACHE STRING
     "Flags for C compiler.")
set(CMAKE_BUILD_TYPE Debug CACHE STRING
"Choose the type of build, options are: Debug Release RelWithDebInfo MinSizeRel")
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
set(CMAKE_STANDARD_WINDOWS_LIBRARIES "kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib  kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib" CACHE STRING "Libraries linked by defalut with all applications")
set(CMAKE_SHLIB_SUFFIX       ".dll" CACHE STRING "Shared library suffix")
set(CMAKE_MODULE_SUFFIX      ".dll" CACHE STRING "Module library suffix")
set(CMAKE_MAKE_PROGRAM      "nmake" CACHE STRING "Program used to build from makefiles.")




