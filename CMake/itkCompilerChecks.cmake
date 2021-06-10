# Minimum compiler version check: GCC
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND
    CMAKE_CXX_COMPILER_VERSION VERSION_LESS 5.1)
  message(FATAL_ERROR "GCC 5.1 or later is required.")
endif ()

# Minimum compiler version check: LLVM Clang
if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang" AND
    CMAKE_CXX_COMPILER_VERSION VERSION_LESS 3.4)
  message(FATAL_ERROR "LLVM Clang 3.4 or later is required.")
endif ()

# Minimum compiler version check: Apple Clang >= 7.0.2 (Xcode 7.2.1)
if (CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang" AND
    CMAKE_CXX_COMPILER_VERSION VERSION_LESS 7.0.2)
  message(FATAL_ERROR "Apple Clang 7.0.2 or later is required.")
endif ()

# Minimum compiler version check: Microsoft C/C++ >= 19.10 (MSVC 14.1, Visual Studio 15 2017)
if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC" AND
    CMAKE_CXX_COMPILER_VERSION VERSION_LESS 19.10)
  message(FATAL_ERROR "Microsoft Visual Studio 2017 or later is required.")
endif ()

# Minimum compiler version check: Intel C++ (ICC)
if (CMAKE_CXX_COMPILER_ID STREQUAL "Intel" AND
    CMAKE_CXX_COMPILER_VERSION VERSION_LESS 17.0)
  message(FATAL_ERROR "Intel C++ (ICC) 17.0 or later is required.")
endif ()

# Make sure we have C++14 enabled.
if(NOT ITK_IGNORE_CMAKE_CXX14_CHECKS)
  # Needed to make sure libraries and executables not built by the
  # itkModuleMacros still have the C++14 compiler flags enabled
  # Wrap this in an escape hatch for unknown compilers
  if(NOT CMAKE_CXX_STANDARD)
    set(CMAKE_CXX_STANDARD 14) # Supported values are 14, 17, 20, and 23.
  endif()
  if(NOT CMAKE_CXX_STANDARD_REQUIRED)
    set(CMAKE_CXX_STANDARD_REQUIRED ON)
  endif()
  if(NOT CMAKE_CXX_EXTENSIONS)
    set(CMAKE_CXX_EXTENSIONS OFF)
  endif()
endif()
