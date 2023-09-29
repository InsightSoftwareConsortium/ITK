# Minimum compiler version check: GCC
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 7)
  message(FATAL_ERROR "GCC 7 or later is required.")
endif()

# Minimum compiler version check: LLVM Clang
if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang" AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 5)
  message(FATAL_ERROR "LLVM Clang 5 or later is required.")
endif()

# Minimum compiler version check: Apple Clang >= 10.0.0 (Xcode 10.0)
if(CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang" AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 10.0.0)
  message(FATAL_ERROR "Apple Clang 10.0.0 or later is required.")
endif()

# Minimum compiler version check: Microsoft C/C++
if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC" AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 19.20)
  message(FATAL_ERROR "Microsoft Visual Studio 2019 16.0 (MSVC 19.20) or later is required.")
endif()

# Minimum compiler version check: Intel C++ (ICC)
if(CMAKE_CXX_COMPILER_ID STREQUAL "Intel" AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 19.1)
  message(FATAL_ERROR "Intel C++ (ICC) 19.1 or later is required.")
endif()

# Make sure we have C++17 enabled.
if(NOT ITK_IGNORE_CMAKE_CXX17_CHECKS)
  # Needed to make sure libraries and executables not built by the
  # itkModuleMacros still have the C++17 compiler flags enabled
  # Wrap this in an escape hatch for unknown compilers
  if(NOT CMAKE_CXX_STANDARD)
    set(CMAKE_CXX_STANDARD 17) # Supported values are 17, 20, and 23.
  endif()
  if(NOT CMAKE_CXX_STANDARD_REQUIRED)
    set(CMAKE_CXX_STANDARD_REQUIRED ON)
  endif()
  if(NOT CMAKE_CXX_EXTENSIONS)
    set(CMAKE_CXX_EXTENSIONS OFF)
  endif()
endif()
