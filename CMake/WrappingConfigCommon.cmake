if(ITK_WRAPPING)
  # WRAPPER_LIBRARY_OUTPUT_DIR. Directory in which generated cxx, xml, and idx files will be placed.
  set(WRAPPER_LIBRARY_OUTPUT_DIR
      "${ITK_BINARY_DIR}/Wrapping"
      CACHE INTERNAL "Need to specify the output library directory globally")
  if(ITK_WRAP_PYTHON)
    set(ITK_WRAP_PYTHON_ROOT_BINARY_DIR
        "${WRAPPER_LIBRARY_OUTPUT_DIR}/Generators/Python"
        CACHE INTERNAL "python binary dir")
    # create the directory to avoid losing case on windows
    file(MAKE_DIRECTORY ${ITK_WRAP_PYTHON_ROOT_BINARY_DIR})

    set(ITK_PYTHON_PACKAGE_DIR "${ITK_WRAP_PYTHON_ROOT_BINARY_DIR}/itk")
    # create the directory to avoid losing case on windows
    file(MAKE_DIRECTORY ${ITK_PYTHON_PACKAGE_DIR})

    set(ITK_WRAP_PYTHON_SWIG_CONFIGURATION_DIR
        "${ITK_PYTHON_PACKAGE_DIR}/Configuration"
        CACHE INTERNAL "python binary dir")
    # create the directory to avoid losing case on windows
    file(MAKE_DIRECTORY ${ITK_WRAP_PYTHON_SWIG_CONFIGURATION_DIR})

    # IF WRAP_PYTHON then we must unconditionally set the CMAKE_LIBRARY_OUTPUT_DIRECTORY
    # If wrapping for python, then put all the shared libraries (both core shared libs,
    # and python shared libs) in the itk python package directory.
    #
    # https://cmake.org/cmake/help/v3.10/prop_tgt/LIBRARY_OUTPUT_DIRECTORY.html#prop_tgt:LIBRARY_OUTPUT_DIRECTORY
    # Multi-configuration generators (VS, Xcode) append a per-configuration subdirectory
    # to the specified directory unless a generator expression is used.
    # Using an always true generator expression to disable multi-config standard behavior
    #
    # When wrapping, the multi-config generators can only be used in degraded state
    # of allowing only a single element int the CMAKE_CONFIGURATION_TYPES and enforcing
    # that CMAKE_BUILD_TYPE match that type (see Wrapping/CMakeLists.txt enforcement)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY
        "$<1:${ITK_PYTHON_PACKAGE_DIR}>"
        CACHE PATH "Shared library directory with generator override" FORCE)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY
        "$<1:${ITK_PYTHON_PACKAGE_DIR}>"
        CACHE PATH "Shared library directory with generator override" FORCE)
  endif()
else()
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY
      ${NO_WRAP_CMAKE_LIBRARY_OUTPUT_DIRECTORY}
      CACHE PATH "Shared library directory")
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY
      ${NO_WRAP_CMAKE_RUNTIME_OUTPUT_DIRECTORY}
      CACHE PATH "Runtime library directory")
endif()
mark_as_advanced(
  FORCE
  CMAKE_RUNTIME_OUTPUT_DIRECTORY
  CMAKE_LIBRARY_OUTPUT_DIRECTORY
  NO_WRAP_CMAKE_LIBRARY_OUTPUT_DIRECTORY
  NO_WRAP_CMAKE_RUNTIME_OUTPUT_DIRECTORY
  Python3_ROOT_DIR)
