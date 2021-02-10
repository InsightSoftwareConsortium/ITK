# This file provides a work-around to provide consistent
# python3 cmake variables for cmake versions pre/post 3.12.0

# If the cmake variable "PYTHON_DEVELOPMENT_REQUIRED" is set to ON
# then the development environments are found.

# CMake < 3.15 will use deprecated search by PythonInterp and PythonLibs.

# CMake >= 3.15 will use FindPython3 and can be controlled accordingly (i.e. setting Python3_ROOT_DIR or
# Python3_FIND_REGISTRY (useful if using PATH to specify Python3 version on Windows).
# Additionally, setting Python3_EXECUTABLE can be used to set the Python version explicitly, but
# may become less reliable with newer versions of CMake (as opposed setting FindPython3 HINTS).  Current
# implementation gives preference to active virtualenvs.

if("${CMAKE_VERSION}" VERSION_LESS_EQUAL "3.15.0")
  # CMake < 3.15 will use deprecated search by PythonInterp and PythonLibs.
  # Use of PythonInterp and PythonLibs is deprecated since cmake version 3.12.0
  # Unstable behavior of FindPython between CMake 3.12 and 3.15, so including in this
  # Only use deprecated mechanisms for older versions of cmake
  #
  set(Python_ADDITIONAL_VERSIONS 3.9 3.8 3.7 3.6)
  find_package(PythonInterp)
  if(PYTHON_DEVELOPMENT_REQUIRED)
    find_package(PythonLibs REQUIRED)
    # check for version mismatch.
    if(PYTHONLIBS_FOUND AND PYTHONINTERP_FOUND
        AND NOT(PYTHON_VERSION_STRING VERSION_EQUAL PYTHONLIBS_VERSION_STRING))
      message(FATAL_ERROR "Python executable (\"${PYTHON_VERSION_STRING}\") and library (\"${PYTHONLIBS_VERSION_STRING}\") version mismatch.")
    endif()
  endif()
  if(PYTHON_VERSION_STRING VERSION_LESS 3.6)
    # if python version is less than 3.6, unset so that it appears that no python version is found.
    # to emulate the same behavior as find(Python3 ..) from cmake 3.12.0+
    unset(PYTHON_EXECUTABLE)
    unset(PYTHONINTERP_FOUND)
    unset(PYTHON_VERSION_STRING)
    unset(PYTHON_INCLUDE_DIRS)
  else()
    ## For forward compatibility with cmake 3.12.0 or greater, emulate variable names from FindPython3.cmake
    set(Python3_EXECUTABLE ${PYTHON_EXECUTABLE})
    set(Python3_Interpreter_FOUND ${PYTHONINTERP_FOUND})
    set(Python3_VERSION ${PYTHON_VERSION_STRING})

    set(Python3_Development_FOUND ${PYTHONLIBS_FOUND})
    set(Python3_INCLUDE_DIRS ${PYTHON_INCLUDE_DIRS})
    set(Python3_LIBRARIES    ${PYTHON_LIBRARIES})
  endif()
else()
  cmake_policy(SET CMP0094 NEW) # makes FindPython3 prefer activated virtualenv Python to latest version
  if(PYTHON_DEVELOPMENT_REQUIRED)
    if(DEFINED Python3_EXECUTABLE) # if already specified
      set(_specified_Python3_EXECUTABLE ${Python3_EXECUTABLE})
    endif()
    # set(Python3_FIND_REGISTRY LAST) # default is FIRST. Do we need/want this?
    find_package(Python3 COMPONENTS Interpreter Development)
    if(DEFINED _specified_Python3_EXECUTABLE)
      set(Python3_EXECUTABLE ${_specified_Python3_EXECUTABLE} CACHE INTERNAL
        "Path to the Python interpreter" FORCE)
    endif()
  else() # if not PYTHON_DEVELOPMENT_REQUIRED, just find some version of Python (don't need to be as specific)
    find_package(Python3 COMPONENTS Interpreter)
  endif()
  if(NOT Python3_EXECUTABLE AND _Python3_EXECUTABLE) # workaround for cases where FindPython3 fails to set correctly
    set(Python3_EXECUTABLE ${_Python3_EXECUTABLE} CACHE INTERNAL
      "Path to the Python interpreter" FORCE)
  endif()
endif()
