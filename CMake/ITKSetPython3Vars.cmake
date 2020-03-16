# This file provides a work-around to provide consistent
# python3 cmake variables for cmake versions pre/post 3.12.0
# NOTE: Only python 3 versions 3.5 and above are searched for.

# If the cmake variable "PYTHON_DEVELOPMENT_REQUIRED" is set to ON
# then the development environments are found.

if("${CMAKE_VERSION}" VERSION_LESS_EQUAL "3.12.0")
  # Use of PythonInterp and PythonLibs is deprecated since cmake version 3.12.0
  # Only use deprecated mechanisms for older versions of cmake
  set(Python_ADDITIONAL_VERSIONS 3.9 3.8 3.7 3.6 3.5)
  find_package(PythonInterp)
  if(PYTHON_DEVELOPMENT_REQUIRED)
    find_package(PythonLibs REQUIRED)
    # check for version mismatch.
    if(PYTHONLIBS_FOUND AND PYTHONINTERP_FOUND
        AND NOT(PYTHON_VERSION_STRING VERSION_EQUAL PYTHONLIBS_VERSION_STRING))
      message(FATAL_ERROR "Python executable (\"${PYTHON_VERSION_STRING}\") and library (\"${PYTHONLIBS_VERSION_STRING}\") version mismatch.")
    endif()
  endif()
  if(PYTHON_VERSION_STRING VERSION_LESS 3.5)
    # if python version is less than 3.5, unset so that it appears that no python version is found.
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
  if(PYTHON_DEVELOPMENT_REQUIRED)
    if(DEFINED Python3_EXECUTABLE)
      set(_specified_Python3_EXECUTABLE ${Python3_EXECUTABLE})
    endif()
    find_package(Python3 COMPONENTS Interpreter Development)
    set(Python3_EXECUTABLE ${_specified_Python3_EXECUTABLE} CACHE INTERNAL
      "Path to the Python interpreter" FORCE)
  else()
    find_package(Python3 COMPONENTS Interpreter)
  endif()
  if(NOT Python3_EXECUTABLE AND _Python3_EXECUTABLE)
    set(Python3_EXECUTABLE ${_Python3_EXECUTABLE} CACHE INTERNAL
      "Path to the Python interpreter" FORCE)
  endif()
endif()
