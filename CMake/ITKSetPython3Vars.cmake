# If the cmake variable "PYTHON_DEVELOPMENT_REQUIRED" is set to ON
# then the development environments are found.

# CMake will use FindPython3 and can be controlled accordingly (i.e. setting Python3_ROOT_DIR or
# Python3_FIND_REGISTRY (useful if using PATH to specify Python3 version on Windows).
# Additionally, setting Python3_EXECUTABLE can be used to set the Python version explicitly, but
# may become less reliable with newer versions of CMake (as opposed setting FindPython3 HINTS).  Current
# implementation gives preference to active virtualenvs.
cmake_policy(SET CMP0094 NEW) # makes FindPython3 prefer activated virtualenv Python to latest version
set(PYTHON_VERSION_MIN 3.9)
set(PYTHON_VERSION_MAX 3.999)
if(DEFINED Python3_EXECUTABLE) # if already specified
  set(_specified_Python3_EXECUTABLE ${Python3_EXECUTABLE})
  # If a specific Python executable is provided, lock the Python version range
  # to the exact version of that executable so FindPython3 searches that version.
  execute_process(
    COMMAND
      "${_specified_Python3_EXECUTABLE}" -c
      "import sys; print('{}.{}'.format(sys.version_info[0], sys.version_info[1]))"
    OUTPUT_VARIABLE _specified_Python3_VERSION_MM
    ERROR_VARIABLE _specified_Python3_VERSION_MM_ERR
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  if(_specified_Python3_VERSION_MM)
    set(PYTHON_VERSION_MIN ${_specified_Python3_VERSION_MM})
    set(PYTHON_VERSION_MAX ${_specified_Python3_VERSION_MM})
  endif()
  unset(_specified_Python3_VERSION_MM)
  unset(_specified_Python3_VERSION_MM_ERR)
endif()

# Tested in cmake 3.26-4.21.  Python3_FIND_ABI causes the Development COMPONENTS to not be found
unset(Python3_FIND_ABI)

if(NOT PYTHON_DEVELOPMENT_REQUIRED)
  # if not PYTHON_DEVELOPMENT_REQUIRED, just find some version of
  # Python (don't need to be as specific)
  find_package(
    Python3
    ${PYTHON_VERSION_MIN}...${PYTHON_VERSION_MAX}
    COMPONENTS
      Interpreter
  )
  set(ITK_WRAP_PYTHON_VERSION "ITK_WRAP_PYTHON=OFF")
else()
  # set(Python3_FIND_REGISTRY LAST) # default is FIRST. Do we need/want this?
  find_package(
    Python3
    ${PYTHON_VERSION_MIN}...${PYTHON_VERSION_MAX}
    COMPONENTS
      Interpreter
      Development # Needed for Modules/Core/Common/src/itkPyCommand.cxx
      Development.Module
      Development.SABIModule
      NumPy
  )
  set(ITK_WRAP_PYTHON_VERSION "${Python3_VERSION}")

  # start section to define package components based on LIMITED_API support and choices
  set(_ITK_MINIMUM_SUPPORTED_LIMITED_API_VERSION 3.11)

  # Force ITK_WRAP_PYTHON_VERSION if SKBUILD_SABI_COMPONENT requests it
  string(
    FIND
    "${SKBUILD_SABI_COMPONENT}"
    "SABIModule"
    _SKBUILD_SABI_COMPONENT_REQUIRED
  )
  if(NOT DEFINED ITK_USE_PYTHON_LIMITED_API)
    if(
      (
        _SKBUILD_SABI_COMPONENT_REQUIRED
          GREATER
          -1
      )
      OR
        ITK_WRAP_PYTHON_VERSION
          VERSION_GREATER_EQUAL
          ${_ITK_MINIMUM_SUPPORTED_LIMITED_API_VERSION}
    )
      set(
        ITK_USE_PYTHON_LIMITED_API
        1
        CACHE BOOL
        "Configure Python's limited API for Python minor version compatibility."
      )
    else()
      set(
        ITK_USE_PYTHON_LIMITED_API
        0
        CACHE BOOL
        "Configure Python's limited API for Python minor version compatibility."
      )
    endif()
    mark_as_advanced(ITK_USE_PYTHON_LIMITED_API)
  endif()
  unset(_SKBUILD_SABI_COMPONENT_REQUIRED)
  if(ITK_USE_PYTHON_LIMITED_API)
    if(CMAKE_VERSION VERSION_LESS "3.26")
      message(
        FATAL_ERROR
        "CMake version ${CMAKE_VERSION} is too old for Python limited API wrapping: "
        "the FindPython Development.SABIModule component requires CMake >= 3.26. "
        "Either upgrade CMake or disable ITK_USE_PYTHON_LIMITED_API."
      )
    endif()

    set(
      _python_find_components
      Interpreter
      Development # Needed for Modules/Core/Common/src/itkPyCommand.cxx
      Development.SABIModule
      NumPy # NumPy Required for ITK, prefer to fail early
    )
  else()
    set(
      _python_find_components
      Interpreter
      Development # Needed for Modules/Core/Common/src/itkPyCommand.cxx
      Development.Module
      NumPy # NumPy Required for ITK, prefer to fail early
    )
  endif()
  set(_missing_required_component FALSE)
  find_package(
    Python3
    ${ITK_WRAP_PYTHON_VERSION}
    COMPONENTS
      ${_python_find_components}
  )
  set(ITK_WRAP_PYTHON_VERSION "${Python3_VERSION}")
  message(STATUS "Python3_FOUND=${Python3_FOUND}")
  foreach(_required_component ${_python_find_components})
    if(NOT Python3_${_required_component}_FOUND)
      message(
        STATUS
        " o Python3 Missing COMPONENT: Python3_${_required_component}_FOUND: ${Python3_${_required_component}_FOUND}"
      )
      set(_missing_required_component TRUE)
    else()
      message(
        STATUS
        " o Python3 Found COMPONENT: Python3_${_required_component}_FOUND: ${Python3_${_required_component}_FOUND}"
      )
    endif()
  endforeach()
  unset(_required_component)
  if(_missing_required_component)
    message(
      FATAL_ERROR
      "At least 1 required Python3 COMPONENT could not be found from : ${_python_find_components}
          in range ${PYTHON_VERSION_MIN}...${PYTHON_VERSION_MAX}:
          Python3_EXECUTABLE=:${Python3_EXECUTABLE}:
          ITK_WRAP_PYTHON_VERSION=:${ITK_WRAP_PYTHON_VERSION}:
          Python3_ROOT_DIR=:${Python3_ROOT_DIR}:
          ---
          Python3_FOUND=${Python3_FOUND}
          Python3_Interpreter_FOUND=${Python3_Interpreter_FOUND}
          Python3_Compiler_FOUND=${Python3_Compiler_FOUND}
          Python3_Development_FOUND=${Python3_Development_FOUND}
          Python3_Development.Module_FOUND=${Python3_Development.Module_FOUND}
          Python3_Development.SABIModule_FOUND=${Python3_Development.SABIModule_FOUND}
          Python3_Development.Embed_FOUND=${Python3_Development.Embed_FOUND}
          Python3_NumPy_FOUND=${Python3_NumPy_FOUND}
    "
    )
  else()
    message(STATUS " o Python3_EXECUTABLE=${Python3_EXECUTABLE}")
    message(STATUS " o Python3_ROOT_DIR=${Python3_ROOT_DIR}")
    message(STATUS " o ITK_WRAP_PYTHON_VERSION=${ITK_WRAP_PYTHON_VERSION}")
  endif()
  unset(_missing_required_component)
  unset(_python_find_components)
  unset(_ITK_MINIMUM_SUPPORTED_LIMITED_API_VERSION)
  # end section to define package components based on LIMITED_API support and choices
  if(DEFINED _specified_Python3_EXECUTABLE)
    set(
      Python3_EXECUTABLE
      ${_specified_Python3_EXECUTABLE}
      CACHE INTERNAL
      "Path to the Python interpreter"
      FORCE
    )
  endif()

  if(NOT Python3_EXECUTABLE AND _specified_Python3_EXECUTABLE) # workaround for cases where FindPython3 fails to set correctly
    set(
      Python3_EXECUTABLE
      ${_specified_Python3_EXECUTABLE}
      CACHE INTERNAL
      "Path to the Python interpreter"
      FORCE
    )
  endif()

  # If a specific Python3_EXECUTABLE is provided by the user, try to infer
  # the corresponding Python3_ROOT_DIR for Unix/macOS/Linux so CMake's
  # FindPython3 locates the matching installation or virtual environment.
  # This is especially important for virtualenv/venv/conda environments.
  if(
    DEFINED
      Python3_EXECUTABLE
    AND
      NOT
        DEFINED
          Python3_ROOT_DIR
    AND
      (
        UNIX
        OR
          APPLE
      )
    AND
      NOT
        WIN32
  )
    # First, try sys.prefix from the provided interpreter (works for venv/conda)
    execute_process(
      COMMAND
        "${Python3_EXECUTABLE}" -c "import sys; print(sys.prefix)"
      OUTPUT_VARIABLE _py_prefix
      ERROR_VARIABLE _py_prefix_err
      OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    if(_py_prefix)
      file(TO_CMAKE_PATH "${_py_prefix}" _py_root_hint)
    endif()

    # Fallback: parent of the interpreter's bin directory, e.g., /path/to/env
    # from /path/to/env/bin/python3
    if(NOT _py_root_hint)
      get_filename_component(_py_exe_dir "${Python3_EXECUTABLE}" DIRECTORY)
      get_filename_component(_py_root_hint "${_py_exe_dir}/.." REALPATH)
    endif()

    if(_py_root_hint)
      set(
        Python3_ROOT_DIR
        "${_py_root_hint}"
        CACHE PATH
        "Which installation or virtual environment of Python to use"
        FORCE
      )
      mark_as_advanced(Python3_ROOT_DIR)
    endif()
    unset(_py_prefix)
    unset(_py_prefix_err)
    unset(_py_exe_dir)
    unset(_py_root_hint)
  endif()
  if(Python3_ROOT_DIR)
    # Add user-visible cache entry if Python3_ROOT_DIR value is set
    set(
      Python3_ROOT_DIR
      ${Python3_ROOT_DIR}
      CACHE PATH
      "Which installation or virtual environment of Python to use"
      FORCE
    )
    mark_as_advanced(Python3_ROOT_DIR)
  endif()
endif()
