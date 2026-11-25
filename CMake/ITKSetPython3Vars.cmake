# If the cmake variable "PYTHON_DEVELOPMENT_REQUIRED" is set to ON
# then the development environments are found.

# CMake will use FindPython3 and can be controlled accordingly (i.e. setting Python3_ROOT_DIR or
# Python3_FIND_REGISTRY (useful if using PATH to specify Python3 version on Windows).
# Additionally, setting Python3_EXECUTABLE can be used to set the Python version explicitly, but
# may become less reliable with newer versions of CMake (as opposed setting FindPython3 HINTS).  Current
# implementation gives preference to active virtualenvs.
cmake_policy(SET CMP0094 NEW) # makes FindPython3 prefer activated virtualenv Python to latest version
# Canonical floor; a specified Python3_EXECUTABLE narrows PYTHON_VERSION_MIN below, so keep it separate.
set(ITK_WRAP_PYTHON_MINIMUM_VERSION 3.11)
set(PYTHON_VERSION_MIN ${ITK_WRAP_PYTHON_MINIMUM_VERSION})
set(PYTHON_VERSION_MAX 3.999)
if(DEFINED Python3_EXECUTABLE) # if already specified
  set(_specified_Python3_EXECUTABLE ${Python3_EXECUTABLE})
  # If a specific Python executable is provided, lock the Python version range
  # to the exact version down to the minor release of that executable so FindPython3 searches that version.
  execute_process(
    COMMAND
      "${_specified_Python3_EXECUTABLE}" -c
      "import sys; print('{}.{}.{}'.format(sys.version_info[0], sys.version_info[1], sys.version_info[2]))"
    OUTPUT_VARIABLE _specified_Python3_VERSION_MM
    ERROR_VARIABLE _specified_Python3_VERSION_MM_ERR
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  if(_specified_Python3_VERSION_MM)
    # execute_process re-runs every configure, so this also rejects incremental
    # reconfigures of a cached below-floor tree, before any wrapping work.
    if(
      PYTHON_DEVELOPMENT_REQUIRED
      AND
        _specified_Python3_VERSION_MM
          VERSION_LESS
          ${ITK_WRAP_PYTHON_MINIMUM_VERSION}
    )
      message(
        FATAL_ERROR
        "ITK Python wrapping (ITK_WRAP_PYTHON=ON) requires Python >= ${ITK_WRAP_PYTHON_MINIMUM_VERSION}, "
        "but the specified Python3_EXECUTABLE=${_specified_Python3_EXECUTABLE} is Python ${_specified_Python3_VERSION_MM}. "
        "Provide a Python >= ${ITK_WRAP_PYTHON_MINIMUM_VERSION} interpreter, or set ITK_WRAP_PYTHON=OFF."
      )
    endif()
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
      # NOTE: dockcross build environments do not supply
      #       `Development` python-dev resources
      Development.Module
      Development.SABIModule
      NumPy
  )
  set(ITK_WRAP_PYTHON_VERSION "${Python3_VERSION}")

  # An empty Python3_VERSION means the range found nothing; otherwise the second
  # find_package below runs unversioned and silently accepts a below-floor Python.
  if(
    NOT
      Python3_VERSION
    OR
      Python3_VERSION
        VERSION_LESS
        ${ITK_WRAP_PYTHON_MINIMUM_VERSION}
  )
    message(
      FATAL_ERROR
      "ITK Python wrapping (ITK_WRAP_PYTHON=ON) requires Python >= ${ITK_WRAP_PYTHON_MINIMUM_VERSION}, "
      "but no such interpreter was found in range ${PYTHON_VERSION_MIN}...${PYTHON_VERSION_MAX} "
      "(resolved version '${Python3_VERSION}', Python3_EXECUTABLE='${Python3_EXECUTABLE}'). "
      "Provide a Python >= ${ITK_WRAP_PYTHON_MINIMUM_VERSION} interpreter, or set ITK_WRAP_PYTHON=OFF."
    )
  endif()

  # start section to define package components based on LIMITED_API support and choices
  # Cached so the abi3 floor is a single source of truth shared with the module
  # wrapping macros (itk_end_wrap_module.cmake USE_SABI).
  set(
    _ITK_MINIMUM_SUPPORTED_LIMITED_API_VERSION
    3.11
    CACHE INTERNAL
    "Minimum Python minor version for the abi3 / Limited API floor"
  )

  # Force ITK_WRAP_PYTHON_VERSION if SKBUILD_SABI_COMPONENT requests it
  string(
    FIND
    "${SKBUILD_SABI_COMPONENT}"
    "SABIModule"
    _SKBUILD_SABI_COMPONENT_REQUIRED
  )
  # A reconfigure sees the cached value, so remember how it was first set.
  if(NOT DEFINED ITK_USE_PYTHON_LIMITED_API_WAS_AUTO)
    if(NOT DEFINED ITK_USE_PYTHON_LIMITED_API)
      set(ITK_USE_PYTHON_LIMITED_API_WAS_AUTO 1 CACHE INTERNAL "")
    else()
      set(ITK_USE_PYTHON_LIMITED_API_WAS_AUTO 0 CACHE INTERNAL "")
    endif()
  endif()
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
  if(ITK_USE_PYTHON_LIMITED_API_WAS_AUTO)
    set(
      _itk_limited_api_origin
      "auto-selected from Python ${ITK_WRAP_PYTHON_VERSION}"
    )
  else()
    set(_itk_limited_api_origin "requested")
  endif()
  unset(_SKBUILD_SABI_COMPONENT_REQUIRED)
  # The value auto-enables by interpreter version, so report what was chosen.
  if(ITK_USE_PYTHON_LIMITED_API)
    message(
      STATUS
      "Python wrapping: limited API (abi3) ON [${_itk_limited_api_origin}]"
    )
  else()
    message(
      STATUS
      "Python wrapping: limited API (abi3) OFF [${_itk_limited_api_origin}]"
    )
  endif()
  unset(_itk_limited_api_origin)
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
      Development.Module
      Development.SABIModule
    )
  else()
    set(
      _python_find_components
      Interpreter
      Development.Module
    )
  endif()
  if(BUILD_TESTING)
    # NumPy Required for testing ITK PythonTests, prefer to fail early if not installed
    list(APPEND _python_find_components NumPy)
  endif()
  set(_missing_required_component FALSE)
  set(_missing_python_components "")
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
      list(APPEND _missing_python_components ${_required_component})
    else()
      message(
        STATUS
        " o Python3 Found COMPONENT: Python3_${_required_component}_FOUND: ${Python3_${_required_component}_FOUND}"
      )
    endif()
  endforeach()
  unset(_required_component)
  if(_missing_required_component)
    set(_missing_component_remedy "")
    if(NumPy IN_LIST _missing_python_components)
      string(
        APPEND
        _missing_component_remedy
        "
          NumPy: ${Python3_EXECUTABLE} -m pip install numpy"
      )
    endif()
    if(
      Development.Module
        IN_LIST
        _missing_python_components
      OR
        Development.SABIModule
          IN_LIST
          _missing_python_components
    )
      string(
        APPEND
        _missing_component_remedy
        "
          Development: install the development headers matching this interpreter
                       (Debian/Ubuntu: apt install python${Python3_VERSION_MAJOR}.${Python3_VERSION_MINOR}-dev)"
      )
    endif()
    message(
      FATAL_ERROR
      "Missing required Python3 COMPONENT(s): ${_missing_python_components}${_missing_component_remedy}

          Or set ITK_WRAP_PYTHON=OFF to build without Python wrapping.
          ---
          Searched for : ${_python_find_components}
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
    unset(_missing_component_remedy)
  else()
    message(STATUS " o Python3_EXECUTABLE=${Python3_EXECUTABLE}")
    message(STATUS " o Python3_ROOT_DIR=${Python3_ROOT_DIR}")
    message(STATUS " o ITK_WRAP_PYTHON_VERSION=${ITK_WRAP_PYTHON_VERSION}")
  endif()
  unset(_missing_required_component)
  unset(_missing_python_components)
  unset(_python_find_components)
  # _ITK_MINIMUM_SUPPORTED_LIMITED_API_VERSION is cached (INTERNAL) and intentionally
  # left set so itk_end_wrap_module.cmake can pin USE_SABI to the same floor.
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
  # the corresponding Python3_ROOT_DIR so CMake's FindPython3 locates the
  # matching installation or virtual environment.
  # This is especially important for virtualenv/venv/conda environments
  # and on Windows where FindPython3 may otherwise pick the wrong installation.
  if(DEFINED Python3_EXECUTABLE AND NOT DEFINED Python3_ROOT_DIR)
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
