# If the cmake variable "PYTHON_DEVELOPMENT_REQUIRED" is set to ON
# then the development environments are found.

# CMake will use FindPython3 and can be controlled accordingly (i.e. setting Python3_ROOT_DIR or
# Python3_FIND_REGISTRY (useful if using PATH to specify Python3 version on Windows).
# Additionally, setting Python3_EXECUTABLE can be used to set the Python version explicitly, but
# may become less reliable with newer versions of CMake (as opposed setting FindPython3 HINTS).  Current
# implementation gives preference to active virtualenvs.

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
