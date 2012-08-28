# Try to find numarray python package
# Once done this will define
#
# PYTHON_NUMARRAY_FOUND        - system has numarray development package and it should be used
# PYTHON_NUMARRAY_INCLUDE_DIR  - directory where the arrayobject.h header file can be found
#
#

  find_path(PYTHON_NUMARRAY_INCLUDE_DIR arrayobject.h
    /usr/include/python2.3/numarray/
    /usr/share/pyshared/numpy/core/include/numpy/
    DOC "Directory where the arrayobject.h header file can be found. This file is part of the numarray package"
    )

  if(PYTHON_NUMARRAY_INCLUDE_DIR)
    set(PYTHON_NUMARRAY_FOUND 1 CACHE INTERNAL "Python numarray development package is available")
  endif()

