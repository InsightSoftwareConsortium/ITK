# Try to find numarray python package
# Once done this will define
#
# PYTHON_NUMARRAY_FOUND        - system has numarray development package and it should be used
# PYTHON_NUMARRAY_INCLUDE_DIR  - directory where the arrayobject.h header file can be found
#
#
  IF(PYTHON_EXECUTABLE)
    FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/det_npp.py "try: import numpy; print numpy.get_numpy_include()\nexcept: pass\n")
    EXEC_PROGRAM("${PYTHON_EXECUTABLE}"
      ARGS "\"${CMAKE_CURRENT_BINARY_DIR}/det_npp.py\""
      OUTPUT_VARIABLE NUMPY_PATH
    )
  ENDIF(PYTHON_EXECUTABLE)

  FIND_PATH(PYTHON_NUMARRAY_INCLUDE_DIR arrayobject.h
    "${NUMPY_PATH}/numpy/"
    "${PYTHON_INCLUDE_PATH}/numarray/"
    "${PYTHON_INCLUDE_PATH}/Numeric/"
    /usr/include/python2.4/numarray/
    /usr/include/python2.3/numarray/
    /usr/include/python2.2/numarray/
    /usr/include/python2.1/numarray/
    DOC "Directory where the arrayobject.h header file can be found. This file is part of the numarray package"
    )

  IF(PYTHON_NUMARRAY_INCLUDE_DIR)
    SET (PYTHON_NUMARRAY_FOUND 1 CACHE INTERNAL "Python numarray development package is available")
  ENDIF(PYTHON_NUMARRAY_INCLUDE_DIR)

