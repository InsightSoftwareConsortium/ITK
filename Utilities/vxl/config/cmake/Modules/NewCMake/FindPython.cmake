# - Test for Python
# Once loaded this will define
#   PYTHON_FOUND           - system has Python
#   PYTHON_INCLUDE_DIR     - path to where Python.h is found
#   PYTHON_INCLUDE_DIRS    - combined include path
#   PYTHON_PC_INCLUDE_PATH - PC directory for Win
#   PYTHON_LIBRARY         - libraries you need to link to
#   PYTHON_DEBUG_LIBRARY   - path to the debug library

# Flag that determines if we were able to successfully build Python.
# Initialize to NO. Change below if yes.
SET(PYTHON_FOUND "NO" CACHE INTERNAL "Was Python successfully built?" )

INCLUDE( ${CMAKE_ROOT}/Modules/FindPythonLibs.cmake )
IF(PYTHON_INCLUDE_DIR)
 IF(PYTHON_LIBRARY OR PYTHON_DEBUG_LIBRARY)
  # everything found
  SET(PYTHON_FOUND "YES" CACHE INTERNAL "Was Python successfully built?")

  IF( WIN32 )
    FIND_PATH(PYTHON_PC_INCLUDE_PATH
      NAMES pyconfig.h
    
      PATHS
      ${PYTHON_INCLUDE_DIRS}
      ${PYTHON_FRAMEWORK_INCLUDES}
      [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.6\\InstallPath]/PC
      [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.5\\InstallPath]/PC
      [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.4\\InstallPath]/PC
      [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.3\\InstallPath]/PC
      [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.2\\InstallPath]/PC
      [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.1\\InstallPath]/PC
      [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.0\\InstallPath]/PC
      [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\1.6\\InstallPath]/PC
      [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\1.5\\InstallPath]/PC
    
      PATH_SUFFIXES
       python2.6
       python2.5
       python2.4
       python2.3
       python2.2
       python2.1
       python2.0
       python1.6
       python1.5
    )

    SET(PYTHON_INCLUDE_DIRS
      ${PYTHON_INCLUDE_DIR}
      ${PYTHON_PC_INCLUDE_PATH}
    )
    #MESSAGE(${PYTHON_INCLUDE_DIRS})

    MARK_AS_ADVANCED(
     PYTHON_PC_INCLUDE_PATH
    )

  ENDIF(WIN32)

 ENDIF(PYTHON_LIBRARY OR PYTHON_DEBUG_LIBRARY)
ENDIF(PYTHON_INCLUDE_DIR)
