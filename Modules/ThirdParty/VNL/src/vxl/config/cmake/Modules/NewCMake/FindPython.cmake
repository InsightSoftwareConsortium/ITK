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
set(PYTHON_FOUND "NO" CACHE INTERNAL "Was Python successfully built?" )
if(BUILD_BRL_PYTHON)
set(Python_ADDITIONAL_VERSIONS 2.7)
find_package(PythonLibs)

if(PYTHON_INCLUDE_DIR)
 if(PYTHON_LIBRARY OR PYTHON_DEBUG_LIBRARY)
  # everything found
  set(PYTHON_FOUND "YES" CACHE INTERNAL "Was Python successfully built?")

  if( WIN32 )
    find_path(PYTHON_PC_INCLUDE_PATH
      NAMES pyconfig.h

      PATHS
      ${PYTHON_INCLUDE_DIRS}
      ${PYTHON_FRAMEWORK_INCLUDES}
      [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.7\\InstallPath]/PC

      PATH_SUFFIXES
       python2.7
    )

    set(PYTHON_INCLUDE_DIRS
      ${PYTHON_INCLUDE_DIR}
      ${PYTHON_PC_INCLUDE_PATH}
    )
    #message(${PYTHON_INCLUDE_DIRS})

    mark_as_advanced(
     PYTHON_PC_INCLUDE_PATH
    )

  endif()

 endif() #PYTHON_LIBRARY OR PYTHON_DEBUG_LIBRARY
endif() #PYTHON_INCLUDE_DIR
endif() # BUILD_BRL_PYTHON
