
# try to find the languages
# this code must be run before the standard OPTION macros, because the settings
# can't be changed.
# The NO_LANGUAGES_AUTO_DETECT var can be set to prevent that code to run, for example
# for rpm packages

IF(NOT NO_LANGUAGES_AUTO_DETECT)
  FIND_PACKAGE(TCL)
  IF(NOT "${TCL_INCLUDE_PATH}" MATCHES "NOTFOUND\$")
    OPTION(WRAP_ITK_TCL "Build cswig Tcl wrapper support." ON)
  ENDIF(NOT "${TCL_INCLUDE_PATH}" MATCHES "NOTFOUND\$")
  
  
  FIND_PACKAGE(PythonLibs)
  FIND_PACKAGE(PythonInterp)
  IF(NOT "${PYTHON_INCLUDE_PATH}" MATCHES "NOTFOUND\$")
    OPTION(WRAP_ITK_PYTHON "Build cswig Python wrapper support." ON)
  ENDIF(NOT "${PYTHON_INCLUDE_PATH}" MATCHES "NOTFOUND\$")
  
  
  
  FIND_PACKAGE(Java)
  FIND_PACKAGE(JNI)
  IF(NOT "${JAVA_INCLUDE_PATH}" MATCHES "NOTFOUND\$")
    OPTION(WRAP_ITK_JAVA "Build cswig Java wrapper support." ON)
  ENDIF(NOT "${JAVA_INCLUDE_PATH}" MATCHES "NOTFOUND\$")


  # mask vars, as it would be done if the language support is activated
  FOREACH(entry TCL_LIBRARY_DEBUG
                TK_LIBRARY_DEBUG
                TCL_STUB_LIBRARY
                TCL_STUB_LIBRARY_DEBUG
                TK_STUB_LIBRARY
                TK_STUB_LIBRARY_DEBUG
                TK_WISH)
    SET(${entry} "${${entry}}" CACHE INTERNAL "This value is not used by ITK.")
  ENDFOREACH(entry)
  MARK_AS_ADVANCED(PYTHON_EXECUTABLE)

ENDIF(NOT NO_LANGUAGES_AUTO_DETECT)

# there is no need to set NO_LANGUAGES_AUTO_DETECT, because options can't be overiten
# from the cmake code
# SET(NO_LANGUAGES_AUTO_DETECT ON CACHE INTERNAL "disable laguage automatic detection")


