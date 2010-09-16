
# try to find the languages
# this code must be run before the standard OPTION macros, because the settings
# can't be changed.
# The NO_LANGUAGES_AUTO_DETECT var can be set to prevent that code to run, for example
# for rpm packages

if(NOT NO_LANGUAGES_AUTO_DETECT)
  find_package(TCL)
  if(NOT "${TCL_INCLUDE_PATH}" MATCHES "NOTFOUND\$")
    option(WRAP_ITK_TCL "Build cswig Tcl wrapper support." ON)
  endif(NOT "${TCL_INCLUDE_PATH}" MATCHES "NOTFOUND\$")


  find_package(PythonLibs)
  find_package(PythonInterp)
  if(NOT "${PYTHON_INCLUDE_PATH}" MATCHES "NOTFOUND\$")
    option(WRAP_ITK_PYTHON "Build cswig Python wrapper support." ON)
  endif(NOT "${PYTHON_INCLUDE_PATH}" MATCHES "NOTFOUND\$")



  find_package(Java)
  find_package(JNI)
  if(NOT "${JAVA_INCLUDE_PATH}" MATCHES "NOTFOUND\$")
    option(WRAP_ITK_JAVA "Build cswig Java wrapper support." ON)
  endif(NOT "${JAVA_INCLUDE_PATH}" MATCHES "NOTFOUND\$")


  # mask vars, as it would be done if the language support is activated
  foreach(entry TCL_LIBRARY_DEBUG
                TK_LIBRARY_DEBUG
                TCL_STUB_LIBRARY
                TCL_STUB_LIBRARY_DEBUG
                TK_STUB_LIBRARY
                TK_STUB_LIBRARY_DEBUG
                TK_WISH)
    set(${entry} "${${entry}}" CACHE INTERNAL "This value is not used by ITK.")
  endforeach(entry)
  mark_as_advanced(PYTHON_EXECUTABLE)

endif(NOT NO_LANGUAGES_AUTO_DETECT)

# there is no need to set NO_LANGUAGES_AUTO_DETECT, because options can't be overiten
# from the cmake code
# set(NO_LANGUAGES_AUTO_DETECT ON CACHE INTERNAL "disable laguage automatic detection")


