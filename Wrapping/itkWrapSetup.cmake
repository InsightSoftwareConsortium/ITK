OPTION(USE_WRAP_ITK "Build external languages support" OFF)
MARK_AS_ADVANCED(USE_WRAP_ITK)
IF(USE_WRAP_ITK)
  MESSAGE("WrapITK is an experimental system for wrapping ITK. It has been tested only on Linux and it is known not to work on Windows platforms.")
  # required for the FlatStructuringElement to be included.
  # without that, the external projects won't build
  # TODO: remove this check once FlatStructuringElement will be moved out of
  #       the review directory
  IF(NOT ITK_USE_REVIEW)
    MESSAGE(SEND_ERROR "WrapITK requires ITK_USE_REVIEW to be ON.")
  ENDIF(NOT ITK_USE_REVIEW)
ENDIF(USE_WRAP_ITK)

#-----------------------------------------------------------------------------
# wrapper config
OPTION(ITK_CSWIG_TCL "Build cswig Tcl wrapper support (requires CableSwig)." OFF)
OPTION(ITK_CSWIG_PYTHON "Build cswig Python wrapper support (requires CableSwig)." OFF)
OPTION(ITK_CSWIG_JAVA "Build cswig Java wrapper support " OFF)

# perl support does not work, contact bill hoffman at kitware
# if you are interested in perl wrapping.  It is close, but
# not there yet.
#OPTION(ITK_CSWIG_PERL "Build cswig Perl wrapper support " OFF)

#-----------------------------------------------------------------------------
# Do we need CableSwig?
SET(ITK_NEED_CableSwig 0)

IF(USE_WRAP_ITK)
  SET(ITK_NEED_CableSwig 1)
ENDIF(USE_WRAP_ITK)

IF(ITK_CSWIG_TCL)
  SET(ITK_NEED_CableSwig 1)
ENDIF(ITK_CSWIG_TCL)

IF(ITK_CSWIG_PYTHON)
  SET(ITK_NEED_CableSwig 1)
ENDIF(ITK_CSWIG_PYTHON)

IF(ITK_CSWIG_JAVA)
  SET(ITK_NEED_CableSwig 1)
ENDIF(ITK_CSWIG_JAVA)

IF(ITK_CSWIG_PERL)
  SET(ITK_NEED_CableSwig 1)
ENDIF(ITK_CSWIG_PERL)

IF(ITK_NEED_CableSwig)

  IF(NOT BUILD_SHARED_LIBS)
    MESSAGE(FATAL_ERROR "Wrapping requires a shared build, change BUILD_SHARED_LIBS to ON")
  ENDIF(NOT BUILD_SHARED_LIBS)
  
  IF(NOT CableSwig_FOUND)
    # find cablewig if not already found
    FIND_PACKAGE(CableSwig)
    SET(CMAKE_MODULE_PATH ${CableSwig_DIR}/SWIG/CMake)
  ENDIF(NOT CableSwig_FOUND)
  
  IF(NOT CableSwig_FOUND)
    # We have not found CableSwig.  Complain.
    MESSAGE(FATAL_ERROR "CableSwig is required for CSwig Wrapping.")
  ENDIF(NOT CableSwig_FOUND)

ENDIF(ITK_NEED_CableSwig)


