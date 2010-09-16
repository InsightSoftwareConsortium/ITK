option(USE_WRAP_ITK "Build external languages support" OFF)
mark_as_advanced(USE_WRAP_ITK)
if(USE_WRAP_ITK)
  message("WrapITK is an experimental system for wrapping ITK. It is tested on Linux, Mac OS and Solaris, but is known to have some problems on some Windows platforms.")
  # required for the FlatStructuringElement to be included.
  # without that, the external projects won't build
  # TODO: remove this check once FlatStructuringElement will be moved out of
  #       the review directory
  if(NOT ITK_USE_REVIEW)
    message(SEND_ERROR "WrapITK requires ITK_USE_REVIEW to be ON.")
  endif(NOT ITK_USE_REVIEW)
endif(USE_WRAP_ITK)

#-----------------------------------------------------------------------------
# wrapper config
option(ITK_CSWIG_TCL "Build cswig Tcl wrapper support (requires CableSwig)." OFF)
option(ITK_CSWIG_PYTHON "Build cswig Python wrapper support (requires CableSwig)." OFF)
option(ITK_CSWIG_JAVA "Build cswig Java wrapper support " OFF)

# perl support does not work, contact bill hoffman at kitware
# if you are interested in perl wrapping.  It is close, but
# not there yet.
#option(ITK_CSWIG_PERL "Build cswig Perl wrapper support " OFF)

#-----------------------------------------------------------------------------
# Do we need CableSwig?
set(ITK_NEED_CableSwig 0)

if(USE_WRAP_ITK)
  set(ITK_NEED_CableSwig 1)
endif(USE_WRAP_ITK)

if(ITK_CSWIG_TCL)
  set(ITK_NEED_CableSwig 1)
endif(ITK_CSWIG_TCL)

if(ITK_CSWIG_PYTHON)
  set(ITK_NEED_CableSwig 1)
endif(ITK_CSWIG_PYTHON)

if(ITK_CSWIG_JAVA)
  set(ITK_NEED_CableSwig 1)
endif(ITK_CSWIG_JAVA)

if(ITK_CSWIG_PERL)
  set(ITK_NEED_CableSwig 1)
endif(ITK_CSWIG_PERL)

if(ITK_NEED_CableSwig)

  if(NOT BUILD_SHARED_LIBS)
    message(FATAL_ERROR "Wrapping requires a shared build, change BUILD_SHARED_LIBS to ON")
  endif(NOT BUILD_SHARED_LIBS)

  # Search first if CableSwig is in the ITK source tree
  if(EXISTS ${ITK_SOURCE_DIR}/Utilities/CableSwig)
    set(CMAKE_MODULE_PATH ${ITK_SOURCE_DIR}/Utilities/CableSwig/SWIG/CMake)

    # CableSwig is included in the source distribution.
    set(ITK_BUILD_CABLESWIG 1)
    set(CableSwig_DIR ${ITK_BINARY_DIR}/Utilities/CableSwig CACHE PATH "CableSwig_DIR: The directory containing CableSwigConfig.cmake.")
    set(CableSwig_FOUND 1)
    set(CableSwig_INSTALL_ROOT ${ITK_INSTALL_LIB_DIR}/CSwig)
    include(${CableSwig_DIR}/CableSwigConfig.cmake OPTIONAL)
    add_subdirectory(Utilities/CableSwig)
  else(EXISTS ${ITK_SOURCE_DIR}/Utilities/CableSwig)
    # If CableSwig is not in the source tree,
    # then try to find a binary build of CableSwig
    find_package(CableSwig)
    set(CMAKE_MODULE_PATH ${CableSwig_DIR}/SWIG/CMake)
  endif(EXISTS ${ITK_SOURCE_DIR}/Utilities/CableSwig)

  if(NOT CableSwig_FOUND)
    # We have not found CableSwig.  Complain.
    message(FATAL_ERROR "CableSwig is required for CSwig Wrapping.")
  endif(NOT CableSwig_FOUND)

endif(ITK_NEED_CableSwig)


