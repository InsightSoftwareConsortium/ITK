if("${ITK_COMMON_BUILD_TYPE}" MATCHES "SHARED")
  set(BUILD_SHARED_LIBS ON)
endif("${ITK_COMMON_BUILD_TYPE}" MATCHES "SHARED")

#-----------------------------------------------------------------------------
# Explicit Instantiation config
option(ITK_EXPLICIT_INSTANTIATION "Try to explicitly build some of the core templates explictitly rather than over and over again." OFF)
mark_as_advanced(ITK_EXPLICIT_INSTANTIATION)
# Display a warning message if we try to build explicit instantiations on windows with
# shared libraries. VXL is not linking using shared libs on windows.
if(ITK_EXPLICIT_INSTANTIATION AND WIN32 AND BUILD_SHARED_LIBS)
  message("Explicit Instiantiation does not work on Windows platform with shared libraries. Turn BUILD_SHARED_LIBS OFF.")
endif(ITK_EXPLICIT_INSTANTIATION AND WIN32 AND BUILD_SHARED_LIBS)

#-----------------------------------------------------------------------------
# Find wrapping language API libraries.
if(ITK_CSWIG_TCL)
  find_package(TCL)
  # Hide useless settings provided by FindTCL.
  foreach(entry TCL_LIBRARY_DEBUG
                TK_LIBRARY_DEBUG
                TCL_STUB_LIBRARY
                TCL_STUB_LIBRARY_DEBUG
                TK_STUB_LIBRARY
                TK_STUB_LIBRARY_DEBUG
                TK_WISH)
    set(${entry} "${${entry}}" CACHE INTERNAL "This value is not used by ITK.")
  endforeach(entry)
endif(ITK_CSWIG_TCL)

if(ITK_CSWIG_PYTHON)
  include(${CMAKE_ROOT}/Modules/FindPythonLibs.cmake)
  find_program(PYTHON_EXECUTABLE
    NAMES python python2.3 python2.2 python2.1 python2.0 python1.6 python1.5
    PATHS
    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.3\\InstallPath]
    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.2\\InstallPath]
    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.1\\InstallPath]
    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.0\\InstallPath]
    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\1.6\\InstallPath]
    [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\1.5\\InstallPath]
    )
  mark_as_advanced(PYTHON_EXECUTABLE)
endif(ITK_CSWIG_PYTHON)

if(ITK_CSWIG_JAVA)
  find_package(Java)
  find_package(JNI)
endif(ITK_CSWIG_JAVA)

if(ITK_CSWIG_PERL)
  find_package(Perl)
  find_package(PerlLibs)
endif(ITK_CSWIG_PERL)

mark_as_advanced(ITK_CSWIG_PYTHON)
mark_as_advanced(ITK_CSWIG_TCL)
mark_as_advanced(ITK_CSWIG_JAVA)
mark_as_advanced(ITK_CSWIG_PERL)
mark_as_advanced(CABLE_INDEX)
mark_as_advanced(CSWIG)
mark_as_advanced(GCCXML)

# set a variable to determine if
# the CSwig directory should be used
set(ITK_CSWIG_DIR 0)
if(ITK_CSWIG_TCL)
  set(ITK_CSWIG_DIR 1)
endif(ITK_CSWIG_TCL)
if(ITK_CSWIG_PYTHON)
  set(ITK_CSWIG_DIR 1)
endif(ITK_CSWIG_PYTHON)
if(ITK_CSWIG_JAVA)
  set(ITK_CSWIG_DIR 1)
endif(ITK_CSWIG_JAVA)

if(ITK_CSWIG_DIR)
  add_subdirectory(CSwig)
endif(ITK_CSWIG_DIR)

if(USE_WRAP_ITK)
  add_subdirectory(WrapITK)
  # be sure that wrapitk is not used with the older system
  foreach(lang TCL PYTHON JAVA)
    if(ITK_CSWIG_${lang} AND WRAP_ITK_${lang})
      message(FATAL_ERROR "ITK_CSWIG_${lang} and WRAP_ITK_${lang} can't be used together. Please disable one of them.")
    endif(ITK_CSWIG_${lang} AND WRAP_ITK_${lang})
  endforeach(lang)
endif(USE_WRAP_ITK)

