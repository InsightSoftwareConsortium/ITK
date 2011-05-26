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

mark_as_advanced(CABLE_INDEX)
mark_as_advanced(CSWIG)
mark_as_advanced(GCCXML)

if(USE_WRAP_ITK)
  add_subdirectory(WrapITK)
endif(USE_WRAP_ITK)

