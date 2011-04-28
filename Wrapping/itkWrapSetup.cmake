option(USE_WRAP_ITK "Build external languages support" OFF)
mark_as_advanced(USE_WRAP_ITK)
if(USE_WRAP_ITK)
  if(NOT ITK_BUILD_SHARED_LIBS)
    message(FATAL_ERROR "Wrapping requires a shared build, change BUILD_SHARED_LIBS to ON")
  endif()
endif(USE_WRAP_ITK)

