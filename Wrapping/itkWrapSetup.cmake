# check whether we should go in the wrapping folder, even with ITK_WRAPPING is OFF
# ITK_WRAPPING is an internal variable that indicates wrapping for any
# language will be attempted.
if(NOT ITK_WRAPPING_REACHED)
  if(ITK_WRAP_PYTHON OR ITK_WRAP_JAVA)
    # force ITK_WRAPPING to ON
    set(ITK_WRAPPING ON CACHE INTERNAL "Build external languages support" FORCE)
  endif()
endif()

if(ITK_WRAPPING)
  if(NOT ITK_BUILD_SHARED_LIBS)
    message(FATAL_ERROR "Wrapping requires a shared build, change BUILD_SHARED_LIBS to ON")
  endif()

  if(ITK_USE_64BITS_IDS AND WIN32)
    message(FATAL_ERROR "Wrapping with ITK_USE_64BITS_IDS is not supported on Windows.
    Please turn OFF ITK_USE_64BITS_IDS.")
  endif()
endif()
