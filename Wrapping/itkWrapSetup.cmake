
# Make it easier to enable the main supported languages, by providing the option even when
# the Wrapping directory has not yet been included
option(ITK_WRAP_PYTHON "Build python support" OFF)

option(ITK_WRAPPING "Build external languages support" OFF)
mark_as_advanced(ITK_WRAPPING)

# check whether we should go in the wrapping folder, even with ITK_WRAPPING is OFF
if(NOT ITK_WRAPPING_REACHED)
  if(ITK_WRAP_PYTHON OR ITK_WRAP_JAVA)
    # force ITK_WRAPPING to ON
    unset(ITK_WRAPPING CACHE)
    option(ITK_WRAPPING "Build external languages support" ON)
    mark_as_advanced(ITK_WRAPPING)
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
