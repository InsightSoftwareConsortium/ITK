
# make easier to unable the main supported languages, by provided the option even when
# the Wrapping direrctory has not yet been included
option(ITK_WRAP_PYTHON "Build python support" OFF)
option(ITK_WRAP_JAVA "Build java support" OFF)

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
endif(ITK_WRAPPING)

