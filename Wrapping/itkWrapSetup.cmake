option(USE_WRAP_ITK "Build external languages support" OFF)
mark_as_advanced(USE_WRAP_ITK)
if(USE_WRAP_ITK)
  # required for the FlatStructuringElement to be included.
  # without that, the external projects won't build
  # TODO: remove this check once FlatStructuringElement will be moved out of
  #       the review directory
  if(NOT ITK_USE_REVIEW)
    message(SEND_ERROR "WrapITK requires ITK_USE_REVIEW to be ON.")
  endif(NOT ITK_USE_REVIEW)
  if(NOT ITK_BUILD_SHARED_LIBS)
    message(FATAL_ERROR "Wrapping requires a shared build, change BUILD_SHARED_LIBS to ON")
  endif()
endif(USE_WRAP_ITK)

