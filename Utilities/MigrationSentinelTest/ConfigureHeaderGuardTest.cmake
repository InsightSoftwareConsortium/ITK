# ccache guard: the sentinel list must never reach a compiled header.
# If it did, every translation unit's preprocessed output would change on most
# merges and ccache would miss fleet-wide.
cmake_minimum_required(VERSION 3.16)

if(NOT EXISTS "${ITK_CONFIGURE_HEADER}")
  message(FATAL_ERROR "itkConfigure.h not found at ${ITK_CONFIGURE_HEADER}")
endif()

file(READ "${ITK_CONFIGURE_HEADER}" _contents)
string(FIND "${_contents}" "ITK_MIGRATION" _pos)
if(NOT _pos EQUAL -1)
  message(
    FATAL_ERROR
    "itkConfigure.h contains 'ITK_MIGRATION'. The migration sentinel list must "
    "live only in ITKConfig.cmake; embedding it in a compiled header "
    "invalidates ccache for every translation unit on every merge."
  )
endif()
message(STATUS "ConfigureHeaderGuardTest: itkConfigure.h is free of sentinels")
