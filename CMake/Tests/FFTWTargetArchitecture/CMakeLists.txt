cmake_minimum_required(VERSION 3.22.1)
project(FFTWTargetArchitectureCheck LANGUAGES NONE)

file(READ "${CMAKE_CURRENT_LIST_DIR}/../../itkExternal_FFTW.cmake" source)
string(FIND "${source}" "set(_fftw_default_neon OFF)" selection_start)
string(FIND "${source}" "# Macro to generate library filename" selection_end)
math(EXPR selection_length "${selection_end} - ${selection_start}")
string(SUBSTRING "${source}" ${selection_start} ${selection_length} selection)
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/selection.cmake" "${selection}")

function(check_c_source_compiles source result)
  set(${result} OFF PARENT_SCOPE)
endfunction()

set(APPLE TRUE)
set(failures "")
foreach(host IN ITEMS arm64 x86_64)
  set(CMAKE_SYSTEM_PROCESSOR "${host}")
  if(host STREQUAL "arm64")
    set(CMAKE_OSX_ARCHITECTURES x86_64)
    set(expected_neon OFF)
    set(expected_sse ON)
  else()
    set(CMAKE_OSX_ARCHITECTURES arm64)
    set(expected_neon ON)
    set(expected_sse OFF)
  endif()
  foreach(
    isa
    IN
    ITEMS
      NEON
      SSE
      SSE2
      AVX
      AVX2
  )
    unset(FFTW_ENABLE_${isa} CACHE)
    unset(FFTW_ENABLE_${isa})
  endforeach()
  include("${CMAKE_CURRENT_BINARY_DIR}/selection.cmake")
  if(
    NOT
      "${FFTW_ENABLE_NEON}"
        STREQUAL
        "${expected_neon}"
    OR
      NOT
        "${FFTW_ENABLE_SSE}"
          STREQUAL
          "${expected_sse}"
    OR
      NOT
        "${FFTW_ENABLE_SSE2}"
          STREQUAL
          "${expected_sse}"
  )
    list(APPEND failures "${host}->${CMAKE_OSX_ARCHITECTURES}")
  endif()
endforeach()
if(failures)
  message(FATAL_ERROR "FFTW defaults disagree with target: ${failures}")
endif()
