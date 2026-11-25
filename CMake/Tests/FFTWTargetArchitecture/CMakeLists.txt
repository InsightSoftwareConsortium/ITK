cmake_minimum_required(VERSION 3.22.1)
project(FFTWTargetArchitectureCheck LANGUAGES NONE)

include(CheckCSourceCompiles)

function(check_c_source_compiles source result)
  set(${result} "${compiler_simd}" PARENT_SCOPE)
endfunction()

function(
  check_target
  processor
  architectures
  expected_neon
  expected_sse
  expected_avx
)
  set(APPLE TRUE)
  set(CMAKE_SYSTEM_PROCESSOR "${processor}")
  set(CMAKE_OSX_ARCHITECTURES "${architectures}")
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
  include(
    "${CMAKE_CURRENT_LIST_DIR}/../../../Modules/ThirdParty/FFTW/itkFFTWOptions.cmake"
  )
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
    OR
      NOT
        "${FFTW_ENABLE_AVX}"
          STREQUAL
          "${expected_avx}"
    OR
      NOT
        "${FFTW_ENABLE_AVX2}"
          STREQUAL
          "${expected_avx}"
  )
    message(
      FATAL_ERROR
      "FFTW defaults disagree with target: ${processor}->${architectures}"
    )
  endif()
endfunction()

set(compiler_simd OFF)
check_target(arm64 x86_64 OFF ON OFF)
check_target(x86_64 arm64 ON OFF OFF)
check_target(x86_64 "" OFF ON OFF)
check_target(aarch64 "" ON OFF OFF)
check_target(i686 "" OFF OFF OFF)
check_target(arm64 "arm64;x86_64" OFF OFF OFF)

set(compiler_simd ON)
check_target(x86_64 "" OFF ON ON)

set(FFTW_ENABLE_AVX OFF CACHE BOOL "" FORCE)
set(CMAKE_SYSTEM_PROCESSOR x86_64)
include(
  "${CMAKE_CURRENT_LIST_DIR}/../../../Modules/ThirdParty/FFTW/itkFFTWOptions.cmake"
)
if(FFTW_ENABLE_AVX)
  message(FATAL_ERROR "FFTW SIMD defaults override an explicit user option")
endif()
