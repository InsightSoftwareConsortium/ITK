include(CheckCSourceCompiles)

set(_fftw_default_neon OFF)
set(_fftw_default_sse OFF)
set(_fftw_default_sse2 OFF)
set(_fftw_default_avx OFF)
set(_fftw_default_avx2 OFF)

set(_fftw_is_universal FALSE)
if(APPLE AND CMAKE_OSX_ARCHITECTURES)
  list(LENGTH CMAKE_OSX_ARCHITECTURES _fftw_arch_count)
  if(_fftw_arch_count GREATER 1)
    set(_fftw_is_universal TRUE)
    message(
      STATUS
      "FFTW: macOS universal binary (${CMAKE_OSX_ARCHITECTURES}): SIMD defaults disabled."
    )
  endif()
endif()

if(NOT _fftw_is_universal)
  set(_fftw_target_processor "${CMAKE_SYSTEM_PROCESSOR}")
  if(APPLE AND CMAKE_OSX_ARCHITECTURES)
    set(_fftw_target_processor "${CMAKE_OSX_ARCHITECTURES}")
  endif()
  if(_fftw_target_processor MATCHES "aarch64|arm64|ARM64")
    set(_fftw_default_neon ON)
  elseif(_fftw_target_processor MATCHES "x86_64|AMD64")
    # SSE2 is an ABI baseline; AVX requires an explicit compiler target.
    set(_fftw_default_sse ON)
    set(_fftw_default_sse2 ON)
    unset(_fftw_compiler_targets_avx CACHE)
    check_c_source_compiles(
      "#ifndef __AVX__\n#error AVX not enabled\n#endif\nint main(void){return 0;}"
      _fftw_compiler_targets_avx
    )
    set(_fftw_default_avx ${_fftw_compiler_targets_avx})
    unset(_fftw_compiler_targets_avx2 CACHE)
    check_c_source_compiles(
      "#ifndef __AVX2__\n#error AVX2 not enabled\n#endif\nint main(void){return 0;}"
      _fftw_compiler_targets_avx2
    )
    set(_fftw_default_avx2 ${_fftw_compiler_targets_avx2})
  endif()
endif()

option(FFTW_ENABLE_NEON "Enable FFTW NEON SIMD codelets" ${_fftw_default_neon})
option(
  FFTW_ENABLE_SSE
  "Enable FFTW SSE SIMD codelets (float only)"
  ${_fftw_default_sse}
)
option(FFTW_ENABLE_SSE2 "Enable FFTW SSE2 SIMD codelets" ${_fftw_default_sse2})
option(FFTW_ENABLE_AVX "Enable FFTW AVX SIMD codelets" ${_fftw_default_avx})
option(FFTW_ENABLE_AVX2 "Enable FFTW AVX2 SIMD codelets" ${_fftw_default_avx2})

message(
  STATUS
  "FFTW SIMD: NEON=${FFTW_ENABLE_NEON} SSE=${FFTW_ENABLE_SSE} SSE2=${FFTW_ENABLE_SSE2} AVX=${FFTW_ENABLE_AVX} AVX2=${FFTW_ENABLE_AVX2}"
)
