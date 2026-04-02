#
# Encapsulates building FFTW as an External Project.
#
# SIMD codelet selection
# ----------------------
# FFTW SIMD codelets are hand-written assembly routines baked into the
# library at compile time.  Passing -march=native to the ITK build does
# NOT activate them; they must be requested explicitly via FFTW's own
# CMake options (ENABLE_NEON, ENABLE_SSE, ENABLE_SSE2, ENABLE_AVX, ENABLE_AVX2).
#
# This file detects appropriate defaults at cmake configure time:
#
#   Native builds (CMAKE_CROSSCOMPILING is false):
#     - ARM64 (aarch64/arm64/ARM64): NEON=ON (mandatory in ARMv8); x86 SIMD off.
#     - x86/x86_64 with GCC/Clang: each of SSE, SSE2, AVX, AVX2 is probed
#       individually via __builtin_cpu_supports() / CheckCSourceRuns so that
#       the detected flags match the actual build-host CPU.  A pre-AVX
#       Sandy Bridge gets SSE+SSE2 only; a Haswell or later gets all four.
#       On MSVC the probes are skipped (intrinsic unavailable) and SIMD
#       defaults to off; users can override via FFTW_ENABLE_* options.
#     - Other architectures: all SIMD off (conservative fallback).
#
#   Cross-compiled builds (CMAKE_CROSSCOMPILING is true):
#     - ARM64: NEON=ON (mandatory); x86 SIMD off.
#     - x86_64: SSE+SSE2 only (baseline; AVX/AVX2 not assumed for target).
#     - Other: all SIMD off.
#
# Every flag is an individually overridable cache option, e.g.:
#   cmake -DFFTW_ENABLE_AVX2=OFF ...
# Note: option() defaults are only applied on the first configure.
# To re-detect after a toolchain change, delete the CMake cache or use
# cmake --fresh, or pass explicit -DFFTW_ENABLE_*= overrides.
#
# ENABLE_SSE (SSE1) is float-only and is not forwarded to the
# double-precision fftwd build.
#
# These instructions follow the guidance provided for modern cmake usage as described:
# https://github.com/dev-cafe/cmake-cookbook/blob/master/chapter-08/recipe-03/c-example/external/upstream/fftw3/CMakeLists.txt
#
include(ITK_CheckCCompilerFlag)

if(NOT ITK_USE_MKL AND NOT ITK_USE_CUFFTW)
  set(msg "ATTENTION: You have enabled the use of FFTW.")
  set(msg "${msg} This library is distributed under a GPL license.")
  set(msg "${msg} By enabling this option, the ITK libraries binary")
  set(msg "${msg} that is built will be covered by a GPL license")
  set(
    msg
    "${msg} and so will any executable that is linked against these libraries."
  )
  message("${msg}")
endif()

if(NOT ITK_USE_SYSTEM_FFTW)
  #
  # fftw limitation -- can't be built in
  # a directory with whitespace in its name.
  if(${CMAKE_CURRENT_BINARY_DIR} MATCHES ".*[ \t].*")
    message(
      FATAL_ERROR
      "Can't build fftw in a directory with whitespace in its name"
    )
  endif()

  if(WIN32)
    # on windows the build of FFTW must match that of ITK
    set(FFTW_BUILD_TYPE ${CMAKE_BUILD_TYPE})
  else()
    set(FFTW_BUILD_TYPE Release)
  endif()

  include(GNUInstallDirs)

  set(_fftw_target_version 3.3.10)
  set(
    _fftw_url_hash
    "2d34b5ccac7b08740dbdacc6ebe451d8a34cf9d9bfec85a5e776e87adf94abfd803c222412d8e10fbaa4ed46f504aa87180396af1b108666cde4314a55610b40"
  )
  set(
    _fftw_url
    "https://data.kitware.com/api/v1/file/hashsum/sha512/${_fftw_url_hash}/download"
  )

  set(FFTW_STAGED_INSTALL_PREFIX "${ITK_BINARY_DIR}/fftw")

  # Detect SIMD defaults (see file header for full policy description).
  # CheckCSourceRuns results are cached after the first cmake configure run.
  include(CheckCSourceRuns)

  set(_fftw_default_neon OFF)
  set(_fftw_default_sse OFF)
  set(_fftw_default_sse2 OFF)
  set(_fftw_default_avx OFF)
  set(_fftw_default_avx2 OFF)

  if(NOT CMAKE_CROSSCOMPILING)
    if(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64|ARM64")
      # NEON is mandatory in ARMv8/AArch64 — every arm64 CPU has it.
      set(_fftw_default_neon ON)
    elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64|AMD64|i686")
      # Probe each x86 SIMD level individually via CPUID so the defaults
      # are accurate for the actual build-host CPU (e.g. pre-AVX Sandy Bridge
      # or pre-AVX2 Ivy Bridge get only the levels their hardware supports).
      # __builtin_cpu_supports is a GCC/Clang intrinsic; skip on MSVC.
      if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang|AppleClang")
        foreach(_fftw_simd IN ITEMS sse sse2 avx avx2)
          check_c_source_runs(
            "int main(void){return __builtin_cpu_supports(\"${_fftw_simd}\")?0:1;}"
            _fftw_cpu_has_${_fftw_simd}
          )
          if(_fftw_cpu_has_${_fftw_simd})
            set(_fftw_default_${_fftw_simd} ON)
          endif()
        endforeach()
      endif()
    endif()
  else()
    # Cross-compiling: conservative architecture-level fallback.
    if(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64|ARM64")
      set(_fftw_default_neon ON)
    elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64|AMD64")
      # SSE/SSE2 are baseline on all 64-bit x86 CPUs; AVX/AVX2 not assumed.
      set(_fftw_default_sse ON)
      set(_fftw_default_sse2 ON)
    endif()
  endif()

  option(
    FFTW_ENABLE_NEON
    "Enable FFTW NEON SIMD codelets (ARM64)"
    ${_fftw_default_neon}
  )
  option(
    FFTW_ENABLE_SSE
    "Enable FFTW SSE SIMD codelets (x86)"
    ${_fftw_default_sse}
  )
  option(
    FFTW_ENABLE_SSE2
    "Enable FFTW SSE2 SIMD codelets (x86)"
    ${_fftw_default_sse2}
  )
  option(
    FFTW_ENABLE_AVX
    "Enable FFTW AVX SIMD codelets (x86)"
    ${_fftw_default_avx}
  )
  option(
    FFTW_ENABLE_AVX2
    "Enable FFTW AVX2 SIMD codelets (x86)"
    ${_fftw_default_avx2}
  )

  message(
    STATUS
    "FFTW SIMD: NEON=${FFTW_ENABLE_NEON} SSE=${FFTW_ENABLE_SSE} SSE2=${FFTW_ENABLE_SSE2} AVX=${FFTW_ENABLE_AVX} AVX2=${FFTW_ENABLE_AVX2}"
  )

  # Macro to generate library filename with appropriate prefix/suffix
  # Args: output_var library_base_name
  macro(_library_name_to_filename output_var library_base_name)
    if(BUILD_SHARED_LIBS)
      set(
        ${output_var}
        ${CMAKE_SHARED_LIBRARY_PREFIX}${library_base_name}${CMAKE_SHARED_LIBRARY_SUFFIX}
      )
    else()
      set(
        ${output_var}
        ${CMAKE_STATIC_LIBRARY_PREFIX}${library_base_name}${CMAKE_STATIC_LIBRARY_SUFFIX}
      )
    endif()
  endmacro()

  _library_name_to_filename(_fftw3f_threads_lib fftw3f_threads)
  _library_name_to_filename(_fftw3f_lib fftw3f)
  set(
    ITK_FFTWF_LIBRARIES_NAMES
    $<BUILD_INTERFACE:${FFTW_STAGED_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/${_fftw3f_threads_lib}>$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${CMAKE_INSTALL_LIBDIR}/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}/${_fftw3f_threads_lib}>
    $<BUILD_INTERFACE:${FFTW_STAGED_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/${_fftw3f_lib}>$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${CMAKE_INSTALL_LIBDIR}/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}/${_fftw3f_lib}>
  )

  set(PROJ_FFTWD_DEPENDS "")
  if(ITK_USE_FFTWF)
    itk_download_attempt_check(FFTW)
    ExternalProject_Add(
      fftwf
      PREFIX fftwf-${_fftw_target_version}
      INSTALL_DIR ${FFTW_STAGED_INSTALL_PREFIX}
      URL
        ${_fftw_url}
      URL_HASH SHA512=${_fftw_url_hash}
      DOWNLOAD_NAME "fftw-${_fftw_target_version}.tar.gz"
      DOWNLOAD_NO_PROGRESS 1
      UPDATE_COMMAND
        ""
      LOG_CONFIGURE 1
      LOG_BUILD 1
      LOG_INSTALL 1
      CMAKE_CACHE_ARGS
        -DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS} -DBUILD_TESTS:BOOL=OFF
        -DCMAKE_BUILD_TYPE:STRING=${FFTW_BUILD_TYPE}
        -DCMAKE_INSTALL_PREFIX:PATH=${FFTW_STAGED_INSTALL_PREFIX}
        -DCMAKE_INSTALL_LIBDIR:STRING=${CMAKE_INSTALL_LIBDIR}
        -DCMAKE_INSTALL_BINDIR:STRING=${CMAKE_INSTALL_BINDIR}
        -DDISABLE_FORTRAN:BOOL=ON -DENABLE_AVX:BOOL=${FFTW_ENABLE_AVX}
        -DENABLE_AVX2:BOOL=${FFTW_ENABLE_AVX2} -DENABLE_FLOAT:BOOL=ON
        -DENABLE_LONG_DOUBLE:BOOL=OFF -DENABLE_NEON:BOOL=${FFTW_ENABLE_NEON}
        -DENABLE_OPENMP:BOOL=OFF -DENABLE_QUAD_PRECISION:BOOL=OFF
        -DENABLE_SSE:BOOL=${FFTW_ENABLE_SSE}
        -DENABLE_SSE2:BOOL=${FFTW_ENABLE_SSE2} -DENABLE_THREADS:BOOL=ON
        -DCMAKE_APPLE_SILICON_PROCESSOR:STRING=${CMAKE_APPLE_SILICON_PROCESSOR}
        -DCMAKE_C_COMPILER_LAUNCHER:PATH=${CMAKE_C_COMPILER_LAUNCHER}
        -DCMAKE_C_COMPILER:PATH=${CMAKE_C_COMPILER}
        -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
        -DCMAKE_OSX_SYSROOT:PATH=${CMAKE_OSX_SYSROOT}
        -DCMAKE_OSX_DEPLOYMENT_TARGET:PATH=${CMAKE_OSX_DEPLOYMENT_TARGET}
        -DCMAKE_OSX_ARCHITECTURES:STRING=${CMAKE_OSX_ARCHITECTURES}
        -DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=${CMAKE_POSITION_INDEPENDENT_CODE}
        -DCMAKE_POLICY_VERSION_MINIMUM:STRING=${ITK_OLDEST_VALIDATED_POLICIES_VERSION}
      INSTALL_BYPRODUCTS
        ${FFTW_STAGED_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/${_fftw3f_lib}
        ${FFTW_STAGED_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/${_fftw3f_threads_lib}
    )
    # set(
    #   FFTW3f_DIR ${FFTW_STAGED_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/cmake/fftw3f
    #   CACHE PATH "Path to internally built single precision FFTW3Config.cmake"
    #   FORCE
    #  )
    # Can not find package, it does not yet exist find_package(FFTW3f CONFIG REQUIRED)
    # but we know where it will eventually be!
    set(FFTW3f_INCLUDE_DIRS ${FFTW_STAGED_INSTALL_PREFIX}/include)
    set(
      FFTW3f_LIBRARY_DIRS
      ${FFTW_STAGED_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}
    )
    set(FFTW_INCLUDE ${FFTW3_INCLUDE_DIRS})

    set(PROJ_FFTWD_DEPENDS "fftwf")
  endif()

  if(ITK_USE_FFTWD)
    _library_name_to_filename(_fftw3_threads_lib fftw3_threads)
    _library_name_to_filename(_fftw3_lib fftw3)
    set(
      ITK_FFTWD_LIBRARIES_NAMES
      $<BUILD_INTERFACE:${FFTW_STAGED_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/${_fftw3_threads_lib}>$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${CMAKE_INSTALL_LIBDIR}/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}/${_fftw3_threads_lib}>
      $<BUILD_INTERFACE:${FFTW_STAGED_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/${_fftw3_lib}>$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/${CMAKE_INSTALL_LIBDIR}/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}/${_fftw3_lib}>
    )

    itk_download_attempt_check(FFTW)
    ExternalProject_Add(
      fftwd
      PREFIX fftwd-${_fftw_target_version}
      INSTALL_DIR ${FFTW_STAGED_INSTALL_PREFIX}
      URL
        ${_fftw_url}
      URL_HASH SHA512=${_fftw_url_hash}
      DOWNLOAD_NAME "fftw-${_fftw_target_version}.tar.gz"
      DOWNLOAD_NO_PROGRESS 1
      UPDATE_COMMAND
        ""
      LOG_CONFIGURE 1
      LOG_BUILD 1
      LOG_INSTALL 1
      CMAKE_CACHE_ARGS
        -DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS} -DBUILD_TESTS:BOOL=OFF
        -DCMAKE_BUILD_TYPE:STRING=${FFTW_BUILD_TYPE}
        -DCMAKE_INSTALL_PREFIX:PATH=${FFTW_STAGED_INSTALL_PREFIX}
        -DCMAKE_INSTALL_LIBDIR:STRING=${CMAKE_INSTALL_LIBDIR}
        -DCMAKE_INSTALL_BINDIR:STRING=${CMAKE_INSTALL_BINDIR}
        -DDISABLE_FORTRAN:BOOL=ON -DENABLE_AVX:BOOL=${FFTW_ENABLE_AVX}
        -DENABLE_AVX2:BOOL=${FFTW_ENABLE_AVX2} -DENABLE_FLOAT:BOOL=OFF
        -DENABLE_LONG_DOUBLE:BOOL=OFF -DENABLE_NEON:BOOL=${FFTW_ENABLE_NEON}
        -DENABLE_OPENMP:BOOL=OFF -DENABLE_QUAD_PRECISION:BOOL=OFF
        -DENABLE_SSE:BOOL=OFF # SSE1 codelets are 32-bit float only; no effect on double-precision
        -DENABLE_SSE2:BOOL=${FFTW_ENABLE_SSE2} -DENABLE_THREADS:BOOL=ON
        -DCMAKE_APPLE_SILICON_PROCESSOR:STRING=${CMAKE_APPLE_SILICON_PROCESSOR}
        -DCMAKE_C_COMPILER_LAUNCHER:PATH=${CMAKE_C_COMPILER_LAUNCHER}
        -DCMAKE_C_COMPILER:PATH=${CMAKE_C_COMPILER}
        -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
        -DCMAKE_OSX_SYSROOT:PATH=${CMAKE_OSX_SYSROOT}
        -DCMAKE_OSX_DEPLOYMENT_TARGET:PATH=${CMAKE_OSX_DEPLOYMENT_TARGET}
        -DCMAKE_OSX_ARCHITECTURES:STRING=${CMAKE_OSX_ARCHITECTURES}
        -DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=${CMAKE_POSITION_INDEPENDENT_CODE}
        -DCMAKE_POLICY_VERSION_MINIMUM:STRING=${ITK_OLDEST_VALIDATED_POLICIES_VERSION}
      DEPENDS
        ${PROJ_FFTWD_DEPENDS} # Avoid potential collisions on install
      INSTALL_BYPRODUCTS
        ${FFTW_STAGED_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/${_fftw3_lib}
        ${FFTW_STAGED_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/${_fftw3_threads_lib}
    )
    # set(
    #   FFTW3_DIR ${FFTW_STAGED_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/cmake/fftw3
    #   CACHE PATH "Path to internally built double precision FFTW3Config.cmake"
    #   FORCE
    #  )
    # Can not find package, it does not yet exist find_package(FFTW3 CONFIG REQUIRED)
    set(FFTW3_INCLUDE_DIRS ${FFTW_STAGED_INSTALL_PREFIX}/include)
    set(
      FFTW3_LIBRARY_DIRS
      ${FFTW_STAGED_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}
    )

    set(FFTW_INCLUDE ${FFTW3_INCLUDE_DIRS})
  endif()

  #
  # copy libraries into install tree, NOTE: DESTINATION MUST EXACTLY MATCH values from main CMakeLists.txt for FFTW_LIBDIR
  install(
    CODE
      "file(GLOB FFTW_LIBS ${FFTW_STAGED_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/*fftw3*)
      file(INSTALL DESTINATION \"\${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}\"
TYPE FILE FILES \${FFTW_LIBS})"
    COMPONENT Development
  )
  #
  # copy headers into install tree
  install(
    CODE
      "file(GLOB FFTW_INC ${FFTW_STAGED_INSTALL_PREFIX}/include/*fftw3*)
file(INSTALL DESTINATION \"\${CMAKE_INSTALL_PREFIX}/include/ITK-${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}\"
TYPE FILE FILES \${FFTW_INC})"
    COMPONENT Development
  )
else()
  #Search the filesystem for compatible versions
  find_package(FFTW) # Use local itk FindFFTW.config to set variables consistently both with/without USE_SYSTEM_FFTW
endif()
