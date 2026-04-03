#
# Encapsulates building FFTW as an External Project.
#
# SIMD codelet selection and binary redistribution policy
# -------------------------------------------------------
# FFTW SIMD codelets are hand-written assembly routines compiled INTO the
# library at build time.  Unlike -march=native on the ITK side, FFTW codelets
# must be requested explicitly via FFTW's own CMake options
# (ENABLE_NEON, ENABLE_SSE, ENABLE_SSE2, ENABLE_AVX, ENABLE_AVX2).
#
# For redistributable binary packages (conda, pip/PyPI, manylinux Docker
# images, etc.) SIMD codelets must only be enabled when the resulting binary
# will run correctly on ALL machines in the target distribution.  The ISA
# baseline mandated by each architecture ABI is universally safe:
#
#   x86_64 / AMD64  : SSE and SSE2 are required by the AMD64 ABI.  Every
#                     64-bit x86 CPU (including all manylinux2014 /
#                     manylinux_2_28 targets) supports them.  DEFAULT ON.
#
#   aarch64 / arm64 : NEON is required by the AArch64 ABI.  Every 64-bit ARM
#                     CPU (Apple Silicon, all Linux aarch64 targets) supports
#                     it.  DEFAULT ON.
#
#   AVX / AVX2      : Not part of the baseline ABI; present only on Sandy
#                     Bridge (2011) and Haswell (2013) and newer CPUs
#                     respectively.  Enabling them by default would produce
#                     binaries that SIGILL on older (but spec-compliant)
#                     x86_64 CPUs.  DEFAULT OFF unless the compiler is already
#                     targeting a micro-architecture that includes them.
#
# Opt-in to AVX / AVX2
# ---------------------
# If the user's toolchain is already generating AVX/AVX2 instructions
# (because they passed -march=native, -mavx2, -march=haswell, or an
# equivalent MSVC /arch: flag) the compiler pre-defines __AVX__ / __AVX2__.
# This file detects those macros at cmake configure time via
# check_c_source_compiles (compile-time, NOT runtime — no build-host CPU
# probe is performed) and auto-enables the matching FFTW codelets so that
# FFTW's generated code aligns with the rest of the ITK build.
# Users who want AVX2 in a redistributed package can set:
#   cmake -DFFTW_ENABLE_AVX2=ON ...
#
# macOS universal binary
# ----------------------
# When CMAKE_OSX_ARCHITECTURES lists more than one value (e.g. "arm64;x86_64")
# a single FFTW configure/build pass cannot correctly serve both slices.
# SIMD defaults are set to OFF in this case; use ITK_USE_SYSTEM_FFTW with a
# proper universal FFTW installation (e.g., built with lipo) if SIMD
# performance is required in a macOS universal build.
#
# Every flag remains individually overridable, e.g.:
#   cmake -DFFTW_ENABLE_AVX2=ON   # opt in to AVX2 for a non-redistributed build
#   cmake -DFFTW_ENABLE_SSE2=OFF  # opt out of SSE2 (unusual)
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
  #
  # Architecture-guaranteed ISA baselines (no runtime probe needed):
  #   - x86_64 mandates SSE + SSE2 in the AMD64 ABI.
  #   - arm64/aarch64 mandates NEON in the AArch64 ABI.
  #
  # AVX/AVX2 opt-in via compiler predefined macros:
  #   check_c_source_compiles (not _runs) reflects what the compiler is
  #   generating for the TARGET architecture, not what the BUILD HOST's CPU
  #   can execute.  This is safe for cross-compilation and redistribution.
  include(CheckCSourceCompiles)

  set(_fftw_default_neon OFF)
  set(_fftw_default_sse OFF)
  set(_fftw_default_sse2 OFF)
  set(_fftw_default_avx OFF)
  set(_fftw_default_avx2 OFF)

  # Detect macOS universal binary build: a single configure+build pass cannot
  # simultaneously produce correct SIMD for both arm64 and x86_64 slices.
  set(_fftw_is_universal FALSE)
  if(APPLE AND CMAKE_OSX_ARCHITECTURES)
    list(LENGTH CMAKE_OSX_ARCHITECTURES _fftw_arch_count)
    if(_fftw_arch_count GREATER 1)
      set(_fftw_is_universal TRUE)
      message(
        STATUS
        "FFTW: macOS universal binary (${CMAKE_OSX_ARCHITECTURES}): "
        "per-architecture SIMD defaults disabled.  "
        "Use ITK_USE_SYSTEM_FFTW with a universal FFTW to enable SIMD."
      )
    endif()
  endif()

  if(NOT _fftw_is_universal)
    if(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64|ARM64")
      # NEON is mandatory in the AArch64 ABI — every arm64 CPU has it.
      # Safe for all conda/pip arm64 packages and manylinux aarch64.
      set(_fftw_default_neon ON)
    elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64|AMD64")
      # SSE and SSE2 are required by the AMD64 ABI — universally present on
      # every 64-bit x86 CPU, including the oldest manylinux build targets.
      # Safe for all conda/pip x86_64 packages.
      set(_fftw_default_sse ON)
      set(_fftw_default_sse2 ON)
      # AVX and AVX2 are NOT part of the AMD64 baseline.  Auto-enable them
      # only when the compiler is already producing those instructions — i.e.
      # when the user explicitly asked for a specific micro-architecture via
      # -march=native, -mavx2, /arch:AVX2, etc.  This compile-time check
      # mirrors the approach recommended by seanm in ITK PR #6006:
      # "the compiler knows what CPU it's compiling for."
      #
      # check_c_source_compiles caches its result by variable name.  Unset
      # the cache entry first so the probe always re-runs against the current
      # CMAKE_C_FLAGS; this ensures that adding -march=native on a subsequent
      # configure is correctly reflected in the auto-detected default.
      # Note: FFTW_ENABLE_AVX / FFTW_ENABLE_AVX2 follow standard option()
      # caching — they are only auto-set from the detected default when not
      # already present in the cache.  To force re-evaluation of the option
      # after a FLAGS change, delete those entries from the CMake cache or
      # pass -DFFTW_ENABLE_AVX2=ON explicitly.
      unset(_fftw_compiler_targets_avx CACHE)
      check_c_source_compiles(
        "#if !defined(__AVX__) || !__AVX__\n#error AVX not enabled\n#endif\nint main(void){return 0;}"
        _fftw_compiler_targets_avx
      )
      if(_fftw_compiler_targets_avx)
        set(_fftw_default_avx ON)
      endif()
      unset(_fftw_compiler_targets_avx2 CACHE)
      check_c_source_compiles(
        "#if !defined(__AVX2__) || !__AVX2__\n#error AVX2 not enabled\n#endif\nint main(void){return 0;}"
        _fftw_compiler_targets_avx2
      )
      if(_fftw_compiler_targets_avx2)
        set(_fftw_default_avx2 ON)
      endif()
    elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "i686|i386")
      # 32-bit x86 ABI does not mandate SSE/SSE2.  Leave defaults OFF;
      # users may opt in explicitly if their minimum target CPU supports them.
    endif()
  endif()

  option(
    FFTW_ENABLE_NEON
    "Enable FFTW NEON SIMD codelets (ARM64; ON by default on aarch64/arm64)"
    ${_fftw_default_neon}
  )
  option(
    FFTW_ENABLE_SSE
    "Enable FFTW SSE SIMD codelets (x86; ON by default on x86_64 — required by AMD64 ABI)"
    ${_fftw_default_sse}
  )
  option(
    FFTW_ENABLE_SSE2
    "Enable FFTW SSE2 SIMD codelets (x86; ON by default on x86_64 — required by AMD64 ABI)"
    ${_fftw_default_sse2}
  )
  option(
    FFTW_ENABLE_AVX
    "Enable FFTW AVX SIMD codelets (Sandy Bridge+; OFF by default for redistribution safety)"
    ${_fftw_default_avx}
  )
  option(
    FFTW_ENABLE_AVX2
    "Enable FFTW AVX2 SIMD codelets (Haswell+; OFF by default for redistribution safety)"
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
