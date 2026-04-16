#!/usr/bin/env bash
set -ex

# Remove any stale in-source CMake artifacts that would cause path-mismatch errors
rm -f "${SRC_DIR}/CMakeCache.txt"
rm -rf "${SRC_DIR}/CMakeFiles" "${SRC_DIR}/CMakeTmp"

BUILD_DIR="${SRC_DIR}/build"
mkdir -p "${BUILD_DIR}"

# Skip configure+build if a previous run already produced the build tree.
# Useful when iterating on install/test steps without recompiling ITK.
# Remove $BUILD_DIR/CMakeCache.txt manually to force a clean rebuild.
if [ -f "${BUILD_DIR}/CMakeCache.txt" ]; then
    echo "Existing build detected — skipping cmake configure and build."
    exit 0
fi

# Inject pre-computed TryRun results for cross-compilation targets
CROSS_CMAKE_ARGS=""
if [ "${BUILD_PLATFORM}" != "${TARGET_PLATFORM}" ]; then
    cp "${RECIPE_DIR}/cross-compile/TryRunResults.cmake" "${BUILD_DIR}/"
    CROSS_CMAKE_ARGS="-C ${BUILD_DIR}/TryRunResults.cmake"
fi

# Use TBB on Linux; macOS has known issues with conda-forge TBB
use_tbb=ON
if [ "$(uname)" = "Darwin" ]; then
    use_tbb=OFF
fi

# Optional ccache passthrough. Activated only when the host shell sets
# ITK_CONDA_USE_CCACHE=1 AND the recipe's build requirements pulled in
# the ccache binary (see recipe.yaml:build conditional).
CCACHE_CMAKE_ARGS=""
if [ "${ITK_CONDA_USE_CCACHE:-0}" = "1" ] && command -v ccache >/dev/null 2>&1; then
    # Sandbox-specific vars: rattler-build creates a fresh work dir per build
    # (rattler-build_libitk_<timestamp>/work/), so tell ccache to:
    #  - rewrite absolute paths relative to SRC_DIR (BASEDIR)
    #  - ignore cwd in the hash (NOHASHDIR)
    #  - skip __FILE__/__TIME__/__DATE__ macros and mtime noise (SLOPPINESS)
    #  - hash the compiler binary contents, not mtime (COMPILERCHECK)
    export CCACHE_BASEDIR="${SRC_DIR}"
    export CCACHE_NOHASHDIR=true
    export CCACHE_SLOPPINESS="file_macro,time_macros,include_file_ctime,include_file_mtime,pch_defines,system_headers"
    export CCACHE_COMPILERCHECK=content

    echo "ccache enabled: $(ccache --version | head -1)"
    echo "CCACHE_DIR=${CCACHE_DIR:-<ccache default>}"
    echo "CCACHE_BASEDIR=${CCACHE_BASEDIR}"
    echo "CCACHE_MAXSIZE=${CCACHE_MAXSIZE:-<ccache default>}"
    ccache --show-stats 2>/dev/null || true
    CCACHE_CMAKE_ARGS="-DCMAKE_C_COMPILER_LAUNCHER=ccache -DCMAKE_CXX_COMPILER_LAUNCHER=ccache"
fi

# Optional compile/link flag passthrough. Appends to — does not replace —
# conda-forge's compiler activation flags (CFLAGS/CXXFLAGS/LDFLAGS are
# already populated by {build_env}/etc/conda/activate.d/*.sh).
# See README_Advanced.md for usage.
if [ -n "${ITK_CONDA_EXTRA_CFLAGS:-}" ]; then
    export CFLAGS="${CFLAGS:-} ${ITK_CONDA_EXTRA_CFLAGS}"
    echo "Appended to CFLAGS: ${ITK_CONDA_EXTRA_CFLAGS}"
fi
if [ -n "${ITK_CONDA_EXTRA_CXXFLAGS:-}" ]; then
    export CXXFLAGS="${CXXFLAGS:-} ${ITK_CONDA_EXTRA_CXXFLAGS}"
    echo "Appended to CXXFLAGS: ${ITK_CONDA_EXTRA_CXXFLAGS}"
fi
if [ -n "${ITK_CONDA_EXTRA_LDFLAGS:-}" ]; then
    export LDFLAGS="${LDFLAGS:-} ${ITK_CONDA_EXTRA_LDFLAGS}"
    echo "Appended to LDFLAGS: ${ITK_CONDA_EXTRA_LDFLAGS}"
fi

# Build type is Release by default; override with ITK_CONDA_BUILD_TYPE.
ITK_BUILD_TYPE="${ITK_CONDA_BUILD_TYPE:-Release}"

cmake ${CROSS_CMAKE_ARGS} ${CCACHE_CMAKE_ARGS} ${ITK_CONDA_EXTRA_CMAKE_ARGS:-} \
    -S "${SRC_DIR}" \
    -B "${BUILD_DIR}" \
    -GNinja \
    -DCMAKE_C_COMPILER_AR="${AR}" \
    -DCMAKE_C_COMPILER_RANLIB="${RANLIB}" \
    -DCMAKE_CXX_COMPILER_AR="${AR}" \
    -DCMAKE_CXX_COMPILER_RANLIB="${RANLIB}" \
    -DCMAKE_BUILD_TYPE:STRING="${ITK_BUILD_TYPE}" \
    -DCMAKE_INSTALL_PREFIX:PATH="${PREFIX}" \
    -DCMAKE_PREFIX_PATH:PATH="${PREFIX}" \
    -DBUILD_SHARED_LIBS:BOOL=ON \
    -DBUILD_TESTING:BOOL=OFF \
    -DBUILD_EXAMPLES:BOOL=OFF \
    -DITK_BUILD_DEFAULT_MODULES:BOOL=ON \
    -DITK_USE_KWSTYLE:BOOL=OFF \
    -DITK_DEFAULT_THREADER:STRING=Pool \
    -DModule_ITKReview:BOOL=ON \
    -DModule_ITKTBB:BOOL=${use_tbb} \
    -DModule_MGHIO:BOOL=ON \
    -DModule_ITKIOTransformInsightLegacy:BOOL=ON \
    -DModule_ITKDeprecated:BOOL=ON \
    -DITK_USE_SYSTEM_EXPAT:BOOL=ON \
    -DITK_USE_SYSTEM_FFTW:BOOL=ON \
    -DITK_USE_SYSTEM_HDF5:BOOL=ON \
    -DITK_USE_SYSTEM_JPEG:BOOL=ON \
    -DITK_USE_SYSTEM_PNG:BOOL=ON \
    -DITK_USE_SYSTEM_TIFF:BOOL=ON \
    -DITK_USE_SYSTEM_ZLIB:BOOL=ON \
    -DITK_USE_SYSTEM_EIGEN:BOOL=ON \
    -DITK_USE_SYSTEM_DOUBLECONVERSION:BOOL=ON \
    -DITK_USE_FFTWD:BOOL=ON \
    -DITK_USE_FFTWF:BOOL=ON \
    -DNIFTI_SYSTEM_MATH_LIB:STRING="" \
    -DITK_FORBID_DOWNLOADS:BOOL=ON \
    -DITK_WRAP_PYTHON:BOOL=ON \
    -DITK_USE_SYSTEM_CASTXML:BOOL=ON \
    -DWRAP_ITK_INSTALL_COMPONENT_IDENTIFIER:STRING=PythonWrapping \
    -DPython3_EXECUTABLE:FILEPATH="${PYTHON}"

cmake --build "${BUILD_DIR}" --config Release
