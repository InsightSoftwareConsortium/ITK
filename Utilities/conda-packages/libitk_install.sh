#!/usr/bin/env bash
set -ex
BUILD_DIR="${SRC_DIR}/build"
# Runtime-flavored CMake install components. The split between libitk,
# libitk-devel, and libitk-wrapping is defined by which components each
# install script claims. Keep this list in sync with the COMPONENT names
# that appear in ITK's install() calls:
#
#   Runtime                - ITK module executables (itk_module_target_install)
#   RuntimeLibraries       - ITK module shared libs + bundled 3rd-party .so
#   Libraries              - bundled DoubleConversion, MINC, OpenJPEG libs
#   libraries              - lowercase, used by bundled libminc
#   Unspecified            - CMake default when no COMPONENT= is given
#   Applications           - bundled OpenJPEG + DoubleConversion executables
#
# Development / Headers go to libitk-devel (see libitk-devel_install.sh).
# PythonWrappingRuntimeLibraries goes to libitk-wrapping.
#
# NOTE: if rattler-build continues to emit "Overdepending against <lib>"
# warnings for system deps (libtiff, libpng, hdf5, ...) after this change,
# inspect the produced libitk-*.conda archive to confirm that the ITKIO*
# module shared libraries (libITKIOTIFF-6.0.so etc.) are actually present
# and have correct NEEDED entries via `readelf -d`. Tracked in AUDIT.md.
for component in Runtime RuntimeLibraries Libraries Unspecified libraries Applications; do
    cmake -DCOMPONENT="${component}" -P "${BUILD_DIR}/cmake_install.cmake"
done
