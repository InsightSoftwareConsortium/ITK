#!/usr/bin/env bash
set -ex
BUILD_DIR="${SRC_DIR}/build"
cmake -DCOMPONENT=Development -P "${BUILD_DIR}/cmake_install.cmake"
cmake -DCOMPONENT=Headers -P "${BUILD_DIR}/cmake_install.cmake"
# GDCM bundles static jpeg libs (gdcmjpeg8/12/16) whose ARCHIVE rules
# install under the "DebugDevel" component. ITKTargets.cmake references
# these .lib files on Windows, so include DebugDevel for cross-platform
# consistency (no-op on Linux/macOS where no static .a is installed here).
cmake -DCOMPONENT=DebugDevel -P "${BUILD_DIR}/cmake_install.cmake"
