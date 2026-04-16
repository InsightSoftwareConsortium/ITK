#!/usr/bin/env bash
set -ex
BUILD_DIR="${SRC_DIR}/build"

# Install Python wrapping artifacts (compiled extension modules + Python files).
# The PythonWrappingRuntimeLibraries component is isolated from the core C++
# runtime by building with -DWRAP_ITK_INSTALL_COMPONENT_IDENTIFIER=PythonWrapping,
# which directs all wrapping install() calls to this dedicated component.
cmake -DCOMPONENT=PythonWrappingRuntimeLibraries -P "${BUILD_DIR}/cmake_install.cmake"
