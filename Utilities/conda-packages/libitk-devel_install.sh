#!/usr/bin/env bash
set -ex
BUILD_DIR="${SRC_DIR}/build"
cmake -DCOMPONENT=Development -P "${BUILD_DIR}/cmake_install.cmake"
cmake -DCOMPONENT=Headers -P "${BUILD_DIR}/cmake_install.cmake"
