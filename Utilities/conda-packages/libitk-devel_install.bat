@echo off
set BUILD_DIR=%SRC_DIR%\build
cmake -DCOMPONENT=Development -P "%BUILD_DIR%\cmake_install.cmake"
if errorlevel 1 exit 1
cmake -DCOMPONENT=Headers -P "%BUILD_DIR%\cmake_install.cmake"
if errorlevel 1 exit 1
