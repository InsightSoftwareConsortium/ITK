@echo off
set BUILD_DIR=%SRC_DIR%\build
cmake -DCOMPONENT=Development -P "%BUILD_DIR%\cmake_install.cmake"
if errorlevel 1 exit 1
cmake -DCOMPONENT=Headers -P "%BUILD_DIR%\cmake_install.cmake"
if errorlevel 1 exit 1
REM GDCM bundles static jpeg libs (gdcmjpeg8/12/16) whose ARCHIVE rules
REM install under the "DebugDevel" component. ITKTargets.cmake references
REM these .lib files, so they must be present for downstream find_package
REM to succeed on Windows.
cmake -DCOMPONENT=DebugDevel -P "%BUILD_DIR%\cmake_install.cmake"
if errorlevel 1 exit 1
cmake -DCOMPONENT=ThirdParty -P "%BUILD_DIR%\cmake_install.cmake"
if errorlevel 1 exit 1
