@echo off
set BUILD_DIR=%SRC_DIR%\build

rem Install Python wrapping artifacts (compiled extension modules + Python files).
rem The PythonWrappingRuntimeLibraries component is isolated from the core C++
rem runtime by building with -DWRAP_ITK_INSTALL_COMPONENT_IDENTIFIER=PythonWrapping,
rem which directs all wrapping install() calls to this dedicated component.
cmake -DCOMPONENT=PythonWrappingRuntimeLibraries -P "%BUILD_DIR%\cmake_install.cmake"
if errorlevel 1 exit 1
