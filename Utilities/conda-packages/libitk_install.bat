@echo off
set BUILD_DIR=%SRC_DIR%\build
REM See libitk_install.sh for component list rationale. Keep these two lists in sync.
for %%c in (Runtime RuntimeLibraries Libraries Unspecified libraries Applications) do (
    cmake -DCOMPONENT=%%c -P "%BUILD_DIR%\cmake_install.cmake"
    if errorlevel 1 exit 1
)
