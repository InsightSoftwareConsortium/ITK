@echo off
setlocal enabledelayedexpansion

set BUILD_DIR=%SRC_DIR%\build
mkdir "%BUILD_DIR%"
cd /d "%BUILD_DIR%"

set CL=/MP

REM Optional ccache passthrough. Activated only when the host shell sets
REM ITK_CONDA_USE_CCACHE=1 AND the recipe's build requirements pulled in
REM the ccache binary (see recipe.yaml:build conditional).
set CCACHE_CMAKE_ARGS=
if "%ITK_CONDA_USE_CCACHE%"=="1" (
    where ccache >nul 2>nul && (
        echo ccache enabled for Windows conda build
        set "CCACHE_CMAKE_ARGS=-DCMAKE_C_COMPILER_LAUNCHER=ccache -DCMAKE_CXX_COMPILER_LAUNCHER=ccache"
    )
)

REM conda-forge's Windows sandbox path (%SRC_DIR%) exceeds ITK's default
REM 50-char source-path limit. Modern Windows (including the GitHub
REM Actions and conda-forge runners) has LongPathsEnabled=1, so the
REM check is safely skipped.
cmake -GNinja %CCACHE_CMAKE_ARGS% ^
    -DITK_SKIP_PATH_LENGTH_CHECKS:BOOL=ON ^
    -DCMAKE_BUILD_TYPE:STRING=Release ^
    -DCMAKE_INSTALL_PREFIX:PATH="%LIBRARY_PREFIX%" ^
    -DCMAKE_PREFIX_PATH:PATH="%LIBRARY_PREFIX%" ^
    -DBUILD_SHARED_LIBS:BOOL=ON ^
    -DBUILD_TESTING:BOOL=OFF ^
    -DBUILD_EXAMPLES:BOOL=OFF ^
    -DITK_BUILD_DEFAULT_MODULES:BOOL=ON ^
    -DITK_USE_KWSTYLE:BOOL=OFF ^
    -DITK_DEFAULT_THREADER:STRING=Pool ^
    -DModule_ITKReview:BOOL=ON ^
    -DModule_ITKTBB:BOOL=ON ^
    -DModule_MGHIO:BOOL=ON ^
    -DModule_ITKIOTransformInsightLegacy:BOOL=ON ^
    -DModule_ITKDeprecated:BOOL=ON ^
    -DITK_USE_SYSTEM_FFTW:BOOL=ON ^
    -DITK_USE_SYSTEM_HDF5:BOOL=ON ^
    -DITK_USE_SYSTEM_JPEG:BOOL=ON ^
    -DITK_USE_SYSTEM_TIFF:BOOL=ON ^
    -DITK_USE_SYSTEM_EIGEN:BOOL=ON ^
    -DITK_USE_SYSTEM_DOUBLECONVERSION:BOOL=ON ^
    -DITK_USE_FFTWD:BOOL=ON ^
    -DITK_USE_FFTWF:BOOL=ON ^
    -DITK_FORBID_DOWNLOADS:BOOL=ON ^
    -DITK_WRAP_PYTHON:BOOL=ON ^
    -DWRAP_ITK_INSTALL_COMPONENT_IDENTIFIER:STRING=PythonWrapping ^
    -DPython3_EXECUTABLE:FILEPATH="%PYTHON%" ^
    ..
if errorlevel 1 exit 1

cmake --build . --config Release
if errorlevel 1 exit 1
