@echo off
:: Thin wrapper: opens an x64 Native Tools shell and runs build-windows.ps1
:: Run this from any Command Prompt on a VS 2022 machine.
::
:: Usage:
::   build-windows.bat                        (baseline)
::   build-windows.bat -Expose 4244,4305,4309 (expose specific warnings)
::
:: Requires:
::   Visual Studio 2022 (default install path assumed; edit VCVARS64 if different)

set "VCVARS64=%ProgramFiles%\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat"
if not exist "%VCVARS64%" set "VCVARS64=%ProgramFiles%\Microsoft Visual Studio\2022\Professional\VC\Auxiliary\Build\vcvars64.bat"
if not exist "%VCVARS64%" set "VCVARS64=%ProgramFiles%\Microsoft Visual Studio\2022\Enterprise\VC\Auxiliary\Build\vcvars64.bat"

if not exist "%VCVARS64%" (
  echo ERROR: Could not find vcvars64.bat for VS 2022.
  echo        Edit VCVARS64 path in %~f0
  exit /b 1
)

set "PS=%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe"
set "SCRIPT=%~dp0build-windows.ps1"

:: Forward all arguments to the PowerShell script
"%PS%" -NoProfile -ExecutionPolicy Bypass -Command ^
  "& { & '%VCVARS64%'; & '%SCRIPT%' %* }"
