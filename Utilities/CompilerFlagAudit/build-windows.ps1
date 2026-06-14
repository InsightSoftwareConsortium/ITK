# ITK Release+ccache build for compiler-flag investigation (Windows/MSVC)
#
# Run from a "Developer PowerShell for VS 2022" (or x64 Native Tools Command Prompt).
#
# Usage:
#   .\build-windows.ps1                          # baseline Release build
#   .\build-windows.ps1 -Expose 4244             # remove #pragma warning(disable:4244), rebuild
#   .\build-windows.ps1 -Expose 4244,4305,4309   # remove multiple suppressions
#   .\build-windows.ps1 -Expose 4127,4244,4305,4309,4505  # all removal candidates
#   .\build-windows.ps1 -Jobs 16                 # parallel job count (default: CPU count)
#
# Prerequisites:
#   - Visual Studio 2022 with C++ workload
#   - CMake 3.22+ and Ninja in PATH (available after 'pixi install -e cxx')
#   - ccache 4.x in PATH  (or set -UseScache to try sccache instead)
#
# Output:
#   build-flag-audit\build.log  -- full build output
#   build-flag-audit\warnings-C<num>.txt -- per-warning filtered output

param(
  [string]  $Expose    = "",     # comma-separated MSVC warning numbers, e.g. "4244,4305"
  [int]     $Jobs      = [System.Environment]::ProcessorCount,
  [switch]  $UseSccache,         # use sccache instead of ccache (sometimes easier on Windows)
  [switch]  $NoPatch            # skip patching, just build (for re-runs after manual edits)
)

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot  = (Resolve-Path "$ScriptDir\..\.." ).Path
$BuildDir  = "$RepoRoot\build-flag-audit"
$Win32Header = "$RepoRoot\Modules\Core\Common\include\itkWin32Header.h"
$Backup    = "$Win32Header.bak"

$Launcher = if ($UseSccache) { "sccache" } else { "ccache" }

# ── Restore on exit ──────────────────────────────────────────────────────────
$restoreNeeded = $false
function Restore-Header {
  if ($restoreNeeded -and (Test-Path $Backup)) {
    Copy-Item $Backup $Win32Header -Force
    Remove-Item $Backup -Force
    Write-Host "[restore] itkWin32Header.h restored"
  }
}
Register-EngineEvent PowerShell.Exiting -Action { Restore-Header } | Out-Null

# ── Patch itkWin32Header.h ────────────────────────────────────────────────────
$ExposeList = @()
if ($Expose -ne "" -and -not $NoPatch) {
  $ExposeList = $Expose -split "," | ForEach-Object { $_.Trim() }
  Copy-Item $Win32Header $Backup -Force
  $restoreNeeded = $true

  $content = Get-Content $Win32Header -Raw
  foreach ($num in $ExposeList) {
    # Remove both the comment line and the pragma line for each warning number
    $content = $content -replace "(?m)^[ \t]*//[^\n]*\n[ \t]*#[ \t]*pragma warning\(disable : $num\)[ \t]*\r?\n", ""
    # Fallback: remove just the pragma line if no comment precedes it
    $content = $content -replace "(?m)^[ \t]*#[ \t]*pragma warning\(disable : $num\)[ \t]*\r?\n", ""
  }
  Set-Content $Win32Header $content -NoNewline
  Write-Host "[patch] Removed #pragma warning(disable) for: $($ExposeList -join ', ')"
  Write-Host "        Edit: $Win32Header"
}

# ── CMake configure ───────────────────────────────────────────────────────────
if (-not (Test-Path "$BuildDir\CMakeCache.txt")) {
  Write-Host "[configure] $BuildDir"
  cmake `
    -B $BuildDir `
    -S $RepoRoot `
    -G Ninja `
    -DCMAKE_BUILD_TYPE:STRING=Release `
    -DBUILD_TESTING:BOOL=ON `
    "-DCMAKE_C_COMPILER_LAUNCHER:STRING=$Launcher" `
    "-DCMAKE_CXX_COMPILER_LAUNCHER:STRING=$Launcher"
  if ($LASTEXITCODE -ne 0) { Restore-Header; exit $LASTEXITCODE }
}

# ── Build ─────────────────────────────────────────────────────────────────────
Write-Host "[build] -j $Jobs"
$LogFile = "$BuildDir\build.log"
cmake --build $BuildDir -j $Jobs 2>&1 | Tee-Object -FilePath $LogFile
$buildExit = $LASTEXITCODE

# ── Extract per-warning files ─────────────────────────────────────────────────
$allWarnings = @{}
foreach ($num in $ExposeList) {
  $hits = Select-String -Path $LogFile -Pattern "warning C$num" | Select-Object -ExpandProperty Line
  if ($hits.Count -gt 0) {
    $outFile = "$BuildDir\warnings-C$num.txt"
    $hits | Set-Content $outFile
    $allWarnings[$num] = $hits.Count
    Write-Host "[C$num] $($hits.Count) warnings -> $outFile"
  } else {
    Write-Host "[C$num] 0 warnings (suppressed via pragma or not triggered)"
  }
}

# ── Summary ───────────────────────────────────────────────────────────────────
Write-Host ""
Write-Host "=== Build summary ==="
$totalWarnings = (Select-String -Path $LogFile -Pattern " warning C\d{4}").Count
Write-Host "Total MSVC warnings: $totalWarnings"
Write-Host "Log: $LogFile"

Restore-Header

if ($buildExit -ne 0) {
  Write-Host "[FAIL] Build exited with code $buildExit"
  exit $buildExit
}
