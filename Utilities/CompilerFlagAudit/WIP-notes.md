# WIP: Compiler Flag Suppression Audit

**Branch:** compiler-suppression-audit
**WIP PR:** InsightSoftwareConsortium/ITK#6442 (draft, CI-triage only — do not merge)
**Status:** investigation in progress — this directory is removed before PR merge

CI watch target for MSVC warnings: `Pixi-Cxx (windows-2022)` GitHub Actions job.

---

## Status

### Done (committed)

| Commit | Change |
|---|---|
| `COMP: Remove obsolete Solaris flags and dead MSVC C4786 pragmas` | Removed Solaris blocks; removed 4786 from Video/MinimalPath; converted DCMTKTransformIO raw pragma to macro; added include guard + fixed tab in MinimalPathTest.h |
| `COMP: Remove dead and fixed compiler-warning suppressions` | Promoted from WIP after CI triage validated it: drops MSVC 4127/4244/4305/4309 + `-Wno-strict-overflow`. 4127/4305/4309 + strict-overflow are dead (0 warnings); 4244 sites fixed (see next row). |
| `COMP: Add explicit casts at MSVC C4244 narrowing sites` | 5 files: `itkProcessObject.h:927` (619 instances), `itkWin32OutputWindow.cxx`, `itkCSVFileReaderBase.cxx` (×4), `itkFixedArrayGTest.cxx`, `itkNarrowBandTest.cxx`. Built + ran affected tests locally (all pass). |
| `COMP: Remove global -Wno-format-nonliteral from warning flags` | 5 targeted `ITK_GCC_SUPPRESS_Wformat_nonliteral` sites already present; global flag dropped. ITK proper clean; only RTK remote site fires (legit). |
| `ENH: Add ccache launcher to configure-release pixi task` | `pixi run build-release` now uses ccache |

**Still WIP (remove at merge-prep):** `WIP: Add compiler-flag audit scripts`, `WIP: Update audit notes` — and this `Utilities/CompilerFlagAudit/` directory.

### Audited — no action needed

| Item | Finding |
|---|---|
| `ITK_GCC_SUPPRESS_Wfloat_equal` (all 15 sites) | All intentional: sentinel comparisons to `NumericTraits<T>::max()`, Set-macro "has value changed?", and VNL FFT header wrap. No code smell. |
| `ITK_GCC_SUPPRESS_Wformat_nonliteral` (5 sites) | All legitimate: user-controlled format strings for numeric series filenames and TIFF field format strings. |
| `ITK_GCC_SUPPRESS_Wmaybe_uninitialized` in `itkSymmetricEigenAnalysis.h` | GCC 11+ false positive in Eigen SIMD code. Keep. |
| `INTEL_SUPPRESS_warning_1292` in `itkMultiThreaderBase.h` | Needed while ICC 19.1 is in compiler matrix. Keep. |
| `#pragma optimize("", off/on)` in `itkCompensatedSummation.hxx` | Kahan summation correctness control, not a warning suppression. Keep. |
| Intel ICC name detection via `icc.*`/`icpc.*` | If ICC is dropped from CDash matrix, remove ICC blocks in ITKSetStandardCompilerFlags.cmake. Verify first. |

---

## Remaining — awaiting CI results

### MSVC (Windows CI)

C4127, C4244, C4305, C4309 exposed in WIP commit. Triage CI output:

```powershell
# After CI run completes, on local Windows build:
.\build-windows.ps1 -Expose 4244,4305,4309
Select-String -Path build-flag-audit\warnings-C4244.txt -Pattern "warning C4244" | Select-Object -First 30
```

Decision criteria per warning number:
- **C4244** — each narrowing site: use `static_cast<>`, or if the value is known safe, use `itk::Math::CastWithRangeCheck` where applicable.
- **C4305** — each truncation: explicit `static_cast<float>(x)` at assignment.
- **C4309** — constant truncation: use the correct literal type suffix (e.g. `0x80u` not `0x80`).
- **C4127** — each `if (constant)`: convert to `if constexpr` or `static_assert` where appropriate.

#### Windows CI result (Pixi-Cxx windows-2022, MSVC, commit 4015d541cf)

Job passed (warnings are not errors in ITK CI). Warning counts:

| Code | Count | Verdict |
|---|---|---|
| C4127 | 0 | **Dead suppression — safe to remove permanently.** |
| C4305 | 0 | **Dead suppression — safe to remove permanently.** |
| C4309 | 0 | **Dead suppression — safe to remove permanently.** |
| C4244 | 629 | **Live — must fix code before re-suppressing or removing.** |

C4244 is highly concentrated:

- **619 / 629** = one line: `itkProcessObject.h(927)` — `progressFixedToFloat()` returns
  `float` but the body computes in `double`. Fix: wrap the return expression in
  `static_cast<float>(...)`. One edit clears 619 instances.
- `itkWin32OutputWindow.cxx(112)` — `__int64`→`int` (1, real code)
- `itkCSVFileReaderBase.cxx(86,154,157,158)` — `streamoff`/`SizeValueType` narrowing (4, real code)
- `itkFixedArrayGTest.cxx(419,420,436,437)` — `__int64`→`unsigned int` (4, test code)
- `itkNarrowBandTest.cxx(44)` — `double`→`TDataType` (1, test code)

All are straightforward `static_cast<>` fixes. None in ThirdParty.

### GCC/Clang (Linux/macOS CI)

`-Wno-strict-overflow` removed in WIP commit. Check CI output for:
```
overflow in expression; result is X with type Y [-Wstrict-overflow]
```
Each site: use unsigned arithmetic, restructure the expression, or use a `__builtin_add_overflow` guard.

#### Local GCC 14.3 result (Release/-O3, suppressions genuinely removed)

Verified after unsetting the cached `ITK_CXX_WARNING_FLAGS` / `ITK_C_WARNING_FLAGS`
(see the cache gotcha below) and a full recompile:

- **`-Wstrict-overflow`: 0 warnings** across ITK proper and all enabled remote
  modules, at -O3 (optimizer on, which is when this warning fires). The
  `-Wno-strict-overflow` suppression is dead on GCC 14 — removal is safe on
  this config. Pending full-matrix CI confirmation.
- **`-Wformat-nonliteral`: 1 site only** — `Modules/Remote/RTK/include/rtkIterationCommands.h:161`,
  a `snprintf(buffer, 1024, m_FileFormat.c_str(), m_IterationCount)` for a
  user-controlled output-filename pattern (`%d` iteration number). Legitimate,
  and it is a *remote module* (RTK upstream owns it), not ITK proper. ITK proper
  is clean — the 5 targeted `ITK_GCC_SUPPRESS_Wformat_nonliteral` sites already
  cover its own cases.

#### Cache gotcha (cost one wasted build)

`ITKSetStandardCompilerFlags.cmake:371` computes the warning flags once and
stores them in the `ITK_CXX_WARNING_FLAGS` / `ITK_C_WARNING_FLAGS` CACHE STRINGs.
Editing the suppression list in source has **no effect on an existing build
tree** — reconfigure skips recomputation. To re-trigger locally:
`cmake -Bbuild -U ITK_CXX_WARNING_FLAGS -U ITK_C_WARNING_FLAGS .` then rebuild.
CI is unaffected (fresh checkout each run).

#### Unrelated to this audit

32 RTK application executables fail to link (undefined references to RTK's own
`*ImageIOFactoryRegister__Private()` symbols). Pre-existing RTK remote-module
linkage issue — warning-flag changes cannot cause undefined-symbol link errors.

---

## Still to investigate

| Item | Status |
|---|---|
| MSVC `4505` (unreferenced local function) | Kept for now; check CI Windows output to see if it fires on template headers |
| `-Wno-uninitialized` (C compiler only) | Try removing; check if C files in ThirdParty wrappers warn |
| `-Wno-long-double` on Apple Silicon | Likely dead on ARM64; test with macOS CI arm64 runner |
| `#pragma warning(disable : 4996)` in RLEImage/MorphologicalContourInterpolation manualTest.cxx | Review which deprecated APIs are called; fix or document |
| `ITK_CLANG_SUPPRESS_Wduplicate_enum` in `itkCommonEnums.h` | Track with `ITK_LEGACY_REMOVE` sweep; keep for now |

---

## Open Questions

- Is Intel ICC still being tested on CDash? Check: https://open.cdash.org/index.php?project=Insight
- Does `4505` fire in Release CI builds?
- Does `-Wno-long-double` still fire on Apple Silicon (ARM64)?
