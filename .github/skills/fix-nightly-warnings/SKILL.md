---
name: fix-nightly-warnings
description: 'Fix ITK nightly build errors or compilation warnings reported on CDash. Use when: addressing CDash nightly failures. Creates a branch, fixes warnings, and opens a PR upstream.'
argument-hint: What warnings should this skill fix?
---

# Fix ITK Nightly Build Errors and Warnings

Creates a focused branch containing fixes for errors or warnings reported on the ITK CDash nightly dashboard, then open a PR upstream.

## When to Use

- CDash nightly build reports new errors, warnings, or Doxygen warnings
- User says "fix nightly errors", "address CDash warnings", or "there are new Doxygen warnings"

## Available Scripts

Scripts are located at `.github/skills/fix-nightly-warnings/scripts/` relative to the repository root. All `python3` commands below assume this directory as the working directory.

- **`scripts/triage_nightly.py`** — Single-command triage: lists builds, fetches warnings, deduplicates by `(sourceFile, flag)`, and outputs an actionable summary grouped by flag. Best for agentic automation.
- **`scripts/list_nightly_warnings.py`** — Lists CDash builds that have warnings or errors. Defaults to `Nightly` builds from the last 24 hours.
- **`scripts/get_build_warnings.py`** — Fetches and summarizes warnings (or errors) for a specific CDash build ID, grouped by source file and warning flag.

Run `python3 scripts/<script>.py --help` for full usage.

## Procedure

### 1. Identify the Warnings

Use the provided scripts to fetch the current nightly builds and their warnings from CDash.

**Step 1a — List nightly builds with warnings:**

```bash
python3 scripts/list_nightly_warnings.py --type Nightly --limit 25 --json | jq '.[] | select(.warnings > 0)'
```

Note: `list_nightly_warnings.py` returns the builds with the most errors then warnings.


**Step 1b — Inspect warnings for a specific build:**

```bash
python3 scripts/get_build_warnings.py --limit 200 --json BUILD_ID | jq '.entries | map(select(.sourceFile | test("ThirdParty") | not)) | group_by(.flag) | .[] | {flag: .[0].flag, count: length}'
```

---

For each build with errors and warnings, fetch the details and summarize the errors and warnings by type and source file.
IGNORE ALL errors and warnings originating from `Modules/ThirdParty/` paths — always filter these out in jq pipelines or with the `--exclude-thirdparty` flag.


If there are build errors, only fix those. If there are warnings, prioritize fixing the most common warning flag that affects the most files.

**Deduplication:** Multiple CDash builds (e.g., GCC and Clang on different sites) often report the same warning from the same source file. Deduplicate warnings by `(sourceFile, flag)` across all builds before analyzing, to avoid redundant work.


### 2. Analyze the Root Cause

For each warning or error type identified in step 1, determine the root cause before editing files:
- Look up the compiler flag (e.g. `-Wthread-safety-negative`) in the compiler documentation.
- Read the affected source files to understand how they are structured.
- Identify the minimal fix: a missing annotation, a suppression pragma, a corrected API usage, etc.
- Confirm that warnings from `Modules/ThirdParty/` are skipped entirely.

**Common warning fixes (quick reference):**

| Flag | Typical Fix |
|------|-------------|
| `-Wshadow` | Rename local variable to avoid shadowing outer scope |
| `-Wunused-parameter` | Add `(void)param;` or use `itkNotUsed()` macro |
| `-Wunused-variable` | Remove the variable or add `[[maybe_unused]]` |
| `-Wdeprecated-declarations` | Update to the replacement API |
| `-Wsign-compare` | Use matching signed/unsigned types or cast explicitly |
| `-Woverloaded-virtual` | Add `using Base::Method;` or `override` keyword |
| `-Winconsistent-missing-override` | Add missing `override` keyword |
| `C4805` (MSVC) | Avoid mixing `bool` and `int` in arithmetic |
| `C4267` (MSVC) | Add explicit narrowing cast `static_cast<>()` |

### 3. Create a New Branch

```bash
git fetch upstream
git checkout -b fix-<warning-type>-warnings upstream/main
```

Example: `fix-doxygen-group-warnings`

### 4. Fix the Source Files

Determine the root cause of each error or warning. Apply the necessary fixes to the affected files to resolve the warnings. Make the minimal changes needed to fix the warnings, avoid changing unrelated documentation, coding or formatting.

### 5. Verify No New Warnings Introduced

Build the affected libraries locally to confirm that the fixes compile cleanly:

```bash
cmake --build build --target <affected-library-target> 2>&1 | grep -c "warning:"
```

If a local build is not feasible, rely on the draft PR's CI checks to verify. Note that some warnings are intentionally suppressed in `CMake/CTestCustom.cmake.in` — do not fix warnings that are already handled there.

### 6. Commit the Changes

Follow the ITK commit message standards. Include a clear description of the fix and include the error or warning message being addressed.

### 7. Draft a Pull Request

Do the following:
- Draft a pull request description that includes a summary of the changes, the warnings or errors fixed, and reference the CDash build if applicable.
- If running interactively, request the user to review and approve the description before submitting the PR. If running as a scheduled/agentic workflow, proceed directly.
- Push the branch to the user's remote.
- Create a DRAFT pull request against the current `upstream/main` branch.

## Quality Checks

Before declaring done:
[] All targeted warnings are fixed
[] No new warnings or errors introduced
[] Changes are limited to the files affected by the warnings
[] Commit message clearly describes the fix and references the CDash issue if applicable

## Key Files for Reference

| File | Purpose |
|------|---------|
| `Documentation/docs/contributing/index.md` | Contributing guidelines |
| `CMake/CTestCustom.cmake.in` | Already-suppressed warning/error patterns — do not fix these |
