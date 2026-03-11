---
name: fix-nightly-warnings
description: 'Fix ITK nightly build errors or compilation warnings reported on CDash. Use when: addressing CDash nightly failures. Creates a branch, fixes warnings, and opens a PR upstream.'
argument-hint: What warnings should this skill fix?
---

# Fix ITK Nightly Build Errors and Warnings

Creates a focused branch containing fixes for errors or warnings reported on the ITK CDash nightly dashboard, then opens a PR upstream.

## When to Use

- CDash nightly build reports new errors, warnings, or Doxygen warnings
- User says "fix nightly errors", "address CDash warnings", or "there are new Doxygen warnings"

## Available Scripts

- **`scripts/list_nightly_warnings.py`** — Lists CDash builds that have warnings or errors. Defaults to `Nightly` builds from the last 24 hours.
- **`scripts/get_build_warnings.py`** — Fetches and summarizes warnings (or errors) for a specific CDash build ID, grouped by source file and warning flag.

Run `python3 scripts/<script>.py --help` for full usage.

## Procedure

### 1. Identify the Warnings

Use the provided scripts to fetch the current nightly builds and their warnings from CDash.

**Step 1a — List nightly builds with warnings:**

```bash
python3 scripts/list_nightly_warnings.py --type Nightly -limit 25 --json | jq '.[] | select(.warnings > 0)'
```

Note: `list_nightly_warnings.py` returns the builds with the most errors then warnings.


**Step 1b — Inspect warnings for a specific build:**

```bash
python3 scripts/get_build_warnings.py --limit 200 --json BUILD_ID | jq 'group_by(.flag) | .[] | {flag: .[0].flag, count: length}'
```

---

For each build with errors and warnings, fetch the details and summarize the errors and warnings by type and source file.
IGNORE ALL errors and warnings originating from `Modules/ThirdParty/` paths.


If there are build errors, only fix those. If there are warnings, prioritize fixing the most common warning flag that affects the most files.


### 2. Analyze the Root Cause

For each warning or error type identified in step 1, determine the root cause before editing files:
- Look up the compiler flag (e.g. `-Wthread-safety-negative`) in the compiler documentation.
- Read the affected source files to understand how they are structured.
- Identify the minimal fix: a missing annotation, a suppression pragma, a corrected API usage, etc.
- Confirm that warnings from `Modules/ThirdParty/` are skipped entirely.

### 3. Create a New Branch

```bash
git fetch upstream
git checkout -b fix-<warning-type>-warnings upstream/main
```

Example: `fix-doxygen-group-warnings`

### 4. Fix the Source Files

Determine the root cause of each error or warning. Apply the necessary fixes to the affected files to resolve the warnings. Make the minimal changes needed to fix the warnings, avoid changing unrelated documentation, coding or formatting.

### 5. Verify No New Warnings Introduced

Build and test to confirm that the fixes removed the targeted errors and warnings and did not introduce new ones.

### 6. Commit the Changes

Follow the ITK commit message standards. Include a clear description of the fix and include the error or warning message being addressed.

### 7. Draft a Pull Request

Do the following:
- Draft a pull request description that includes a summary of the changes, the warnings or errors fixed, and reference the CDash build if applicable.
- Request the User to review and approve the description before submitting the PR.
- Push the branch to the user's remote
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
