# SPDX and SBOM Tooling

This directory holds the scripts that support ITK's SPDX-2.3 Software
Bill of Materials workflow (see `CMake/ITKSBOMGeneration.cmake` and
`CMake/ITKSBOMValidation.cmake`). All scripts require Python 3.10 or
later. Shared constants, JSON I/O, and path helpers live in
`_common.py`.

| Script | Purpose |
|--------|---------|
| `generate_sbom.py` | Read the `sbom-inputs.json` manifest emitted by `ITKSBOMGeneration.cmake` and write the final `sbom.spdx.json`. Invoked from CMake; may also be run by hand for debugging. |
| `add_headers.py` | Prepend `SPDX-FileCopyrightText` / `SPDX-License-Identifier` lines to ITK-owned source files. Idempotent; skips files that already carry an SPDX header. Used by the `check-spdx-headers` pre-commit hook with `--check --files <paths>` to enforce SPDX on new files. Handles shebangs, UTF-8 BOM, and CRLF line endings safely. |
| `verify_versions.py` | Cross-check that each ThirdParty module's `SPDX_VERSION` in `itk-module.cmake` matches the tag declared in its `UpdateFromUpstream.sh`. Skips modules tracking `master`, commit SHAs, or ITK-custom `for/*` tags. Invoked by CTest as `ITKSBOMVersionConsistency`. |
| `validate_light.py` | In-tree lightweight validator for the generated SBOM. Checks required SPDX 2.3 fields, license-reference integrity, and SPDXID uniqueness. Always runs via CTest `ITKSBOMValidation`. |
| `validate_with_spdx_tools.py` | Full SPDX 2.3 schema validation via the optional `spdx-tools` pip package. Returns CTest skip code 77 when `spdx-tools` is not installed so the test is optional rather than a hard dependency. Invoked by CTest as `ITKSBOMSchemaValidation`. |
| `compute_fingerprint.py` | SHA-256 fingerprint over the sorted, canonicalized SBOM package metadata (name, version, license, PURL). Used for drift detection between branches and in CI via CTest `ITKSBOMFingerprint` when `ITK_SBOM_FINGERPRINT_BASELINE` is set. |

### Typical workflows

Add SPDX headers to new files:
```
python3 Utilities/SPDX/add_headers.py <source-tree>
```

Verify SBOM matches upstream versions:
```
python3 Utilities/SPDX/verify_versions.py .
```

Validate a generated SBOM:
```
python3 Utilities/SPDX/validate_light.py build/sbom.spdx.json
# For full schema validation (pip install spdx-tools first):
python3 Utilities/SPDX/validate_with_spdx_tools.py build/sbom.spdx.json
```

Track SBOM changes:
```
# Baseline the current state:
python3 Utilities/SPDX/compute_fingerprint.py build/sbom.spdx.json \
    --compare Utilities/SPDX/sbom-fingerprint.baseline --update

# Later, verify no drift:
python3 Utilities/SPDX/compute_fingerprint.py build/sbom.spdx.json \
    --compare Utilities/SPDX/sbom-fingerprint.baseline
```

### Future improvements

See [TODO.md](./TODO.md) for planned SPDX best-practice enhancements
that were scoped out of the initial landing.

### Related files

- `Utilities/KWStyle/ITKHeader.h` — canonical ITK file header template,
  enforced by the KWStyle CTest. Starts with the two SPDX lines followed
  by the Apache-2.0 notice block.
- `REUSE.toml` (repo root) — REUSE 3.x blanket license annotations for
  ITK-owned files that do not carry per-file SPDX headers.
- `LICENSES/` (repo root) — canonical SPDX license texts required by
  REUSE 3.x.
