# Future SPDX / SBOM Compliance Work

These enhancements were scoped out of the initial SBOM landing in PR
#5817 but are worth revisiting as follow-up issues. Ordered roughly
by cost-to-payoff.

## 1. Host `documentNamespace` URIs at `itk.org/spdx/` (recommended)

SPDX 2.3 §6.5 does *not* require `documentNamespace` URIs to resolve,
but Annex B "best practices" recommends that creators who own the
domain make them resolvable. Three progressive options:

- **Minimum (cheapest):** add a redirect on `itk.org` so any
  `/spdx/...` path 302s to
  `https://github.com/InsightSoftwareConsortium/ITK/releases`.
  One Apache/nginx rule; never needs updating.
- **Better:** publish a static `itk.org/spdx/index.html` explaining
  what the namespace URIs are and pointing to the GitHub release
  SBOM assets.
- **Automated:** extend the release workflow so each tagged release
  uploads `sbom.spdx.json` to `itk.org/spdx/ITK-<version>-<uuid>.spdx.json`
  (and attaches it as a GitHub release asset, see item 2).
  Cadence: once per release (handful per year). No per-build work.

Only release SBOMs are worth hosting — the configure-time SBOM
produced for every developer build tree is ephemeral and should not
be published.

## 2. Attach `sbom.spdx.json` to every GitHub release (recommended)

Extend the existing release workflow (`.github/workflows/*release*`)
to upload the SBOM produced by a canonical CI configuration as a
release asset. This is the single most-requested SBOM consumption
pattern (FDA 524B submissions, anchore/syft/grype ingestion).

## 3. `filesAnalyzed: true` + `packageVerificationCode` for `SPDXRef-ITK`

NTIA "minimum elements" baseline encourages emitting a
`packageVerificationCode` — a SHA1 over the sorted list of file
SHA1s inside the package — for the top-level ITK package on release
builds. The current SBOM sets `filesAnalyzed: false` everywhere to
avoid the per-file walk. Implementation notes:

- Add an opt-in CMake cache option (off by default, on in release
  CI) to walk the install tree and feed file hashes into
  `generate_sbom.py`.
- When on, flip `filesAnalyzed` to `true` on `SPDXRef-ITK` only,
  emit `packageVerificationCode`, and list file-level SPDX records.

Non-trivial — expected cost: one full-day implementation, plus
triage of any files (test data, generated headers) that must be
excluded from the hash set.

## 4. `CONTAINS` / `BUILD_TOOL_OF` relationships

Some downstream SBOM consumers prefer richer relationship graphs
than the current `DESCRIBES` + `DEPENDS_ON` pair. Adding:

- `SPDXRef-ITK CONTAINS SPDXRef-<ThirdParty>` for each vendored
  ThirdParty module (distinct from the existing `DEPENDS_ON` to
  external-only dependencies like FFTW).
- `SPDXRef-<BuildTool> BUILD_TOOL_OF SPDXRef-ITK` for CMake /
  Python / Ninja if we ever decide to record them as packages.

Low cost; requires deciding whether ThirdParty modules are
"contained" (they are, physically) or merely "depended on".

## 5. Continuous REUSE 3.x compliance

Add a `reuse lint --quiet` job to CI (either as a new pre-commit
hook or a dedicated GitHub Actions step) so `REUSE.toml`, per-file
SPDX headers, and `LICENSES/` stay in sync. The initial landing
confirmed local `reuse lint` passes; this just prevents regression.

## 6. Stronger PURL validation

`generate_sbom.py` currently copies `purl` values through verbatim.
Adding a `packageurl-python` parse step would catch malformed PURLs
at SBOM generation time (affects CVE-feed tools downstream). Gated
on an optional pip dependency so it remains opt-in, mirroring the
existing `spdx-tools` pattern in `validate_with_spdx_tools.py`.

## 7. Test coverage for `generate_sbom.py`

Add `Utilities/SPDX/tests/` with pytest coverage for:
- manifest parsing (blocks, key=value, escaping round-trip)
- LicenseRef format validation
- `build_sbom` output shape
- CTest wire-up so the suite runs under `ITK_BUILD_SPDX_TESTS`.

Pure Python, no ITK build dependency — cheap to add.
