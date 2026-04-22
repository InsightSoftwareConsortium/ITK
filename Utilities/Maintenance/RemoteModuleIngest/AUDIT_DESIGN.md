# Pre-Ingest Audit — design notes

This document captures the design of the audit pass that runs on every
upstream remote-module repo before an ingest.  Evolved across two
feedback rounds on PR #6093:

- **v2** (blowekamp): bloat-gated mode selection with thresholds.
- **v3** (dzenanz + hjmjohnson refinement): structural whitelist
  (only `include/`, `src/`, `test/`, `wrapping/`, `CMakeLists.txt`,
  `itk-module.cmake`, `*.cmake` transfer), mandatory CID
  normalization of every test-data content-link
  (`.md5` / `.shaNNN` → `.cid`; raw binaries uploaded via
  `@web3-storage/w3cli`), `examples/` routed to top-level `Examples/`
  in a separate follow-up PR.

The audit is implemented as a phase inside `ingest-remote-module.sh`
(or the standalone `audit-upstream.sh` if we split it out for re-use).

## What the audit answers

1. How much pack data would a full-history merge add to ITK? (estimate)
2. Which single blobs are the largest? (top-20 list)
3. What's the size-distribution of blobs in the upstream history?
4. Which paths exist only in pre-tip history — i.e., appear in some
   historical commit but are not in the current tip tree? These are
   the cheap strip candidates (strip-them-out commits become empty and
   are dropped by `--prune-empty=always`).
5. How many commits and how many distinct authors?
6. Are there any files that match the copyright-review patterns
   (PDFs, videos, > 1 MiB images)?
7. Given all of the above, which of the three modes (full / filtered /
   squash) is the audit's recommendation?

## Pipeline

```
upstream repo (read-only clone, shallow-unshallowed)
  │
  ├── apply WHITELIST via filter-repo --paths (dry)
  │   → how much history survives whitelisting alone?
  │   → how many commits become empty under the whitelist?
  │
  ├── enumerate test/ content-links
  │   → counts by algorithm: md5, sha1..sha512, cid, raw-binary
  │   → for each, estimate the CID-normalization cost:
  │       * existing hash: resolve + recompute CID (no upload needed)
  │       * raw binary:   must upload (w3 up) -- flag if > 100 MiB
  │
  ├── git log --format=... --numstat       → commit count, authors, line-churn
  │                                          (pre- and post-whitelist)
  │
  ├── git rev-list --objects --all         → every blob ever reachable
  │   │                                      (with commit-of-introduction)
  │   └── git cat-file --batch-check       → (sha, type, size, path)
  │                                          grouped into histogram
  │                                          and top-N list
  │
  ├── git filter-repo --analyze            → authoritative path-size report,
  │   (into a throwaway dir)                 dropped-paths (pre-tip-only),
  │                                          directory-sum sizes
  │
  ├── diff upstream-tip-tree vs whitelist  → paths present in tip but excluded
  │                                          by whitelist (e.g., README, docs/,
  │                                          paper/) -- listed for reviewer
  │                                          awareness, not for strip decision
  │
  └── pattern scan over post-whitelist blobs
        ├── *.pdf, *.mp4, > 1 MiB images → copyright review
        └── blobs > threshold still present after whitelist
                                            (rare -- whitelist covers most
                                             cases; surviving large blobs
                                             usually mean a big file in
                                             test/ without a content-link)
```

## `audit.json` schema

Machine-readable output consumed by the rest of `ingest-remote-module.sh`
to decide on mode and to populate `INGEST_LOG.md` after the merge.

```json
{
  "upstream_url": "https://github.com/.../ITK<Name>.git",
  "upstream_sha": "203260b9...",
  "audit_date":   "2026-04-22",
  "audit_tool_version": "ingest-remote-module.sh @ <script_sha>",

  "commits":          136,
  "distinct_authors": 12,
  "authors": [
    {"name": "Jean-Marie Mirebeau", "email": "...", "commits": 142, "lines_added": 8234, "lines_removed": 1112},
    {"name": "Matt McCormick",      "email": "...", "commits": 48,  "lines_added": 1203, "lines_removed": 889},
    ...
  ],

  "tip_tree_size_bytes":       2211123,
  "pack_estimate_full_bytes":  8192512,
  "pack_estimate_filtered_bytes": 1298432,

  "largest_blobs": [
    {"sha": "a1b2c3d...", "path": "paper/figures/brain_section.png", "size": 4299341, "intro_commit": "..."},
    {"sha": "9f8e7d6...", "path": "Old/legacy_impl/DiffusionOldTest.mha", "size": 2298764, "intro_commit": "..."},
    ...
  ],

  "blob_size_histogram": {
    "0-10KiB":      487,
    "10-100KiB":     42,
    "100KiB-1MiB":   18,
    "1-10MiB":        4,
    "gt-10MiB":       0
  },

  "strip_candidates": [
    {"path": "Old/",          "only_in_history": true,  "commits": 87, "pack_bytes": 1468000},
    {"path": "paper/",        "only_in_history": true,  "commits": 12, "pack_bytes": 4507000},
    {"path": "docs/anim/",    "only_in_history": true,  "commits":  5, "pack_bytes":  622000}
  ],

  "copyright_review": [
    {"path": "paper/paper.pdf",           "size":  892213, "reason": "pdf"},
    {"path": "docs/figures/teaser.png",   "size": 1389221, "reason": ">1MiB image"}
  ],

  "recommendation": {
    "mode": "filtered-history",
    "rationale": [
      "pack_estimate_full_bytes 7.8 MiB > 2 MiB full-history cap",
      "4 blobs >= 1 MiB (all in strip-candidate paths)"
    ],
    "proposed_filter_args": {
      "invert_paths": ["Old/", "paper/", "docs/anim/"],
      "strip_blobs_bigger_than": "1M"
    },
    "expected_post_filter": {
      "pack_bytes":   1298432,
      "largest_blob": 124000,
      "blobs_ge_1mib": 0
    }
  }
}
```

## Human-readable rendering

The same data pretty-printed to stdout (the `--audit-only` mode
stops here; the full-ingest path continues into the mode selected):

```
=== Upstream audit: ITKAnisotropicDiffusionLBR @ 203260b9 ===
Commits:                 136
Distinct authors:        12
Tree size (tip):         2.1 MiB
Pack estimate (all):     7.8 MiB
Pack estimate (filtered): 1.3 MiB

Largest blobs introduced (top 10 of 64 ≥ 256 KiB):
   4.1 MiB  paper/figures/brain_section.png      (commit a1b2c3d)
   2.2 MiB  Old/legacy_impl/DiffusionOldTest.mha (commit 9f8e7d6)
   0.8 MiB  examples/Data/Fiber.png              (commit c4d5e6f)
   ...

Blob size histogram:
   0–10   KiB : 487 blobs
   10–100 KiB :  42 blobs
   100K–1 MiB :  18 blobs
   1–10   MiB :   4 blobs
   > 10   MiB :   0 blobs

Paths that exist only in pre-tip history (cheap strip candidates):
   Old/              (87 commits, 1.4 MiB)
   paper/            (12 commits, 4.3 MiB)
   docs/anim/        ( 5 commits, 0.6 MiB)

Copyright-review candidates (HUMAN REVIEW REQUIRED):
   paper/paper.pdf            (892 KiB — pdf)
   docs/figures/teaser.png  (1.3 MiB — >1MiB image)

Recommended mode: filtered-history
  rationale:
    - pack_estimate_full_bytes 7.8 MiB > 2 MiB full-history cap
    - 4 blobs >= 1 MiB (all in strip-candidate paths)
  proposed filter args:
    --invert-paths --paths 'Old/' 'paper/' 'docs/anim/'
    --strip-blobs-bigger-than 1M
  post-filter estimate: 1.3 MiB pack, 0 blobs >= 1 MiB
```

## Decision algorithm (pseudocode)

```python
def recommend_mode(audit):
    rationale = []

    # Mandatory squash trigger: even after filter-repo stripping the
    # pack delta is still too large.  Commit count is intentionally
    # NOT a gate -- per PR #6093 consensus, with the whitelist in
    # place the surviving commits reflect real authorship and should
    # not push a module into squash just for being long-lived.
    if audit.pack_estimate_filtered_bytes > 3 * MiB:
        rationale.append("post-filter pack still > 3 MiB filtered cap")
        return "squash", rationale

    # Full-history gates — all size gates must pass
    full_ok = (
        audit.pack_estimate_full_bytes <= 700 * KiB and
        audit.largest_blob_bytes <= 85 * KiB and
        audit.blobs_over_341kib == 0 and
        not audit.strip_candidates
    )
    if full_ok:
        return "full-history", ["all size thresholds met"]

    # Filtered gates — post-filter size values must pass the filtered caps
    if (audit.pack_estimate_filtered_bytes <= 3 * MiB and
        audit.expected_post_filter["largest_blob"] <= 700 * KiB and
        audit.expected_post_filter["blobs_over_341kib"] <= 1):
        for fail_reason in explain_why_full_failed(audit):
            rationale.append(fail_reason)
        return "filtered-history", rationale

    # Fall-through
    rationale.append("even after filter-repo strip, size thresholds exceeded")
    return "squash", rationale
```

## Overrides

The auto-recommendation is a default, not a mandate. Humans can
override:

```bash
./ingest-remote-module.sh <Name> <Group> \
    --mode=filtered-history \
    --strip-paths 'Old/' 'papers/' \
    --strip-blobs-bigger-than 1M \
    --mode-justification "audit recommends squash due to 2100 commits, \
but most of those commits are automated dependabot bumps to a deleted \
'node_modules' path; filter-repo strip drops them cleanly"
```

When the CLI mode disagrees with the audit recommendation, the script
requires `--mode-justification "..."`; the justification string is
embedded into the merge commit body so reviewers can see why the
default was overridden.

## What lives where (file layout)

| File | Role |
|---|---|
| `ingest-remote-module.sh` | End-to-end driver. Contains the audit phase, the three mode implementations, and `INGEST_LOG.md` emission. |
| `audit.json` | Per-run machine output in the ingest tempdir. Consumed by the script to pick a mode and to populate `INGEST_LOG.md`. |
| `INGEST_LOG.md` | Append-only human-readable record, one block per module. |
| `INGESTION_STRATEGY.md` | High-level strategy document. Thresholds live here so they're reviewable separately from the script. |
| `CLEANUP_CHECKLIST.md` | What to strip (at merge time vs at filter-repo time vs copyright-review). |
| `AUDIT_DESIGN.md` | This file. |

## Open questions

1. **Threshold tuning.** 700 KiB full-history pack cap and 85 KiB
   single-blob cap are intentionally aggressive (roughly 1/3 of the
   first-draft values after the #6093 "keep thresholds very low"
   direction). After the first ~5 ingests, compare actual post-ingest
   pack deltas against the audit estimates; adjust.  With these
   aggressive defaults, expect most modules to land in squash mode.
2. **filter-repo blob-size cap with `.md5` fixtures.** ExternalData
   .md5 stubs are tiny; the blob cap shouldn't affect them. But if
   an upstream module committed actual image fixtures (without the
   `.md5` + ExternalData indirection), those could legitimately be >
   256 KiB. Audit should flag them with a "keep — is this a test
   fixture?" note rather than auto-stripping.
3. **Submodules.** None of the current Tier-A targets use git
   submodules, but future ingests might. Audit should refuse to run
   until submodules are inlined or removed.
4. **Author email normalization.** A single contributor may appear
   under multiple emails in upstream git history. Audit currently
   counts them separately; a `.mailmap` at the upstream repo root
   (if present) should be respected. Otherwise we may over-count
   distinct authors and bias toward squash mode.
