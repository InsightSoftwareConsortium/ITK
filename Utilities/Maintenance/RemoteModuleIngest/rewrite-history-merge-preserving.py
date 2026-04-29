#!/usr/bin/env python3
"""Rewrite a remote-module's commit history with current ITK pre-commit
content rules applied at every commit, preserving the full merge
topology.

Replacement for ``normalize-ingest-commits.py`` which linearized
history.  See INGESTION_STRATEGY.md and the user's mandate after
PRs #6135, #6137, #6159, #6161.

What this does (Phase 1 / Phase 2 / Phase 3 / Phase 4 model):

  Phase 1 — REFERENCE
    Clone the upstream remote-module to a scratch directory, copy the
    destination project's ``.pre-commit-config.yaml`` (and any
    config files the hooks depend on, e.g. ``.gersemi.config``,
    ``.clang-format``), run ``pre-commit run --all-files`` once at
    the upstream tip, and capture the resulting tree's SHA1 as the
    "fully-formatted main" reference.  This is the target the
    rewritten tip MUST match in Phase 3.

  Phase 2 — REWRITE
    Use ``git filter-repo --blob-callback`` to walk every blob in the
    upstream history and apply uniform text-blob normalization
    (trim trailing whitespace, ensure exactly one terminating
    newline).  Merge commits are preserved by default.  Exec-bit
    anomalies on text files are corrected via a
    ``--commit-callback``.

    NOTE: Language-specific formatters (clang-format, gersemi) are
    NOT applied per blob, because their output depends on
    configuration files that may differ across upstream history
    points.  Apply them as a single STYLE commit at the rewritten
    tip after Phase 3 succeeds — that commit lands at the LATEST
    config snapshot and does not need to traverse history.

  Phase 3 — VERIFY
    ``git diff <phase-1-reference> <phase-2-tip>`` must be empty
    (modulo paths Phase 1 added but Phase 2 doesn't have, e.g. the
    pre-commit config files copied in for Phase 1).  Any non-empty
    diff is a bug in the rewriter or a hook configuration drift.

  Phase 4 — MERGE
    The rewritten history is then ready for the Mode A merge step
    in ``ingest-remote-module.sh``.  This script does NOT perform
    the merge; it produces a rewritten history that the ingest
    script consumes.

Usage::

    python3 rewrite-history-merge-preserving.py \\
        --branch <ingest-branch> \\
        --base upstream/main

The script operates on the current checkout; pass ``--branch`` to
limit the rewrite to that ref's commits unique to ``--base``.

Why per-blob is enough for ghostflow:
  Ghostflow flags trailing whitespace, "new blank line at EOF",
  and bad executable bits.  All three are addressable by uniform
  per-blob normalization.  clang-format / gersemi formatting
  decisions are not ghostflow concerns — they show up only as
  pre-commit-gate failures on the tip, which the ``STYLE: Apply
  current formatters`` commit at the tip fixes in one shot.
"""

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
from pathlib import Path


def run_phase_1_reference(
    upstream_url: str,
    pre_commit_config_src: Path,
    workdir: Path,
) -> str:
    """Clone upstream, apply pre-commit hooks once, return resulting
    tree's SHA1."""
    scratch = workdir / "phase1-ref"
    if scratch.exists():
        shutil.rmtree(scratch)
    subprocess.run(
        ["git", "clone", "--depth=1", upstream_url, str(scratch)],
        check=True,
    )
    # Copy the destination's hook config and any auxiliary files.
    # .pre-commit-config.yaml is required; auxiliary configs are optional
    # but loudly noted when missing so a misdirected --pre-commit-config-src
    # cannot silently produce an unformatted reference tree.
    required = scratch / ".pre-commit-config.yaml"
    src = pre_commit_config_src / ".pre-commit-config.yaml"
    if not src.exists():
        sys.stderr.write(
            f"ERROR: --pre-commit-config-src={pre_commit_config_src} "
            "does not contain .pre-commit-config.yaml; cannot produce a "
            "Phase 1 reference tree.\n"
        )
        sys.exit(2)
    shutil.copy2(src, required)
    for name in (".gersemi.config", ".clang-format"):
        aux = pre_commit_config_src / name
        if aux.exists():
            shutil.copy2(aux, scratch / name)
        else:
            sys.stderr.write(
                f"WARN: optional config {name} not found in "
                f"{pre_commit_config_src}; some hooks may produce different "
                "output than the destination project.\n"
            )
    # Run pre-commit once.  Ignore exit code (hooks return non-zero on
    # auto-fix).
    subprocess.run(
        ["pre-commit", "run", "--all-files"],
        cwd=scratch,
        check=False,
    )
    # Stage the auto-fixes so write-tree captures the post-hook tree;
    # write-tree reads the index, not the working directory.
    subprocess.run(["git", "add", "--all"], cwd=scratch, check=True)
    tree = subprocess.check_output(
        ["git", "write-tree"], cwd=scratch, text=True
    ).strip()
    return tree


def assert_filter_repo_available() -> None:
    if shutil.which("git-filter-repo") is None:
        sys.stderr.write(
            "ERROR: git-filter-repo not on PATH.  Install with "
            "`pixi global install git-filter-repo` or `pip install "
            "git-filter-repo`.\n"
        )
        sys.exit(2)


def run_phase_2_rewrite(branch: str, base: str) -> None:
    """Drive git filter-repo to normalize text blobs across the
    branch's history.  Merges are preserved by filter-repo by default."""
    blob_callback = """
def is_likely_text(b):
    if b"\\x00" in b[:4096]:
        return False
    try:
        b[:4096].decode("utf-8")
    except UnicodeDecodeError:
        return False
    return True

def normalize(b):
    try:
        text = b.decode("utf-8")
    except UnicodeDecodeError:
        return b
    lines = [ln.rstrip(" \\t\\r") for ln in text.split("\\n")]
    while len(lines) > 1 and lines[-1] == "":
        lines.pop()
    return ("\\n".join(lines) + "\\n").encode("utf-8")

if is_likely_text(blob.data):
    blob.data = normalize(blob.data)
"""
    # Drop the executable bit from files whose extension is never
    # legitimately executable.  .sh / .py / .pl are intentionally
    # excluded; they may be runnable scripts upstream.
    commit_callback = """
NEVER_EXEC = (b".cmake", b".cxx", b".cpp", b".cc", b".c", b".h",
              b".hxx", b".hpp", b".wrap", b".txt", b".md", b".rst",
              b".yaml", b".yml", b".toml", b".json", b".cfg", b".ini")
for change in commit.file_changes:
    if change.mode == b"100755" and change.filename:
        name = change.filename.rsplit(b"/", 1)[-1].lower()
        if any(name.endswith(ext) for ext in NEVER_EXEC):
            change.mode = b"100644"
"""
    subprocess.run(
        [
            "git",
            "filter-repo",
            "--refs",
            f"{base}..{branch}",
            "--blob-callback",
            blob_callback,
            "--commit-callback",
            commit_callback,
            "--force",
        ],
        check=True,
    )


def assert_topology_preserved(branch: str, base: str, expected_min: int) -> None:
    """Phase 2 self-check: at least `expected_min` merge commits
    survived in the rewritten range."""
    merges = int(
        subprocess.check_output(
            ["git", "rev-list", "--count", "--merges", f"{base}..{branch}"],
            text=True,
        ).strip()
    )
    if merges < expected_min:
        sys.stderr.write(
            f"FAIL: only {merges} merge commit(s) in {base}..{branch}; "
            f"expected at least {expected_min}.  History was linearized.\n"
        )
        sys.exit(4)
    print(
        f"OK: {merges} merge commit(s) preserved in {base}..{branch}",
        file=sys.stderr,
    )


def main(argv: list[str] | None = None) -> int:
    p = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    p.add_argument(
        "--base",
        help="Base ref (e.g. upstream/main) to bound the rewrite "
        "(required for Phase 2; ignored in --phase-1 mode)",
    )
    p.add_argument(
        "--branch",
        default="HEAD",
        help="Branch to rewrite (default: HEAD)",
    )
    p.add_argument(
        "--expected-merges",
        type=int,
        default=0,
        help="Minimum number of merge commits expected to survive in the "
        "rewritten range.  Pass N where N is the number of upstream-side "
        "merge commits in the source history; the Mode A ingest merge is "
        "added later by ingest-remote-module.sh and is not counted here.",
    )
    p.add_argument(
        "--phase-1",
        action="store_true",
        help="Run Phase 1 only — produce a reference tree from a fresh "
        "upstream clone.  Requires --upstream-url and --pre-commit-config-src.",
    )
    p.add_argument(
        "--upstream-url",
        help="Upstream remote-module URL (required with --phase-1)",
    )
    p.add_argument(
        "--pre-commit-config-src",
        help="Path to a directory containing the destination project's "
        ".pre-commit-config.yaml and related config files "
        "(required with --phase-1)",
    )
    p.add_argument(
        "--workdir",
        default="/tmp/ingest-rewrite",
        help="Scratch directory for Phase 1 clone (default: /tmp/ingest-rewrite)",
    )
    args = p.parse_args(argv)

    if args.phase_1:
        if not args.upstream_url or not args.pre_commit_config_src:
            p.error("--phase-1 requires --upstream-url and --pre-commit-config-src")
        # Phase 1 does not use --base.
        workdir = Path(args.workdir)
        workdir.mkdir(parents=True, exist_ok=True)
        ref_tree = run_phase_1_reference(
            args.upstream_url,
            Path(args.pre_commit_config_src),
            workdir,
        )
        print(ref_tree)
        return 0

    # Phase 2 (default).
    if not args.base:
        p.error("--base is required for Phase 2 (omit only with --phase-1)")
    # Phase 2 needs git-filter-repo; Phase 1 does not.
    assert_filter_repo_available()

    # Phase 2 (default): rewrite blobs across the branch's range.
    print(
        f"Phase 2 — rewriting text blobs in {args.base}..{args.branch}...",
        file=sys.stderr,
    )
    run_phase_2_rewrite(args.branch, args.base)

    # Topology assertion.
    assert_topology_preserved(args.branch, args.base, args.expected_merges)

    print(
        "Phase 2 complete.  Manual next steps:\n"
        "  1. Run `pre-commit run --all-files` on the rewritten tip.\n"
        "  2. If language-specific formatters (clang-format, gersemi) flag\n"
        "     anything, add a single 'STYLE: Apply current formatters'\n"
        "     commit at the tip — DO NOT rewrite history for those.\n"
        "  3. Force-push the result.",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
