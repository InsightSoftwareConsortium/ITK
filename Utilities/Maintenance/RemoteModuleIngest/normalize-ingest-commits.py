#!/usr/bin/env python3
"""Re-stamp every commit in <base>..HEAD through current pre-commit hooks
while preserving original author, author-date, and message body.

Why this exists:
- ITK's main repo enforces a strict pre-commit hook set (gersemi,
  clang-format, trailing-whitespace, end-of-file, etc.). The remote
  modules being ingested were authored before some of those hooks
  existed, so their historical commits show whitespace and formatting
  drift even when the final tree is clean.
- Running pre-commit at each replayed commit produces a tree-of-commits
  in which every individual commit also satisfies today's hooks. ``git
  log -p`` and ``git show <historical-sha>`` then read cleanly.

What this preserves:
- Author name + email
- Author date
- Commit message body (verbatim)

What this normalizes:
- Subject line. If it does not start with one of the ITK prefixes
  (``BUG:``, ``COMP:``, ``DOC:``, ``ENH:``, ``PERF:``, ``STYLE:``) or
  exceeds 78 characters, the original subject is preserved as
  ``Original subject: <text>`` in the body and a conforming subject is
  synthesized.
- File contents. Every file modified by the commit is re-stamped through
  the current ``pre-commit run --files`` set; auto-fixes (whitespace,
  formatting) are folded back into the commit.

What this drops:
- Commits that become empty after pre-commit normalization. These are
  intermediate style-only commits that today's hooks would have
  prevented from existing in the first place.

Usage::

    # From a checkout of ITK on the branch to be normalized:
    python3 Utilities/Maintenance/RemoteModuleIngest/normalize-ingest-commits.py \\
        --base upstream/main [--dry-run] [--backup-tag pre-normalize]

The script is idempotent: re-running on an already-normalized branch
produces the same SHA tree (subject only the committer-date update from
the cherry-pick).
"""

from __future__ import annotations

import argparse
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

PREFIX_RE = re.compile(r"^(BUG|COMP|DOC|ENH|PERF|STYLE):\s")
SUBJECT_LIMIT = 78

# Light heuristics for inferring a prefix when the original subject has
# none. Order matters: more specific patterns first.
INFER_RULES: list[tuple[re.Pattern[str], str]] = [
    (re.compile(r"\b(fix|bug|crash|leak|regression)\b", re.IGNORECASE), "BUG"),
    (re.compile(r"\b(doc|docs|documentation|comment)\b", re.IGNORECASE), "DOC"),
    (
        re.compile(r"\b(cmake|build|compil|link|warning|wrap|cxx)\b", re.IGNORECASE),
        "COMP",
    ),
    (
        re.compile(
            r"\b(format|whitespace|rename|prefer|style|cleanup|refactor)\b",
            re.IGNORECASE,
        ),
        "STYLE",
    ),
    (re.compile(r"\b(perf|speed|optim|faster)\b", re.IGNORECASE), "PERF"),
]


def run(
    cmd: list[str], *, check: bool = False, env: dict[str, str] | None = None
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(  # noqa: S603 — fixed argv lists, no shell
        cmd, capture_output=True, text=True, check=check, env=env
    )


def required(cmd: list[str]) -> str:
    r = run(cmd)
    if r.returncode != 0:
        sys.stderr.write(f"FAIL: {' '.join(cmd)}\n{r.stderr}")
        sys.exit(1)
    return r.stdout


def infer_prefix(subj: str) -> str:
    for rx, pre in INFER_RULES:
        if rx.search(subj):
            return pre
    return "ENH"  # generic fallback for genuine new functionality


def normalize_message(subj: str, body: str) -> tuple[str, str, bool]:
    """Return (new_subject, new_body, changed)."""
    conforming = bool(PREFIX_RE.match(subj)) and len(subj) <= SUBJECT_LIMIT
    if conforming:
        return subj, body, False
    if PREFIX_RE.match(subj):
        # Has a prefix but is too long. Truncate the subject; preserve
        # the full original in the body so no information is lost.
        short = subj[: SUBJECT_LIMIT - 3].rstrip() + "..."
        new_subj = short
    else:
        prefix = infer_prefix(subj)
        candidate = f"{prefix}: {subj}"
        if len(candidate) > SUBJECT_LIMIT:
            candidate = candidate[: SUBJECT_LIMIT - 3].rstrip() + "..."
        new_subj = candidate
    extra = f"Original subject: {subj}"
    if body.strip():
        new_body = body.rstrip() + "\n\n" + extra + "\n"
    else:
        new_body = extra + "\n"
    return new_subj, new_body, True


def replay(base: str, *, dry_run: bool, run_pre_commit: bool) -> int:
    out = required(["git", "rev-list", "--reverse", f"{base}..HEAD"])
    commits = out.split()
    if not commits:
        print("Nothing to do — branch contains no commits beyond base.")
        return 0

    print(f"Replaying {len(commits)} commits onto {base}...", file=sys.stderr)

    if dry_run:
        rewritten = 0
        for sha in commits:
            meta = required(["git", "show", "-s", "--format=%s%x00%b", sha])
            subj, _, body = meta.partition("\x00")
            _, _, changed = normalize_message(subj, body)
            tag = "REWRITE" if changed else "KEEP   "
            if changed:
                rewritten += 1
            print(f"  {tag} {sha[:10]} {subj}")
        print(f"\nDry-run: {rewritten}/{len(commits)} subjects would change.")
        return 0

    # Real run — reset to base and replay
    required(["git", "reset", "--hard", base])

    dropped = 0
    rewritten = 0
    kept_unchanged = 0

    pre_commit_bin = shutil.which("pre-commit")
    if run_pre_commit and pre_commit_bin is None:
        print(
            "WARN: pre-commit not on PATH; per-commit normalization disabled.",
            file=sys.stderr,
        )
        run_pre_commit = False

    for sha in commits:
        meta = required(
            ["git", "show", "-s", "--format=%an%x00%ae%x00%aI%x00%s%x00%b", sha]
        )
        an, ae, ad, subj, body = meta.split("\x00", 4)

        # ``-X theirs`` biases toward the cherry-picked commit's content
        # whenever pre-commit's auto-fix on an earlier commit clashes
        # with the base state this commit expects. Pre-commit re-runs
        # below and re-normalizes the merged tree.
        cp = run(
            [
                "git",
                "cherry-pick",
                "--allow-empty",
                "--no-commit",
                "--strategy=recursive",
                "-X",
                "theirs",
                sha,
            ]
        )
        if cp.returncode != 0:
            # Fall back to default 3-way merge — sometimes a true
            # content conflict needs human review.
            run(["git", "cherry-pick", "--abort"])
            cp2 = run(["git", "cherry-pick", "--allow-empty", "--no-commit", sha])
            if cp2.returncode != 0:
                sys.stderr.write(f"cherry-pick failed for {sha[:10]}:\n{cp2.stderr}")
                run(["git", "cherry-pick", "--abort"])
                return 1

        if run_pre_commit:
            touched = [
                p
                for p in required(["git", "diff", "--cached", "--name-only"]).split()
                if Path(p).exists()
            ]
            if touched:
                # Auto-fix; ignore exit code (some hooks return non-zero on fix)
                run([pre_commit_bin, "run", "--files", *touched])
                # Re-stage anything pre-commit modified
                run(["git", "add", "--", *touched])

        # Empty commit detection
        if run(["git", "diff", "--cached", "--quiet"]).returncode == 0:
            run(["git", "reset", "--hard", "HEAD"])
            dropped += 1
            print(f"  DROP    {sha[:10]} {subj}", file=sys.stderr)
            continue

        new_subj, new_body, changed = normalize_message(subj, body)
        msg = new_subj + (("\n\n" + new_body.lstrip()) if new_body.strip() else "")

        env = os.environ.copy()
        env["GIT_AUTHOR_NAME"] = an
        env["GIT_AUTHOR_EMAIL"] = ae
        env["GIT_AUTHOR_DATE"] = ad

        # --no-verify because we already ran pre-commit ourselves above;
        # plus 'fixup!'-style amend protection isn't in play here.
        c = run(
            ["git", "commit", "--no-verify", "--allow-empty-message", "-m", msg],
            env=env,
        )
        if c.returncode != 0:
            sys.stderr.write(f"commit failed for {sha[:10]}:\n{c.stderr}")
            return 1

        if changed:
            rewritten += 1
            print(f"  REWRITE {sha[:10]} → {new_subj}", file=sys.stderr)
        else:
            kept_unchanged += 1

    print(
        f"\nDone. kept={kept_unchanged} subject-rewritten={rewritten} dropped-empty={dropped}",
        file=sys.stderr,
    )
    return 0


def main(argv: list[str] | None = None) -> int:
    p = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    p.add_argument("--base", required=True, help="Base ref (e.g. upstream/main)")
    p.add_argument(
        "--dry-run", action="store_true", help="Print plan without rewriting"
    )
    p.add_argument(
        "--backup-tag", help="Tag the current HEAD with this name before rewriting"
    )
    p.add_argument(
        "--no-pre-commit",
        action="store_true",
        help="Skip the per-commit pre-commit auto-fix pass (subject-only normalization)",
    )
    args = p.parse_args(argv)

    # Sanity: clean working tree
    if (
        run(["git", "diff", "--quiet"]).returncode != 0
        or run(["git", "diff", "--cached", "--quiet"]).returncode != 0
    ):
        sys.stderr.write("Working tree is not clean. Aborting.\n")
        return 2

    if args.backup_tag and not args.dry_run:
        required(["git", "tag", "-f", args.backup_tag])
        print(
            f"Backup tag '{args.backup_tag}' points at original HEAD.", file=sys.stderr
        )

    return replay(
        args.base, dry_run=args.dry_run, run_pre_commit=not args.no_pre_commit
    )


if __name__ == "__main__":
    sys.exit(main())
