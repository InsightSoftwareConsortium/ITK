#!/usr/bin/env python


description = """
Manage ITK migration sentinels.

Each CMake/MigrationSentinels/ITK_MIGRATION_*.md file asserts that one
downstream-visible change is present in the ITK source tree. The filenames are
collected into ITK_MIGRATION_SENTINELS at configure time and exported through
ITKConfig.cmake, letting downstream projects gate on a specific change during
the interval between ITK tags.

Sentinels are deliberately short-lived. They are removed at each tag, after
which the version arm of the downstream condition carries the same meaning.
"""

import argparse
import re
import subprocess
import sys
from pathlib import Path

# Approved sentinel name patterns, most specific first. ITK_MIGRATION_PR<N> is
# strongly preferred because it is traceable to the pull request.
SENTINEL_PATTERNS = (
    ("ITK_MIGRATION_PR<N>", re.compile(r"^ITK_MIGRATION_PR[1-9][0-9]*$")),
    (
        "ITK_MIGRATION_HASH_<githash>",
        re.compile(r"^ITK_MIGRATION_HASH_[0-9a-f]{7,40}$"),
    ),
    (
        "ITK_MIGRATION_<DESCRIPTIVE_NAME>",
        re.compile(r"^ITK_MIGRATION_[A-Z][A-Z0-9_]*$"),
    ),
)

SENTINEL_GLOB = "ITK_MIGRATION_*.md"


def default_sentinel_dir() -> Path:
    """CMake/MigrationSentinels relative to this script's location in ITK."""
    return (
        Path(__file__).resolve().parent.parent.parent / "CMake" / "MigrationSentinels"
    )


def matched_pattern(name: str) -> str | None:
    for label, pattern in SENTINEL_PATTERNS:
        if pattern.match(name):
            return label
    return None


def sentinel_files(sentinel_dir: Path) -> list[Path]:
    return sorted(sentinel_dir.glob(SENTINEL_GLOB))


def validate(sentinel_dir: Path) -> int:
    if not sentinel_dir.is_dir():
        print(f"error: {sentinel_dir} is not a directory", file=sys.stderr)
        return 1

    errors = []

    for path in sentinel_files(sentinel_dir):
        name = path.stem
        if matched_pattern(name) is None:
            errors.append(
                f"{path.name}: '{name}' matches no approved pattern "
                f"({', '.join(label for label, _ in SENTINEL_PATTERNS)})"
            )
        lines = [line.strip() for line in path.read_text().splitlines()]
        described = [line for line in lines if line]
        if len(described) != 1:
            errors.append(
                f"{path.name}: expected exactly one non-empty description line, "
                f"found {len(described)}"
            )

    # A file whose prefix is misspelled is silently ignored by the CMake glob,
    # so the sentinel would never be published. Catch it here instead.
    for path in sorted(sentinel_dir.iterdir()):
        if path.is_dir() or path.name == "README.md":
            continue
        if path.suffix == ".md" and path.name.startswith("ITK_MIGRATION_"):
            continue
        errors.append(
            f"{path.name}: not matched by the '{SENTINEL_GLOB}' glob, so it "
            f"would be silently ignored; rename it or remove it"
        )

    for message in errors:
        print(f"error: {message}", file=sys.stderr)

    if errors:
        print(f"{len(errors)} sentinel problem(s) found", file=sys.stderr)
        return 1

    print(f"{len(sentinel_files(sentinel_dir))} sentinel(s) validated")
    return 0


def expire_itk(sentinel_dir: Path, dry_run: bool) -> int:
    """Remove every sentinel file. Run once, immediately after tagging.

    Every sentinel present at a tag ships in that tag by definition, so the
    directory is emptied rather than filtered. Downstream projects that have
    not yet been rewritten keep working: the version arm of their condition is
    now true even though the sentinel is gone.
    """
    if not sentinel_dir.is_dir():
        print(f"error: {sentinel_dir} is not a directory", file=sys.stderr)
        return 1

    paths = sentinel_files(sentinel_dir)
    if not paths:
        print("no sentinels to expire")
        return 0

    for path in paths:
        print(f"expiring {path.name}")
    if dry_run:
        print(f"dry run: {len(paths)} sentinel(s) left in place")
        return 0

    subprocess.run(
        ["git", "rm", "--quiet", "--"] + [str(p) for p in paths],
        check=True,
    )
    print(f"expired {len(paths)} sentinel(s); commit the result")
    return 0


# The two-arm downstream condition. \s matches newlines, so the condition may
# be wrapped across lines. Both if() and elseif() are handled: a missed elseif
# would silently keep a sentinel reference alive after the sentinel is gone.
CONDITION_RE = re.compile(
    r"(?P<indent>[ \t]*)(?P<kw>if|elseif)\s*\(\s*"
    r"ITK_VERSION\s+VERSION_GREATER_EQUAL\s+(?P<tag>[0-9][0-9A-Za-z.]*)\s+"
    r"OR\s+(?P<sentinel>ITK_MIGRATION_[A-Za-z0-9_]+)\s+"
    r"IN_LIST\s+ITK_MIGRATION_SENTINELS\s*\)"
)

CMAKE_FILE_GLOBS = ("*.cmake", "CMakeLists.txt")


EXCLUDED_DIR_NAMES = {".git", "build", "_build", ".pixi"}


def _cmake_files(repo: Path) -> list[Path]:
    found: list[Path] = []
    for pattern in CMAKE_FILE_GLOBS:
        found.extend(repo.rglob(pattern))
    return sorted(p for p in found if EXCLUDED_DIR_NAMES.isdisjoint(p.parts))


def expire_downstream(repo: Path, tag: str, sentinel_dir: Path, dry_run: bool) -> int:
    """Reduce two-arm conditions for `tag` to their version arm.

    Idempotent: a condition already reduced no longer matches CONDITION_RE.
    """
    if not repo.is_dir():
        print(f"error: {repo} is not a directory", file=sys.stderr)
        return 1

    known = (
        {p.stem for p in sentinel_files(sentinel_dir)}
        if sentinel_dir.is_dir()
        else set()
    )

    rewritten = 0
    skipped_other_tag = 0
    unknown_sentinels = []

    for path in _cmake_files(repo):
        try:
            original = path.read_text(encoding="utf-8")
        except UnicodeDecodeError:
            print(f"warning: skipping {path}: not valid UTF-8", file=sys.stderr)
            continue

        def replace(match: re.Match) -> str:
            nonlocal rewritten, skipped_other_tag
            if match.group("tag") != tag:
                skipped_other_tag += 1
                return match.group(0)
            sentinel = match.group("sentinel")
            if known and sentinel not in known:
                unknown_sentinels.append(f"{path}: {sentinel}")
            rewritten += 1
            return (
                f"{match.group('indent')}{match.group('kw')}"
                f"(ITK_VERSION VERSION_GREATER_EQUAL {tag})"
            )

        updated = CONDITION_RE.sub(replace, original)
        if updated != original and not dry_run:
            path.write_text(updated, encoding="utf-8")

    for entry in unknown_sentinels:
        print(
            f"warning: {entry} is not published by ITK; it may be a typo or "
            f"already expired",
            file=sys.stderr,
        )

    action = "would rewrite" if dry_run else "rewrote"
    print(f"{action} {rewritten} condition(s) for tag {tag}")
    if skipped_other_tag:
        print(f"left {skipped_other_tag} condition(s) for other tags untouched")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description=description,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    subparsers = parser.add_subparsers(dest="command", required=True)

    validate_parser = subparsers.add_parser(
        "validate", help="check that every sentinel is well-formed"
    )
    validate_parser.add_argument(
        "--sentinel-dir",
        type=Path,
        default=default_sentinel_dir(),
        help="directory holding the sentinel files",
    )

    expire_itk_parser = subparsers.add_parser(
        "expire-itk", help="git rm every sentinel file (run once, after tagging)"
    )
    expire_itk_parser.add_argument(
        "--sentinel-dir",
        type=Path,
        default=default_sentinel_dir(),
        help="directory holding the sentinel files",
    )
    expire_itk_parser.add_argument(
        "--dry-run",
        action="store_true",
        help="list what would be removed without removing it",
    )

    expire_downstream_parser = subparsers.add_parser(
        "expire-downstream",
        help="reduce two-arm conditions in a downstream repo to the version arm",
    )
    expire_downstream_parser.add_argument(
        "--tag", required=True, help="version whose conditions should be reduced"
    )
    expire_downstream_parser.add_argument(
        "--repo", type=Path, required=True, help="downstream repository to rewrite"
    )
    expire_downstream_parser.add_argument(
        "--sentinel-dir",
        type=Path,
        default=default_sentinel_dir(),
        help="ITK sentinel directory, used to flag unknown sentinels",
    )
    expire_downstream_parser.add_argument(
        "--dry-run", action="store_true", help="report without writing"
    )

    args = parser.parse_args()
    if args.command == "validate":
        return validate(args.sentinel_dir)
    if args.command == "expire-itk":
        return expire_itk(args.sentinel_dir, args.dry_run)
    if args.command == "expire-downstream":
        return expire_downstream(args.repo, args.tag, args.sentinel_dir, args.dry_run)
    return 1


if __name__ == "__main__":
    sys.exit(main())
