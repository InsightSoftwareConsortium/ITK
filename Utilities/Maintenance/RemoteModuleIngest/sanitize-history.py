#!/usr/bin/env python3
"""sanitize-history.py — Apply ITK formatting and commit-prefix conventions
to every commit on a filter-repo'd ingestion branch.

Per the v4 strategy (Phase A, step 5), this script walks the rewritten
ingestion branch and applies the SAME formatters that ITK's
`.pre-commit-config.yaml` enforces, so the rewritten history is gated
through identical rules.  Concretely, for every blob:

  Content-aware (filename-blind, sniff-driven):
    1. C/C++ blobs    -> `clang-format -style=file` (ITK's .clang-format)
    2. Python blobs   -> `black`
    3. CMake blobs    -> `gersemi --config .gersemi.config`
    4. All text blobs -> trailing-whitespace strip
                       + end-of-file fixer (final \\n)
                       + mixed-line-ending (CRLF -> LF)
    5. Binary / .sha* / .vtk / .vtp / .svg content -> skip (left as-is)

  Commit-message:
    Examine the subject; if it does not start with a recognized ITK
    prefix (BUG/COMP/DOC/ENH/PERF/STYLE), auto-prepend one based on
    a conservative heuristic.  Every decision is logged to a sidecar
    file for operator audit.

git-filter-repo's streaming model emits blobs before commits and does
not expose a public "fetch blob by id" API in commit_callback context,
so all blob-content rewriting happens in blob_callback.  This is
filename-blind by necessity — ITK's pre-commit excludes (which are
filename-pattern based) are approximated via content-sniffing.  For
typical ITK module ingests, the upstream whitelist (via filter-repo's
--paths-from-file step) has already pruned ThirdParty / Data /
build / pixi-cache paths, so the exclude set narrows to a few
content-detectable extensions (.sha / .sha512 / .svg / .vtk / .vtp).

Usage (invoked from ingest-module-v4.sh; not normally run directly):

    python3 sanitize-history.py \\
        --repo /path/to/filter-repo'd/clone \\
        --clang-format-style /path/to/itk/.clang-format \\
        --gersemi-config /path/to/itk/.gersemi.config \\
        --log-dir /path/to/log/dir
"""

from __future__ import annotations

import argparse
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

try:
    import git_filter_repo as fr
except ImportError:
    print(
        "Error: git_filter_repo Python module not found.\n"
        "  Install with:  pip install git-filter-repo\n"
        "  Or:            pixi global install git-filter-repo",
        file=sys.stderr,
    )
    sys.exit(1)


# ---------------------------------------------------------------------------
# Commit-subject prefix logic
# ---------------------------------------------------------------------------

PREFIX_RE = re.compile(rb"^\s*(BUG|COMP|DOC|ENH|PERF|STYLE|WIP)\s*:")
SUBJECT_MAX_LEN = 78  # ITK ghostflow / kw-commit-msg rule
SUBJECT_WORDBREAK_MIN = 40  # only break at a space past this offset


def truncate_subject_to_body(subject: bytes, rest: bytes) -> tuple[bytes, bytes, bool]:
    """If the subject exceeds SUBJECT_MAX_LEN, truncate it (preferring a word
    break) and prepend the excess to the body.  Returns (new_subject, new_rest,
    changed)."""
    if len(subject) <= SUBJECT_MAX_LEN:
        return subject, rest, False
    cut = SUBJECT_MAX_LEN
    space_pos = subject.rfind(b" ", 0, SUBJECT_MAX_LEN)
    if space_pos > SUBJECT_WORDBREAK_MIN:
        cut = space_pos
    head = subject[:cut].rstrip()
    tail = subject[cut:].lstrip()
    if not tail:
        return head, rest, True
    if rest:
        # rest starts with at least one '\n' from the original separator
        new_rest = b"\n\n" + tail + rest
    else:
        new_rest = b"\n\n" + tail + b"\n"
    return head, new_rest, True


def ensure_blank_line_after_subject(rest: bytes) -> tuple[bytes, bool]:
    """Subject must be followed by a blank line if a body exists.  rest, when
    non-empty, starts with '\\n' (the original line separator).  If the next
    char isn't also '\\n', insert one to make it a proper blank line."""
    if not rest:
        return rest, False
    if rest.startswith(b"\n\n"):
        return rest, False
    # rest starts with single '\n' followed by body content; convert to "\n\n"
    return b"\n" + rest, True


def heuristic_prefix(subject: bytes) -> bytes:
    s = subject.lower()
    if (
        b"fix " in s
        or s.startswith(b"fix")
        or b" bug" in s
        or b"bug " in s
        or b"crash" in s
        or b"segfault" in s
        or b"leak" in s
        or b"regression" in s
        or b"corrupt" in s
        or b"deadlock" in s
    ):
        return b"BUG"
    if (
        b"cmake" in s
        or b"compil" in s
        or b"warning" in s
        or b"build" in s
        or b" ci " in s
        or s.startswith(b"ci:")
        or b" gcc" in s
        or b"clang " in s
        or b" msvc" in s
    ):
        return b"COMP"
    if (
        b"doc " in s
        or b"docs" in s
        or s.startswith(b"doc")
        or b"readme" in s
        or b"comment" in s
        or b"license" in s
        or b"sphinx" in s
        or b"doxygen" in s
    ):
        return b"DOC"
    if (
        b"style" in s
        or b"whitespace" in s
        or b"format" in s
        or b"rename" in s
        or b" lint" in s
        or s.startswith(b"lint")
        or b"clang-format" in s
        or b"cleanup" in s
        or b"clean up" in s
        or b"reorder" in s
    ):
        return b"STYLE"
    if (
        b"perf" in s
        or b"optim" in s
        or b"speed" in s
        or b"faster" in s
        or b"efficien" in s
    ):
        return b"PERF"
    return b"ENH"


# ---------------------------------------------------------------------------
# Content-type sniffing — filename-blind, conservative
# ---------------------------------------------------------------------------

CXX_HINTS = (
    b"#include",
    b"#pragma",
    b"#ifndef ITK",
    b"#ifndef itk",
    b"#ifndef __itk",
    b"namespace itk",
    b"namespace ITK",
    b"template <",
    b"template<",
)

PY_SHEBANG_HINTS = (
    b"#!/usr/bin/env python",
    b"#!/usr/bin/python",
    b"#!python",
)
PY_KEYWORD_HINTS = (
    b"\nimport ",
    b"\nfrom ",
    b"\ndef ",
    b"\nclass ",
)

CMAKE_HINTS = (
    b"cmake_minimum_required",
    b"\nproject(",
    b"\nadd_library(",
    b"\nadd_executable(",
    b"\nadd_subdirectory(",
    b"\nfind_package(",
    b"\ntarget_link_libraries(",
    b"\ntarget_include_directories(",
    b"\nset(CMAKE_",
    b"\ninclude(CMakeParseArguments",
    b"\nitk_module(",
    b"\nitk_module_impl",
    b"\nitk_module_test",
    b"\nitk_add_test",
    b"\nset_tests_properties",
    b"\nitk_wrap_",
)

# Single-line content-link / hash file signatures
HEX_HASH_RE = re.compile(rb"^[0-9a-f]{32,128}\s*$")

# Skip-entirely sniffs (content cannot be safely text-fixed)
SKIP_HINTS = (
    b"<?xml",  # SVG, VTP, generic XML (rare in tests)
    b"# vtk DataFile Version",  # legacy VTK
    b"<VTKFile",  # modern VTK XML
)


def is_binary(head: bytes) -> bool:
    """True if the leading bytes contain a NUL — fast text/binary heuristic."""
    return b"\x00" in head[:8192]


def is_skip_content(data: bytes, head: bytes) -> bool:
    """Return True for content that should be left untouched (CID/sha
    content-links, VTK volumes, SVG, etc.)."""
    if any(s in head[:512] for s in SKIP_HINTS):
        return True
    # Single-token hex hash file (CID content-link sidecar)
    if data.count(b"\n") <= 1 and HEX_HASH_RE.match(data.strip()):
        return True
    return False


def sniff_kind(data: bytes) -> str | None:
    """Return 'cxx', 'py', 'cmake', 'text', or None.

    'text' = generic plain text — will get only universal whitespace/EOF
    fixers, no kind-specific formatter.
    None = binary or skip-content (do nothing)."""
    if not data:
        return None

    head = data[:8192]
    if is_binary(head):
        return None
    if is_skip_content(data, head):
        return None

    head_lstripped = head.lstrip()

    if any(head_lstripped.startswith(s) for s in PY_SHEBANG_HINTS):
        return "py"

    cxx_hit = any(hint in head for hint in CXX_HINTS)
    cmake_hit = any(hint in b"\n" + head for hint in CMAKE_HINTS)
    py_hit = any(hint in b"\n" + head for hint in PY_KEYWORD_HINTS)

    # Disambiguate when multiple hits: cxx > cmake > py > generic text
    # CXX wins because its hints are highly specific; #include is unambiguous.
    # CMake wins over python because cmake_minimum_required + add_library are
    # specific, while `import` / `from` could be a comment in CMake.
    if cxx_hit:
        return "cxx"
    if cmake_hit:
        return "cmake"
    if py_hit:
        return "py"
    return "text"


# ---------------------------------------------------------------------------
# Universal text fixers (correspond to pre-commit-hooks
# trailing-whitespace, end-of-file-fixer, mixed-line-ending)
# ---------------------------------------------------------------------------


def fix_mixed_line_ending(data: bytes) -> bytes:
    # ITK convention: LF only.  Convert CRLF and bare CR -> LF.
    return data.replace(b"\r\n", b"\n").replace(b"\r", b"\n")


def fix_trailing_whitespace(data: bytes) -> bytes:
    # Strip horizontal whitespace from end of every line.
    lines = data.split(b"\n")
    return b"\n".join(line.rstrip(b" \t") for line in lines)


def fix_end_of_file(data: bytes) -> bytes:
    if not data:
        return data
    return data.rstrip(b"\n") + b"\n"


def apply_universal_text_fixers(data: bytes) -> bytes:
    return fix_end_of_file(fix_trailing_whitespace(fix_mixed_line_ending(data)))


# ---------------------------------------------------------------------------
# Formatter wrappers — fail-soft (return original on error)
# ---------------------------------------------------------------------------


def fmt_cxx(data: bytes, clang_format_bin: str) -> bytes:
    try:
        proc = subprocess.run(
            [
                clang_format_bin,
                "-style=file",
                "-assume-filename=ingested.cxx",
            ],
            input=data,
            capture_output=True,
            check=True,
            timeout=60,
        )
        return proc.stdout or data
    except (
        subprocess.CalledProcessError,
        subprocess.TimeoutExpired,
        FileNotFoundError,
    ):
        return data


def fmt_py(data: bytes, black_bin: str) -> bytes:
    try:
        proc = subprocess.run(
            [black_bin, "-q", "-"],
            input=data,
            capture_output=True,
            check=True,
            timeout=60,
        )
        return proc.stdout or data
    except (
        subprocess.CalledProcessError,
        subprocess.TimeoutExpired,
        FileNotFoundError,
    ):
        return data


TEXT_FILE_EXTS = (
    b".c",
    b".cc",
    b".cpp",
    b".cxx",
    b".h",
    b".hpp",
    b".hxx",
    b".txx",
    b".tcc",
    b".py",
    b".cmake",
    b".wrap",
    b".md",
    b".rst",
    b".txt",
    b".yml",
    b".yaml",
    b".json",
    b".toml",
    b".ini",
    b".cfg",
    b".in",
    b".tex",
    b".bib",
    b".css",
    b".html",
    b".xml",
    b".dox",
)
TEXT_FILE_BASENAMES = (
    b"cmakelists.txt",
    b"license",
    b"readme",
    b"copying",
    b"authors",
    b"changelog",
    b"itk-module.cmake",
)


def filename_is_text(filename: bytes) -> bool:
    """Return True for filenames that are clearly source/text and should
    therefore never carry the executable bit (used to fix mode-bit drift in
    upstream module commits)."""
    lower = filename.lower()
    if any(lower.endswith(e) for e in TEXT_FILE_EXTS):
        return True
    base = lower.rsplit(b"/", 1)[-1]
    return base in TEXT_FILE_BASENAMES


# Many ITK remote-module itk-module.cmake files do
#   file(READ "${MY_CURRENT_DIR}/README.rst" DOCUMENTATION)
# to populate the CMake `DESCRIPTION` variable.  The v4 whitelist
# excludes the upstream README (the operator ships a new README.md as
# part of the ingest PR), so leaving that line as-is breaks every
# historical commit's build.  Rewriting the reference to README.md
# makes every commit independently buildable in the ingested tree.
README_RST_REF_RE = re.compile(
    rb"(file\s*\(\s*READ\s+[^)]*?)README\.rst", re.IGNORECASE
)


def patch_readme_reference(data: bytes) -> tuple[bytes, bool]:
    """Rewrite `file(READ ".../README.rst" ...)` -> README.md.  Returns
    (new_data, changed)."""
    new_data, n = README_RST_REF_RE.subn(rb"\1README.md", data)
    return new_data, n > 0


# Upstream remote modules wrap their top-level CMakeLists.txt in a
# standalone-build guard so the same file works both as a
# remote-fetched module and as a stand-alone CMake project:
#
#     if(NOT ITK_SOURCE_DIR)
#       find_package(ITK REQUIRED)
#       list(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
#       include(ITKModuleExternal)
#     else()
#       itk_module_impl()
#     endif()
#
# In an in-tree ingested module, ITK_SOURCE_DIR is always defined, so
# the if-branch is dead code.  Reviewers flag it on every ingest (e.g.
# @dzenanz on PR #6206 IOMeshSTL).  Strip the wrapper here so it is
# never present in the ingest history.
STANDALONE_BUILD_GUARD_RE = re.compile(
    rb"if\s*\(\s*NOT\s+ITK_SOURCE_DIR\s*\)[ \t]*\n"
    rb"[ \t]*find_package\s*\(\s*ITK\s+REQUIRED\s*\)[ \t]*\n"
    rb"[ \t]*list\s*\(\s*APPEND\s+CMAKE_MODULE_PATH\s+"
    rb"\$\{\s*ITK_CMAKE_DIR\s*\}\s*\)[ \t]*\n"
    rb"[ \t]*include\s*\(\s*ITKModuleExternal\s*\)[ \t]*\n"
    rb"[ \t]*else\s*\(\s*\)[ \t]*\n"
    rb"([ \t]*)itk_module_impl\s*\(\s*\)[ \t]*\n"
    rb"[ \t]*endif\s*\(\s*\)",
    re.IGNORECASE,
)


def patch_standalone_build_guard(data: bytes) -> tuple[bytes, bool]:
    """Replace the `if(NOT ITK_SOURCE_DIR) ... else() itk_module_impl() endif()`
    standalone-build wrapper with a bare `itk_module_impl()`.  Returns
    (new_data, changed).  The replacement preserves the indentation of the
    inner `itk_module_impl()` line so post-rewrite gersemi has a stable
    starting point."""
    new_data, n = STANDALONE_BUILD_GUARD_RE.subn(rb"\1itk_module_impl()", data)
    return new_data, n > 0


def fmt_cmake(data: bytes, gersemi_bin: str, gersemi_config: str | None) -> bytes:
    cmd = [gersemi_bin]
    if gersemi_config:
        cmd += ["--config", gersemi_config]
    cmd += ["-"]  # stdin -> stdout
    try:
        proc = subprocess.run(
            cmd,
            input=data,
            capture_output=True,
            check=True,
            timeout=60,
        )
        return proc.stdout or data
    except (
        subprocess.CalledProcessError,
        subprocess.TimeoutExpired,
        FileNotFoundError,
    ):
        return data


# ---------------------------------------------------------------------------
# Stateful sanitizer with logging
# ---------------------------------------------------------------------------


class HistorySanitizer:
    def __init__(
        self,
        *,
        clang_format_bin: str,
        black_bin: str,
        gersemi_bin: str,
        gersemi_config: str | None,
        log_dir: Path,
    ):
        self.clang_format_bin = clang_format_bin
        self.black_bin = black_bin
        self.gersemi_bin = gersemi_bin
        self.gersemi_config = gersemi_config
        self.log_dir = log_dir
        self.log_dir.mkdir(parents=True, exist_ok=True)
        self.prefix_log = (log_dir / "commit-prefix-log.txt").open("w", buffering=1)
        self.format_log = (log_dir / "format-actions.log").open("w", buffering=1)
        self.commit_count = 0
        self.prefix_changes = 0
        self.subject_truncations = 0
        self.subject_blank_inserts = 0
        self.mode_normalizations = 0
        self.readme_ref_patches = 0
        self.standalone_guard_patches = 0
        self.blob_count = 0
        self.format_changes = 0
        self.kind_counts: dict[str, int] = {}

    # -- callbacks -----------------------------------------------------------

    def blob_callback(self, blob: fr.Blob, _metadata: dict) -> None:
        self.blob_count += 1
        kind = sniff_kind(blob.data)
        if kind is None:
            return
        self.kind_counts[kind] = self.kind_counts.get(kind, 0) + 1

        original_data = blob.data
        original_len = len(original_data)

        # Step 1: kind-specific formatter
        if kind == "cxx":
            new_data = fmt_cxx(original_data, self.clang_format_bin)
        elif kind == "py":
            new_data = fmt_py(original_data, self.black_bin)
        elif kind == "cmake":
            new_data, guard_unwrapped = patch_standalone_build_guard(original_data)
            if guard_unwrapped:
                self.standalone_guard_patches += 1
            new_data = fmt_cmake(new_data, self.gersemi_bin, self.gersemi_config)
            new_data, readme_patched = patch_readme_reference(new_data)
            if readme_patched:
                self.readme_ref_patches += 1
        else:  # 'text'
            new_data = original_data

        # Step 2: universal text fixers — applied to every text blob,
        # even ones the kind-specific formatter already passed over.
        new_data = apply_universal_text_fixers(new_data)

        if new_data == original_data:
            return

        self.format_changes += 1
        try:
            self.format_log.write(
                f"{kind:5s}  {original_len:8d} -> {len(new_data):8d}  "
                f"{(blob.original_id or b'').decode('utf-8', 'replace')[:12]}\n"
            )
        except Exception:
            pass
        blob.data = new_data

    def commit_callback(self, commit: fr.Commit, _metadata: dict) -> None:
        self.commit_count += 1

        # ---- subject sanitization ----
        msg = commit.message
        nl = msg.find(b"\n")
        subject = msg if nl == -1 else msg[:nl]
        rest = b"" if nl == -1 else msg[nl:]

        # Step 1: ensure ITK prefix
        prepended = False
        if not PREFIX_RE.match(subject):
            prefix = heuristic_prefix(subject)
            subject = prefix + b": " + subject
            self.prefix_changes += 1
            prepended = True
            try:
                old = (
                    (msg if nl == -1 else msg[:nl])
                    .decode("utf-8", "replace")
                    .strip()[:120]
                )
                sha = (commit.original_id or b"<new>").decode("utf-8", "replace")
                self.prefix_log.write(f"{sha[:12]} {prefix.decode()}: {old}\n")
            except Exception:
                pass

        # Step 2: enforce 78-char subject limit (ghostflow / kw-commit-msg)
        subject, rest, truncated = truncate_subject_to_body(subject, rest)
        if truncated:
            self.subject_truncations += 1

        # Step 3: ensure subject is followed by a blank line if a body exists
        rest, blank_inserted = ensure_blank_line_after_subject(rest)
        if blank_inserted:
            self.subject_blank_inserts += 1

        if prepended or truncated or blank_inserted:
            commit.message = subject + rest

        # ---- file-mode normalization (clear executable bit on text files) ----
        if commit.file_changes:
            for change in commit.file_changes:
                if change.type not in (b"M", b"A"):
                    continue
                if not change.filename or not change.mode:
                    continue
                if change.mode != b"100755":
                    continue
                if filename_is_text(change.filename):
                    change.mode = b"100644"
                    self.mode_normalizations += 1

    def close(self) -> None:
        for f in (self.prefix_log, self.format_log):
            try:
                f.close()
            except Exception:
                pass


def main() -> int:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--repo", type=Path, required=True)
    p.add_argument("--clang-format-style", type=Path, required=True)
    p.add_argument(
        "--gersemi-config",
        type=Path,
        default=None,
        help="Path to ITK's .gersemi.config (optional but strongly recommended)",
    )
    p.add_argument("--log-dir", type=Path, required=True)
    p.add_argument(
        "--clang-format-bin", default=os.environ.get("CLANG_FORMAT_BIN", "clang-format")
    )
    p.add_argument("--black-bin", default=os.environ.get("BLACK_BIN", "black"))
    p.add_argument("--gersemi-bin", default=os.environ.get("GERSEMI_BIN", "gersemi"))
    args = p.parse_args()

    if not args.repo.exists():
        print(f"Error: repo path does not exist: {args.repo}", file=sys.stderr)
        return 2
    if not args.clang_format_style.exists():
        print(
            f"Error: clang-format file does not exist: {args.clang_format_style}",
            file=sys.stderr,
        )
        return 2
    for binname, binval in (
        ("clang-format", args.clang_format_bin),
        ("black", args.black_bin),
        ("gersemi", args.gersemi_bin),
    ):
        if shutil.which(binval) is None:
            print(f"Error: {binname} not found at {binval}", file=sys.stderr)
            return 2

    target_style = args.repo / ".clang-format"
    if not target_style.exists():
        target_style.write_bytes(args.clang_format_style.read_bytes())

    gersemi_config_path: str | None = None
    if args.gersemi_config:
        if not args.gersemi_config.exists():
            print(
                f"Error: gersemi config does not exist: {args.gersemi_config}",
                file=sys.stderr,
            )
            return 2
        gersemi_config_path = str(args.gersemi_config)
    else:
        print(
            "Warning: --gersemi-config not supplied; gersemi will run with defaults.",
            file=sys.stderr,
        )

    print(f"sanitize-history: rewriting commits in {args.repo}", file=sys.stderr)
    print(f"  clang-format:    {args.clang_format_bin}", file=sys.stderr)
    print(f"  black:           {args.black_bin}", file=sys.stderr)
    print(f"  gersemi:         {args.gersemi_bin}", file=sys.stderr)
    print(f"  style file:      {args.clang_format_style}", file=sys.stderr)
    print(f"  gersemi config:  {gersemi_config_path or '(default)'}", file=sys.stderr)
    print(f"  log dir:         {args.log_dir}", file=sys.stderr)

    os.chdir(args.repo)

    sanitizer = HistorySanitizer(
        clang_format_bin=args.clang_format_bin,
        black_bin=args.black_bin,
        gersemi_bin=args.gersemi_bin,
        gersemi_config=gersemi_config_path,
        log_dir=args.log_dir,
    )

    filter_args = fr.FilteringOptions.parse_args(["--force"])
    repo_filter = fr.RepoFilter(
        filter_args,
        blob_callback=sanitizer.blob_callback,
        commit_callback=sanitizer.commit_callback,
    )

    try:
        repo_filter.run()
    finally:
        sanitizer.close()

    kc = sanitizer.kind_counts
    print(
        f"\nsanitize-history complete:\n"
        f"  commits walked:        {sanitizer.commit_count}\n"
        f"  prefix auto-prepended: {sanitizer.prefix_changes}\n"
        f"  subjects truncated:    {sanitizer.subject_truncations}\n"
        f"  blank lines inserted:  {sanitizer.subject_blank_inserts}\n"
        f"  exec-bits cleared:     {sanitizer.mode_normalizations}\n"
        f"  README.rst->.md fixes: {sanitizer.readme_ref_patches}\n"
        f"  standalone-guard rm:   {sanitizer.standalone_guard_patches}\n"
        f"  blobs scanned:         {sanitizer.blob_count}\n"
        f"  blobs reformatted:     {sanitizer.format_changes}\n"
        f"  by-kind sniff:         "
        f"cxx={kc.get('cxx', 0)} "
        f"py={kc.get('py', 0)} "
        f"cmake={kc.get('cmake', 0)} "
        f"text={kc.get('text', 0)} "
        f"skip={sanitizer.blob_count - sum(kc.values())}\n"
        f"  logs:                  {args.log_dir}/",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
