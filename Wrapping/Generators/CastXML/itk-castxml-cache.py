#!/usr/bin/env python3
"""
Two-level content-addressed cache wrapper for ITK's CastXML wrapping step.

Invocation (replaces ccache + castxml in the cmake COMMAND):
    python3 itk-castxml-cache.py /path/to/castxml [castxml args...]
    python3 itk-castxml-cache.py --no-cache /path/to/castxml [castxml args...]
    python3 itk-castxml-cache.py --evict DAYS [--max-size GB]  # e.g. --evict 13.9 --max-size 2.0

Cache key hierarchy:
  L1 (fast, no subprocess): sha256 of key version + castxml content-hash +
      inc file + cxx file + flags.
      The castxml binary is fingerprinted by SHA-256 of its content (not mtime), so
      `ninja -t clean` re-links the binary without changing the L1 key.  Each L1
      entry also records the `-E` header set with content hashes plus a listing
      fingerprint of every -I include dir; an L1 hit is honored only when both
      still match, so a changed header, or a new header appearing in any include
      dir (shadowing), correctly misses instead of restoring stale XML.  A new
      header inside an existing subdirectory of an include dir is not detected
      (same residual gap as compiler depfiles).
  L2 (content-addressed): sha256 of key version + castxml content-hash +
      normalised `castxml -E` preprocessed output.  Preprocessor line markers
      (# N "path") are stripped before hashing, making L2 keys path-independent
      across build directories; the castxml content-hash ensures a castxml
      upgrade never restores XML produced by the previous binary.

Lookup:
  L1 HIT (recorded header hashes still match) → restore L2 → DONE (no subprocess)
  L1 miss / header changed → run castxml -E → compute L2 key
              L2 HIT  → restore; refresh L1 map
              L2 miss → run full castxml; store; write L1 map

Storage formats (ITK_WRAP_CACHE_FORMAT):
  gzip (default): output.xml.gz, ~8x smaller than raw XML.  Decompressed on
      restore.  253 MB for a full 807-module ITK build; each build directory
      gets its own decompressed copy (copy is nearly as fast as a hardlink).
  uncompressed: output.xml, plain copy on restore.  ~2.2 G per full build.
      Set ITK_WRAP_CACHE_FORMAT=uncompressed to opt in.

Multi-path cascade (ITK_WRAP_CACHE, colon-separated):
  Reads search all paths in order, returning the first hit.  Writes go to the
  first path that accepts them (atomic rename succeeds).  A read-only shared
  NFS cache can be listed after the user's writable local SSD cache, e.g.:
      export ITK_WRAP_CACHE=/local/ssd/cache:/nfs/lab/shared-cache
  Students benefit from the lab cache for L2 hits (saving the full castxml
  run) while writing L1 maps only to their own writable local cache.

Eviction: run once after a full wrapping build via --evict DAYS.
  Skipped immediately when nothing was stored since the previous eviction, so
  null builds pay two stat calls instead of a cache-tree walk.  Time pass
  removes entries older than DAYS (the CMake ITK_WRAP_CASTXML_CACHE_MAX_DAYS
  variable supplies DAYS in ITK builds); size pass then removes oldest entries
  until total is under ITK_WRAP_CACHE_MAX_SIZE GB (default 2.0).  Staging
  leftovers and entries lacking their _ok marker are removed after a one-hour
  grace period.  --evict touches only the first writable cache in the list.

Environment:
  ITK_WRAP_CACHE          colon-separated cache roots (default: ~/.cache/itk-wrap)
  ITK_WRAP_CACHE_FORMAT   storage format: gzip (default) or uncompressed
  ITK_WRAP_CACHE_VERBOSE  set to 1 for hit/miss logging to stderr
  ITK_WRAP_CACHE_BYPASS   set to 1 to bypass all caching (same as --no-cache)
  ITK_WRAP_CACHE_MAX_SIZE max cache size in GB enforced by --evict (default: 2.0)
"""

import gzip
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
import zlib

# Bump when the key algorithm changes; old entries orphan and age out.
# Mixed into both the L1 and L2 keys so a bump invalidates every lookup path.
_KEY_VERSION = b"v2\x00"

# Strip "# N " preprocessor line markers so the L2 hash is path-independent.
_LINE_MARKER_RE = re.compile(rb"^# \d+ ", re.MULTILINE)


def _strip_line_markers(data: bytes) -> bytes:
    return b"\n".join(
        line for line in data.splitlines() if not _LINE_MARKER_RE.match(line)
    )


# Captures the file path from a preprocessor line marker: # N "path" flags
_DEP_MARKER_RE = re.compile(rb'^# \d+ "([^"]*)"', re.MULTILINE)


def _extract_dep_paths(preprocessed: bytes) -> list:
    """Return sorted real file paths named in `castxml -E` line markers."""
    paths = set()
    for m in _DEP_MARKER_RE.finditer(preprocessed):
        p = m.group(1).decode("utf-8", "surrogateescape")
        # Line markers escape backslashes on Windows (C:\\path\\file.h)
        p = p.replace("\\\\", "\\")
        if p and not p.startswith("<"):  # skip <built-in>, <command line>
            paths.add(p)
    return sorted(paths)


def _hash_file(path: str) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 16), b""):
            h.update(chunk)
    return h.hexdigest()


def _deps_manifest(paths: list) -> list:
    """Return [[path, sha256], ...] for the preprocessed dependency set."""
    manifest = []
    for p in paths:
        try:
            manifest.append([p, _hash_file(p)])
        except OSError:
            manifest.append([p, ""])
    return manifest


def _deps_unchanged(deps: list) -> bool:
    """True when every recorded dependency still hashes to its stored value."""
    for dep in deps:
        if not (isinstance(dep, list) and len(dep) == 2):
            return False  # malformed manifest entry: force a miss
        path, expected = dep
        try:
            actual = _hash_file(path)
        except OSError:
            actual = ""  # matches _deps_manifest's record for unreadable paths
        if actual != expected:
            return False
    return True


def _incdirs_from_inc_file(inc_file: str) -> list:
    """Return the ordered, deduplicated -I directories from an @inc response file."""
    dirs = []
    seen = set()
    try:
        with open(inc_file, encoding="utf-8", errors="surrogateescape") as f:
            for line in f:
                token = line.strip().strip('"')
                if token.startswith("-I"):
                    d = token[2:].strip()
                    if d and d not in seen:
                        seen.add(d)
                        dirs.append(d)
    except OSError:
        pass
    return dirs


def _incdirs_fingerprint(inc_file: str) -> str:
    """Hash of the top-level entry names of every -I dir.

    A header appearing in (or vanishing from) any include dir changes the
    fingerprint, so a newly-added shadowing header forces an L1 miss and the
    `castxml -E` pass re-resolves includes against the real search path.
    """
    h = hashlib.sha256(_KEY_VERSION)
    for d in _incdirs_from_inc_file(inc_file):
        h.update(d.encode("utf-8", "surrogateescape"))
        h.update(b"\x00")
        try:
            names = sorted(os.listdir(d))
        except OSError:
            names = []
        for name in names:
            h.update(name.encode("utf-8", "surrogateescape"))
            h.update(b"\x00")
        h.update(b"\x01")
    return h.hexdigest()


def _cache_roots():
    """Return ordered list of cache root directories from ITK_WRAP_CACHE.

    The env var is a colon-separated list (like PATH).  Reads search all
    roots in order; writes go to the first root that accepts them.
    """
    raw = os.environ.get("ITK_WRAP_CACHE", "")
    if not raw:
        return [os.path.join(os.path.expanduser("~"), ".cache", "itk-wrap")]
    sep = ";" if sys.platform == "win32" else ":"
    return [p for p in raw.split(sep) if p]


def _verbose():
    return os.environ.get("ITK_WRAP_CACHE_VERBOSE", "") == "1"


def _log(msg):
    if _verbose():
        print(f"itk-castxml-cache: {msg}", file=sys.stderr)


def _use_uncompressed():
    """Return True when uncompressed storage is explicitly requested via ITK_WRAP_CACHE_FORMAT."""
    return os.environ.get("ITK_WRAP_CACHE_FORMAT", "").lower() in (
        "uncompressed",
        "raw",
        "plain",
    )


def _max_cache_gb():
    """Parse ITK_WRAP_CACHE_MAX_SIZE (default 2.0) into GB as a float."""
    try:
        return float(os.environ.get("ITK_WRAP_CACHE_MAX_SIZE", "2.0"))
    except ValueError:
        return 2.0


def _remove_if_stale(path, grace_cutoff):
    """Remove a dead dir (crashed staging / missing _ok) past the grace period."""
    try:
        if os.stat(path).st_mtime < grace_cutoff:
            shutil.rmtree(path, ignore_errors=True)
            return 1
    except OSError:
        pass
    return 0


def _evict(cache_root, max_size=2.0, days=13.9):
    """Evict cache entries; run once after a full wrapping build via --evict.

    Exits immediately when nothing was stored since the previous eviction.
    L2: remove entries whose _ok mtime is older than `days`, then remove the
    oldest by mtime until the total is under `max_size` GB.
    L1: remove mapping entries whose mtime is older than `days`.
    Staging leftovers and entries without _ok are removed after a one-hour
    grace period so crashed stores cannot grow the cache unbounded.
    """
    l2_root = os.path.join(cache_root, "l2")
    if not os.path.isdir(l2_root):
        return

    last_store = os.path.join(cache_root, "_last_store")
    last_evict = os.path.join(cache_root, "_last_evict")
    try:
        evict_mtime = os.stat(last_evict).st_mtime
    except OSError:
        evict_mtime = None
    if evict_mtime is not None:
        try:
            store_mtime = os.stat(last_store).st_mtime
        except OSError:
            store_mtime = 0.0
        if evict_mtime >= store_mtime:
            _log("evict: no stores since last eviction — skipping")
            return

    max_bytes = int(max_size * (1 << 30))
    cutoff = time.time() - days * 86400
    grace_cutoff = time.time() - 3600
    kept = []
    total = 0
    removed = 0

    for shard in os.listdir(l2_root):
        shard_dir = os.path.join(l2_root, shard)
        if not os.path.isdir(shard_dir):
            continue
        for key in os.listdir(shard_dir):
            entry = os.path.join(shard_dir, key)
            if ".tmp" in key or key.endswith(".old"):
                removed += _remove_if_stale(entry, grace_cutoff)
                continue
            ok = os.path.join(entry, "_ok")
            try:
                mtime = os.stat(ok).st_mtime
            except OSError:
                removed += _remove_if_stale(entry, grace_cutoff)
                continue
            try:
                entry_bytes = sum(
                    os.path.getsize(os.path.join(entry, fn)) for fn in os.listdir(entry)
                )
                if mtime < cutoff:
                    shutil.rmtree(entry)
                    removed += 1
                else:
                    kept.append((mtime, entry_bytes, entry))
                    total += entry_bytes
            except OSError:
                pass

    if total > max_bytes:
        kept.sort(key=lambda x: x[0])  # oldest first
        for _mtime, entry_bytes, entry in kept:
            if total <= max_bytes:
                break
            try:
                shutil.rmtree(entry)
                total -= entry_bytes
                removed += 1
            except OSError:
                pass

    l1_root = os.path.join(cache_root, "l1")
    if os.path.isdir(l1_root):
        for shard in os.listdir(l1_root):
            shard_dir = os.path.join(l1_root, shard)
            if not os.path.isdir(shard_dir):
                continue
            for key_name in os.listdir(shard_dir):
                key_dir = os.path.join(shard_dir, key_name)
                l1f = os.path.join(key_dir, "l2_key")
                try:
                    if os.stat(l1f).st_mtime < cutoff:
                        shutil.rmtree(key_dir)
                        removed += 1
                except OSError:
                    removed += _remove_if_stale(key_dir, grace_cutoff)

    try:
        with open(last_evict, "w"):
            pass
    except OSError:
        pass

    _log(
        f"evict: removed {removed} entries"
        f" (age>{days}d or size>{max_size:.1f}GB);"
        f" {total / (1 << 30):.2f} GB remaining"
    )


def _bypass_mode():
    """Return True when caching should be skipped entirely.

    Controlled by --no-cache as the first positional arg (before castxml binary)
    or by ITK_WRAP_CACHE_BYPASS=1 in the environment.  Use for single-use builds
    where the castxml -E overhead would cost more than the cache saves.
    """
    return os.environ.get("ITK_WRAP_CACHE_BYPASS", "") == "1"


def _parse_args(argv):
    """
    Parse a castxml command line into structured components.

    Strips a leading --no-cache flag (before the castxml binary) when present.
    Returns (castxml_bin, output_xml, inc_file, cxx_file, passthrough_flags, no_cache)
    where passthrough_flags preserves the original ordering for subprocess use.
    """
    no_cache = False
    if argv and argv[0] == "--no-cache":
        no_cache = True
        argv = argv[1:]

    if not argv:
        return None, None, None, None, [], no_cache

    castxml_bin = argv[0]
    output_xml = None
    inc_file = None
    cxx_file = None
    passthrough_flags = []

    i = 1
    while i < len(argv):
        arg = argv[i]
        if arg == "-o" and i + 1 < len(argv):
            output_xml = argv[i + 1]
            # Include in passthrough so castxml writes its output normally
            passthrough_flags.extend([arg, argv[i + 1]])
            i += 2
        elif arg.startswith("@"):
            inc_file = arg[1:]
            passthrough_flags.append(arg)
            i += 1
        elif arg.endswith(".cxx"):
            cxx_file = arg
            passthrough_flags.append(arg)
            i += 1
        else:
            passthrough_flags.append(arg)
            i += 1

    return castxml_bin, output_xml, inc_file, cxx_file, passthrough_flags, no_cache


def _castxml_content_hash(castxml_bin, primary_root):
    """Return a stable SHA-256 of the castxml binary, cached on disk.

    Sidecar file stores "size mtime_ns sha256" so re-hashing only happens when
    size or mtime changes.  After `ninja -t clean`, castxml is re-linked with
    the same content → same hash → stable L1 key → L1 hits on warm rebuilds.
    Sidecar lives in the first (writable) cache root.
    """
    try:
        st = os.stat(castxml_bin)
    except OSError:
        return "missing"

    # One sidecar per binary path (path key avoids slashes in filename)
    path_key = hashlib.sha256(castxml_bin.encode()).hexdigest()[:16]
    sidecar = os.path.join(primary_root, "_binhash", path_key)

    try:
        with open(sidecar) as f:
            parts = f.read().split()
            if (
                len(parts) == 3
                and int(parts[0]) == st.st_size
                and int(parts[1]) == st.st_mtime_ns
            ):
                return parts[2]
    except (OSError, ValueError):
        pass

    # Sidecar miss or stale — hash the binary (one-time cost per unique binary)
    h = hashlib.sha256()
    try:
        with open(castxml_bin, "rb") as f:
            for chunk in iter(lambda: f.read(1 << 20), b""):
                h.update(chunk)
    except OSError:
        return "unreadable"

    content_hash = h.hexdigest()
    try:
        os.makedirs(os.path.dirname(sidecar), exist_ok=True)
        # Unique staging name + os.replace: concurrency-safe and refreshable on Windows
        fd, tmp = tempfile.mkstemp(dir=os.path.dirname(sidecar), suffix=".tmp")
        try:
            with os.fdopen(fd, "w") as f:
                f.write(f"{st.st_size} {st.st_mtime_ns} {content_hash}\n")
            os.replace(tmp, sidecar)
        except OSError:
            os.unlink(tmp)
    except OSError:
        pass
    return content_hash


def _l1_key(bin_hash, inc_file, cxx_file, passthrough_flags):
    """Compute L1 cache key from direct inputs only (~0.2s, no subprocess)."""
    h = hashlib.sha256(_KEY_VERSION)

    # Stable content fingerprint — survives re-link with unchanged binary.
    h.update(f"castxml\x00{bin_hash}\x00".encode())

    # Response file: include dirs + defines passed via @file
    if inc_file:
        try:
            with open(inc_file, "rb") as f:
                h.update(f.read())
        except OSError:
            h.update(b"\x00inc_miss\x00")
    h.update(b"\x00")

    if cxx_file:
        try:
            with open(cxx_file, "rb") as f:
                h.update(f.read())
        except OSError:
            h.update(b"\x00cxx_miss\x00")
    h.update(b"\x00")

    # Remaining flags (--castxml-cc-gnu, compiler path, std flags, etc.)
    # Skip -o and the output xml path — irrelevant to content.
    skip_next = False
    for flag in passthrough_flags:
        if skip_next:
            skip_next = False
            continue
        if flag == "-o":
            skip_next = True
            continue
        if flag.endswith(".xml"):
            continue
        h.update(flag.encode())
        h.update(b"\x00")

    return h.hexdigest()


def _build_preprocess_cmd(castxml_bin, passthrough_flags, pre_output):
    """
    Build a `castxml -E` command that preprocesses the same inputs.

    Strips --castxml-output, --castxml-start (XML-generation flags),
    replaces -o with the temp preprocess output path, appends -E.
    """
    cmd = [castxml_bin]
    skip_next = False
    for arg in passthrough_flags:
        if skip_next:
            skip_next = False
            continue
        if arg.startswith("--castxml-output"):
            continue
        if arg.startswith("--castxml-start"):
            if "=" not in arg:
                skip_next = True
            continue
        if arg == "-o":
            skip_next = True  # drop -o and its value (xml output path)
            continue
        if arg.endswith(".xml"):
            continue
        cmd.append(arg)
    cmd.extend(["-E", "-o", pre_output])
    return cmd


def _compute_l2_key(castxml_bin, passthrough_flags, bin_hash):
    """Run castxml -E; return (l2_key, deps_manifest) or (None, None) on failure."""
    pre_path = None
    try:
        with tempfile.NamedTemporaryFile(suffix=".i", delete=False) as tmp:
            pre_path = tmp.name
        cmd = _build_preprocess_cmd(castxml_bin, passthrough_flags, pre_path)
        result = subprocess.run(cmd, capture_output=True)
        if result.returncode != 0:
            _log(f"castxml -E failed (exit {result.returncode})")
            return None, None
        with open(pre_path, "rb") as f:
            pre = f.read()
        h = hashlib.sha256(_KEY_VERSION)
        # Same preprocessed text from a different castxml must never collide.
        h.update(f"castxml\x00{bin_hash}\x00".encode())
        h.update(_strip_line_markers(pre))
        return h.hexdigest(), _deps_manifest(_extract_dep_paths(pre))
    except OSError:
        return None, None
    finally:
        if pre_path is not None:
            try:
                os.unlink(pre_path)
            except OSError:
                pass


def _l1_file(cache_root, l1_key):
    return os.path.join(cache_root, "l1", l1_key[:2], l1_key, "l2_key")


def _l2_dir(cache_root, l2_key):
    return os.path.join(cache_root, "l2", l2_key[:2], l2_key)


def _restore_xml(cache_root, l2_key, output_xml):
    """Restore cached XML to output_xml from one cache root.  Returns True on success."""
    entry = _l2_dir(cache_root, l2_key)
    ok_file = os.path.join(entry, "_ok")
    xml_plain = os.path.join(entry, "output.xml")
    xml_gz = os.path.join(entry, "output.xml.gz")

    if not os.path.isfile(ok_file):
        return False

    try:
        os.makedirs(os.path.dirname(os.path.abspath(output_xml)), exist_ok=True)
        if os.path.isfile(xml_plain):
            # copyfile (not copy2): a fresh mtime keeps ninja's edge up to date
            shutil.copyfile(xml_plain, output_xml)
        elif os.path.isfile(xml_gz):
            with gzip.open(xml_gz, "rb") as src, open(output_xml, "wb") as dst:
                shutil.copyfileobj(src, dst)
        else:
            return False
    except (OSError, zlib.error, EOFError):
        return False  # truncated/corrupt cache entry: miss, fall through to -E

    try:
        if os.path.getsize(output_xml) == 0:
            return False  # empty decompression: treat as corrupt
    except OSError:
        return False

    try:
        os.utime(ok_file, None)
    except OSError:
        pass

    return True


def _restore_from_caches(roots, l2_key, output_xml):
    """Search all cache roots for l2_key, restore on first hit."""
    for root in roots:
        if _restore_xml(root, l2_key, output_xml):
            return root  # return the root that had the hit
    return None


def _write_l1_mapping(l1f, l2_key, deps, incfp):
    """Atomically write the L1 manifest (L2 key, dependency hashes, incdir fingerprint)."""
    os.makedirs(os.path.dirname(l1f), exist_ok=True)
    # Unique staging name + os.replace: concurrency-safe and refreshable on Windows
    fd, tmp = tempfile.mkstemp(dir=os.path.dirname(l1f), suffix=".tmp")
    try:
        with os.fdopen(fd, "w") as f:
            json.dump({"l2_key": l2_key, "deps": deps, "incfp": incfp}, f)
        os.replace(tmp, l1f)
    except OSError:
        try:
            os.unlink(tmp)
        except OSError:
            pass
        raise


def _store(roots, l1_key, l2_key, output_xml, deps, incfp):
    """Store output_xml to L2 cache and write L1→L2 mapping atomically.

    Tries each root in order and writes to the first that accepts an atomic
    rename.  Read-only roots (e.g. a shared NFS lab cache) are silently
    skipped.
    """
    uncompressed = _use_uncompressed()

    # L2 entry — write to first writable root
    write_root = None
    for root in roots:
        entry = _l2_dir(root, l2_key)
        tmp = None
        try:
            if not os.path.isfile(output_xml):
                continue
            os.makedirs(os.path.dirname(entry), exist_ok=True)
            # Unique staging dir: concurrent stores of the same key cannot
            # delete or interleave into each other's staging area.
            tmp = tempfile.mkdtemp(dir=os.path.dirname(entry), prefix=l2_key + ".tmp")
            # mkdtemp creates 0700; restore umask-derived perms for shared caches
            umask = os.umask(0)
            os.umask(umask)
            os.chmod(tmp, 0o777 & ~umask)

            if uncompressed:
                shutil.copy2(output_xml, os.path.join(tmp, "output.xml"))
            else:
                with (
                    open(output_xml, "rb") as src,
                    gzip.open(
                        os.path.join(tmp, "output.xml.gz"), "wb", compresslevel=6
                    ) as dst,
                ):
                    shutil.copyfileobj(src, dst)

            with open(os.path.join(tmp, "_meta.json"), "w") as f:
                json.dump(
                    {
                        "l1_key": l1_key,
                        "l2_key": l2_key,
                        "format": "uncompressed" if uncompressed else "gzip",
                    },
                    f,
                )
            open(os.path.join(tmp, "_ok"), "w").close()  # noqa: WPS515

            old = entry + ".old"
            shutil.rmtree(
                old, ignore_errors=True
            )  # clear stale .old from a prior crash
            if os.path.exists(entry):
                os.rename(entry, old)
            try:
                os.rename(tmp, entry)
            except OSError:
                if os.path.exists(old):
                    os.rename(old, entry)
                raise
            shutil.rmtree(old, ignore_errors=True)
            write_root = root
            break
        except OSError as exc:
            _log(f"L2 store failed for {root}: {exc}")
            if tmp is not None:
                shutil.rmtree(tmp, ignore_errors=True)

    if write_root is None:
        _log("L2 store failed in all cache roots — no entry written")
        return

    _touch_store_marker(write_root)

    # L1→L2 mapping — write to same root that accepted the L2 entry
    try:
        _write_l1_mapping(_l1_file(write_root, l1_key), l2_key, deps, incfp)
    except OSError as exc:
        _log(f"L1 map store failed: {exc}")


def _touch_store_marker(root):
    """Record that this root received a write, so --evict knows to run."""
    try:
        with open(os.path.join(root, "_last_store"), "w"):
            pass
    except OSError:
        pass


def _store_l1_mapping(roots, l1_key, l2_key, deps, incfp):
    """Write an L1→L2 mapping to the first writable root (no L2 write)."""
    for root in roots:
        try:
            _write_l1_mapping(_l1_file(root, l1_key), l2_key, deps, incfp)
            _touch_store_marker(root)
            return
        except OSError:
            continue


def _run_castxml(castxml_bin, passthrough_flags):
    result = subprocess.run([castxml_bin] + passthrough_flags)
    return result.returncode


def main():
    argv = sys.argv[1:]
    if not argv:
        print(__doc__, file=sys.stderr)
        return 1

    # Stand-alone eviction subcommand: python3 itk-castxml-cache.py --evict DAYS
    if argv[0] == "--evict":
        import argparse

        p = argparse.ArgumentParser(prog="itk-castxml-cache.py --evict")
        p.add_argument(
            "days",
            type=float,
            help="Remove entries not used in this many days (e.g. 13.9)",
        )
        p.add_argument(
            "--max-size",
            type=float,
            default=None,
            help="Max cache size in GB after time eviction (default: ITK_WRAP_CACHE_MAX_SIZE or 2.0)",
        )
        p.add_argument(
            "--cache-dir", default=None, help="Cache root (overrides ITK_WRAP_CACHE)"
        )
        args = p.parse_args(argv[1:])
        max_size = args.max_size if args.max_size is not None else _max_cache_gb()
        roots = [args.cache_dir] if args.cache_dir else _cache_roots()
        _evict(roots[0], max_size=max_size, days=args.days)
        return 0

    castxml_bin, output_xml, inc_file, cxx_file, passthrough_flags, no_cache = (
        _parse_args(argv)
    )

    if castxml_bin is None:
        print("itk-castxml-cache: no castxml binary specified", file=sys.stderr)
        return 1
    if no_cache or _bypass_mode() or not output_xml or not cxx_file:
        return _run_castxml(castxml_bin, passthrough_flags)

    roots = _cache_roots()
    # primary_root is used for binary sidecar storage; writes go to first writable
    primary_root = roots[0]

    bin_hash = _castxml_content_hash(castxml_bin, primary_root)
    incfp = _incdirs_fingerprint(inc_file) if inc_file else ""

    # ── L1 check (fast, no subprocess) ──────────────────────────────────────
    l1_key = _l1_key(bin_hash, inc_file, cxx_file, passthrough_flags)

    stored = None
    stored_l1f = None
    for root in roots:
        l1f = _l1_file(root, l1_key)
        if os.path.isfile(l1f):
            try:
                with open(l1f) as f:
                    entry = json.load(f)
            except (OSError, ValueError):
                continue
            if isinstance(entry, dict) and entry.get("l2_key"):
                stored = entry
                stored_l1f = l1f
                break

    # ── L1 HIT: skip castxml -E only when recorded headers are unchanged and
    #    no header has appeared in (or left) any include dir since the store ──
    if (
        stored is not None
        and stored.get("deps")
        and stored.get("incfp") == incfp
        and _deps_unchanged(stored["deps"])
    ):
        hit_root = _restore_from_caches(roots, stored["l2_key"], output_xml)
        if hit_root is not None:
            try:
                os.utime(stored_l1f, None)  # mark used so age eviction keeps it
            except OSError:
                pass
            _log(f"HIT  {os.path.basename(cxx_file)} (l1→l2={stored['l2_key'][:8]})")
            return 0
        # L2 entry missing or corrupt despite L1 hit — fall through to -E check.
        _log(f"L2 entry missing for {cxx_file}, re-running castxml -E")

    # ── castxml -E to compute actual L2 key (L1 miss or header changed) ─────
    actual_l2_key, deps = _compute_l2_key(castxml_bin, passthrough_flags, bin_hash)

    if actual_l2_key is None:
        _log(f"preprocess failed for {cxx_file} — passing through")
        return _run_castxml(castxml_bin, passthrough_flags)

    # ── L2 store lookup (handles cross-dir hits: L1 miss, L2 populated) ─────
    hit_root = _restore_from_caches(roots, actual_l2_key, output_xml)
    if hit_root is not None:
        _log(f"HIT  {os.path.basename(cxx_file)} (l2={actual_l2_key[:8]})")
        # Populate L1 map so the next rebuild skips castxml -E
        _store_l1_mapping(roots, l1_key, actual_l2_key, deps, incfp)
        return 0

    # ── Full castxml run ─────────────────────────────────────────────────────
    _log(f"MISS {os.path.basename(cxx_file)}")
    try:
        os.unlink(output_xml)
    except OSError:
        pass
    rc = _run_castxml(castxml_bin, passthrough_flags)
    if rc == 0:
        _store(roots, l1_key, actual_l2_key, output_xml, deps, incfp)
    return rc


if __name__ == "__main__":
    sys.exit(main())
