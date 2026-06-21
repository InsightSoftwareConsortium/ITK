#!/usr/bin/env python3
"""
Two-level content-addressed cache wrapper for ITK's CastXML wrapping step.

Invocation (replaces ccache + castxml in the cmake COMMAND):
    python3 itk-castxml-cache.py /path/to/castxml [castxml args...]
    python3 itk-castxml-cache.py --no-cache /path/to/castxml [castxml args...]
    python3 itk-castxml-cache.py --evict [--max-size 2G]

Cache key hierarchy:
  L1 (fast, no subprocess): sha256 of castxml content-hash + inc file + cxx file + flags.
      The castxml binary is fingerprinted by SHA-256 of its content (not mtime), so
      `ninja -t clean` re-links the binary without changing the L1 key.
  L2 (content-addressed): sha256 of normalised `castxml -E` preprocessed output.
      Preprocessor line markers (# N "path") are stripped before hashing,
      making L2 keys path-independent across build directories.

Lookup:
  L1 HIT  → stored L2 key → L2 entry exists → restore → DONE (no subprocess)
  L1 miss → run castxml -E → compute L2 key
              L2 HIT  → restore; refresh L1 map
              L2 miss → run full castxml; store; write L1 map

Storage formats (ITK_WRAP_CACHE_FORMAT):
  gzip (default): output.xml.gz, ~10x smaller, decompressed on restore.
  uncompressed:   output.xml, restored via hardlink when cache and build share
      a filesystem.  Multiple build directories (A/B/C/D testing) each get a
      hardlink to the same L2 inode — no per-build disk duplication.  Falls
      back to a plain copy on cross-device links or permission errors.

Multi-path cascade (ITK_WRAP_CACHE, colon-separated):
  Reads search all paths in order, returning the first hit.  Writes go to the
  first path that accepts them (atomic rename succeeds).  A read-only shared
  NFS cache can be listed after the user's writable local SSD cache, e.g.:
      export ITK_WRAP_CACHE=/local/ssd/cache:/nfs/lab/shared-cache
  Students benefit from the lab cache for L2 hits (saving the full castxml
  run) while writing L1 maps only to their own writable local cache.

Eviction: LRU via background fork after each write (rate-limited to once/60s).
  _ok sentinel mtime tracks "last useful" time; oldest entries evicted first.
  --evict only touches the first writable cache in the list.

Environment:
  ITK_WRAP_CACHE          colon-separated cache roots (default: ~/.cache/itk-wrap)
  ITK_WRAP_CACHE_FORMAT   storage format: gzip (default) or uncompressed
  ITK_WRAP_CACHE_VERBOSE  set to 1 for hit/miss logging to stderr
  ITK_WRAP_CACHE_BYPASS   set to 1 to bypass all caching (same as --no-cache)
  ITK_WRAP_CACHE_MAX_SIZE max cache size before LRU eviction (default: 2G)
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

# Bump when the key algorithm changes; old entries become unreachable orphans
# (different hash → different L2 path) and are pruned by LRU eviction.
_KEY_VERSION = b"v3\x00"

# Matches C preprocessor line markers: "# N "  (where N is an integer).
# These carry only source-file locations — not C++ semantics — so stripping
# them makes the L2 hash path-independent.
_LINE_MARKER_RE = re.compile(rb"^# \d+ ", re.MULTILINE)


def _strip_line_markers(data: bytes) -> bytes:
    return b"\n".join(
        line for line in data.splitlines() if not _LINE_MARKER_RE.match(line)
    )


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
    """Return True when uncompressed storage + hardlink restore is requested."""
    return os.environ.get("ITK_WRAP_CACHE_FORMAT", "").lower() in (
        "uncompressed",
        "raw",
        "plain",
    )


def _max_cache_bytes():
    """Parse ITK_WRAP_CACHE_MAX_SIZE (default 2G) into bytes."""
    raw = os.environ.get("ITK_WRAP_CACHE_MAX_SIZE", "2G").strip().upper()
    for suffix, mult in (
        ("T", 1 << 40),
        ("G", 1 << 30),
        ("M", 1 << 20),
        ("K", 1 << 10),
    ):
        if raw.endswith(suffix):
            try:
                return int(raw[:-1]) * mult
            except ValueError:
                pass
    try:
        return int(raw)
    except ValueError:
        return 2 << 30  # 2 GiB default


def _evict_lru(cache_root, max_bytes):
    """Remove least-recently-used L2 entries until total is under max_bytes.

    The _ok sentinel mtime is updated on every cache HIT (in _restore_xml),
    so it tracks "last time this entry was actually useful to a build."
    Entries whose _ok is oldest are evicted first.
    """
    l2_root = os.path.join(cache_root, "l2")
    if not os.path.isdir(l2_root):
        return

    entries = []
    total = 0
    for shard in os.listdir(l2_root):
        shard_dir = os.path.join(l2_root, shard)
        if not os.path.isdir(shard_dir):
            continue
        for key in os.listdir(shard_dir):
            entry = os.path.join(shard_dir, key)
            ok = os.path.join(entry, "_ok")
            try:
                mtime = os.stat(ok).st_mtime
                entry_bytes = sum(
                    os.path.getsize(os.path.join(entry, fn)) for fn in os.listdir(entry)
                )
                entries.append((mtime, entry_bytes, entry))
                total += entry_bytes
            except OSError:
                pass

    if total <= max_bytes:
        return

    _log(
        f"evict: {total / 1e9:.2f} GB used, limit {max_bytes / 1e9:.2f} GB"
        f" — evicting {len(entries)} candidates (oldest first)"
    )
    entries.sort(key=lambda x: x[0])  # ascending mtime → oldest first

    removed = 0
    for _mtime, entry_bytes, entry in entries:
        if total <= max_bytes:
            break
        try:
            shutil.rmtree(entry)
            total -= entry_bytes
            removed += 1
        except OSError:
            pass
    _log(f"evict: removed {removed} entries, {total / 1e9:.2f} GB remaining")


def _evict_async(cache_root):
    """Fork a background process to run LRU eviction without blocking the build.

    Rate-limited via a _evict_ts sentinel: won't re-check within 60 seconds.
    This prevents all 816 parallel build workers from checking simultaneously.
    """
    sentinel = os.path.join(cache_root, "_evict_ts")
    try:
        if time.time() - os.stat(sentinel).st_mtime < 60:
            return
    except OSError:
        pass
    try:
        open(sentinel, "w").close()  # noqa: WPS515
    except OSError:
        return
    if sys.platform == "win32":
        return
    pid = os.fork()
    if pid == 0:
        try:
            _evict_lru(cache_root, _max_cache_bytes())
        finally:
            os._exit(0)


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
        tmp = sidecar + ".tmp"
        with open(tmp, "w") as f:
            f.write(f"{st.st_size} {st.st_mtime_ns} {content_hash}\n")
        os.rename(tmp, sidecar)
    except OSError:
        pass
    return content_hash


def _l1_key(castxml_bin, inc_file, cxx_file, passthrough_flags, primary_root):
    """Compute L1 cache key from direct inputs only (~0.2s, no subprocess)."""
    h = hashlib.sha256()

    # Stable content fingerprint — survives re-link with unchanged binary.
    h.update(
        f"castxml\x00{_castxml_content_hash(castxml_bin, primary_root)}\x00".encode()
    )

    # Response file: include dirs + defines passed via @file
    if inc_file:
        try:
            with open(inc_file, "rb") as f:
                h.update(f.read())
        except OSError:
            h.update(b"\x00inc_miss\x00")
    h.update(b"\x00")

    # Input .cxx source
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


def _compute_l2_key(castxml_bin, passthrough_flags):
    """
    Run castxml -E and hash the normalised preprocessed output.

    L1 key is NOT mixed in: two build directories with identical source produce
    identical L2 keys and share cache entries regardless of castxml binary mtime.
    Returns the L2 key string, or None if preprocessing fails.
    """
    with tempfile.NamedTemporaryFile(suffix=".i", delete=False) as tmp:
        pre_path = tmp.name

    try:
        cmd = _build_preprocess_cmd(castxml_bin, passthrough_flags, pre_path)
        result = subprocess.run(cmd, capture_output=True)
        if result.returncode != 0:
            _log(f"castxml -E failed (exit {result.returncode})")
            return None
        h = hashlib.sha256(_KEY_VERSION)
        with open(pre_path, "rb") as f:
            h.update(_strip_line_markers(f.read()))
        return h.hexdigest()
    except OSError:
        return None
    finally:
        try:
            os.unlink(pre_path)
        except OSError:
            pass


def _l1_file(cache_root, l1_key):
    return os.path.join(cache_root, "l1", l1_key[:2], l1_key, "l2_key")


def _l2_dir(cache_root, l2_key):
    return os.path.join(cache_root, "l2", l2_key[:2], l2_key)


def _restore_xml(cache_root, l2_key, output_xml):
    """Restore cached XML to output_xml from one cache root.  Returns True on success.

    When ITK_WRAP_CACHE_FORMAT=uncompressed, tries os.link() first (zero-copy,
    zero-disk-duplication for A/B builds sharing a filesystem), falling back to
    a plain copy.  For gzip entries, decompresses as before.
    """
    entry = _l2_dir(cache_root, l2_key)
    ok_file = os.path.join(entry, "_ok")
    xml_plain = os.path.join(entry, "output.xml")
    xml_gz = os.path.join(entry, "output.xml.gz")

    if not os.path.isfile(ok_file):
        return False

    try:
        os.makedirs(os.path.dirname(os.path.abspath(output_xml)), exist_ok=True)

        if os.path.isfile(xml_plain):
            # Hardlink (zero-copy) when cache and build share a filesystem.
            try:
                try:
                    os.unlink(output_xml)
                except OSError:
                    pass
                os.link(xml_plain, output_xml)
            except OSError:
                shutil.copy2(xml_plain, output_xml)

        elif os.path.isfile(xml_gz):
            with gzip.open(xml_gz, "rb") as src, open(output_xml, "wb") as dst:
                shutil.copyfileobj(src, dst)

        else:
            return False

        # Touch _ok to record "last useful" time for LRU eviction.
        os.utime(ok_file, None)
        return True
    except OSError:
        return False


def _restore_from_caches(roots, l2_key, output_xml):
    """Search all cache roots for l2_key, restore on first hit."""
    for root in roots:
        if _restore_xml(root, l2_key, output_xml):
            return root  # return the root that had the hit
    return None


def _store(roots, l1_key, l2_key, output_xml):
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
        tmp = entry + ".tmp"
        try:
            if os.path.exists(tmp):
                shutil.rmtree(tmp)
            os.makedirs(tmp, exist_ok=True)

            if os.path.isfile(output_xml):
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

            if os.path.exists(entry):
                shutil.rmtree(entry)
            os.rename(tmp, entry)
            write_root = root
            break
        except OSError as exc:
            _log(f"L2 store failed for {root}: {exc}")
            shutil.rmtree(tmp, ignore_errors=True)

    if write_root is None:
        _log("L2 store failed in all cache roots — no entry written")
        return

    # L1→L2 mapping — write to same root that accepted the L2 entry
    l1f = _l1_file(write_root, l1_key)
    try:
        os.makedirs(os.path.dirname(l1f), exist_ok=True)
        with open(l1f + ".tmp", "w") as f:
            f.write(l2_key)
        os.rename(l1f + ".tmp", l1f)
    except OSError as exc:
        _log(f"L1 map store failed: {exc}")

    _evict_async(write_root)


def _store_l1_mapping(roots, l1_key, l2_key):
    """Write an L1→L2 mapping to the first writable root (no L2 write)."""
    for root in roots:
        l1f = _l1_file(root, l1_key)
        try:
            os.makedirs(os.path.dirname(l1f), exist_ok=True)
            with open(l1f + ".tmp", "w") as f:
                f.write(l2_key)
            os.rename(l1f + ".tmp", l1f)
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

    # Stand-alone eviction subcommand: python3 itk-castxml-cache.py --evict
    if argv[0] == "--evict":
        import argparse

        p = argparse.ArgumentParser(prog="itk-castxml-cache.py --evict")
        p.add_argument(
            "--max-size",
            default=None,
            help="Max cache size before eviction (e.g. 1G, 500M)",
        )
        p.add_argument(
            "--cache-dir", default=None, help="Cache root (overrides ITK_WRAP_CACHE)"
        )
        args = p.parse_args(argv[1:])
        roots = [args.cache_dir] if args.cache_dir else _cache_roots()
        if args.max_size:
            os.environ["ITK_WRAP_CACHE_MAX_SIZE"] = args.max_size
        _evict_lru(roots[0], _max_cache_bytes())
        return 0

    castxml_bin, output_xml, inc_file, cxx_file, passthrough_flags, no_cache = (
        _parse_args(argv)
    )

    if no_cache or _bypass_mode() or not output_xml or not cxx_file:
        return _run_castxml(castxml_bin, passthrough_flags)

    roots = _cache_roots()
    # primary_root is used for binary sidecar storage; writes go to first writable
    primary_root = roots[0]

    # ── L1 check (fast, no subprocess) ──────────────────────────────────────
    l1_key = _l1_key(castxml_bin, inc_file, cxx_file, passthrough_flags, primary_root)

    stored_l2_key = None
    for root in roots:
        l1f = _l1_file(root, l1_key)
        if os.path.isfile(l1f):
            try:
                with open(l1f) as f:
                    stored_l2_key = f.read().strip() or None
                if stored_l2_key:
                    break
            except OSError:
                pass

    # ── L1 HIT: skip castxml -E entirely ────────────────────────────────────
    if stored_l2_key is not None:
        hit_root = _restore_from_caches(roots, stored_l2_key, output_xml)
        if hit_root is not None:
            _log(f"HIT  {os.path.basename(cxx_file)} (l1→l2={stored_l2_key[:8]})")
            return 0
        # L2 entry missing or corrupt despite L1 hit — fall through to -E check.
        _log(f"L2 entry missing for {cxx_file}, re-running castxml -E")

    # ── castxml -E to compute actual L2 key (L1 miss or L2 corrupt) ─────────
    actual_l2_key = _compute_l2_key(castxml_bin, passthrough_flags)

    if actual_l2_key is None:
        _log(f"preprocess failed for {cxx_file} — passing through")
        return _run_castxml(castxml_bin, passthrough_flags)

    # ── L2 store lookup (handles cross-dir hits: L1 miss, L2 populated) ─────
    hit_root = _restore_from_caches(roots, actual_l2_key, output_xml)
    if hit_root is not None:
        _log(f"HIT  {os.path.basename(cxx_file)} (l2={actual_l2_key[:8]})")
        # Populate L1 map so the next rebuild skips castxml -E
        _store_l1_mapping(roots, l1_key, actual_l2_key)
        return 0

    # ── Full castxml run ─────────────────────────────────────────────────────
    _log(f"MISS {os.path.basename(cxx_file)}")
    # Unlink before write: severs any prior hardlink to the L2 store so
    # castxml's write does not corrupt a shared inode.
    try:
        os.unlink(output_xml)
    except OSError:
        pass
    rc = _run_castxml(castxml_bin, passthrough_flags)
    if rc == 0:
        _store(roots, l1_key, actual_l2_key, output_xml)
    return rc


if __name__ == "__main__":
    sys.exit(main())
