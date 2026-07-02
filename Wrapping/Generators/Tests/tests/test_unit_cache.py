"""L0: pure functions of itk-castxml-cache.py — no subprocess, no ITK build."""

import os
import sys

import pytest


def test_strip_line_markers(cache_mod):
    data = b'# 1 "/a/b.h"\nint x;\n# 22 "/c/d.h" 2\nint y;\n'
    assert cache_mod._strip_line_markers(data) == b"int x;\nint y;"


def test_extract_dep_paths_dedup_and_pseudo(cache_mod):
    pre = b'# 1 "/a/b.h"\n# 5 "/a/b.h"\n# 1 "<built-in>"\n# 1 "<command line>"\n'
    assert cache_mod._extract_dep_paths(pre) == ["/a/b.h"]


def test_extract_dep_paths_unescapes_windows_backslashes(cache_mod):
    pre = b'# 1 "C:\\\\src\\\\ITK\\\\itkImage.h"\n'
    assert cache_mod._extract_dep_paths(pre) == ["C:\\src\\ITK\\itkImage.h"]


def test_deps_manifest_and_unchanged_roundtrip(cache_mod, tmp_path):
    h = tmp_path / "a.h"
    h.write_text("v1")
    deps = cache_mod._deps_manifest([str(h)])
    assert cache_mod._deps_unchanged(deps)
    h.write_text("v2")
    assert not cache_mod._deps_unchanged(deps)


def test_deps_unchanged_deleted_header_misses(cache_mod, tmp_path):
    h = tmp_path / "a.h"
    h.write_text("v1")
    deps = cache_mod._deps_manifest([str(h)])
    h.unlink()
    assert not cache_mod._deps_unchanged(deps)


def test_deps_unreadable_at_store_and_check_is_consistent(cache_mod):
    deps = cache_mod._deps_manifest(["/nonexistent/path.h"])
    assert deps == [["/nonexistent/path.h", ""]]
    assert cache_mod._deps_unchanged(deps)


def test_deps_malformed_manifest_misses(cache_mod):
    assert not cache_mod._deps_unchanged([["path-without-hash"]])
    assert not cache_mod._deps_unchanged(["not-a-list"])


def test_parse_args(cache_mod):
    binp, out, inc, cxx, flags, no_cache = cache_mod._parse_args(
        ["/bin/castxml", "-o", "a.xml", "@inc.txt", "mod.cxx", "--flag"]
    )
    assert (binp, out, inc, cxx, no_cache) == (
        "/bin/castxml",
        "a.xml",
        "inc.txt",
        "mod.cxx",
        False,
    )
    assert flags == ["-o", "a.xml", "@inc.txt", "mod.cxx", "--flag"]


def test_parse_args_no_cache_flag(cache_mod):
    binp, _out, _inc, _cxx, _flags, no_cache = cache_mod._parse_args(
        ["--no-cache", "/bin/castxml", "mod.cxx"]
    )
    assert binp == "/bin/castxml" and no_cache


def test_cache_roots_split(cache_mod, monkeypatch):
    sep = ";" if sys.platform == "win32" else ":"
    monkeypatch.setenv("ITK_WRAP_CACHE", f"/one{sep}{sep}/two")
    assert cache_mod._cache_roots() == ["/one", "/two"]
    monkeypatch.delenv("ITK_WRAP_CACHE")
    assert len(cache_mod._cache_roots()) == 1


def test_l1_key_includes_key_version(cache_mod):
    k1 = cache_mod._l1_key("binhash", None, None, [])
    orig = cache_mod._KEY_VERSION
    try:
        cache_mod._KEY_VERSION = b"vTEST\x00"
        k2 = cache_mod._l1_key("binhash", None, None, [])
    finally:
        cache_mod._KEY_VERSION = orig
    assert k1 != k2


def test_l1_key_includes_bin_hash_and_flag_order(cache_mod):
    assert cache_mod._l1_key("h1", None, None, ["-a", "-b"]) != cache_mod._l1_key(
        "h2", None, None, ["-a", "-b"]
    )
    assert cache_mod._l1_key("h1", None, None, ["-a", "-b"]) != cache_mod._l1_key(
        "h1", None, None, ["-b", "-a"]
    )


def test_incdirs_from_inc_file(cache_mod, tmp_path):
    inc = tmp_path / "m.inc"
    inc.write_text('"-I/one"\n"-I/two"\n"-I/one"\n"-DFOO"\n')
    assert cache_mod._incdirs_from_inc_file(str(inc)) == ["/one", "/two"]


def test_incdirs_from_inc_file_isystem(cache_mod, tmp_path):
    inc = tmp_path / "m.inc"
    inc.write_text(
        '"-I/one"\n"-isystem" "/sys/a"\n"-isystem" "/sys/b"\n"-isystem/sys/c"\n"-isystem" "/sys/a"\n'
    )
    assert cache_mod._incdirs_from_inc_file(str(inc)) == [
        "/one",
        "/sys/a",
        "/sys/b",
        "/sys/c",
    ]


def test_incdirs_fingerprint_covers_isystem_dirs(cache_mod, tmp_path):
    d = tmp_path / "sysinc"
    d.mkdir()
    (d / "a.h").write_text("x")
    inc = tmp_path / "m.inc"
    inc.write_text(f'"-isystem" "{d}"\n')
    fp1 = cache_mod._incdirs_fingerprint(str(inc))
    (d / "b.h").write_text("shadow")
    assert cache_mod._incdirs_fingerprint(str(inc)) != fp1


def test_incdirs_fingerprint_changes_on_new_header(cache_mod, tmp_path):
    d = tmp_path / "inc"
    d.mkdir()
    (d / "a.h").write_text("x")
    inc = tmp_path / "m.inc"
    inc.write_text(f'"-I{d}"\n')
    fp1 = cache_mod._incdirs_fingerprint(str(inc))
    (d / "b.h").write_text("shadow")
    fp2 = cache_mod._incdirs_fingerprint(str(inc))
    assert fp1 != fp2
    (d / "b.h").unlink()
    assert cache_mod._incdirs_fingerprint(str(inc)) == fp1


def test_incdirs_fingerprint_ignores_content_edits(cache_mod, tmp_path):
    d = tmp_path / "inc"
    d.mkdir()
    (d / "a.h").write_text("x")
    inc = tmp_path / "m.inc"
    inc.write_text(f'"-I{d}"\n')
    fp1 = cache_mod._incdirs_fingerprint(str(inc))
    (d / "a.h").write_text("y")  # content change is the deps manifest's job
    assert cache_mod._incdirs_fingerprint(str(inc)) == fp1


def test_max_cache_gb_bad_value_defaults(cache_mod, monkeypatch):
    monkeypatch.setenv("ITK_WRAP_CACHE_MAX_SIZE", "not-a-number")
    assert cache_mod._max_cache_gb() == 2.0


def test_docstring_documents_no_phantom_env_vars(cache_mod):
    assert "ITK_WRAP_CACHE_MAX_DAYS" not in cache_mod.__doc__
