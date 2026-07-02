"""L1: hit/miss/staleness/corruption/eviction scenarios with the fake castxml."""

import gzip
import os
import time


def test_cold_miss_stores_and_marks(fake):
    r = fake.invoke()
    assert r.returncode == 0, r.stderr
    assert fake.invocations() == ["PRE", "FULL"]
    assert fake.out_xml.stat().st_size > 0
    assert (fake.cache / "_last_store").exists()
    assert list(fake.cache.glob("l2/*/*/_ok"))
    assert list(fake.cache.glob("l1/*/*/l2_key"))


def test_warm_l1_hit_no_subprocess(fake):
    fake.invoke()
    first = fake.out_xml.read_bytes()
    fake.reset_log()
    fake.out_xml.unlink()
    r = fake.invoke()
    assert r.returncode == 0
    assert fake.invocations() == []
    assert fake.out_xml.read_bytes() == first


def test_restored_output_mtime_is_fresh(fake):
    fake.invoke()
    fake.cxx_file.touch()
    time.sleep(0.05)
    fake.out_xml.unlink()
    fake.invoke()
    assert fake.out_xml.stat().st_mtime >= fake.cxx_file.stat().st_mtime


def test_header_content_edit_misses(fake):
    fake.invoke()
    (fake.incdir_a / "foo.h").write_text("content-A v2\n")
    fake.reset_log()
    fake.invoke()
    assert fake.invocations() == ["PRE", "FULL"]
    assert b"content-A v2" in fake.out_xml.read_bytes()


def test_new_shadowing_header_forces_recheck(fake):
    fake.invoke()
    fake.reset_log()
    fake.invoke()
    assert fake.invocations() == []  # warmed
    (fake.incdir_a / "bar.h").write_text("shadow\n")  # earlier dir, same name
    fake.reset_log()
    fake.invoke()
    assert "PRE" in fake.invocations()  # incdir fingerprint miss -> -E re-check


def test_castxml_upgrade_serves_new_output(fake, monkeypatch):
    fake.invoke()
    with open(fake.bin, "a") as f:
        f.write("# upgraded\n")  # binary content change
    monkeypatch.setenv("FAKE_CASTXML_VERSION", "2")
    fake.reset_log()
    fake.out_xml.unlink()
    fake.invoke(extra_env={"FAKE_CASTXML_VERSION": "2"})
    assert "FULL" in fake.invocations()
    assert b"castxml-version='2'" in fake.out_xml.read_bytes()


def test_truncated_gzip_recovers(fake):
    fake.invoke()
    gz = next(fake.cache.glob("l2/*/*/output.xml.gz"))
    gz.write_bytes(gz.read_bytes()[:20])  # truncate -> EOFError territory
    fake.reset_log()
    fake.out_xml.unlink()
    r = fake.invoke()
    assert r.returncode == 0
    assert fake.out_xml.stat().st_size > 0
    assert "FULL" in fake.invocations()  # re-ran and re-stored


def test_empty_cached_xml_not_served(fake):
    fake.invoke()
    gz = next(fake.cache.glob("l2/*/*/output.xml.gz"))
    gz.write_bytes(gzip.compress(b""))
    fake.reset_log()
    fake.out_xml.unlink()
    r = fake.invoke()
    assert r.returncode == 0
    assert fake.out_xml.stat().st_size > 0


def test_bypass_modes_run_castxml_directly(fake, monkeypatch):
    r = fake.invoke(no_cache=True)
    assert r.returncode == 0
    assert fake.invocations() == ["FULL"]
    assert not list(fake.cache.glob("l2/*"))

    fake.reset_log()
    r = fake.invoke(extra_env={"ITK_WRAP_CACHE_BYPASS": "1"})
    assert r.returncode == 0
    assert fake.invocations() == ["FULL"]
    assert not list(fake.cache.glob("l2/*"))


def test_failed_castxml_stores_nothing(fake):
    r = fake.invoke(extra_env={"FAKE_FAIL": "1"})
    assert r.returncode != 0
    assert not list(fake.cache.glob("l2/*/*/_ok"))


def test_multi_root_reads_secondary_writes_primary(fake, monkeypatch, tmp_path):
    fake.invoke()  # populate what will become the secondary root
    secondary = fake.cache
    primary = tmp_path / "primary"
    primary.mkdir()
    sep = ";" if os.name == "nt" else ":"
    monkeypatch.setenv("ITK_WRAP_CACHE", f"{primary}{sep}{secondary}")
    fake.reset_log()
    fake.out_xml.unlink()
    r = fake.invoke()
    assert r.returncode == 0
    assert fake.out_xml.stat().st_size > 0
    assert "FULL" not in fake.invocations()  # served from secondary


def test_uncompressed_format_roundtrip(fake, monkeypatch):
    monkeypatch.setenv("ITK_WRAP_CACHE_FORMAT", "uncompressed")
    fake.invoke(extra_env={"ITK_WRAP_CACHE_FORMAT": "uncompressed"})
    entry = next(fake.cache.glob("l2/*/*/output.xml"))
    time.sleep(1.1)
    fake.reset_log()
    fake.out_xml.unlink()
    fake.invoke(extra_env={"ITK_WRAP_CACHE_FORMAT": "uncompressed"})
    assert fake.invocations() == []
    assert fake.out_xml.stat().st_mtime > entry.stat().st_mtime + 0.5


def test_evict_removes_dead_and_staging_dirs_after_grace(fake):
    fake.invoke()
    fake.evict()  # establish _last_evict
    dead = fake.cache / "l2" / "zz" / "deadentry"
    dead.mkdir(parents=True)
    (dead / "output.xml.gz").write_bytes(b"x")  # no _ok
    staging = fake.cache / "l2" / "zz" / "abc.tmpq1w2e3"
    staging.mkdir()
    past = time.time() - 7200
    os.utime(dead, (past, past))
    os.utime(staging, (past, past))
    (fake.cache / "_last_store").touch()  # signal stores -> eviction must run
    fake.evict()
    assert not dead.exists()
    assert not staging.exists()


def test_evict_early_exits_when_no_stores(fake):
    fake.invoke()
    fake.evict()
    past = time.time() - 7200
    os.utime(fake.cache / "_last_store", (past, past))
    before = sorted(p.name for p in fake.cache.rglob("*"))
    fake.evict()
    assert sorted(p.name for p in fake.cache.rglob("*")) == before


def test_evict_age_removes_old_entries(fake):
    fake.invoke()
    ok = next(fake.cache.glob("l2/*/*/_ok"))
    past = time.time() - 30 * 86400
    os.utime(ok, (past, past))
    (fake.cache / "_last_store").touch()
    fake.evict(days="13.9")
    assert not ok.exists()
