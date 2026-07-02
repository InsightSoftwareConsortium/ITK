"""L4-sim: tar round-trip to a different absolute path — models what
actions/cache and Azure Cache@2 do to the cache directory between CI runs."""

import shutil


def test_cache_relocation_preserves_hits(fake, monkeypatch, tmp_path):
    fake.invoke()
    assert fake.invocations() == ["PRE", "FULL"]

    relocated = tmp_path / "relocated" / "deeper" / "cache"
    relocated.parent.mkdir(parents=True)
    shutil.make_archive(str(tmp_path / "ci-cache"), "tar", fake.cache)
    shutil.unpack_archive(str(tmp_path / "ci-cache.tar"), relocated)

    monkeypatch.setenv("ITK_WRAP_CACHE", str(relocated))
    fake.reset_log()
    fake.out_xml.unlink()
    r = fake.invoke(extra_env={"ITK_WRAP_CACHE": str(relocated)})
    assert r.returncode == 0
    assert fake.invocations() == []  # pure L1 hit from the relocated cache
    assert fake.out_xml.stat().st_size > 0


def test_cross_branch_restore_selectively_misses(fake, monkeypatch, tmp_path):
    """Restore-keys prefix fallback across branches is safe by construction:
    unchanged modules hit, the changed module misses."""
    fake.invoke()  # "branch A" populates the cache

    # "branch B": one header differs
    (fake.incdir_a / "foo.h").write_text("branch-B content\n")
    fake.reset_log()
    fake.invoke()
    assert fake.invocations() == ["PRE", "FULL"]  # changed module misses

    # reverting to branch-A content: the L1 map now records branch-B deps, so
    # the -E pass re-runs once and lands an L2 hit on the branch-A entry
    (fake.incdir_a / "foo.h").write_text("content-A v1\n")
    fake.reset_log()
    fake.out_xml.unlink()
    fake.invoke()
    assert fake.invocations() == ["PRE"]  # no FULL: branch-A XML still cached
