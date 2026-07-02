"""L1: pyi_generator prune gating, self-heal, and guard behavior against a
synthetic pkl DB (no ITK build required)."""

import pickle
import subprocess
import sys

import pytest

from conftest import PYI_GENERATOR, _load

UPSERT = (
    "INSERT INTO pkl_data(key, data) VALUES(?,?)"
    " ON CONFLICT(key) DO UPDATE SET data=excluded.data"
)


@pytest.fixture
def pkl_tree(pkl_db_mod, tmp_path):
    """A pkl dir with 6 synthetic classes, index manifests, and stamp files."""
    gen = _load("pyi_generator_for_pickling", PYI_GENERATOR)
    cls = gen.ITKClass
    # igenerator pickles ITKClass from a script run as __main__; reproduce that
    cls.__module__ = "__main__"
    setattr(sys.modules["__main__"], "ITKClass", cls)

    pkl_dir = tmp_path / "itk-pkl"
    pyi_dir = tmp_path / "pyi"
    pkl_dir.mkdir()
    pyi_dir.mkdir()
    conn = pkl_db_mod.open_pkl_db(str(pkl_dir))
    keys = []
    for i in range(6):
        name = f"FakeClass{i}"
        key = f"{name}.itk{name}"
        obj = cls(name)
        with conn:
            conn.execute(UPSERT, (key, pickle.dumps(obj)))
        (pkl_dir / f"itk{name}.index.txt").write_text(key + "\n")
        keys.append(key)
    conn.close()
    (pkl_dir / "ITKFake.stamp").write_text("")
    (pkl_dir / "ITKFake2.stamp").write_text("")

    # CMake writes GlobalIdxFilesList.txt with forward-slash paths on every
    # platform; pyi_generator normalizes its glob results to match.
    index_files = sorted(p.as_posix() for p in pkl_dir.glob("*.index.txt"))
    full_list = tmp_path / "full_list.txt"
    full_list.write_text(";".join(index_files))
    partial_list = tmp_path / "partial_list.txt"
    partial_list.write_text(index_files[0])
    return pkl_dir, pyi_dir, full_list, partial_list, keys


def run_pyi(pkl_dir, pyi_dir, list_file, *extra):
    return subprocess.run(
        [
            sys.executable,
            str(PYI_GENERATOR),
            "--pyi_dir",
            str(pyi_dir),
            "--pkl_dir",
            str(pkl_dir),
            "--index_list_file",
            str(list_file),
            *extra,
        ],
        capture_output=True,
        text=True,
    )


def rowcount(pkl_db_mod, pkl_dir):
    import sqlite3

    conn = sqlite3.connect(pkl_db_mod._pkl_db_path(str(pkl_dir)))
    n = conn.execute("SELECT count(*) FROM pkl_data").fetchone()[0]
    conn.close()
    return n


def test_healthy_run_generates_stubs(pkl_db_mod, pkl_tree):
    pkl_dir, pyi_dir, full_list, _partial, _keys = pkl_tree
    r = run_pyi(pkl_dir, pyi_dir, full_list, "--prune")
    assert r.returncode == 0, r.stderr
    assert (pyi_dir / "__init__.pyi").exists()
    assert rowcount(pkl_db_mod, pkl_dir) == 6


def test_partial_manifest_without_prune_preserves_rows(pkl_db_mod, pkl_tree):
    """External-wrap-project simulation: shared DB must survive."""
    pkl_dir, pyi_dir, _full, partial_list, _keys = pkl_tree
    r = run_pyi(pkl_dir, pyi_dir, partial_list)
    assert r.returncode == 0, r.stderr
    assert rowcount(pkl_db_mod, pkl_dir) == 6


def test_partial_manifest_with_prune_deletes(pkl_db_mod, pkl_tree):
    pkl_dir, pyi_dir, _full, partial_list, _keys = pkl_tree
    r = run_pyi(pkl_dir, pyi_dir, partial_list, "--prune")
    assert r.returncode == 0, r.stderr
    assert rowcount(pkl_db_mod, pkl_dir) == 1


def test_prune_skipped_when_manifest_missing(pkl_db_mod, pkl_tree, tmp_path):
    pkl_dir, pyi_dir, full_list, _partial, _keys = pkl_tree
    with_missing = tmp_path / "with_missing.txt"
    with_missing.write_text(
        full_list.read_text() + ";" + (pkl_dir / "DOES_NOT_EXIST.index.txt").as_posix()
    )
    r = run_pyi(pkl_dir, pyi_dir, with_missing, "--prune")
    assert r.returncode == 0, r.stderr
    assert "skipping pkl DB pruning" in r.stdout
    assert rowcount(pkl_db_mod, pkl_dir) == 6


def test_self_heal_on_damaged_db(pkl_db_mod, pkl_tree):
    """Deleted DB rows with fresh stamps -> stamps removed + actionable error."""
    import sqlite3

    pkl_dir, pyi_dir, full_list, _partial, keys = pkl_tree
    conn = sqlite3.connect(pkl_db_mod._pkl_db_path(str(pkl_dir)))
    with conn:
        conn.execute("DELETE FROM pkl_data WHERE key IN (?,?)", keys[:2])
    conn.close()
    assert list(pkl_dir.glob("*.stamp"))
    r = run_pyi(pkl_dir, pyi_dir, full_list, "--prune")
    assert r.returncode != 0
    assert "missing from the pkl database" in r.stderr
    assert not list(pkl_dir.glob("*.stamp"))


def test_bogus_pkl_dir_rejected(pkl_tree):
    _pkl, pyi_dir, full_list, _partial, _keys = pkl_tree
    r = run_pyi("/nonexistent/pkl-dir", pyi_dir, full_list)
    assert r.returncode != 0
    assert "Invalid directory provided" in r.stderr
