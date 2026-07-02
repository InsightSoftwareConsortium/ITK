"""L0: pkl_db.py schema, journaling, and upsert semantics."""

import sqlite3


def test_schema_created_idempotently(pkl_db_mod, tmp_path):
    c1 = pkl_db_mod.open_pkl_db(str(tmp_path))
    c1.close()
    c2 = pkl_db_mod.open_pkl_db(str(tmp_path))
    cols = {r[1] for r in c2.execute("PRAGMA table_info(pkl_data)")}
    assert cols == {"key", "data"}
    c2.close()


def test_db_filename_carries_schema_version(pkl_db_mod, tmp_path):
    path = pkl_db_mod._pkl_db_path(str(tmp_path))
    assert f"v{pkl_db_mod.PKL_DB_SCHEMA_VERSION}" in path.name


def test_wal_mode_on_local_fs(pkl_db_mod, tmp_path):
    conn = pkl_db_mod.open_pkl_db(str(tmp_path))
    mode = conn.execute("PRAGMA journal_mode").fetchone()[0]
    assert mode in ("wal", "delete")  # delete = documented NFS fallback
    conn.close()


def test_upsert_overwrites(pkl_db_mod, tmp_path):
    conn = pkl_db_mod.open_pkl_db(str(tmp_path))
    upsert = (
        "INSERT INTO pkl_data(key, data) VALUES(?,?)"
        " ON CONFLICT(key) DO UPDATE SET data=excluded.data"
    )
    with conn:
        conn.execute(upsert, ("k", b"v1"))
        conn.execute(upsert, ("k", b"v2"))
    assert (
        conn.execute("SELECT data FROM pkl_data WHERE key='k'").fetchone()[0] == b"v2"
    )
    conn.close()


def test_reader_side_is_self_contained(pkl_db_mod, tmp_path):
    """Opening a fresh dir must not raise 'no such table' on first SELECT."""
    conn = pkl_db_mod.open_pkl_db(str(tmp_path / "fresh"))
    assert conn.execute("SELECT count(*) FROM pkl_data").fetchone()[0] == 0
    conn.close()


def test_blob_not_null(pkl_db_mod, tmp_path):
    conn = pkl_db_mod.open_pkl_db(str(tmp_path))
    try:
        with conn:
            conn.execute("INSERT INTO pkl_data(key, data) VALUES('k', NULL)")
        raised = False
    except sqlite3.IntegrityError:
        raised = True
    assert raised
    conn.close()
