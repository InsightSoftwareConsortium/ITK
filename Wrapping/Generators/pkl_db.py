import sqlite3
from pathlib import Path

PKL_DB_SCHEMA_VERSION = 3


def _pkl_db_path(pkl_dir: str) -> Path:
    """Return the build-tree-local pkl SQLite DB path.

    The DB is intermediate handoff state keyed by bare class name, so it must
    stay local to one build tree; it is never shared via ITK_WRAP_CACHE.
    """
    return Path(pkl_dir) / f"itk-pkl-v{PKL_DB_SCHEMA_VERSION}.db"


def open_pkl_db(pkl_dir: str) -> sqlite3.Connection:
    """Open (creating if absent) the build-tree-local pkl DB."""
    db_path = _pkl_db_path(pkl_dir)
    db_path.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(db_path, timeout=30)
    try:
        # WAL needs shared memory; on filesystems without it (e.g. NFS) fall
        # back to the default rollback journal, serialized by the busy timeout.
        conn.execute("PRAGMA journal_mode=WAL")
    except sqlite3.OperationalError:
        pass
    conn.execute(
        "CREATE TABLE IF NOT EXISTS pkl_data (key TEXT PRIMARY KEY, data BLOB NOT NULL)"
    )
    conn.commit()
    return conn
