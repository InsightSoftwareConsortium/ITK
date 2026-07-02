"""L2: multiprocess stress — parallel pkl writers and racing castxml stores."""

import multiprocessing
import sqlite3
import subprocess
import sys

from conftest import PKL_DB

UPSERT = (
    "INSERT INTO pkl_data(key, data) VALUES(?,?)"
    " ON CONFLICT(key) DO UPDATE SET data=excluded.data"
)

WRITER = f"""
import sys
sys.path.insert(0, {str(PKL_DB.parent)!r})
from pkl_db import open_pkl_db
wid, pkl_dir = int(sys.argv[1]), sys.argv[2]
conn = open_pkl_db(pkl_dir)
with conn:
    for i in range(50):
        conn.execute({UPSERT!r}, (f"shared-{{i}}", b"w%d" % wid))
        conn.execute({UPSERT!r}, (f"w{{wid}}-{{i}}", b"x"))
conn.close()
"""


def test_parallel_writers_no_lock_errors(pkl_db_mod, tmp_path):
    n = 32
    procs = [
        subprocess.Popen(
            [sys.executable, "-c", WRITER, str(i), str(tmp_path)],
            stderr=subprocess.PIPE,
        )
        for i in range(n)
    ]
    errs = [p.communicate()[1] for p in procs]
    assert all(p.returncode == 0 for p in procs), b"\n".join(errs)[:2000]
    conn = sqlite3.connect(pkl_db_mod._pkl_db_path(str(tmp_path)))
    count = conn.execute("SELECT count(*) FROM pkl_data").fetchone()[0]
    assert count == 50 + n * 50  # 50 shared + 50 per writer
    conn.close()


def test_keyset_delete_against_concurrent_upserts(pkl_db_mod, tmp_path):
    conn = pkl_db_mod.open_pkl_db(str(tmp_path))
    with conn:
        for i in range(200):
            conn.execute(UPSERT, (f"live-{i}", b"x"))
        for i in range(200):
            conn.execute(UPSERT, (f"stale-{i}", b"x"))
    with conn:
        conn.execute("CREATE TEMP TABLE _live_keys (key TEXT PRIMARY KEY)")
        conn.executemany(
            "INSERT OR IGNORE INTO _live_keys VALUES(?)",
            [(f"live-{i}",) for i in range(200)],
        )
        conn.execute(
            "DELETE FROM pkl_data WHERE key NOT IN (SELECT key FROM _live_keys)"
        )
        conn.execute("DROP TABLE _live_keys")
    keys = {r[0] for r in conn.execute("SELECT key FROM pkl_data")}
    assert keys == {f"live-{i}" for i in range(200)}
    conn.close()


def _invoke_worker(args):
    cache_script, castxml_bin, inc, cxx, out, env_pairs = args
    import os
    import subprocess as sp

    env = dict(os.environ)
    env.update(env_pairs)
    r = sp.run(
        [
            sys.executable,
            cache_script,
            castxml_bin,
            "--castxml-cc-gnu",
            "g++",
            f"@{inc}",
            "-o",
            out,
            cxx,
        ],
        env=env,
        capture_output=True,
    )
    return r.returncode


def test_racing_stores_same_key_leave_valid_entry(fake, tmp_path):
    from conftest import CACHE_SCRIPT

    outs = [str(fake.build / f"out{i}.xml") for i in range(8)]
    env_pairs = {
        "ITK_WRAP_CACHE": str(fake.cache),
        "FAKE_LOG": str(fake.log),
        "FAKE_HDRS": str(fake.incdir_a / "foo.h"),
    }
    args = [
        (
            str(CACHE_SCRIPT),
            str(fake.bin),
            str(fake.inc_file),
            str(fake.cxx_file),
            o,
            env_pairs,
        )
        for o in outs
    ]
    with multiprocessing.Pool(8) as pool:
        rcs = pool.map(_invoke_worker, args)
    assert all(rc == 0 for rc in rcs)
    oks = list(fake.cache.glob("l2/*/*/_ok"))
    assert len(oks) == 1
    # the surviving entry must serve a valid warm hit
    fake.reset_log()
    fake.out_xml.unlink(missing_ok=True)
    import os

    for k, v in env_pairs.items():
        os.environ[k] = v
    r = fake.invoke(extra_env=env_pairs)
    assert r.returncode == 0
    assert fake.out_xml.stat().st_size > 0
    assert "FULL" not in fake.invocations()
