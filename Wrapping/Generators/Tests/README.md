# Wrapping infrastructure tests

Validation suite for the CastXML two-level cache
(`../CastXML/itk-castxml-cache.py`), the SQLite pkl database
(`../pkl_db.py`), and the pyi stub pipeline
(`../Python/itk/pyi_generator.py`).

Registered as the `WrappingInfrastructure` CTest when wrapping and
`BUILD_TESTING` are enabled and `pytest` is available in the build
interpreter (skipped otherwise).  Also runs standalone with only
Python >= 3.11 and `pytest` — no compiler, CMake, or ITK build:

```bash
python -m pytest Wrapping/Generators/Tests   # from the ITK source root
ctest -R WrappingInfrastructure              # from a configured build tree
```

Runs in ~7 s.  `ITK_SOURCE_DIR` points the suite at a different checkout
(default: the enclosing tree).  The fake castxml (`fake_castxml.py`) logs
every invocation so tests assert exact subprocess counts: cold = 2
(`-E` + full), L2-hit = 1, L1-hit = 0.

| File | Covers |
|---|---|
| `tests/test_unit_cache.py` | key/manifest/parse functions, Windows path unescaping, include-dir fingerprints |
| `tests/test_unit_pkl_db.py` | schema, WAL (+ NFS fallback), upsert, reader self-containment |
| `tests/test_integration.py` | cold/warm/staleness/corruption/eviction/bypass/multi-root |
| `tests/test_pyi_generator.py` | `--prune` gating (external-project safety), DB self-heal, guards |
| `tests/test_concurrency.py` | 32 parallel pkl writers, keyset-DELETE, racing same-key stores |
| `tests/test_relocate.py` | cache relocation (models `actions/cache` / Azure `Cache@2` transport) |
| `smoke_realbuild.sh` | optional real-castxml double build (Linux, needs a configured tree) |

A cross-platform mirror with a {Linux, macOS, Windows} × {3.11, 3.13}
GitHub Actions matrix lives at
<https://github.com/hjmjohnson/itk-wrap-cache-tests>.
