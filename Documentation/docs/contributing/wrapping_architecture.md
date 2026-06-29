Python Wrapping Architecture
============================

ITK's Python wrapping pipeline converts C++ template declarations into
importable Python extension modules (`.abi3.so`) and type-stub files
(`.pyi`). The pipeline runs in two distinct phases: a **configure phase**
driven by CMake and a **build phase** driven by Ninja.

## Configure phase: `.wrap` → `castxml_inputs/`

Each ITK module that supports wrapping contains a `wrapping/` subdirectory
with one `.wrap` file per submodule. A `.wrap` file is a CMake script that
calls macros such as `itk_wrap_class()` and `itk_wrap_template()` to declare
which C++ template instantiations should be exposed to Python.

CMake processes every `.wrap` file at configure time and writes three
files per submodule into `<build>/Wrapping/castxml_inputs/`:

| Generated file | How | Contents |
|---|---|---|
| `<submodule>.cxx` | `configure_file` | `#include` directives + `_wrapping_` namespace with `using` aliases for every requested template instantiation |
| `<submodule>.castxml.inc` | `file(GENERATE …)` | Compiler `-I` and `-D` flags needed to parse the `.cxx` file |
| `<submodule>SwigInterface.h.in` | `configure_file` | `#include` list used by SWIG |

CMake also registers one `add_custom_command` per submodule (for CastXML)
and one per ITK module (for `igenerator.py`). No compilation happens at
configure time; only the input files and build rules are written.

## Build phase: CastXML → igenerator → SWIG → compile → link

### Step 1 — CastXML (816 independent jobs)

Each submodule produces exactly one XML file:

```
castxml_inputs/<submodule>.cxx
castxml_inputs/<submodule>.castxml.inc     ──▶  itk-castxml-cache.py  ──▶  castxml_inputs/<submodule>.xml
Modules/.../include/<Class>.h (many)
```

`itk-castxml-cache.py` wraps the CastXML binary with a two-level
content-addressed cache (`~/.cache/itk-wrap` or `$ITK_WRAP_CACHE`):

- **L1** — hash of the `.cxx` file content → L2 key
- **L2** — `castxml -E` (preprocessor only) output hash → cached `output.xml.gz`

A cache hit avoids running CastXML entirely. All 816 CastXML jobs are
independent and run fully in parallel. No CastXML job reads or modifies
another submodule's `.xml` output.

### Step 2 — `igenerator.py` (96 per-module jobs)

Each ITK module (e.g. `ITKImageIntensity`) batches all of its submodules
into a single `igenerator.py` invocation:

```
castxml_inputs/itkAbsImageFilter.xml ──┐
castxml_inputs/itkImage.xml          ──┤
castxml_inputs/itkOffset.xml         ──┤  igenerator.py [ITKImageIntensity]
... (all N submodule XMLs)           ──┘  --submodule-order "sub1;sub2;...;subN"
                                               │
                    ┌──────────────────────────┼─────────────────────────────┐
                    │  per submodule (×N)       │                             │
                    ▼                           ▼                             ▼
         Typedefs/<sub>.i            itk-pkl/<sub>.index.txt      itk-pkl-v1.db  (SQLite)
         Typedefs/<sub>.idx          (lists DB keys; byproduct)   (class metadata; WAL mode)
         Typedefs/<sub>SwigInterface.h
                                          + per module (×1):
                                            itk-pkl/<Module>.stamp
                                            itk/Configuration/<Module>_snake_case.py
```

`igenerator.py` uses `pygccxml` to parse each `.xml` file and emit SWIG
interface (`.i`) and index (`.idx`) files, class-metadata rows in a shared
SQLite database consumed later by `pyi_generator.py`, and a `.index.txt`
manifest listing the DB keys for each submodule.

The SQLite database (`itk-pkl-v1.db`) is written to the `itk-pkl/` directory
inside the build tree by default.  Set `ITK_WRAP_CACHE` to redirect it to a
shared location (e.g. a CI cache volume).

**Ninja scheduling**: an `igenerator.py` job for module A starts as soon
as all of A's CastXML jobs are complete, even while CastXML is still
running for module B. There is no global barrier between the CastXML and
`igenerator.py` layers.

### Step 3 — SWIG, compile, link (per submodule)

```
Typedefs/<sub>.i
Typedefs/<sub>SwigInterface.h  ──▶  swig  ──▶  Modules/.../<sub>Python.cpp
                                                Generators/Python/itk/<sub>Python.py

Modules/.../<sub>Python.cpp  ──▶  ccache + g++  ──▶  .o  ──▶  link  ──▶  _<Module>Python.abi3.so
```

### Step 4 — `pyi_generator.py` (one global job)

After **all** 816 `.index.txt` files exist, `pyi_generator.py` reads every
`.index.txt`, queries the SQLite database for each key, and writes the
type-stub files used by IDEs:

```
itk-pkl/<sub>.index.txt (×816) ──▶  pyi_generator.py  ──▶  _proxies.pyi
itk-pkl-v1.db (SQLite)               (queries DB via keys in .index.txt)  __init__.pyi
```

## Key file reference

| Path pattern | Written by | Read by |
|---|---|---|
| `Wrapping/castxml_inputs/<sub>.cxx` | CMake `configure_file` | CastXML |
| `Wrapping/castxml_inputs/<sub>.castxml.inc` | CMake `file(GENERATE)` | `itk-castxml-cache.py` |
| `Wrapping/castxml_inputs/<sub>.xml` | `itk-castxml-cache.py` / CastXML | `igenerator.py` |
| `Wrapping/Typedefs/<sub>.i` | `igenerator.py` | SWIG |
| `Wrapping/Typedefs/<sub>.idx` | `igenerator.py` | SWIG |
| `Wrapping/Generators/Python/itk-pkl/<sub>.index.txt` | `igenerator.py` | `pyi_generator.py` |
| `Wrapping/Generators/Python/itk-pkl/itk-pkl-v1.db` | `igenerator.py` | `pyi_generator.py` |
| `Wrapping/Generators/Python/itk-pkl/<Module>.stamp` | `igenerator.py` | ninja (tracks DB write completeness) |
| `Wrapping/Generators/Python/itk/_<Module>Python.abi3.so` | linker | Python `import itk` |
| `Wrapping/Generators/Python/itk/_proxies.pyi` | `pyi_generator.py` | IDEs |

## Caches

### CastXML cache (`itk-castxml-cache.py`)

Controls via environment:

| Variable | Default | Purpose |
|---|---|---|
| `ITK_WRAP_CACHE` | `~/.cache/itk-wrap` | Cache root for CastXML `.xml.gz` files |
| `ITK_WRAP_CACHE_VERBOSE` | unset | Set to `1` to log HIT/MISS per file |

The CastXML cache is content-addressed and generator-version-stamped
(`_KEY_VERSION` in `itk-castxml-cache.py`). It is shared across build
directories; a fresh configure reuses XML from a previous build on the same
machine.

### pkl SQLite database (`igenerator.py` / `pyi_generator.py`)

| Variable | Default | Purpose |
|---|---|---|
| `ITK_WRAP_CACHE` | *(build tree `itk-pkl/`)* | Redirect pkl DB to a shared location |

The pkl database (`itk-pkl-v1.db`) defaults to the build tree's `itk-pkl/`
directory and is a build artifact, not a user-level cache.  Developers who
want to share it across build trees (or populate it from CI) must opt in by
setting `ITK_WRAP_CACHE` explicitly:

```bash
export ITK_WRAP_CACHE=~/.cache/itk-wrap        # personal shared cache
export ITK_WRAP_CACHE=$(dirname "$CCACHE_DIR")  # CI: same root as ccache
```

Both the CastXML cache and the pkl database use the same `ITK_WRAP_CACHE`
root, so a single variable covers both.

### ccache

CastXML re-runs produce identical `.xml` files (the content is deterministic)
but are slow. The CastXML cache eliminates that cost. For the C++ compilation
steps (Step 3), ccache caches compiled `.o` files keyed on source content.
Both caches are independent and complement each other.

### Build-phase timing on 2-core CI runners

On a 2-core CI runner (cold caches throughout):

| Phase | Approx. time | Notes |
|---|---|---|
| CastXML (816 jobs, 2 cores) | ~32 min | Eliminated on warm-cache runs |
| igenerator + SWIG + C++ compile | ~225 min | Reduced by ccache on subsequent runs |
| Tests | ~44 min | |

CastXML is ~10 % of the cold-cache total.  The C++ compilation phase
dominates; `ccache` is the primary lever there.

## Ninja dependency graph summary

```
.wrap files (configure time, not in graph)
    │ cmake configure_file / file(GENERATE)
    ▼
castxml_inputs/<sub>.cxx + .castxml.inc + .h headers
    │ itk-castxml-cache.py  [816 parallel jobs]
    ▼
castxml_inputs/<sub>.xml          (write-once; never mutated after creation)
    │ igenerator.py  [96 per-module jobs; starts per-module, not globally gated]
    ▼
Typedefs/<sub>.i + .idx + SwigInterface.h
itk-pkl/<sub>.index.txt (DB keys) + itk-pkl-v1.db (SQLite) + <Module>.stamp
    │ swig + ccache compile + link  [parallel per submodule]
    ▼
_<Module>Python.abi3.so
    │ pyi_generator.py  [1 global job; needs all .index.txt]
    ▼
_proxies.pyi + __init__.pyi
```

## Troubleshooting

**`No pkl keys were found in index files in itk-pkl`**
: The `.index.txt` manifests exist (so ninja considers `igenerator.py`
  up-to-date) but the pkl database is absent or stale. Delete the stamp
  files to force `igenerator.py` to re-run and repopulate the DB:
  ```bash
  find <build>/Wrapping/Generators/Python/itk-pkl -name "*.index.txt" -o -name "*.stamp" | xargs rm -f
  ninja -C <build>
  ```

**CastXML cache not being used**
: Set `ITK_WRAP_CACHE_VERBOSE=1` and rebuild one module to confirm HIT or
  MISS log lines. Ensure `ITK_WRAP_CASTXML_CACHE=ON` is set in CMake.
  A version bump to `_KEY_VERSION` in `itk-castxml-cache.py` forces a cold
  cache for all entries.

**Adding a new wrapped class**
: Add `itk_wrap_class()` / `itk_wrap_template()` calls to the relevant
  `.wrap` file. Re-run CMake to regenerate the `.cxx` and `.castxml.inc`
  files, then build normally. CMake will automatically include the new
  submodule in the `--submodule-order` passed to `igenerator.py`.
