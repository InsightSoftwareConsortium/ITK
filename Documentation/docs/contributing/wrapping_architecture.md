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

CMake processes every `.wrap` file at configure time and writes three files
per submodule into `<build>/Wrapping/castxml_inputs/`:

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
         Typedefs/<sub>.i            itk-pkl/<sub>.index.txt      itk-pkl/<Class>.<sub>.pkl
         Typedefs/<sub>.idx          (lists .pkl paths; byproduct) (pickle of class metadata)
         Typedefs/<sub>SwigInterface.h
                                          + per module (×1):
                                            itk-pkl/<Module>.pkl.stamp
                                            itk/Configuration/<Module>_snake_case.py
```

`igenerator.py` uses `pygccxml` to parse each `.xml` file and emit SWIG
interface (`.i`) and index (`.idx`) files, pickle (`.pkl`) class-metadata
files consumed later by `pyi_generator.py`, and a `.index.txt` manifest
that lists the `.pkl` paths for each submodule.

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
`.index.txt`, loads the referenced `.pkl` files, and writes the type-stub
files used by IDEs:

```
itk-pkl/<sub>.index.txt (×816) ──▶  pyi_generator.py  ──▶  _proxies.pyi
itk-pkl/<Class>.<sub>.pkl            (reads .pkl via .index.txt)   __init__.pyi
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
| `Wrapping/Generators/Python/itk-pkl/<Class>.<sub>.pkl` | `igenerator.py` | `pyi_generator.py` |
| `Wrapping/Generators/Python/itk-pkl/<Module>.pkl.stamp` | `igenerator.py` | ninja (tracks pkl completeness) |
| `Wrapping/Generators/Python/itk/_<Module>Python.abi3.so` | linker | Python `import itk` |
| `Wrapping/Generators/Python/itk/_proxies.pyi` | `pyi_generator.py` | IDEs |

## Caches

### CastXML cache (`itk-castxml-cache.py`)

Controls via environment:

| Variable | Default | Purpose |
|---|---|---|
| `ITK_WRAP_CACHE` | `~/.cache/itk-wrap` | Cache root directory |
| `ITK_WRAP_CACHE_VERBOSE` | unset | Set to `1` to log HIT/MISS per file |

The cache is content-addressed and generator-version-stamped (`_KEY_VERSION`
in `itk-castxml-cache.py`). It is shared across build directories, so a
fresh configure reuses XML from a previous build on the same machine.

### ccache

CastXML re-runs produce identical `.xml` files (the content is deterministic)
but are slow. The CastXML cache eliminates that cost. For the C++ compilation
steps (Step 3), ccache caches compiled `.o` files keyed on source content.
Both caches are independent and complement each other.

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
itk-pkl/<sub>.index.txt + <Class>.<sub>.pkl + <Module>.pkl.stamp
    │ swig + ccache compile + link  [parallel per submodule]
    ▼
_<Module>Python.abi3.so
    │ pyi_generator.py  [1 global job; needs all .index.txt]
    ▼
_proxies.pyi + __init__.pyi
```

## Troubleshooting

**`No pickle files were found in directory itk-pkl`**
: The `.index.txt` file exists (so ninja considers `igenerator.py`
  up-to-date) but the `.pkl` files it references are absent. Delete the
  stale `.index.txt` and `<Module>.pkl.stamp` files to force `igenerator.py`
  to re-run:
  ```bash
  find <build>/Wrapping/Generators/Python/itk-pkl -name "*.index.txt" -o -name "*.pkl.stamp" | xargs rm -f
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
