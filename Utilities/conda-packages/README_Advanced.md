# Advanced Utilities/conda-packages build options

Advanced / opt-in features of the `Utilities/conda-packages/` build. The baseline
workflow is in [`README.md`](./README.md); read that first.

## ccache — compiler caching across rattler-build runs

Rattler-build creates a fresh work directory (`rattler-build_libitk_<ts>/`)
per invocation. Without caching, every run does a full ITK rebuild (~30+
minutes on a workstation). Enabling ccache drops subsequent builds to a
few minutes when only install scripts or recipe metadata change.

### Opting in

ccache is disabled by default. To enable it, set `ITK_CONDA_USE_CCACHE=1`
in the host shell before invoking `pixi build` or `rattler-build build`.
When the gate is off, ccache is not pulled into `build:` requirements and
the CMake compiler-launcher flags are not set — the build is byte-for-byte
identical to a non-ccache build.

### Full usage — host shell variables

```bash
# ── REQUIRED ────────────────────────────────────────────────
export ITK_CONDA_USE_CCACHE=1           # gate: activates everything below

# ── STRONGLY RECOMMENDED ───────────────────────────────────
export CCACHE_DIR=$HOME/.ccache         # where cache lives (persists across runs)

# ── OPTIONAL ───────────────────────────────────────────────
export CCACHE_MAXSIZE=40G               # cap cache size (default 5 GB is too small for ITK)

# ── RUN ─────────────────────────────────────────────────────
pixi run --environment dev rattler-build build --experimental \
    --recipe Utilities/conda-packages/recipe.yaml \
    --output-dir ~/my-itk-channel/
```

### Variable reference

| Variable | Set by | Purpose |
|---|---|---|
| `ITK_CONDA_USE_CCACHE` | **host** | Gate. Only `=1` activates the ccache path. |
| `CCACHE_DIR` | **host** | Path to the cache store. Falls back to `$XDG_CACHE_HOME/ccache` or `~/.cache/ccache`. |
| `CCACHE_MAXSIZE` | **host** | Cache size cap (e.g. `40G`). Alternatively run `ccache -M 40G` once. |
| `CCACHE_BASEDIR` | `build.sh` | Set to `$SRC_DIR` so ccache rewrites rattler-build's timestamped paths relative. |
| `CCACHE_NOHASHDIR` | `build.sh` | Excludes cwd from the hash (same reason). |
| `CCACHE_SLOPPINESS` | `build.sh` | `file_macro,time_macros,include_file_ctime,include_file_mtime,pch_defines,system_headers` — relaxes checks that otherwise prevent hits across sandbox reruns. |
| `CCACHE_COMPILERCHECK` | `build.sh` | `content` — hashes the compiler binary contents rather than mtime, so conda reinstalls of the toolchain don't invalidate the entire cache. |

Sandbox-specific variables (the bottom four) are set inside `build.sh`
because `$SRC_DIR` changes per rattler-build invocation and the host
shell cannot know it in advance.

### How it is wired

1. **`recipe.yaml`**
   - `build.script.env` forwards `ITK_CONDA_USE_CCACHE`, `CCACHE_DIR`,
     and `CCACHE_MAXSIZE` from the host shell into the sandbox via
     `${{ env.get(...) }}` jinja.
   - `requirements.build` conditionally adds `ccache` when the env var
     equals `"1"`:
     ```yaml
     - if: env.get("ITK_CONDA_USE_CCACHE", default="0") == "1"
       then: ccache
     ```
2. **`build.sh`** — detects the gate, exports the sandbox-specific vars,
   prints `ccache --show-stats`, and appends
   `-DCMAKE_{C,CXX}_COMPILER_LAUNCHER=ccache` to the CMake line.
3. **`build.bat`** — the same pattern for Windows (note: ccache on Windows
   is less mature; recent `ccache >=4.8` is recommended).

### Verifying hit rate

After the first (cold) build completes:

```bash
ccache --show-stats
```

Expected ordering for the next run:

| Run | Cache behavior | Wall time |
|-----|----------------|-----------|
| 1st (cold) | All misses, cache populates | ~30 min (baseline) |
| 2nd (no source changes, e.g. install-script tweak) | ~100% hits | ~2–5 min |
| Nth (incremental source change) | Hits on untouched TUs | proportional to changed files |

If the second run still shows many misses, check:

- Compiler binary changed between runs (happens when conda updates the
  compiler package) — `CCACHE_COMPILERCHECK=content` mitigates this.
- Source tree path changed — `CCACHE_BASEDIR` is supposed to handle this
  via path relativization; verify `$SRC_DIR` is consistent.
- Recipe changed a compile flag — a genuine miss, not a bug.

### Turning it back off

Unset the gate:

```bash
unset ITK_CONDA_USE_CCACHE
```

Or just start a fresh shell. With the gate off, the recipe does not depend
on `ccache` and the build script does not enable the compiler launcher.

---

## Building with a specific set of compiler flags

Conda-forge's compiler packages populate `CFLAGS`, `CXXFLAGS`, and `LDFLAGS`
from their activation scripts (`{build_env}/etc/conda/activate.d/*.sh`).
These flags carry `--sysroot`, target triples, and conda-forge's hardening
defaults — replacing them wholesale will break the build. Therefore the
recipe exposes **append-only** variables prefixed `ITK_CONDA_EXTRA_*`.
Anything you set there is appended to what conda's activation produced.

### Full usage — host shell variables

```bash
# ── BUILD TYPE OVERRIDE (default: Release) ─────────────────
export ITK_CONDA_BUILD_TYPE=RelWithDebInfo     # or Debug, MinSizeRel

# ── APPEND TO CONDA-FORGE COMPILER FLAGS ───────────────────
export ITK_CONDA_EXTRA_CFLAGS="-march=native -DFOO=1"
export ITK_CONDA_EXTRA_CXXFLAGS="-march=native -Wno-deprecated-declarations"
export ITK_CONDA_EXTRA_LDFLAGS="-Wl,--as-needed"

# ── RAW EXTRA CMAKE ARGS (whole tokens, quoted) ────────────
export ITK_CONDA_EXTRA_CMAKE_ARGS="-DITK_USE_GPU:BOOL=ON -DITK_WRAP_DOC:BOOL=OFF"

# ── RUN ────────────────────────────────────────────────────
pixi run --environment dev rattler-build build --experimental \
    --recipe Utilities/conda-packages/recipe.yaml \
    --output-dir ~/my-itk-channel/
```

All five are optional — unset any you do not need.

### Variable reference

| Variable | Default | What it does |
|---|---|---|
| `ITK_CONDA_BUILD_TYPE` | `Release` | Value of `-DCMAKE_BUILD_TYPE`. Useful values: `Release`, `RelWithDebInfo`, `Debug`, `MinSizeRel`. |
| `ITK_CONDA_EXTRA_CFLAGS` | empty | Appended to `CFLAGS` inside the sandbox (after conda activation). C-only flags. |
| `ITK_CONDA_EXTRA_CXXFLAGS` | empty | Appended to `CXXFLAGS`. C++-only flags. |
| `ITK_CONDA_EXTRA_LDFLAGS` | empty | Appended to `LDFLAGS`. Linker flags. |
| `ITK_CONDA_EXTRA_CMAKE_ARGS` | empty | Pasted verbatim into the `cmake` command line **before** the hardcoded `-D…` flags. Each token becomes a separate cmake argument. |

### Recipes for common scenarios

**Native-architecture optimized build (desktop/workstation use only —
not redistributable as a conda package):**

```bash
export ITK_CONDA_EXTRA_CFLAGS="-march=native -mtune=native"
export ITK_CONDA_EXTRA_CXXFLAGS="-march=native -mtune=native"
```

**RelWithDebInfo for profiling:**

```bash
export ITK_CONDA_BUILD_TYPE=RelWithDebInfo
# CMake's RelWithDebInfo defaults to -O2 -g; add -fno-omit-frame-pointer
# if you want flame graphs to resolve properly.
export ITK_CONDA_EXTRA_CXXFLAGS="-fno-omit-frame-pointer"
```

**AddressSanitizer-instrumented build:**

```bash
export ITK_CONDA_BUILD_TYPE=Debug
export ITK_CONDA_EXTRA_CFLAGS="-fsanitize=address -fno-omit-frame-pointer"
export ITK_CONDA_EXTRA_CXXFLAGS="-fsanitize=address -fno-omit-frame-pointer"
export ITK_CONDA_EXTRA_LDFLAGS="-fsanitize=address"
```

**Override module selection without editing `build.sh`:**

```bash
export ITK_CONDA_EXTRA_CMAKE_ARGS="-DModule_ITKReview:BOOL=OFF -DModule_ITKVideoBridgeVXL:BOOL=ON"
```

Note: `ITK_CONDA_EXTRA_CMAKE_ARGS` is pasted **before** the recipe's
hardcoded `-D` flags, so it **cannot** override a flag that `build.sh`
sets explicitly (the last `-D` wins in CMake). To change a hardcoded
flag, edit `build.sh` directly.

### Verifying flags reached the compiler

After the build starts, `build.sh` prints lines like:

```
Appended to CXXFLAGS: -march=native -Wno-deprecated-declarations
```

For the authoritative view, check the `compile_commands.json` that ninja
generates in the rattler-build work directory (use
`rattler-build build --keep-build` so the work dir survives):

```bash
jq '.[0].command' \
   output/bld/rattler-build_libitk_*/work/build/compile_commands.json
```

### Host-shell `CFLAGS` is NOT forwarded

A deliberate choice. Conda-forge's compiler activation sets `CFLAGS` inside
the sandbox **before** the build script runs; forwarding the host's value
as-is would either replace the activation output (breaking the build) or
silently double up. The append-only `ITK_CONDA_EXTRA_CFLAGS` pattern keeps
both the conda-forge flags and the user's additions, with no ambiguity.

If you genuinely need to replace conda-forge's compiler flags (rare, and
usually a sign something else is wrong), edit the `cmake` command in
`build.sh` directly — do not try to do it via environment variables.
