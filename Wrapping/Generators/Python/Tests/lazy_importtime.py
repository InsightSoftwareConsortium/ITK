# ==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
# ==========================================================================

"""Quantitative cold-start benchmark for ``import itk``.

Runs the canonical PEP 562 lazy-loading sanity check:

  1. ``python -X importtime -c 'import itk'``: total import time as
     reported by CPython's bundled tracer; gates the lazy-loading
     contract (no SWIG ``.so`` or template registration during
     ``import itk`` itself).

  2. Snapshot ``len(sys.modules)`` immediately after ``import itk``;
     the count must stay below a generous ceiling so a future
     accidental eager import surfaces as a hard test failure rather
     than a silent regression.

The numeric thresholds are intentionally loose so the test does not
flap on slow CI runners; they are tightened over time as the
ecosystem reduces import cost (PEP 810, ``.pyi`` stubs, SWIG codegen
trimming).
"""

import os
import re
import subprocess
import sys


# Hard ceilings.  Tuned empirically against the PR-6183 lazy-loading
# baseline with full Python wrapping: ``import itk`` adds ~520 entries
# to sys.modules, made up of:
#   - numpy + transitive (~130)
#   - one ``itk.<Module>`` synthetic submodule per wrapped SWIG module
#     (~100 in a full wrap)
#   - two ``itk.Configuration.<Module>{Config,_snake_case}`` data
#     modules per wrapped SWIG module (~200 in a full wrap)
#   - itk.support.* and stdlib import trampolines (~90)
# The ceiling is set generously so slow CI runners do not flap.  It
# scales linearly with the wrapped-module count: each new SWIG module
# added to the default wrap contributes roughly 3 entries (one
# synthetic submodule + two Configuration data modules), so the
# ceiling must be raised when new modules are added to the default
# wrap.  Tighten if the headroom shrinks; raise if a wrap-set
# expansion (e.g. new remote-module ingest) takes the live count
# within ~50 of the ceiling.
MAX_CUMULATIVE_IMPORT_MS = 5000.0
MAX_SYS_MODULES_AFTER_IMPORT_ITK = 750


_IMPORTTIME_LINE = re.compile(
    r"^import time:\s+(?P<self_us>\d+)\s+\|\s+(?P<cumulative_us>\d+)\s+\|\s+(?P<name>.+)$"
)


def _measure_importtime() -> tuple[float, str]:
    """Return ``(cumulative_ms, raw_log)`` for ``python -X importtime
    -c 'import itk'``.  CPython's importtime output goes to stderr."""
    proc = subprocess.run(
        [sys.executable, "-X", "importtime", "-c", "import itk"],
        check=True,
        capture_output=True,
        text=True,
        env={**os.environ, "ITK_EAGER_IMPORT": "0"},
    )
    log = proc.stderr
    cumulative_us = 0.0
    for line in log.splitlines():
        match = _IMPORTTIME_LINE.match(line)
        if match and match.group("name").strip() == "itk":
            cumulative_us = float(match.group("cumulative_us"))
            break
    if cumulative_us == 0.0:
        raise RuntimeError(
            "Could not parse cumulative import time for 'itk' from "
            "python -X importtime output:\n" + log
        )
    return cumulative_us / 1000.0, log


def _measure_sys_modules_after_import() -> int:
    """Return ``len(sys.modules)`` after a fresh ``import itk`` in a
    subprocess (avoids contamination from this driver's own imports)."""
    proc = subprocess.run(
        [
            sys.executable,
            "-c",
            "import itk, sys; print(len(sys.modules))",
        ],
        check=True,
        capture_output=True,
        text=True,
        env={**os.environ, "ITK_EAGER_IMPORT": "0"},
    )
    return int(proc.stdout.strip())


def main() -> int:
    cumulative_ms, log = _measure_importtime()
    print(f"itk cumulative import time: {cumulative_ms:.1f} ms")
    if cumulative_ms > MAX_CUMULATIVE_IMPORT_MS:
        print(
            f"FAIL: import time {cumulative_ms:.1f} ms exceeds ceiling "
            f"{MAX_CUMULATIVE_IMPORT_MS:.1f} ms.  Last 25 lines of "
            f"-X importtime log:",
            file=sys.stderr,
        )
        for line in log.splitlines()[-25:]:
            print(line, file=sys.stderr)
        return 1

    n_modules = _measure_sys_modules_after_import()
    print(f"len(sys.modules) after 'import itk': {n_modules}")
    if n_modules > MAX_SYS_MODULES_AFTER_IMPORT_ITK:
        print(
            f"FAIL: {n_modules} entries in sys.modules exceeds ceiling "
            f"{MAX_SYS_MODULES_AFTER_IMPORT_ITK}.  An accidental eager "
            f"import has likely been introduced.",
            file=sys.stderr,
        )
        return 1

    print("OK: itk lazy-loading cold-start within budget.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
