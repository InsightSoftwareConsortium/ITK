# SPDX-FileCopyrightText: Copyright NumFOCUS
# SPDX-License-Identifier: Apache-2.0
"""Shared helpers for ITK SPDX/SBOM tooling.

Centralizes constants, JSON I/O, and path discovery used by the
``generate_sbom``, ``validate_light``, ``validate_with_spdx_tools``,
``verify_versions``, ``compute_fingerprint``, and ``add_headers``
scripts in this package.

Requires Python 3.10 or later.
"""

from __future__ import annotations

import json
from pathlib import Path

# --- SPDX / ITK-wide constants ---------------------------------------------

SPDX_VERSION: str = "SPDX-2.3"
SPDX_DATA_LICENSE: str = "CC0-1.0"

ITK_SPDX_LICENSE: str = "Apache-2.0"
ITK_SPDX_COPYRIGHT: str = "Copyright NumFOCUS"
ITK_SPDX_SUPPLIER: str = "Organization: NumFOCUS"

# Exit codes shared between scripts and CTest.
EXIT_OK: int = 0
EXIT_FAIL: int = 1
EXIT_USAGE: int = 2
# CTest treats return code 77 as "test skipped".
EXIT_SKIP: int = 77


def load_sbom(path: Path) -> dict:
    """Read and parse an SPDX SBOM JSON file.

    Raises ``OSError`` or ``json.JSONDecodeError`` on failure; callers
    decide how to surface the error.
    """
    with open(path, encoding="utf-8") as f:
        return json.load(f)


def repo_root_from_script(script_file: str) -> Path:
    """Return the ITK source root given ``__file__`` of a script in this pkg.

    Scripts live at ``<ITK>/Utilities/SPDX/*.py``; the root is two
    parents up.
    """
    return Path(script_file).resolve().parents[2]
