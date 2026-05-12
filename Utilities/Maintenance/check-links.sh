#!/bin/bash

#==========================================================================
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
#==========================================================================*/


# Run the lychee link checker against ITK's documentation, examples,
# release notes, and other text artifacts to surface broken links.
#
# This script is intended for periodic manual / cron use, NOT for the
# per-PR CI pipeline.  Lychee against the full ITK tree triggers
# rate-limit (HTTP 429) responses on hosts such as github.com,
# doi.org, and journal mirrors that the CI runner cannot work around
# (see lycheeverse/lychee#1574).  Running locally with caching across
# invocations is reliable; running on every PR is not.
#
# Usage:
#   Utilities/Maintenance/check-links.sh
#       Scans the entire repository.  Reads
#       Utilities/Maintenance/lychee.toml for include / exclude
#       configuration.  Cache persists between runs at .lycheecache.
#       Exit code 0 means no broken links; non-zero indicates findings
#       in the report.
#
#   Utilities/Maintenance/check-links.sh path1 [path2 ...]
#       Scans only the listed files / directories (relative to the
#       repository root).
#
# Output:
#   Plain-text report on stdout plus a Markdown summary at
#   .lychee-report.md (gitignored — local artifact).
#
# Dependencies:
#   - lychee (https://lychee.cli.rs/) >= 0.15
#       Install via: cargo install lychee
#                    or: brew install lychee
#                    or: pre-built binary from
#                        https://github.com/lycheeverse/lychee/releases
#   - bash >= 4

set -uo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
LYCHEE_CONFIG="${REPO_ROOT}/Utilities/Maintenance/lychee.toml"
LYCHEE_REPORT="${REPO_ROOT}/.lychee-report.md"
LYCHEE_CACHE="${REPO_ROOT}/.lycheecache"

if ! command -v lychee >/dev/null 2>&1; then
    echo "error: 'lychee' not found on PATH" >&2
    echo "       see the install hints in $(basename "$0")" >&2
    exit 127
fi

if [[ ! -f "${LYCHEE_CONFIG}" ]]; then
    echo "error: ${LYCHEE_CONFIG} is missing" >&2
    exit 1
fi

cd "${REPO_ROOT}"

# Default scan paths if none given.
if [[ $# -eq 0 ]]; then
    set -- .
fi

echo "Running lychee against: $*"
echo "Config: ${LYCHEE_CONFIG}"
echo "Cache:  ${LYCHEE_CACHE}"
echo "Report: ${LYCHEE_REPORT}"
echo

lychee \
    --config "${LYCHEE_CONFIG}" \
    --cache --cache-exclude-status 429 --max-cache-age 1d \
    --format markdown --output "${LYCHEE_REPORT}" \
    "$@"
status=$?

echo
echo "Report written to ${LYCHEE_REPORT}"
exit $status
