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


#% NAME
#%     check-links.sh - report broken links in the ITK source tree
#%
#% SYNOPSIS
#%     check-links.sh [PATH...]
#%     check-links.sh -h | --help
#%
#% DESCRIPTION
#%     Run the lychee link checker on ITK's documentation, examples, release
#%     notes, and other text files. Includes and excludes come from
#%     Utilities/Maintenance/lychee.toml. With no PATH the whole repository
#%     is scanned.
#%
#%     The script is meant for manual or periodic use, not for per-PR CI.
#%     A scan of the whole tree draws rate-limit (HTTP 429) responses from
#%     hosts such as github.com, doi.org, and journal mirrors, which a CI
#%     runner cannot work around (lycheeverse/lychee#1574). A local run with
#%     the cache kept between runs is reliable.
#%
#%     The exit status is that of lychee: 0 when no broken link is found,
#%     nonzero otherwise. It is 127 when lychee is not on PATH, and 1 when
#%     lychee.toml is missing.
#%
#% OPTIONS
#%     PATH
#%         File or directory to scan, relative to the repository root.
#%         Several may be given.
#%     -h, --help
#%         Print this help and exit.
#%
#% IMPLEMENTATION
#%     Needs lychee 0.15 or later and bash 4 or later. Install lychee with
#%     "cargo install lychee", "brew install lychee", or a binary from
#%     https://github.com/lycheeverse/lychee/releases (see
#%     https://lychee.cli.rs/). Needs network access.
#%
#%     The script may be run from any directory; it changes to the
#%     repository root. It writes a Markdown report to .lychee-report.md
#%     and a cache to .lycheecache at the root, both ignored by git. Cached
#%     results expire after one day, and HTTP 429 responses are not cached.
#%
#% EXAMPLES
#%     Utilities/Maintenance/check-links.sh
#%         Check every link in the repository.
#%     Utilities/Maintenance/check-links.sh Documentation Examples
#%         Check only the links under Documentation and Examples.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Utilities/Maintenance/lychee.toml:
#%         The lychee configuration used by this script.

set -uo pipefail

usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

case "${1-}" in
  -h | --help)
    usage
    exit 0
    ;;
esac

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
