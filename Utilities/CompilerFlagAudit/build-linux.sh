#!/usr/bin/env bash
# ITK Release+ccache build for compiler-flag investigation (Linux)
# Usage:
#   ./build-linux.sh                        # baseline Release build
#   ./build-linux.sh --remove-strict-overflow  # patch out -Wno-strict-overflow first
#   ./build-linux.sh --jobs 8               # override parallel job count
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
BUILD_DIR="${REPO_ROOT}/build-flag-audit"
JOBS="${JOBS:-$(nproc)}"
REMOVE_STRICT_OVERFLOW=0

for arg in "$@"; do
  case "$arg" in
    --remove-strict-overflow) REMOVE_STRICT_OVERFLOW=1 ;;
    --jobs) shift; JOBS="$1" ;;
    *) echo "Unknown arg: $arg"; exit 1 ;;
  esac
done

FLAGS_FILE="${REPO_ROOT}/CMake/ITKSetStandardCompilerFlags.cmake"
BACKUP="${FLAGS_FILE}.bak"

restore() {
  if [[ -f "${BACKUP}" ]]; then
    mv "${BACKUP}" "${FLAGS_FILE}"
    echo "[restore] ${FLAGS_FILE} restored"
  fi
}
trap restore EXIT

if [[ ${REMOVE_STRICT_OVERFLOW} -eq 1 ]]; then
  cp "${FLAGS_FILE}" "${BACKUP}"
  sed -i 's/\s*-Wno-strict-overflow//' "${FLAGS_FILE}"
  echo "[patch] removed -Wno-strict-overflow from ${FLAGS_FILE}"
fi

cmake \
  -B "${BUILD_DIR}" \
  -S "${REPO_ROOT}" \
  -G Ninja \
  -DCMAKE_BUILD_TYPE:STRING=Release \
  -DBUILD_TESTING:BOOL=ON \
  -DCMAKE_C_COMPILER_LAUNCHER:STRING=ccache \
  -DCMAKE_CXX_COMPILER_LAUNCHER:STRING=ccache

cmake --build "${BUILD_DIR}" -j "${JOBS}" 2>&1 | tee "${BUILD_DIR}/build.log"

echo ""
echo "=== Warning summary ==="
grep -c " warning:" "${BUILD_DIR}/build.log" || true
echo "Log: ${BUILD_DIR}/build.log"
