#!/usr/bin/env bash
# ITK Release+ccache build for compiler-flag investigation (macOS)
# Usage:
#   ./build-macos.sh                           # baseline Release build
#   ./build-macos.sh --remove-strict-overflow  # patch out -Wno-strict-overflow
#   ./build-macos.sh --check-long-double       # test if -Wno-long-double still fires
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
BUILD_DIR="${REPO_ROOT}/build-flag-audit"
JOBS="${JOBS:-$(sysctl -n hw.logicalcpu)}"
REMOVE_STRICT_OVERFLOW=0
CHECK_LONG_DOUBLE=0

for arg in "$@"; do
  case "$arg" in
    --remove-strict-overflow) REMOVE_STRICT_OVERFLOW=1 ;;
    --check-long-double)      CHECK_LONG_DOUBLE=1 ;;
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

cp "${FLAGS_FILE}" "${BACKUP}"
PATCHED=0

if [[ ${REMOVE_STRICT_OVERFLOW} -eq 1 ]]; then
  sed -i.tmp 's/[[:space:]]*-Wno-strict-overflow//' "${FLAGS_FILE}"
  echo "[patch] removed -Wno-strict-overflow"
  PATCHED=1
fi

if [[ ${CHECK_LONG_DOUBLE} -eq 1 ]]; then
  # Remove -Wno-long-double to see if AppleClang still warns on ARM64 or x86_64
  sed -i.tmp 's/[[:space:]]*-Wno-long-double[[:space:]]*#Needed on APPLE//' "${FLAGS_FILE}"
  echo "[patch] removed -Wno-long-double (check if warnings appear)"
  PATCHED=1
fi

[[ ${PATCHED} -eq 0 ]] && rm -f "${BACKUP}"

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
grep -c "warning:" "${BUILD_DIR}/build.log" || echo "0 warnings"
if [[ ${CHECK_LONG_DOUBLE} -eq 1 ]]; then
  echo "--- long-double hits ---"
  grep "long.double\|Wlong-double" "${BUILD_DIR}/build.log" | sort -u || echo "(none)"
fi
echo "Log: ${BUILD_DIR}/build.log"
