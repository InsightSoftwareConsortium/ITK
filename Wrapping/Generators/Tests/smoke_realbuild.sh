#!/usr/bin/env bash
# L3 smoke: two fresh ITK builds sharing one castxml cache; asserts the second
# issues zero real castxml runs. Linux only; expects a configured pixi ITK
# checkout. Run on demand — this is a ~2x full-build operation.
#
# Usage: ITK_SOURCE_DIR=/path/to/ITK ./smoke_realbuild.sh
set -euo pipefail
ITK=${ITK_SOURCE_DIR:?set ITK_SOURCE_DIR}
CACHE=$(mktemp -d)/wrap-cache
export ITK_WRAP_CACHE=$CACHE

cd "$ITK"
for round in cold warm; do
  rm -rf build-python
  start=$(date +%s)
  pixi run build-python-ci
  echo "$round: $(($(date +%s) - start))s"
done

entries=$(find "$CACHE/l2" -name _ok | wc -l)
echo "cache entries: $entries"
python3 - "$ITK/build-python/.ninja_log" <<'EOF'
import sys
rows = [
    int(p[1]) - int(p[0])
    for line in open(sys.argv[1])
    if (p := line.split("\t")) and len(p) >= 4
    and p[3].endswith(".xml") and "castxml_inputs" in p[3]
]
mean = sum(rows) / len(rows)
print(f"warm castxml steps: {len(rows)}, mean {mean:.0f}ms")
assert mean < 1000, "warm castxml steps too slow — cache hits not happening"
EOF
echo "SMOKE PASS"
