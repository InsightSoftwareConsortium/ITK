#!/usr/bin/env bash
# Cross-platform launcher for Pixi-Python launched local Python pre-commit hooks

# Determine Python executable path based on OS
if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "cygwin" || "$OSTYPE" == "mingw" || "$OSTYPE" == "win32" ]]; then
    PYTHON_EXE="./.pixi/envs/pre-commit/python.exe"
else
    PYTHON_EXE="./.pixi/envs/pre-commit/bin/python"
fi

# The pixi env is per-worktree; provision it on first use in fresh worktrees.
if [[ ! -x "$PYTHON_EXE" ]] && command -v pixi >/dev/null 2>&1; then
    echo "pre-commit pixi environment missing; running 'pixi install -e pre-commit'..." >&2
    pixi install -e pre-commit >&2 || true
fi

if [[ ! -x "$PYTHON_EXE" ]]; then
    echo "Error: Python executable not found at $PYTHON_EXE" >&2
    echo "Run 'pixi install -e pre-commit' in this checkout to create it." >&2
    exit 1
fi

# Get target script path from first argument
TARGET_SCRIPT="$1"
shift  # Remove first arg, leaving remaining args for the target script

# Execute target script with Python
exec "$PYTHON_EXE" "$TARGET_SCRIPT" "$@"
