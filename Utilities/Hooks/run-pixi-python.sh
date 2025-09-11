#!/usr/bin/env bash
# Cross-platform launcher for Pixi-Python launched local Python pre-commit hooks

# Determine Python executable path based on OS
if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "cygwin" || "$OSTYPE" == "mingw" || "$OSTYPE" == "win32" ]]; then
    PYTHON_EXE="./.pixi/envs/pre-commit/python.exe"
else
    PYTHON_EXE="./.pixi/envs/pre-commit/bin/python"
fi

# Verify Python executable exists
if [[ ! -x "$PYTHON_EXE" ]]; then
    echo "Error: Python executable not found at $PYTHON_EXE" >&2
    exit 1
fi

# Get target script path from first argument
TARGET_SCRIPT="$1"
shift  # Remove first arg, leaving remaining args for the target script

# Execute target script with Python
exec "$PYTHON_EXE" "$TARGET_SCRIPT" "$@"
