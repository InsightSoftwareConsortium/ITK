#!/usr/bin/env bash
# Formats the given BibTeX files in place with the options doxygen.bib is kept in.

# Utilities/GitSetup/setup-precommit installs pixi under the git directory, off PATH.
pixi=$(command -v pixi)
for git_dir in "$(git rev-parse --git-dir)" "$(git rev-parse --git-common-dir)"; do
    if [[ -z "$pixi" && -x "$git_dir/hooks/pixi/bin/pixi" ]]; then
        pixi="$git_dir/hooks/pixi/bin/pixi"
    fi
done
if [[ -z "$pixi" ]]; then
    echo "Error: pixi not found; run Utilities/SetupForDevelopment.sh or install pixi." >&2
    exit 1
fi

# bibtex-tidy requires the input files before the options.
exec "$pixi" run --frozen -e pre-commit bibtex-tidy "$@" --modify --align=13 --sort --no-escape --quiet
