#!/usr/bin/env bash

die() {
	echo 1>&2 "$@" ; exit 1
}

# Make sure we are inside the repository.
cd "${BASH_SOURCE%/*}" &&

exe_ext=""
if [[ "$(uname -o)" == "Msys" ]]; then
    exe_ext=".exe"
fi

# check if curl executable exists
if ! command -v curl &> /dev/null; then
    die "curl is required."
fi &&
git_dir=$(git rev-parse --git-dir) &&
mkdir -p "$git_dir/hooks" &&
(
cd "$git_dir/hooks" &&
pixi_exe=$git_dir/hooks/pixi/bin/pixi$exe_ext &&
if [[ -f "$pixi_exe" ]]; then
    $pixi_exe run pre-commit-install
    exit $?
fi &&
curl -fsSL https://pixi.sh/install.sh | PIXI_HOME=$git_dir/hooks/pixi PIXI_NO_PATH_UPDATE=1 bash &&
$pixi_exe run pre-commit-install
)
