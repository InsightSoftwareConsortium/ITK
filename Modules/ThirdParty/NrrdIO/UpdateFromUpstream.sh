#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="NrrdIO"
readonly ownership="Teem Upstream <kwrobot@kitware.com>"
readonly subtree="Modules/ThirdParty/NrrdIO/src/NrrdIO"
readonly exact_tree_match=false
readonly repo="https://github.com/InsightSoftwareConsortium/teem"
readonly tag="for/itk-nrrdio-2f56b646b"
readonly paths="
"

extract_source () {
    git_archive
}

source "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
