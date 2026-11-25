#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="PocketFFT"
readonly ownership="PocketFFT Upstream <kwrobot@kitware.com>"
readonly subtree="Modules/ThirdParty/PocketFFT/src/itkpocketfft"
readonly exact_tree_match=false
readonly repo="https://github.com/InsightSoftwareConsortium/pocketfft"
readonly tag="for/itk-pocketfft-cpp-c90e55b"
readonly paths="
pocketfft_hdronly.h
LICENSE.md
"

extract_source () {
    git_archive
}

source "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
