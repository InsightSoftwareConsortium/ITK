#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name='MetaIO'
readonly ownership='MetaIO Maintainers <metaio@itk.org>'
readonly subtree="Modules/ThirdParty/MetaIO/src/MetaIO"
readonly repo="https://github.com/Kitware/MetaIO.git"
readonly tag="master"
readonly shortlog=false
readonly paths="
  src
  License.txt
"

extract_source () {
    git_archive
    pushd "${extractdir}/${name}-reduced"
    echo "* -whitespace" > .gitattributes
    popd
}

. "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
