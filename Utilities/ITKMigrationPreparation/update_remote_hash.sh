#!/bin/bash
# \author Hans J. Johnson

#% NAME
#%     update_remote_hash.sh - update a remote module's GIT_TAG and open a PR
#%
#% SYNOPSIS
#%     update_remote_hash.sh REMOTE_FILE BRANCH_NAME
#%     update_remote_hash.sh -h | --help
#%
#% DESCRIPTION
#%     Help update a remote module and open a pull request for it. The
#%     remote module's repository is kept in a directory named after
#%     REMOTE_FILE without its .remote.cmake suffix; it is cloned, with its
#%     submodules, if that directory does not exist. The script fetches
#%     origin, rebases the checked-out branch onto origin/main, and sets
#%     every GIT_TAG line in REMOTE_FILE to the resulting HEAD.
#%
#%     If HEAD then differs from origin/main, because the branch carries
#%     local commits, the script checks out BRANCH_NAME at HEAD (unless it is
#%     already the current branch), pushes it to origin, and opens a pull
#%     request with hub. The pull request title is "ENH: C++11 updates
#%     warning fixes", and the COMMIT_MSG environment variable, if set, is
#%     appended to its body. Otherwise the script checks out main and
#%     rebases it onto origin/main.
#%
#% OPTIONS
#%     REMOTE_FILE
#%         Name of a *.remote.cmake file in the current directory.
#%     BRANCH_NAME
#%         Branch to push and open the pull request from.
#%     -h, --help
#%         Print this help and exit.
#%
#% IMPLEMENTATION
#%     Requires git, GNU sed, network access to the module's repository, push
#%     rights to its origin, and the hub command authenticated with GitHub.
#%
#%     Run from the directory that holds REMOTE_FILE and pass the bare file
#%     name: the script edits ../REMOTE_FILE from inside the module
#%     directory, so a path with a directory part edits the wrong file. The
#%     clone URL is the last GIT_REPOSITORY line of REMOTE_FILE, expanded by
#%     the shell with git_protocol set to https.
#%
#%     The script stops at the first failing command. If either argument is
#%     missing it prints an error and exits with status 255.
#%
#% EXAMPLES
#%     cd Modules/Remote
#%     ../../Utilities/ITKMigrationPreparation/update_remote_hash.sh \
#%         Cleaver.remote.cmake update-cleaver
#%         Update Cleaver.remote.cmake, opening a pull request from the
#%         branch update-cleaver if the module has local commits.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Utilities/Maintenance/UpdateRemoteModules.sh:
#%         Update the GIT_TAG of every remote module, or with -ongreen, of
#%         each remote module whose CI passes.

usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

case "${1-}" in
  -h | --help)
    usage
    exit 0
    ;;
esac

set -e

remoteFile="$1"
if [[ -z "${remoteFile}" ]]; then
  echo "ERROR: remoteFile is required"
  exit -1
fi

branchName="$2"
if [[ -z "${branchName}" ]]; then
  echo "ERROR: Branch name is required"
  exit -1
fi

echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
echo ""
remoteCache=${remoteFile//.remote.cmake}

if [[ ! -d ${remoteCache} ]]; then
  export git_protocol=https
  clone_url=$(eval "echo $(cat ${remoteFile} | grep GIT_REPOSITORY |tail -1 |awk '{print $2}')")
  echo clone_url
  git clone ${clone_url} ${remoteCache}
  pushd  ${remoteCache}
    git submodule init
    git submodule update --recursive
  popd
fi

pushd ${remoteCache}
git fetch origin
git rebase origin/main
HEAD_HASH=$(git rev-parse --verify HEAD)
sed -i"" "s/GIT_TAG .*/GIT_TAG ${HEAD_HASH}/g" ../${remoteFile}
MASTER_HASH=$(git rev-parse --verify origin/main)
if [[ "${HEAD_HASH}" != "${MASTER_HASH}" ]]; then
    CURR_BRANCH=$(git rev-parse --abbrev-ref HEAD)
    if [[ "${CURR_BRANCH}" != "${branchName}" ]]; then
      git checkout ${HEAD_HASH} -b ${branchName};
      git submodule update --recursive
    fi
    git push --set-upstream origin ${branchName};
    hub pull-request -m "ENH: C++11 updates warning fixes

This pull request incorporates code changes
that happened in parallel with updating the
core ITK code to ITKv5.

${COMMIT_MSG}
";
else
  ## HEAD_HASH == MASTER_HASH
  git checkout main
  git rebase origin/main
fi
if [[ $? -ne 0 ]]; then
   echo "*** ERROR---------------- -------- $(pwd)";
   echo "*** ERROR---------------- -------- $(pwd)";
   echo "*** ERROR---------------- -------- $(pwd)";
   echo "*** ERROR---------------- -------- $(pwd)";
fi;
popd;
echo ""
echo "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
echo ""
