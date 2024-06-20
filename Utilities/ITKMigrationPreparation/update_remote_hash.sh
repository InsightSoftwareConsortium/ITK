#!/bin/bash
# \author Hans J. Johnson
set -e

##
## A script to assist with making pull requests and updating
## remote modules.
##
## export BRANCH_NAME=ITKv5_20180128  ## <-- The name of the branch for this auto-merge-request
## export COMMIT_MSG=""  ## <-- The commit message for the changes to the patch set
## for remMod in $(pwd)/*.remote.cmake; do
##   bash ~/Dashboard/src/ITK/Utilities/ITKv5Preparation/update_remote_hash.sh $remMod ${BRANCH_NAME};
## done
##
## This depends on https://github.com/defunkt/github-gem
##

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
git rebase origin/master
HEAD_HASH=$(git rev-parse --verify HEAD)
sed -i"" "s/GIT_TAG .*/GIT_TAG ${HEAD_HASH}/g" ../${remoteFile}
MASTER_HASH=$(git rev-parse --verify origin/master)
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
  git checkout master
  git rebase origin/master
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
