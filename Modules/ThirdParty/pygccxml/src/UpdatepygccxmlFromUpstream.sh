#!/bin/bash
#
# UpdatepygccxmlFromUpstream.sh
#
# See ITK/Modules/ThirdParty/VNL/src/README-ITK.txt
# for more commentary on how this update process works.
#
echo "-------------------------------------------------------"
echo This script will update source code for the pygccxml
echo library from github.com/gccxml/pygccxml
echo "-------------------------------------------------------"
if [ ! -f Modules/ThirdParty/pygccxml/src/UpdatepygccxmlFromUpstream.sh ]
then
    echo The current working directory $(pwd) is not the top level
    echo of the ITK source code tree. Please change to the
    echo ITK source directory and re-run this script
    exit 1
fi

# Update the git tag for the version you are merging
git_url="https://github.com/gccxml/pygccxml"
git_tag="v1.6.2"

#
# Once the merge has been done
# EDIT THIS SCRIPT to change the hash tag at which to begin the
# next update...
#
# This merge done July 24, 2014
git branch pygccxml-upstream d4624a99ea62221f251e8e77d813289ad441d513

#
# Make a temp directory to handle the import of the upstream source
mkdir pygccxml-Tmp
cd pygccxml-Tmp
#
# base a temporary git repo off of the parent ITK directory
git init
#
# pull in the upstream branch
git pull .. pygccxml-upstream
#
# empty out all existing source
rm -rf *
#
# download and copy the necessary double-conversion source
echo Cloning upstream HEAD from https://github.com/gccxml/pygccxml
echo NOTE: to check out a particular revision for merging
echo you have to add a git checkout '<hash>'
echo or git checkout '<branchname>'
echo after the git clone command
git clone ${git_url}
#
# recover the upstream commit date.
cd pygccxml
upstream_date="$(git log -n 1 --format='%cd')"
upstream_sha="$(git rev-parse HEAD)"
cd ..

mkdir pgccxml-tmp2
cp -r pygccxml/pygccxml/* pgccxml-tmp2/
# get rid of pygccxml clone
rm -fr pygccxml
# Move the files to the pygccxml folder
mv pgccxml-tmp2 pygccxml
# add upstream files in Git
git add --all

# get a Change-Id to keep track of the change in gerrit
tree=$(git write-tree)
change_id=$(git commit-tree $tree -p HEAD </dev/null)

# commit new source
GIT_AUTHOR_NAME='GCC-XML Upstream' \
GIT_AUTHOR_EMAIL='pygccxml@gccxml.org' \
GIT_AUTHOR_DATE="${upstream_date}" \
git commit -q -m "ENH: pygccxml ${git_tag} (reduced)

This corresponds to commit hash
  ${upstream_sha}
from upstream repository
  ${git_url}

Change-Id: I${change_id}"

#
# push to the pygccxml-upstream branch in the
# ITK tree
git push .. HEAD:pygccxml-upstream
cd ..
#
# get rid of temporary repository
rm -fr pygccxml-Tmp
#
# checkout a new update branch off of the master.
git checkout -b pygccxml-update master
#
# use subtree merge to bring in upstream changes
git merge -s recursive -X subtree=Modules/ThirdParty/pygccxml/src/pygccxml pygccxml-upstream
echo "---------------------------------"
echo NOW Fix all conflicts and test new code.
echo "---------------------------------"
echo Commit the merged/fixed/verified code and run this command
echo git rev-parse --short=16 pygccxml-upstream
echo "---------------------------------"
echo to get the commit hash from which the pygccxml-upstream
echo branch must be started on the next update.
echo "---------------------------------"
echo edit the line \"git branch pygccxml-upstream\" above.
echo Once you have commited this change to the UpdatepygccxmlFromUpstream.sh script,
echo use \"git gerrit-push\" to push this new update branch back to itk.org.
