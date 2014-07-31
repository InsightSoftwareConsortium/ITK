#!/bin/bash
#
# UpdateMetaIOFromUpstream.sh
#
# See ITK/Modules/ThirdParty/VNL/src/README-ITK.txt
# for more commentary on how this update process works.
#
echo "-------------------------------------------------------"
echo This script will update source code for the MetaIO
echo library from github.com/Kitware/MetaIO
echo "-------------------------------------------------------"
if [ ! -f Modules/ThirdParty/MetaIO/License.txt ]
then
    echo The current working directory $(pwd) is not the top level
    echo of the ITK source code tree. Please change to the
    echo ITK source directory and re-run this script
    exit 1
fi

git_url="https://github.com/Kitware/MetaIO.git"

#
# Once the merge has been done
# EDIT THIS SCRIPT to change the hash tag at which to begin the
# next update...
#
# This merge done July 17, 2014
git branchMetaIO-upstream 23a51d63c519814a

#
# Make a temp directory to handle the import of the upstream source
mkdir MetaIO-Tmp
cd MetaIO-Tmp
#
# base a temporary git repo off of the parent ITK directory
git init
#
# pull in the upstream branch
git pull .. MetaIO-upstream
#
# empty out all existing source
rm -rf *
#
# download and copy the necessary double-conversion source
echo Cloning upstream HEAD from code.google.com
echo NOTE: to check out a particular revision for merging
echo you have to add a git checkout '<hash>'
echo or git checkout '<branchname>'
echo after the git clone command
git clone ${git_url}
#
# recover the upstream commit date.
cd MetaIO
upstream_date="$(git log -n 1 --format='%cd')"
upstream_sha="$(git rev-parse HEAD)"
cd ..

#
# Check to see if License.txt file changed -- if changed, fix in a separate
# commit.
if  diff MetaIO/License.txt ../Modules/ThirdParty/MetaIO/License.txt
then
    echo License.txt file unchanged
else
    echo The MetaIO License.txt file has changed.  Please
    echo add the change in a separate commit by hand and run this script again.
    exit 1
fi

cp -r MetaIO/src/* .
# get rid of MetaIO clone
rm -fr MetaIO
#
# add upstream files in Git
git add --all

#
# commit new source
GIT_AUTHOR_NAME='MetaIO Maintainers' \
GIT_AUTHOR_EMAIL='metaio@itk.org' \
GIT_AUTHOR_DATE="${upstream_date}" \
git commit -q -m "MetaIO (reduced)

This corresponds to commit hash
  ${upstream_sha}
from upstream repository
  ${git_url}"

#
# push to theMetaIO-upstream branch in the
# ITK tree
git push .. HEAD:MetaIO-upstream
cd ..
#
# get rid of temporary repository
rm -fr MetaIO-Tmp
#
# checkout a new update branch off of the master.
git checkout -b MetaIO-update master
#
# use subtree merge to bring in upstream changes
git merge -s recursive -X subtree=Modules/ThirdParty/MetaIO/src/MetaIO MetaIO-upstream
echo "---------------------------------"
echo NOW Fix all conflicts and test new code.
echo "---------------------------------"
echo Commit the merged/fixed/verified code and run this command
echo git rev-parse --short=16 MetaIO-upstream
echo "---------------------------------"
echo to get the commit hash from which theMetaIO-upstream
echo branch must be started on the next update.
echo "---------------------------------"
echo edit the line \"git branchMetaIO-upstream\" above.
echo Once you have commited this change to the UpdateMetaIOFromUpstream.sh script,
echo use \"git gerrit-push\" to push this new update branch back to itk.org.
